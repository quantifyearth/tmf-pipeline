module Git = Current_git

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Raw = struct
  module Build = struct
    type t = {
      pool : unit Current.Pool.t option;
      timeout : Duration.t option;
      level : Current.Level.t option;
      builder : builder;
    }

    module Key = struct
      type t = {
        spec : Obuilder_spec.t;
        source : [ `No_context | `Git of Current_git.Commit.t ];
      }

      let source_to_json = function
        | `No_context -> `Null
        | `Git commit ->
            `Assoc [ ("git", `String (Current_git.Commit.marshal commit)) ]

      let source_of_json = function
        | `Null -> `No_context
        | `Assoc [ ("git", `String git) ] ->
            `Git (Current_git.Commit.unmarshal git)
        | _ -> failwith "Unknown context for obuilder"

      let to_json t =
        `Assoc
          [
            ( "spec",
              `String
                (Obuilder_spec.sexp_of_t t.spec |> Sexplib0.Sexp.to_string_hum)
            );
            ("source", source_to_json t.source);
          ]

      let of_json v =
        match Yojson.Safe.from_string v with
        | `Assoc [ ("spec", `String spec); ("source", v) ] ->
            let spec =
              Sexplib.Conv.sexp_of_string spec |> Obuilder_spec.t_of_sexp
            in
            let source = source_of_json v in
            { spec; source }
        | _ -> failwith "Failed to unmarshal "

      let digest t = Yojson.Safe.to_string (to_json t)
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = struct
      type t = { ctx : Key.t; snapshot : string }

      let marshal t =
        `Assoc
          [
            ("snapshot", `String t.snapshot);
            ("input", `String (Key.digest t.ctx));
          ]
        |> Yojson.Safe.to_string

      let unmarshal s =
        match Yojson.Safe.from_string s with
        | `Assoc [ ("snapshot", `String snapshot); ("input", `String s) ] ->
            let ctx = Key.of_json s in
            { snapshot; ctx }
        | _ -> failwith "Failed to unmarshal obuilder output"
    end

    let id = "obuilder-build"

    let with_context ~job context fn =
      let open Lwt_result.Infix in
      match context with
      | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
      | `Dir path ->
          Current.Process.with_tmpdir ~prefix:"build-context-" @@ fun dir ->
          Current.Process.exec ~cwd:dir ~cancellable:true ~job
            ("", [| "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." |])
          >>= fun () -> fn dir
      | `Git commit -> Current_git.with_checkout ~job commit fn

    let job_logger job tag msg =
      match tag with
      | `Heading ->
          Current.Job.log job "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
      | `Note ->
          Current.Job.log job "%a@." Fmt.(styled (`Fg `Yellow) string) msg
      | `Output -> Current.Job.log job "%s@." msg

    let build { builder; timeout; pool; level } (job : Current.Job.t)
        (k : Key.t) : Value.t Current.or_error Lwt.t =
      let open Lwt.Infix in
      let (Builder ((module B), builder)) = builder in
      let level = Option.value level ~default:Current.Level.Average in
      Current.Job.start ?timeout ?pool job ~level >>= fun () ->
      with_context ~job k.source @@ fun dir ->
      let ctx =
        Obuilder.Context.v ~src_dir:(Fpath.to_string dir) ~log:(job_logger job)
          ()
      in
      Current.Job.log job "OBUILDER SPEC: %a" Obuilder_spec.pp k.spec;
      B.build builder ctx k.spec >>= function
      | Error `Cancelled -> Lwt.return (Error (`Msg "Cancelled"))
      | Error (`Msg _) as e -> Lwt.return e
      | Ok snapshot -> Lwt.return (Ok { Value.snapshot; ctx = k })

    let pp = Key.pp
    let auto_cancel = true
  end

  module BuildC = Current_cache.Make (Build)

  let build ?pool ?timeout ?level ?schedule builder spec source =
    let key = Build.Key.{ spec; source } in
    let ctx = Build.{ pool; timeout; level; builder } in
    BuildC.get ?schedule ctx key
end

let pp_sp_label = Fmt.(option (sp ++ string))

let get_build_context = function
  | `No_context -> Current.return `No_context
  | `Git commit -> Current.map (fun x -> `Git x) commit

let build ?level ?schedule ?label ?pool spec builder src =
  let open Current.Syntax in
  Current.component "build%a" pp_sp_label label
  |> let> commit = get_build_context src and> spec = spec in
     Raw.build ?pool ?level ?schedule builder spec commit

let run ?level ?schedule ?label ?pool builder ?(rom = []) ~snapshot cmd =
  let open Current.Syntax in
  Current.component "run%a" pp_sp_label label
  |> let> (snapshot : Raw.Build.Value.t) = snapshot
     and> rom = Current.list_seq rom in
     let spec = snapshot.ctx.spec in
     let spec =
       Obuilder_spec.stage ~child_builds:spec.child_builds ~from:spec.from
         (spec.ops @ [ Obuilder_spec.run ~rom "%s" cmd ])
     in
     Raw.build ?pool ?level ?schedule builder spec snapshot.ctx.source
