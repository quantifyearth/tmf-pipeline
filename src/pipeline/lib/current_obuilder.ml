module Git = Current_git

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

type store = Obuilder.Store_spec.store

let ( / ) = Filename.concat

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

      let source_hash = function
        | `No_context -> `Null
        | `Git commit ->
            `Assoc [ ("git", `String (Current_git.Commit.hash commit)) ]

      let source_to_json = function
        | `No_context -> `Null
        | `Git commit ->
            `Assoc [ ("git", Current_git.Commit.to_yojson commit) ]

      let source_of_json = function
        | `Null -> `No_context
        | `Assoc [ ("git", git) ] ->
             let git = Current_git.Commit.of_yojson git |> Result.get_ok in
            `Git git
        | _ -> failwith "Unknown context for obuilder"

      let to_json ~source t =
        let spec = Obuilder_spec.sexp_of_t t.spec |> Sexplib.Sexp.to_string in
        `Assoc
          [
            ("spec", `String spec);
            ("spec_digest", `String (Digest.string spec |> Digest.to_hex));
            ("source", source t.source);
          ]

      let of_json = function
        | `Assoc [ ("spec", `String spec); ("spec_digest", _); ("source", v) ]
          ->
            let spec = Sexplib.Sexp.of_string spec |> Obuilder_spec.t_of_sexp in
            let source = source_of_json v in
            { spec; source }
        | _ -> failwith "Failed to unmarshal "

      let digest t = Yojson.Safe.to_string (to_json ~source:source_hash t)
      let pp f t = Yojson.Safe.pretty_print f (to_json ~source:source_hash t)
    end

    module Value = struct
      type t = { ctx : Key.t; snapshot : string }

      let to_yojson t =
        `Assoc
          [
            ("snapshot", `String t.snapshot);
            ("input", Key.to_json ~source:Key.source_to_json t.ctx);
          ]

      let marshal t = to_yojson t |> Yojson.Safe.to_string

      let of_yojson = function
        | `Assoc [ ("snapshot", `String snapshot); ("input", s) ] ->
            let ctx = Key.of_json s in
            Ok { snapshot; ctx }
        | _ -> Error "Failed to unmarshal obuilder output"

      let unmarshal s = Yojson.Safe.from_string s |> of_yojson |> Result.get_ok
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
      Current.Job.log job "Obuilder spec(%s): %a"
        (Digest.to_hex @@ Digest.string @@ Key.digest k)
        Obuilder_spec.pp k.spec;
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

  module Read = struct
    type t = {
      pool : unit Current.Pool.t option;
      timeout : Duration.t option;
      level : Current.Level.t option;
      store : store;
    }

    module Key = struct
      type t = { id : Build.Value.t; files : string list } [@@deriving yojson]

      let digest t = to_yojson t |> Yojson.Safe.to_string
      let pp f t = Yojson.Safe.pretty_print f (to_yojson t)
    end

    module Value = struct
      type t = { id : string; files : (string * string option) list }
      [@@deriving yojson]

      let marshal t = to_yojson t |> Yojson.Safe.to_string
      let unmarshal s = Yojson.Safe.from_string s |> of_yojson |> Result.get_ok
    end

    let contents (Obuilder.Store_spec.Store ((module S), v)) ~snapshot ~path =
      let open Lwt.Infix in
      S.result v snapshot.Build.Value.snapshot >>= function
      | None -> Lwt.return_none
      | Some dir -> (
          let path = dir / "rootfs" / path |> Fpath.v in
          match Bos.OS.File.read path with
          | Ok c -> Lwt.return (Some c)
          | _ -> Lwt.return_none)

    let id = "obuilder-read"

    let build { store; timeout; pool; level } (job : Current.Job.t) (k : Key.t)
        : Value.t Current.or_error Lwt.t =
      let open Lwt.Infix in
      let level = Option.value level ~default:Current.Level.Average in
      Current.Job.start ?timeout ?pool job ~level >>= fun () ->
      Lwt_list.map_p
        (fun path -> contents store ~snapshot:k.id ~path >|= fun c -> (path, c))
        k.files
      >>= fun lst ->
      let t = { id = k.id.snapshot; Value.files = lst } in
      Lwt_result.return t

    let pp = Key.pp
    let auto_cancel = true
  end

  module ReadC = Current_cache.Make (Read)

  let contents ?pool ?timeout ?level ?schedule store snapshot files =
    let key = Read.Key.{ files; id = snapshot } in
    let ctx = Read.{ pool; timeout; level; store } in
    ReadC.get ?schedule ctx key
end

let pp_sp_label = Fmt.(option (sp ++ string))

let get_build_context = function
  | `No_context -> Current.return `No_context
  | `Git commit -> Current.map (fun x -> `Git x) commit

let build ?level ?schedule ?label ?pool spec builder src =
  let open Current.Syntax in
  Current.component "build%a" pp_sp_label label
  |> let> commit = get_build_context src in
     Raw.build ?pool ?level ?schedule builder spec commit

let run ?level ?schedule ?label ?pool builder
    ?(rom : (string * string * Raw.Build.Value.t) list Current.t option) ?(extra_files : (string * string) list Current.t option) ?env
    ~snapshot cmd =
  let open Current.Syntax in
  Current.component "run%a" pp_sp_label label
  |> let> (snapshot : Raw.Build.Value.t) = snapshot
     and> rom = Current.option_seq rom
     and> _extra_files = Current.option_seq extra_files in
     let spec = snapshot.ctx.spec in
     let env =
       match env with Some (k, v) -> [ Obuilder_spec.env k v ] | None -> []
     in
     let rom, symlinks =
       match rom with
       | None -> ([], [])
       | Some vs ->
           let roms =
             List.map
               (fun (build_dir, _, snap) ->
                 Obuilder_spec.Rom.of_build ~hash:snap.Raw.Build.Value.snapshot
                   ~build_dir ("/data" / snap.snapshot))
               vs
           in
           ( roms,
             List.map2
               (fun (_, wdir, snap) rom ->
                 Obuilder_spec.run ~rom:[ rom ] "ln -s %s/* %s/data/"
                   ("/data" / snap.Raw.Build.Value.snapshot)
                   wdir)
               vs roms )
     in
     let spec =
       Obuilder_spec.stage ~child_builds:spec.child_builds ~from:spec.from
         (spec.ops @ env @ symlinks @ [ Obuilder_spec.run ~rom "%s" cmd ])
     in
     Raw.build ?pool ?level ?schedule builder spec snapshot.ctx.source

let contents ?level ?schedule ?label ?pool ~snapshot store files =
  let open Current.Syntax in
  Current.component "read%a" pp_sp_label label
  |> let> snapshot = snapshot in
     Raw.contents ?pool ?level ?schedule store snapshot files
