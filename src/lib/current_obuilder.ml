module Git = Current_git

module Raw = struct
  module Build = struct
    type t = {
      pool : unit Current.Pool.t option;
      timeout : Duration.t option;
      level : Current.Level.t option;
      store : [ `Btrfs of string | `Rsync of string | `Zfs of string ];
      sandbox : string * Obuilder.Sandbox.config;
    }

    module Key = struct
      type t = {
        spec : Obuilder_spec.t;
        source : [ `No_context | `Git of Current_git.Commit.t | `Dir of Fpath.t ];
      }

      let source_to_json = function
        | `No_context -> `Null
        | `Git commit -> `String (Current_git.Commit.hash commit)
        | `Dir path -> `String (Fpath.to_string path)

      let to_json t =
        `Assoc
          [
            ( "spec",
              `String (Obuilder_spec.sexp_of_t t.spec |> Sexplib0.Sexp.to_string)
            );
            ("source", source_to_json t.source);
          ]

      let digest t = Yojson.Safe.to_string (to_json t)
      let pp f t = Yojson.Safe.pretty_print f (to_json t)
    end

    module Value = Current.String

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

    let build { store; sandbox = state_dir, config; timeout; pool; level }
        (job : Current.Job.t) (k : Key.t) : Value.t Current.or_error Lwt.t =
      let open Lwt.Infix in
      let level = Option.value level ~default:Current.Level.Average in
      Current.Job.start ?timeout ?pool job ~level >>= fun () ->
      with_context ~job k.source @@ fun dir ->
      Obuilder.Store_spec.to_store Obuilder.Rsync_store.Hardlink store
      >>= fun (Store ((module Store), store)) ->
      Obuilder.Sandbox.create ~state_dir config >>= fun sandbox ->
      let module Builder =
        Obuilder.Builder (Store) (Obuilder.Sandbox) (Obuilder.Docker)
      in
      let builder = Builder.v ~store ~sandbox in
      let ctx =
        Obuilder.Context.v ~src_dir:(Fpath.to_string dir) ~log:(job_logger job)
          ()
      in
      Builder.build builder ctx k.spec >>= function
      | Error `Cancelled -> Lwt.return (Error (`Msg "Cancelled"))
      | Error (`Msg _) as e -> Lwt.return e
      | Ok _ as v -> Lwt.return v

    let pp = Key.pp
    let auto_cancel = true
  end

  module BuildC = Current_cache.Make (Build)

  let build ?pool ?timeout ?level ?schedule store sandbox spec source =
    let key = Build.Key.{ spec; source } in
    let ctx = Build.{ pool; timeout; level; store; sandbox } in
    BuildC.get ?schedule ctx key
end

let pp_sp_label = Fmt.(option (sp ++ string))

let get_build_context = function
  | `No_context -> Current.return `No_context
  | `Git commit -> Current.map (fun x -> `Git x) commit
  | `Dir path -> Current.map (fun path -> `Dir path) path

let build ?level ?schedule ?label ?pool store sandbox spec src =
  let open Current.Syntax in
  Current.component "build%a" pp_sp_label label
  |> let> commit = get_build_context src and> spec in
     Raw.build ?pool ?level ?schedule store sandbox spec commit
