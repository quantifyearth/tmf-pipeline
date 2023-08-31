open Cmdliner
module Rpc = Current_rpc.Impl (Current)

let uri = Uri.of_string "https://pipeline.quantify.earth"

(* [has_role] is provided to the web frontend to allow it to dynamically
   decide based on the user and the role whether they are permitted that
   kind of access. If [user] is [None] this represents a user without
   authentication. *)
let has_role user role =
  match user with
  | None ->
      (* role = `Viewer || role = `Monitor *)
      true
      (* Without authentication we allow users just to view things *)
  | Some user -> (
      match (Current_web.User.id user, role) with
      | ( ( "github:avsm" | "github:sadiqj" | "github:patricoferris"
          | "github:mdales" ),
          _ ) ->
          true (* These users have all roles *)
      | _ -> role = `Viewer)

let _label l t =
  let open Current.Syntax in
  Current.component "%s" l
  |> let> v = t in
     Current.Primitive.const v

let read_first_line path =
  let ch = open_in path in
  Fun.protect (fun () -> input_line ch) ~finally:(fun () -> close_in ch)

let read_channel_uri path =
  try
    let uri = read_first_line path in
    Current_slack.channel (Uri.of_string (String.trim uri))
  with ex ->
    Fmt.failwith "Failed to read slack URI from %S: %a" path Fmt.exn ex

module Current_obuilder = Evaluations.Current_obuilder

let combine_projects_and_configuration projects config =
  let find_project (config : Evaluations.Config.t) projects =
    let find project =
      let fname = Fpath.basename project |> Filename.chop_extension in
      String.equal (string_of_int config.vcs_id) fname
    in
    let project = List.find_opt find projects in
    Option.map (fun p -> (p, config)) project
  in
  find_project config projects

let pipeline ?auth _token _config _store builder engine_config slack =
  let open Current.Syntax in
  let _channel = Some (read_channel_uri slack) in
  let _web_ui =
    let base = uri in
    fun repo -> Uri.with_query' base [ ("repo", repo) ]
  in
  let pipeline () =
    let open Evaluations in
    (* To avoid recalculating too much we pin each subdirectory to a specific SHA
       so we don't needlessly redownload. If the workflow works well we may wish to
       use branches upstream that we push to. *)
    let tmf_inputs =
      Evaluations.Repos.tmf_implementation
        "e2c5c23e9fcd271f3899f56f192d79cd7c28684c"
    in
    let tmf_matching =
      Evaluations.Repos.tmf_implementation
        "43609ab19848c62e2a278c4b423643ac313cd616"
    in
    let tmf_outputs =
      Evaluations.Repos.tmf_implementation
        "ba2d0ef283dfdfd63614545eba431d07cf5881ca"
    in
    (* Control the number of obuilder jobs that can run in parallel *)
    let pool = Current.Pool.create ~label:"obuilder" 1 in
    let inputs =
      Current_obuilder.build ~pool ~label:"tmf-inputs"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_inputs)
    in
    let outputs =
      Current_obuilder.build ~pool ~label:"tmf-outputs"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_outputs)
    in
    let matching =
      Current_obuilder.build ~pool ~label:"tmf-matching"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_matching)
    in
    let data = Evaluations.Repos.tmf_data () in
    let projects_dir = Current_gitfile.directory data (Fpath.v "projects") in
    let configurations =
      let+ configs =
        Current_gitfile.directory_contents data (Fpath.v "configurations")
      and+ projects = projects_dir in
      let map_config (_, c) =
        match Evaluations.Config.of_yojson (Yojson.Safe.from_string c) with
        | Error m ->
            Logs.err (fun f -> f "Error: %s (%s)" m c);
            None
        | Ok c -> combine_projects_and_configuration projects.files c
      in
      List.filter_map map_config configs
    in
    let others =
      Current.component "Evaluate Projects"
      |> let** projects_dir = projects_dir
         and* configurations = configurations in
         (* TODO: Make this a parameter so we can run the pipeline but not for ALL projects. *)
         let projects = configurations in
         let evals =
           List.map
             (fun (project_name, project_config) ->
               Evaluations.evaluate ~pool ~projects_dir ~inputs ~outputs
                 ~matching
                 ~project_name:(Fpath.filename project_name)
                 ~builder project_config)
             projects
         in
         Current.all evals
    in
    others
  in
  let custom_css = "custom.css" in
  let engine = Current.Engine.create ~config:engine_config pipeline in
  (* Extra routes (in addtion to the Engine's) for authentication and webhooks. *)
  let store = Fpath.v "/obuilder-zfs" in
  (* TODO: not hardcoded *)
  let routes =
    Web.static_routes ~store ~engine builder custom_css
    @ [ Routes.((s "login" /? nil) @--> Current_github.Auth.login auth) ]
  in
  let site =
    Current_web.Site.v ~custom_css ~has_role ~name:"4c-evaluations"
      (routes @ Current_web.routes engine)
  in
  (engine, site)

let token_url =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A github <user>:<token> URL" ~docv:"GITHUB_TOKEN_FILE"
       [ "github-token-file" ]

let slack =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info
       ~doc:"A file containing the URI of the endpoint for status updates."
       ~docv:"URI-FILE" [ "slack" ]

let store = Obuilder.Store_spec.cmdliner

let cmd =
  let doc = "Deployer for 4C sites and projects" in
  let main () config auth (store : Obuilder.Store_spec.store Lwt.t) sandbox
      engine_config mode token slack capnp =
    Logs.info (fun f -> f "Successfully set credentials");
    let open Lwt.Infix in
    let builder =
      store >>= fun (Store ((module Store), store_v) as store) ->
      Obuilder.Sandbox.create ~state_dir:"obuilder-state" sandbox
      >>= fun sandbox ->
      let module Builder =
        Obuilder.Builder (Store) (Obuilder.Sandbox) (Obuilder.Docker)
      in
      Lwt.return
      @@ ( store,
           Current_obuilder.Builder
             ((module Builder), Builder.v ~store:store_v ~sandbox) )
    in
    let store, builder = Lwt_main.run builder in
    let engine, site =
      pipeline ?auth token config store builder engine_config slack
    in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id capnp "engine" in
    let restore =
      Capnp_rpc_net.Restorer.single service_id (Rpc.engine engine)
    in
    let main =
      Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
      let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
      let ch = open_out "engine.cap" in
      output_string ch (Uri.to_string uri ^ "\n");
      close_out ch;
      Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ]
    in
    match Lwt_main.run main with
    | Ok s -> print_endline s
    | Error (`Msg m) -> failwith m
  in
  Cmd.v
    (Cmd.info "tmf-pipeline" ~doc)
    Term.(
      const main $ Common.setup_log $ Config.cmdliner
      $ Current_github.Auth.cmdliner $ store $ Obuilder.Sandbox.cmdliner
      $ Current.Config.cmdliner $ Current_web.cmdliner $ token_url $ slack
      $ Capnp_rpc_unix.Vat_config.cmd)

let () = Cmd.(exit @@ eval cmd)
