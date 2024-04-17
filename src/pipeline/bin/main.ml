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

let pipeline ?auth builder engine_config slack project_ids =
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
        "53d8ed3db59435b8c8db42fda95fe58e93e4b6f7"
    in
    (* Changing this hash will recompute (i.e. download) the JRC dataset again. Proceed
       with caution. *)
    let jrc_input =
      Evaluations.Repos.tmf_implementation
        "e2c5c23e9fcd271f3899f56f192d79cd7c28684c"
    in
    (* Changing this hash will recompute the fine circular coverage (FCC) values which takes
       time and lots of disk space. Proceed with caution. *)
    let tmf_matching =
      Evaluations.Repos.tmf_implementation
        "fa0906da430185f5c93ffca56a391c089b6dc991"
    in
    (* Changing this hash will recompute potential matches and find pairs. *)
    let tmf_matching_post_fcc =
      Evaluations.Repos.tmf_implementation
        (* "32ae2b5d326e1e07b94999bedab2ceda05e8441c" *)
        "552616d6e41526c631ea8ce43ed1b59ca356241e"
    in
    let elevation_fix =
      Evaluations.Repos.tmf_implementation
        "e20a685f2952727e7d2f9e962428b64988ca0f19"
    in
    (* Changing this hash impacts additionality, leakage and permanence. *)
    let tmf_outputs =
      Evaluations.Repos.tmf_implementation
        "ac9ae25aac095827be8e5740774eb9737ece57bd"
    in
    (* Control the number of obuilder jobs that can run in parallel *)
    let pool = Current.Pool.create ~label:"obuilder" 1 in
    let inputs =
      Current_obuilder.build ~pool ~label:"tmf-inputs"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_inputs)
    in
    let jrc_input =
      Current_obuilder.build ~pool ~label:"tmf-jrc-input"
        Evaluations.Python.spec_with_data_dir builder (`Git jrc_input)
    in
    let outputs =
      Current_obuilder.build ~pool ~label:"tmf-outputs"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_outputs)
    in
    let matching =
      Current_obuilder.build ~pool ~label:"tmf-matching-fcc"
        Evaluations.Python.spec_with_data_dir builder (`Git tmf_matching)
    in
    let matching_post_fcc =
      Current_obuilder.build ~pool ~label:"tmf-matching-post-fcc"
        Evaluations.Python.spec_with_data_dir builder
        (`Git tmf_matching_post_fcc)
    in
    let _elevation_fix = 
      Current_obuilder.build ~pool ~label:"tmf-elevation-fix"
      Evaluations.Python.spec_with_data_dir builder
      (`Git elevation_fix)
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
         let projects = configurations in
         let projects =
           List.filter
             (fun (_, (c : Evaluations.Config.t)) -> List.mem c.vcs_id project_ids)
             projects
         in
         let evals =
           List.map
             (fun (project_name, (project_config : Evaluations.Config.t)) ->
              string_of_int project_config.vcs_id,
               Evaluations.evaluate ~pool ~projects_dir ~inputs ~outputs
                 ~matching ~jrc_input ~matching_post_fcc
                 ~project_name:(Fpath.filename project_name)
                 ~builder project_config)
             projects
         in
         let evals = List.fold_left (fun acc (_, (add, copy)) -> (Current.ignore_value add) :: (Current.ignore_value copy) :: acc) [] evals in 
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

let slack =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info
       ~doc:"A file containing the URI of the endpoint for status updates."
       ~docv:"URI-FILE" [ "slack" ]

let project_ids =
  Arg.required
  @@ Arg.opt Arg.(some (list int)) (Some [ 1201 ])
  @@ Arg.info
       ~doc:
         "A list of project IDs to run separated by commas. Defaults to 1201."
       ~docv:"PROJECT-IDS" [ "project-ids" ]

let store = Obuilder.Store_spec.cmdliner

let cmd =
  let doc = "Deployer for 4C sites and projects" in
  let main () auth (store : Obuilder.Store_spec.store Lwt.t) sandbox
      engine_config mode slack project_ids capnp =
    Logs.info (fun f -> f "Successfully set credentials");
    let open Lwt.Infix in
    let builder =
      store >>= fun (Store ((module Store), store_v)) ->
      Obuilder.Sandbox.create ~state_dir:"obuilder-state" sandbox
      >>= fun sandbox ->
      let module Builder =
        Obuilder.Builder (Store) (Obuilder.Sandbox) (Obuilder.Docker)
      in
      Lwt.return
      @@ Current_obuilder.Builder
           ((module Builder), Builder.v ~store:store_v ~sandbox)
    in
    let builder = Lwt_main.run builder in
    let engine, site = pipeline ?auth builder engine_config slack project_ids in
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
      (* Setup hoke daemon, this allows us to use capnp endpoints to send specifications
         to build by the underlying obuilder instance or to hook into a shell of an existing
         image. Quite experimental, but can be useful. *)
      let daemon capnp =
        let builder =
          let (Current_obuilder.Builder ((module B), v)) = builder in
          Hoke.Builder.Builder ((module B), v)
        in
        let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
        let load ~validate:_ ~sturdy_ref =
          let sr = Capnp_rpc_lwt.Sturdy_ref.cast sturdy_ref in
          Capnp_rpc_net.Restorer.grant (Hoke.Client.v ~sr builder) |> Lwt.return
        in
        let loader = Hoke_cmd.Store.create ~make_sturdy ~load "hoke.index" in
        let services =
          Capnp_rpc_net.Restorer.Table.of_loader (module Hoke_cmd.Store) loader
        in
        Hoke_cmd.Server.daemon capnp services builder loader.store "./secrets"
      in
      Lwt.choose
        [
          Current.Engine.thread engine; Current_web.run ~mode site; daemon capnp;
        ]
    in
    match Lwt_main.run main with
    | Ok s -> print_endline s
    | Error (`Msg m) -> failwith m
  in
  Cmd.v
    (Cmd.info "tmf-pipeline" ~doc)
    Term.(
      const main $ Common.setup_log $ Current_github.Auth.cmdliner $ store
      $ Obuilder.Sandbox.cmdliner $ Current.Config.cmdliner
      $ Current_web.cmdliner $ slack $ project_ids
      $ Capnp_rpc_unix.Vat_config.cmd)

let () = Cmd.(exit @@ eval cmd)
