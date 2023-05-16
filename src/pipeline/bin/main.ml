open Cmdliner

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

let label l t =
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

let pipeline ?auth token config builder engine_config slack =
  let _channel = Some (read_channel_uri slack) in
  let _web_ui =
    let base = uri in
    fun repo -> Uri.with_query' base [ ("repo", repo) ]
  in
  let others, wlts =
    List.fold_left
      (fun (others, wlt) v ->
        match String.split_on_char '_' v with
        | "WLT" :: _ -> (others, v :: wlt)
        | _ -> (v :: others, wlt))
      ([], []) config.Config.projects
  in
  let pipeline () =
    let commit = Evaluations.Repos.evaluations token in
    let spec = Current.return Evaluations.Python.spec in
    let img =
      Current_obuilder.build ~label:"tmf" spec builder (`Git commit)
    in
    let others =
      List.map
        (fun project_name ->
          Current.ignore_value
          @@ Evaluations.evaluate ~project_name ~builder img)
        others
    in
    let wlts =
      List.map
        (fun project_name ->
          Current.ignore_value
          @@ Evaluations.evaluate ~project_name ~builder img)
        wlts
    in
    let wlts =
      let input = label "WLT Projects" img in
      let _, v =
        Current.collapse_list ~key:"projects" ~value:"wlt" ~input wlts
      in
      v
    in
    Current.all (others @ [ wlts ])
  in
  let custom_css = "custom.css" in
  let engine = Current.Engine.create ~config:engine_config pipeline in
  (* Extra routes (in addtion to the Engine's) for authentication and webhooks. *)
  let routes =
    Web.static_routes custom_css
    @ [
        Routes.((s "login" /? nil) @--> Current_github.Auth.login auth)
        (* Routes.(
           (s "webhooks" / s "github" /? nil)
           @--> Current_github.webhook ~engine ~webhook_secret
                  ~get_job_ids:Index.get_job_ids); *);
      ]
  in
  (* let authn = Current_github.Auth.make_login_uri (Option.get auth) in *)
  let site =
    Current_web.Site.v ~custom_css ~has_role ~name:"4c-evaluations"
      (routes @ Current_web.routes engine)
  in
  (engine, site)

(* HACK: For cloning private submodules, the deployer is given a
   https://<user>:<token>@github.com URL to set globally... *)

let init_git token_url =
  let token_path = Fpath.v token_url in
  assert (Bos.OS.File.exists token_path |> Result.get_ok);
  match
    Bos.OS.Cmd.run
      Bos.Cmd.(v "git" % "config" % "--global" % "credential.helper" % "store")
  with
  | Error (`Msg m) -> failwith m
  | Ok () ->
      let home = Sys.getenv "HOME" in
      let path = Fpath.(v home / ".git-credentials") in
      Bos.OS.File.write path (Bos.OS.File.read token_path |> Result.get_ok)

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
  let main () config auth (store : Obuilder.Store_spec.store Lwt.t) sandbox engine_config mode token slack =
    match init_git token with
    | Error (`Msg m) -> failwith m
    | Ok () -> (
        Logs.info (fun f -> f "Successfully set credentials");
        let builder =
          let open Lwt.Infix in
          store >>= fun (Store ((module Store), store)) ->
          Obuilder.Sandbox.create ~state_dir:"obuilder-state" sandbox
          >>= fun sandbox ->
          let module Builder =
            Obuilder.Builder (Store) (Obuilder.Sandbox) (Obuilder.Docker)
          in
          Lwt.return
          @@ Current_obuilder.Builder
               ((module Builder), Builder.v ~store ~sandbox)
        in
        let builder = Lwt_main.run builder in
        let engine, site = pipeline ?auth token config builder engine_config slack in
        match
          Lwt_main.run
            (Lwt.choose
               [ Current.Engine.thread engine; Current_web.run ~mode site ])
        with
        | Ok s -> print_endline s
        | Error (`Msg m) -> failwith m)
  in
  Cmd.v
    (Cmd.info "tmf-pipeline" ~doc)
    Term.(
      const main $ Common.setup_log $ Config.cmdliner
      $ Current_github.Auth.cmdliner $ store $ Obuilder.Sandbox.cmdliner
      $ Current.Config.cmdliner $ Current_web.cmdliner $ token_url $ slack)

let () = Cmd.(exit @@ eval cmd)
