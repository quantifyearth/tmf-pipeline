open Lwt.Syntax
open Cmdliner

let cap =
  Arg.required
  @@ Arg.pos 0 Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The engine.cap file." ~docv:"CAP" []

let main uri =
  let run =
    let* engine = Hoke.engine uri in
    Hoke.ui engine
  in
  Lwt_main.run run

let () =
  let info = Cmd.info "hoke" ~doc:"Have a hoke around some builds" in
  exit @@ Cmd.eval @@ Cmd.v info Term.(const main $ cap)
