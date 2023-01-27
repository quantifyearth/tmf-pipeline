open Cmdliner

let init style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let setup_log =
  Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())
