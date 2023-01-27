type t = {
  prune_threshold : float;
  build_pool_size : int;
  build_timeout : int;
  projects : string list;
}

let v prune_threshold build_pool_size build_timeout projects =
  { prune_threshold; build_pool_size; build_timeout; projects }

open Cmdliner

let config =
  Arg.value
  @@ Arg.opt Arg.string "evaluations.json"
  @@ Arg.info ~doc:"Evaluation pipeline configuration" ~docv:"CONFIG"
       [ "config" ]

module U = Yojson.Safe.Util

let ( / ) v s = U.member s v |> function `Null -> None | v -> Some v

let config_of_json yojson =
  let prune_threshold =
    yojson / "prune_threshold" |> Option.map U.to_float
    |> Option.value ~default:50.
  in
  let build_pool_size =
    yojson / "build_pool_size" |> Option.map U.to_int |> Option.value ~default:2
  in
  let build_timeout =
    yojson / "build_timeout" |> Option.map U.to_int |> Option.value ~default:120
  in
  let projects =
    yojson / "projects" |> Option.get |> U.to_list |> List.map U.to_string
  in
  v prune_threshold build_pool_size build_timeout projects

let v_file_path path =
  let content =
    In_channel.with_open_text path @@ fun ic -> Yojson.Safe.from_channel ic
  in
  config_of_json content

let cmdliner = Term.(const v_file_path $ config)

let password_path =
  let open Fpath in
  let root = v (if Sys.win32 then "C:\\ProgramData\\Docker" else "/run") in
  root / "secrets" / "carboncredits-hub" |> to_string

let auth =
  if Sys.file_exists password_path then (
    let ch = open_in_bin password_path in
    let len = in_channel_length ch in
    let password = really_input_string ch len |> String.trim in
    close_in ch;
    Some ("carboncredits", password))
  else None
