(* Very niche ocurrent plugins for working directly
   with the 4C evaluations repository *)

module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder

module Python = struct
  let spec =
    let open Obuilder_spec in
    stage ~from:"python:3.10-bullseye"
      [
        workdir "/usr/src/app";
        copy ~from:`Context [ "requirements.txt" ] ~dst:".";
        run "pip install --no-cache-dir -r requirements.txt";
        copy ~from:`Context [ "." ] ~dst:".";
      ]
  (*
    let run ?label ?(args=[]) ?script_path img =
      D.run ?label img ~args:("python" :: (Option.to_list script_path) @ args) *)
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let evaluations () =
    Git.clone ~schedule ~gref:"pf341-automate"
      "https://github.com/carboncredits/4C_evaluations"
end
(*
  (* Generating the configuration file, we can then use this to cache
    each individual step in the pipeline *)
  module Config = struct
    let generate ~project_name img =
      Python.run ~label:("config " ^ String.lowercase_ascii project_name) ~script_path:"eval.py" ~args:[ "config"; project_name ] img
  end

  let evaluate ~project_name img =
    let open Current.Syntax in
    let v = Config.generate ~project_name img in
    let v =
      Current.component "polygons" |>
      let** () = v in
      Python.run ~label:("polygons " ^ String.lowercase_ascii project_name) ~args:[ "-c"; "print('HAHAHA todo...')" ] img
    in
    let v =
      Current.component "points" |>
      let** () = v in
      Python.run ~label:("points " ^ String.lowercase_ascii project_name) ~args:[ "-c"; "print('HAHAHA todo...')" ] img
    in
    let v =
      Current.component "landcover" |>
      let** () = v in
      Python.run ~label:("landcover " ^ String.lowercase_ascii project_name) ~args:[ "-c"; "print('HAHAHA todo...')" ] img
    in
    v *)
(* end *)
