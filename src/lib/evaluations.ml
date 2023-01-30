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
        copy ~from:`Context [ "requirements.txt" ] ~dst:"./";
        run ~network:[ "host" ] "pip install --no-cache-dir -r requirements.txt";
        copy ~from:`Context [ "." ] ~dst:"./";
      ]

  let run ?label ?(args = []) ?script_path ~builder snapshot =
    Current_obuilder.run ?label builder ~snapshot
      ~cmd:(String.concat " " (("python" :: Option.to_list script_path) @ args))
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let evaluations user =
    Git.clone ~credentials:(`User user) ~schedule ~gref:"pf341-automate"
      "https://github.com/carboncredits/4C_evaluations"
end

(* Generating the configuration file, we can then use this to cache
   each individual step in the pipeline *)
module Config = struct
  let generate ~project_name img =
    Python.run
      ~label:("config " ^ String.lowercase_ascii project_name)
      ~script_path:"eval.py" ~args:[ "config"; project_name ] img
end

let evaluate ~project_name ~builder img =
  let python_run = Python.run ~builder in
  Config.generate ~project_name ~builder img
  |> python_run
       ~label:("polygons " ^ String.lowercase_ascii project_name)
       ~args:[ "-c"; "print('HAHAHA todo...')" ]
  |> python_run
       ~label:("points " ^ String.lowercase_ascii project_name)
       ~args:[ "-c"; "print('HAHAHA todo...')" ]
  |> python_run
       ~label:("landcover " ^ String.lowercase_ascii project_name)
       ~args:[ "-c"; "print('HAHAHA todo...')" ]
