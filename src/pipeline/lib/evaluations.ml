(* Very niche ocurrent plugins for working directly
   with the 4C evaluations repository *)
open Current.Syntax
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

  let evaluations token =
    Git.clone ~credentials:(`Token token) ~gref:"main" ~schedule "https://github.com/carboncredits/tmf-implementation"
end

(* Generating the configuration file, we can then use this to cache
   each individual step in the pipeline *)
module Setup = struct
  let generate ~project_name img =
    Python.run
      ~label:("setup " ^ String.lowercase_ascii project_name)
      ~script_path:"main.py" ~args:[] img
end

let merge_builds a b =
  let* a and* _ = b in
  Current.return a

let evaluate ~project_name ~builder img =
  let python_run = Python.run ~builder in
  let setup = Setup.generate ~project_name ~builder img in
  let additionality =
    python_run
       ~label:("additionality " ^ String.lowercase_ascii project_name)
       ~args:[ "-c"; "print('HAHAHA todo...')" ] setup
  in
  let leakage =
    python_run
       ~label:("leakage " ^ String.lowercase_ascii project_name)
       ~args:[ "-c"; "print('HAHAHA todo...')" ] setup
  in
  let permanence =
    python_run
        ~label:("additionality " ^ String.lowercase_ascii project_name)
        ~args:[ "-c"; "print('HAHAHA todo...')" ] (merge_builds additionality leakage)
  in
  permanence

