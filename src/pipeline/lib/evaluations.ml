(* Very niche ocurrent plugins for working directly
   with the 4C evaluations repository *)
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder

let arkdir = "/data"
let wdir = "/usr/src/app"
let ( / ) = Filename.concat

module Python = struct
  let spec =
    let open Obuilder_spec in
    stage ~from:"python:3.10-bullseye"
      [
        workdir wdir;
        copy ~from:`Context [ "requirements.txt" ] ~dst:"./";
        run ~network:[ "host" ] "pip install --no-cache-dir -r requirements.txt";
        run "mkdir ./data";
        copy ~from:`Context [ "." ] ~dst:"./";
      ]

  let run ?label ?(args = []) ?script_path ?rom ~builder snapshot =
    Current_obuilder.run ?label builder ~snapshot ?rom
      (String.concat " " (("python" :: Option.to_list script_path) @ args))
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let evaluations _token =
    Git.clone ~gref:"main" ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"
end

(* Generating the configuration file, we can then use this to cache
   each individual step in the pipeline *)
module Setup = struct
  let generate ~project_name img =
    Python.run
      ~label:("setup " ^ String.lowercase_ascii project_name)
      ~args:[] img
end

let snapshots_to_rom
    (lst :
      (Current_obuilder.Raw.Build.Value.t Current.t * string * string) list) =
  List.map
    (fun (v, build_dir, target) ->
      let+ (snap : Current_obuilder.Raw.Build.Value.t) = v in
      Obuilder_spec.Rom.of_build ~hash:snap.snapshot ~build_dir target)
    lst

let evaluate ~project_name ~store:_ ~builder img =
  let python_run = Python.run ~builder in
  let additionality =
    python_run
      ~label:("additionality " ^ String.lowercase_ascii project_name)
      ~script_path:"main.py"
      ~args:[ "--method"; "additionality" ]
      img
  in
  let leakage =
    python_run
      ~label:("leakage " ^ String.lowercase_ascii project_name)
      ~script_path:"main.py" ~args:[ "--method"; "leakage" ] img
  in
  let permanence =
    let rom =
      Current.list_seq
        [
          Current.map (fun v -> (wdir / "data", wdir, v)) additionality;
          Current.map (fun v -> (wdir / "data", wdir, v)) leakage;
        ]
    in
    python_run ~rom
      ~label:("permanence " ^ String.lowercase_ascii project_name)
      ~script_path:"main.py"
      ~args:[ "--method"; "permanence" ]
      img
  in
  permanence
