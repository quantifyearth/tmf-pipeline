open! Import
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile

let arkdir = "/data"
let wdir = "/home/tmf/app"

let data_spec =
  let open Obuilder_spec in
  (* Use any old small image, but because of obuilder we can't use scratch
     which would be ideal. *)
  stage ~from:"alpine:3.14" [ workdir arkdir; copy [ "." ] ~dst:arkdir ]

module Python = struct
  (* TODO: This should be based on the CI Dockerfile! *)
  let spec =
    let open Obuilder_spec in
    stage ~from:"ghcr.io/osgeo/gdal:ubuntu-small-3.6.4"
      [
        run ~network:[ "host" ]
          "apt-get update -qqy && apt-get install -qy git libpq-dev \
           python3-pip  && rm -rf /var/lib/apt/lists/* && rm -rf \
           /var/cache/apt/*";
        run "useradd -ms /bin/bash -u 1000 tmf";
        workdir wdir;
        run "chown -R tmf:tmf /home/tmf";
        Obuilder_spec.user_unix ~uid:1000 ~gid:1000;
        run "pip install --upgrade pip";
        run "pip install numpy";
        run "pip install gdal[numpy]==3.6.4";
        copy ~from:`Context [ "requirements.txt" ] ~dst:"./";
        run ~network:[ "host" ] "pip install --no-cache-dir -r requirements.txt";
        run "whoami && ls -la . && mkdir ./data";
        copy ~from:`Context [ "." ] ~dst:"./";
      ]

  let run ?label ?(args = []) ?script_path ?rom ?extra_files ~builder snapshot =
    Current_obuilder.run ?label builder ~snapshot ?rom ?extra_files
      (String.concat " " (("python" :: Option.to_list script_path) @ args))
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let tmf_implementation () =
    Git.clone ~gref:"main" ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"

  let tmf_data () =
    Git.clone ~gref:"main" ~schedule "git@github.com:carboncredits/tmf-data.git"
end

let snapshots_to_rom
    (lst :
      (Current_obuilder.Raw.Build.Value.t Current.t * string * string) list) =
  List.map
    (fun (v, build_dir, target) ->
      let+ (snap : Current_obuilder.Raw.Build.Value.t) = v in
      Obuilder_spec.Rom.of_build ~hash:snap.snapshot ~build_dir target)
    lst

let evaluate ~project_name ~builder ~config_img img =
  let python_run = Python.run ~builder in
  let buffer =
    let rom =
      Current.list_seq [ Current.map (fun v -> (arkdir, wdir, v)) config_img ]
    in
    python_run ~rom ~label:"buffer"
      ~script_path:"./methods/inputs/generate_boundary.py"
      ~args:[ "data/" ^ project_name; "data/output.geojson" ]
      img
  in
  let additionality =
    let rom =
      Current.list_seq
        [ Current.map (fun v -> (wdir / "data", wdir, v)) buffer ]
    in
    python_run ~rom ~label:"additionality" ~script_path:"main.py"
      ~args:[ "--method"; "additionality" ]
      img
  in
  let leakage =
    let rom =
      Current.list_seq
        [ Current.map (fun v -> (wdir / "data", wdir, v)) buffer ]
    in
    python_run ~rom ~label:"leakage" ~script_path:"main.py"
      ~args:[ "--method"; "leakage" ] img
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
  Current.ignore_value permanence
