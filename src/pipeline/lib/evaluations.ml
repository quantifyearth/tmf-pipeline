open! Import
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile

let arkdir = "/inputs"
let wdir = "/home/tmf/app"
let input_dir = "./inputs"
let output_dir = "./data"
let ( / ) = Filename.concat

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
          "apt-get update -qqy && apt-get install -qy git wget libpq-dev \
           python3-pip  && rm -rf /var/lib/apt/lists/* && rm -rf \
           /var/cache/apt/*";
        run "useradd -ms /bin/bash -u 1000 tmf";
        workdir wdir;
        run "chown -R tmf:tmf /home/tmf";
        Obuilder_spec.user_unix ~uid:1000 ~gid:1000;
        run ~network:[ "host" ] "pip install --upgrade pip";
        run ~network:[ "host" ] "pip install numpy";
        run ~network:[ "host" ] "pip install gdal[numpy]==3.6.4";
        copy ~from:`Context [ "requirements.txt" ] ~dst:"./";
        run ~network:[ "host" ] "pip install --no-cache-dir -r requirements.txt";
        run "whoami && ls -la . && mkdir ./inputs";
        run "echo 'verbose=off' > /home/tmf/.wgetrc";
        copy ~from:`Context [ "." ] ~dst:"./";
      ]

  let spec_with_data_dir =
    let open Obuilder_spec in
    stage ~from:"ghcr.io/osgeo/gdal:ubuntu-small-3.6.4"
      [
        run ~network:[ "host" ]
          "apt-get update -qqy && apt-get install -qy git wget libpq-dev \
           python3-pip  && rm -rf /var/lib/apt/lists/* && rm -rf \
           /var/cache/apt/*";
        run "useradd -ms /bin/bash -u 1000 tmf";
        workdir wdir;
        run "chown -R tmf:tmf /home/tmf";
        Obuilder_spec.user_unix ~uid:1000 ~gid:1000;
        run ~network:[ "host" ] "pip install --upgrade pip";
        run ~network:[ "host" ] "pip install numpy";
        run ~network:[ "host" ] "pip install gdal[numpy]==3.6.4";
        copy ~from:`Context [ "requirements.txt" ] ~dst:"./";
        run ~network:[ "host" ] "pip install --no-cache-dir -r requirements.txt";
        run "whoami && ls -la . && mkdir ./inputs && mkdir ./data";
        run "echo 'verbose=off' > /home/tmf/.wgetrc";
        copy ~from:`Context [ "." ] ~dst:"./";
      ]

  let run ?label ?(args = []) ?script_path ?rom ?extra_files ?env ?network
      ?secrets ~ctx_secrets ~builder snapshot =
    let ctx_secrets = [ ("earthdata", ctx_secrets) ] in
    Current_obuilder.run ?label builder ~snapshot ?rom ?extra_files ?env
      ?secrets ~ctx_secrets ?network
      (String.concat " " (("python" :: Option.to_list script_path) @ args))
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let tmf_implementation gref =
    Git.clone ~gref ~schedule
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

let jrc ~builder img =
  Python.run ~builder ~ctx_secrets:""
    ~label:"JRC"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"./methods/inputs/download_jrc_data.py"
    ~args:[ output_dir / "zip"; output_dir / "tif" ]
    img

let label l t =
  let open Current.Syntax in
  Current.component "%s" l
  |> let> v = t in
     Current.Primitive.const v

let evaluate ~project_name ~builder ~jrc ~gedi_base_img ~config_img img =
  (* TODO: Move this out of the pipeline and in general provide a generic way
     to handle secrets. *)
  let ctx_secrets =
    Bos.OS.File.read (Fpath.v "./secrets/.env") |> Result.get_ok
  in
  let python_run = Python.run ~ctx_secrets ~builder in
  let buffer =
    let rom =
      Current.list_seq [ Current.map (fun v -> (arkdir, wdir, v)) config_img ]
    in
    python_run ~rom ~label:"buffer"
      ~script_path:"./methods/inputs/generate_boundary.py"
      ~args:[ input_dir / project_name; output_dir / "output.geojson" ]
      img
  in
  let gedi =
    let earthdata =
      Obuilder_spec.Secret.v "earthdata" ~target:"/home/tmf/app/.env"
    in
    let rom =
      Current.list_seq
        [ Current.map (fun v -> (wdir / "data", wdir, v)) buffer ]
    in
    python_run ~label:"GEDI" ~network:[ "host" ]
      ~script_path:"./methods/inputs/download_gedi_data.py"
      ~args:[ input_dir / "output.geojson"; output_dir / "gedi" ]
      ~secrets:[ earthdata ] ~rom gedi_base_img
  in
  let luc =
    let rom =
      Current.list_seq
        [
          Current.map (fun v -> (wdir / "data", wdir, v)) buffer;
          Current.map (fun v -> (wdir / "data", wdir, v)) jrc;
        ]
    in
    python_run ~rom ~label:"LUC"
      ~script_path:"./methods/inputs/generate_luc_layer.py"
      ~args:
        [
          input_dir / "output.geojson";
          input_dir / "tif/products/tmf_v1/AnnualChange";
          output_dir / "luc.tif";
        ]
      img
  in
  let carbon =
    let rom =
      Current.list_seq
        [
          Current.map (fun v -> (wdir / "data", wdir, v)) buffer;
          Current.map (fun v -> (wdir / "data", wdir, v)) luc;
          Current.map (fun v -> (wdir / "data", wdir, v)) gedi;
        ]
    in
    python_run ~rom ~label:"carbon density (todo)" ~script_path:"main.py"
      ~args:[ "--method"; "additionality" ]
      img
  in
  let additionality =
    let rom =
      Current.list_seq
        [ Current.map (fun v -> (wdir / "data", wdir, v)) carbon ]
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
    Current.collapse ~key:"project" ~value:project_name
      ~input:(label project_name img)
    @@ python_run ~rom
         ~label:("permanence " ^ String.lowercase_ascii project_name)
         ~script_path:"main.py"
         ~args:[ "--method"; "permanence" ]
         img
  in
  Current.ignore_value permanence
