open! Import
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile
module Config = Config

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

  let run ?label ?(args = []) ?(pre = []) ?(as_module = false) ?pre_symlinks
      ?script_path ?rom ?extra_files ?env ?network ?secrets ~ctx_secrets
      ~builder snapshot =
    let ctx_secrets = [ ("earthdata", ctx_secrets) ] in
    Current_obuilder.run ?label builder ~snapshot ?rom ?extra_files ?env
      ?secrets ~ctx_secrets ?network ?pre_symlinks
      (pre
      @ [
          String.concat " "
            ((("python" :: (if as_module then [ "-m" ] else []))
             @ Option.to_list script_path)
            @ args);
        ])
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let tmf_implementation gref =
    Git.clone ~gref ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"

  let tmf_data () =
    (* ALL PROJECTS *)
    (* Git.clone ~gref:"db7a7d1255d4a4860c87646a5443f28da4f05692" ~schedule "git@github.com:carboncredits/tmf-data.git" *)
    Git.clone ~gref:"f924f30efdae63c88c7e2465318226f84eb9501a" ~schedule
      "git@github.com:carboncredits/tmf-data.git"
end

let snapshots_to_rom
    (lst :
      (Current_obuilder.Raw.Build.Value.t Current.t * string * string) list) =
  List.map
    (fun (v, build_dir, target) ->
      let+ (snap : Current_obuilder.Raw.Build.Value.t) = v in
      Obuilder_spec.Rom.of_build ~hash:snap.snapshot ~build_dir target)
    lst

let jrc ~pool ~builder img =
  Python.run ~pool ~builder ~ctx_secrets:""
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

let accessibility_fname = "accessibility.tif"

let accessibility ~pool ~builder ~jrc img =
  let download =
    Python.run ~pool ~builder ~ctx_secrets:""
      ~label:"accessibility"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ]
      ~script_path:"./methods/inputs/download_accessibility.py"
      ~args:[ output_dir / accessibility_fname ]
      img
  in
  let rom =
    Current.list_seq
      [
        Current.map (fun v -> (wdir / "data", wdir, v)) download;
        Current.map (fun v -> (wdir / "data", wdir, v)) jrc;
      ]
  in
  Python.run ~pool ~builder ~ctx_secrets:"" ~rom
    ~label:"accessibility tiles"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"./methods/inputs/generate_access_tiles.py"
    ~args:
      [
        input_dir / accessibility_fname;
        input_dir / "tif/products/tmf_v1/AnnualChange";
        output_dir / "accessibility_tiles";
      ]
    img

let srtm_zip_dir = "srtm_zip"
let srtm_tif_dir = "srtm_tif"

let _srtm_elevation ~pool ~builder ~project_name ~boundaries
    ~pixel_matching_boundaries img =
  let rom =
    Current.list_seq
      [
        Current.map (fun v -> (wdir / "data", wdir, v)) boundaries;
        Current.map
          (fun v -> (wdir / "data", wdir, v))
          pixel_matching_boundaries;
      ]
  in
  Python.run ~pool ~builder ~ctx_secrets:"" ~rom
    ~label:"srtm elev."
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"./methods/inputs/download_srtm_data.py"
    ~args:
      [
        input_dir / project_name;
        (input_dir / project_name) ^ "-matches.geojson";
        output_dir / srtm_zip_dir;
        output_dir / srtm_tif_dir;
      ]
    img

let ecoregions ~pool ~builder ~jrc img =
  let ecoregions_download =
    Python.run ~pool ~builder ~ctx_secrets:""
      ~label:"ecoregions [v]"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ] ~script_path:"./methods/inputs/download_shapefiles.py"
      ~args:[ "ecoregion"; output_dir / "ecoregions.geojson" ]
      img
  in
  let rom =
    Current.list_seq
      [
        Current.map (fun v -> (wdir / "data", wdir, v)) ecoregions_download;
        Current.map (fun v -> (wdir / "data", wdir, v)) jrc;
      ]
  in
  Python.run ~pool ~builder ~ctx_secrets:""
    ~label:"ecoregions"
      (* Not sure if these env variables are actually used in this part? *)
    ~rom
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ]
    ~script_path:"./methods/inputs/generate_ecoregion_rasters.py"
    ~args:
      [
        input_dir / "ecoregions.geojson";
        input_dir / "tif/products/tmf_v1/AnnualChange";
        output_dir / "ecoregions.tif";
      ]
    img

let countries ~pool ~builder img =
  Python.run ~pool ~builder ~ctx_secrets:""
    ~label:"countries"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"./methods/inputs/download_shapefiles.py"
    ~args:[ "country"; output_dir / "countries.geojson" ]
    img

let label l t =
  let open Current.Syntax in
  Current.component "%s" l
  |> let> v = t in
     Current.Primitive.const v

let evaluate ~pool ~project_name ~builder ~jrc ~gedi_base_img ~config_img img
    (project_config : Config.t) =
  (* TODO: Move this out of the pipeline and in general provide a generic way
     to handle secrets. *)
  let ctx_secrets =
    Bos.OS.File.read (Fpath.v "./secrets/.env") |> Result.get_ok
  in
  let eco = ecoregions ~pool ~builder ~jrc gedi_base_img in
  let country = countries ~pool ~builder gedi_base_img in
  let access = accessibility ~pool ~builder ~jrc gedi_base_img in
  let python_run = Python.run ~pool ~ctx_secrets ~builder in
  let buffer =
    let rom =
      Current.list_seq [ Current.map (fun v -> (arkdir, wdir, v)) config_img ]
    in
    python_run ~rom ~label:"buffer"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
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
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~as_module:true ~script_path:"methods.inputs.download_gedi_data"
      ~args:[ input_dir / "output.geojson"; output_dir / "gedi" ]
      ~secrets:[ earthdata ] ~rom gedi_base_img
  in
  let gedi_import =
    let rom =
      Current.list_seq [ Current.map (fun v -> (wdir / "data", wdir, v)) gedi ]
    in
    python_run ~label:"import GEDI" ~network:[ "host" ]
      ~env:
        [
          ("PYTHONPATH", wdir ^ ":$PYTHONPATH");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("DB_HOST", "aello");
          ("DB_USER", "tmf_writer");
          ("DB_NAME", "tmf_gedi");
        ]
      ~as_module:true ~script_path:"methods.inputs.import_gedi_data"
      ~args:[ input_dir / "gedi" ]
      ~rom gedi_base_img
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
          (* We don't really need whats in the GEDI import image, but the import
             must have been completed. *)
          Current.map (fun v -> (wdir / "data", wdir, v)) gedi_import;
        ]
    in
    python_run ~rom ~label:"carbon density"
      ~script_path:"./methods/inputs/generate_carbon_density.py"
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("DB_HOST", "aello");
          ("DB_USER", "tmf_reader");
          ("DB_NAME", "tmf_gedi");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
        (* Needs host network for DB conneciton *)
      ~network:[ "host" ]
      ~args:
        [
          input_dir / "output.geojson";
          input_dir / "luc.tif";
          (output_dir / project_name) ^ "-carbon-density.csv";
        ]
      img
  in
  let other_projects =
    Current_obuilder.run ~pool ~label:"other projects"
      ~shell:[ Obuilder_spec.shell [ "/bin/sh"; "-c" ] ]
      ~snapshot:config_img builder
      [ "rm /inputs/" ^ project_name ]
  in
  let _matching_area =
    (* Requires project shapefile, country code, country shapefile, ecoregions shapefile, other projects and output *)
    let rom =
      Current.list_seq
        [
          Current.map (fun v -> (arkdir, wdir, v)) config_img;
          Current.map (fun v -> (arkdir, wdir / "projects", v)) other_projects;
          Current.map (fun v -> (wdir / "data", wdir, v)) country;
          Current.map (fun v -> (wdir / "data", wdir, v)) eco;
          Current.map (fun v -> (wdir / "data", wdir, v)) access;
        ]
    in
    python_run ~rom ~label:"matching area"
      ~script_path:"./methods/inputs/generate_matching_area.py"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~pre_symlinks:
        [ Obuilder_spec.run "mkdir projects && mkdir projects/inputs" ]
      ~args:
        [
          input_dir / project_name;
          project_config.country_code;
          input_dir / "countries.geojson";
          input_dir / "ecoregions.geojson";
          "projects" / "inputs";
          (output_dir / project_name) ^ "-matches.geojson";
        ]
      gedi_base_img
  in
  (* let elev =
       srtm_elevation ~pool ~builder ~project_name ~boundaries:config_img
         ~pixel_matching_boundaries:matching_area gedi_base_img
     in *)
  let additionality =
    let rom =
      Current.list_seq
        [
          Current.map (fun v -> (wdir / "data", wdir, v)) carbon;
          (* Current.map (fun v -> (wdir / "data", wdir, v)) matching_area; *)
          (* Current.map (fun v -> (wdir / "data", wdir, v)) elev; *)
        ]
    in
    Current.collapse ~key:"project" ~value:project_name
      ~input:(label project_name img)
    @@ python_run ~rom ~label:"additionality" ~script_path:"main.py"
         ~args:[ "--method"; "additionality" ]
         img
  in
  let _leakage =
    let rom =
      Current.list_seq
        [ Current.map (fun v -> (wdir / "data", wdir, v)) carbon ]
    in
    python_run ~rom ~label:"leakage" ~script_path:"main.py"
      ~args:[ "--method"; "leakage" ] img
  in
  (* let _permanence =
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
     in *)
  Current.ignore_value additionality
