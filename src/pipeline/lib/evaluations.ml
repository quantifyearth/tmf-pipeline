open! Import
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile
module Config = Config

type rom = string * string * Current_obuilder.output

let wdir = "/home/tmf/app"
let input_dir = "./inputs"
let output_dir = "./data"
let ( / ) = Filename.concat

let data_spec =
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
      run "mkdir /home/tmf/app/data";
      workdir "/home/tmf/app/data";
      copy [ "." ] ~dst:"/home/tmf/app/data";
    ]

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

  let run ?label ?(args = []) ?(pre = []) ?pre_symlinks ?script_path ?rom
      ?extra_files ?env ?network ?secrets ?(ctx_secrets = "") ~builder snapshot
      =
    let ctx_secrets = [ ("earthdata", ctx_secrets) ] in
    Current_obuilder.run ?label builder ~snapshot ?rom ?extra_files ?env
      ?secrets ~ctx_secrets ?network ?pre_symlinks
      (pre
      @ [
          String.concat " "
            ((("python" :: [ "-m" ]) @ Option.to_list script_path) @ args);
        ])
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let tmf_implementation gref =
    Git.clone ~gref ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"

  let tmf_data ?(gref = "f924f30efdae63c88c7e2465318226f84eb9501a") () =
    (* ALL PROJECTS *)
    (* Git.clone ~gref:"db7a7d1255d4a4860c87646a5443f28da4f05692" ~schedule "git@github.com:carboncredits/tmf-data.git" *)
    Git.clone ~gref ~schedule "git@github.com:carboncredits/tmf-data.git"
end

(* Manging data as inputs and outputs to builds tracking the
   container images they are located in to automatically generate
   the *)
module Data : sig
  module Kind : sig
    type output = Output
  end

  type _ path

  type t
  (** A data index *)

  val v : input:string -> output:string -> string -> t
  (** Create a new data index *)

  val extend_path : t -> _ path -> string -> _ path
  (** Extend a path *)

  val with_output :
    t ->
    string ->
    (string -> Current_obuilder.output Current.term) ->
    Kind.output path * Current_obuilder.output Current.term
  (** [with_output idx fname fn] will run [fn] with an output at [fname] recording the link between
      the output filename and the resulting OBuilder output. *)

  type inputs

  val with_input : t -> Kind.output path -> (string -> inputs -> 'a) -> 'a
  (** Using one output as the input to something else *)

  val extend_inputs :
    t -> Kind.output path -> (string -> inputs -> 'a) -> inputs -> 'a

  val collect_inputs : inputs -> rom list Current.term
end = struct
  module Kind = struct
    type output = Output
  end

  type _ path = string * string
  type e = E : _ path -> e

  type t = {
    input : string;
    output : string;
    data : string;
    table : (e, Current_obuilder.output Current.term) Hashtbl.t;
  }

  let extend_path t (dir, f) s =
    let i = Hashtbl.find t.table (E (dir, f)) in
    let o = (dir, Filename.concat f s) in
    Hashtbl.add t.table (E o) i;
    o

  let v ~input ~output data =
    { input; output; data; table = Hashtbl.create 128 }

  let path_to_string (dir, f) = Filename.concat dir f

  let with_output t path fn =
    let output = (t.output, path) in
    let build = fn (path_to_string output) in
    Hashtbl.add t.table (E output) build;
    (output, build)

  type inputs = rom Current.term list

  let with_input t ((_dir, f) as o) fn =
    let img = Hashtbl.find t.table (E o) in
    let input = path_to_string (t.input, f) in
    let rom = Current.map (fun v -> (t.data / "data", t.data, v)) img in
    fn input [ rom ]

  let extend_inputs t ((_dir, f) as o) fn inputs =
    let img = Hashtbl.find t.table (E o) in
    let input = path_to_string (t.input, f) in
    let rom = Current.map (fun v -> (t.data / "data", t.data, v)) img in
    fn input (rom :: inputs)

  let collect_inputs = Current.list_seq
end

let data = Data.v ~input:input_dir ~output:output_dir wdir

let snapshots_to_rom
    (lst :
      (Current_obuilder.Raw.Build.Value.t Current.t * string * string) list) =
  List.map
    (fun (v, build_dir, target) ->
      let+ (snap : Current_obuilder.Raw.Build.Value.t) = v in
      Obuilder_spec.Rom.of_build ~hash:snap.snapshot ~build_dir target)
    lst

let jrc ~pool ~builder img =
  Data.with_output data "jrc" @@ fun out ->
  Python.run ~pool ~builder
    ~label:"JRC"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_jrc_data"
    ~args:[ out / "zip"; out / "tif" ]
    img

let accessibility_fname = "accessibility.tif"

let accessibility ~pool ~builder ~jrc img =
  let access, _download =
    Data.with_output data accessibility_fname @@ fun output ->
    Python.run ~pool ~builder
      ~label:"accessibility"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ] ~script_path:"methods.inputs.download_accessibility"
      ~args:[ output ] img
  in
  Data.with_input data access @@ fun access ->
  Data.extend_inputs data jrc @@ fun jrc is ->
  let rom = Data.collect_inputs is in
  Data.with_output data "accessibility_tiles" @@ fun out ->
  Python.run ~pool ~builder ~ctx_secrets:"" ~rom
    ~label:"accessibility tiles"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.generate_access_tiles"
    ~args:[ access; jrc; out ] img

let srtm_zip_dir = "srtm_zip"
let srtm_tif_dir = "srtm_tif"

let srtm_elevation ~pool ~builder ~boundaries ~pixel_matching_boundaries img =
  Data.with_input data boundaries @@ fun bounds ->
  Data.extend_inputs data pixel_matching_boundaries @@ fun pixel_bounds is ->
  let rom = Data.collect_inputs is in
  Data.with_output data srtm_tif_dir @@ fun tif ->
  Python.run ~pool ~builder ~rom
    ~label:"srtm elev."
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_srtm_data"
    ~args:[ bounds; pixel_bounds; output_dir / srtm_zip_dir; tif ]
    img

let ecoregions ~pool ~builder ~jrc img =
  let out, _ecoregions_download =
    Data.with_output data "ecoregions.geojson" @@ fun out ->
    Python.run ~pool ~builder ~ctx_secrets:""
      ~label:"ecoregions [v]"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ] ~script_path:"methods.inputs.download_shapefiles"
      ~args:[ "ecoregion"; out ] img
  in
  let eco =
    Data.with_input data out @@ fun download ->
    Data.extend_inputs data jrc @@ fun jrc is ->
    let rom = Data.collect_inputs is in
    Data.with_output data "ecoregions.tif" @@ fun output ->
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
      ~script_path:"methods.inputs.generate_ecoregion_rasters"
      ~args:[ download; jrc; output ] img
  in
  (out, eco)

let countries ~pool ~builder img =
  Data.with_output data "countries.geojson" @@ fun out ->
  Python.run ~pool ~builder ~ctx_secrets:""
    ~label:"countries"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_shapefiles"
    ~args:[ "country"; out ] img

let label l t =
  let open Current.Syntax in
  Current.component "%s" l
  |> let> v = t in
     Current.Primitive.const v

let evaluate ~pool ~projects_dir ~project_name ~builder ~inputs ~matching
    ~outputs (project_config : Config.t) =
  (* TODO: Move this out of the pipeline and in general provide a generic way
     to handle secrets. *)
  let project_name_no_geojson = Filename.chop_extension project_name in
  let ctx_secrets =
    Bos.OS.File.read (Fpath.v "./secrets/.env") |> Result.get_ok
  in
  let jrc, _img = jrc ~pool ~builder inputs in
  let jrc = Data.extend_path data jrc "tif/products/tmf_v1/AnnualChange" in
  let eco_download, (eco, _) = ecoregions ~pool ~builder ~jrc inputs in
  let country, _ = countries ~pool ~builder inputs in
  let access, _ = accessibility ~pool ~builder ~jrc inputs in
  let python_run = Python.run ~pool ~ctx_secrets ~builder in
  let config_path, config_img =
    let _path, img =
      Data.with_output data "project_name" @@ fun _ ->
      Current_obuilder.build ~pool ~label:"config" data_spec builder
        (`Dir projects_dir.Current_gitfile.Raw.Git_dir.Value.dir)
    in
    let path, _ =
      Data.with_output data project_name @@ fun _ ->
      Current_obuilder.run ~pool ~label:("just project " ^ project_name_no_geojson)
        ~shell:[ Obuilder_spec.shell [ "/bin/sh"; "-c" ] ]
        ~snapshot:img builder
        [ Fmt.str "find /home/tmf/app/data ! -name '%s' -type f -exec rm -f {} +" project_name ]
    in
    path, img
  in
  let buffer, _ =
    Data.with_input data config_path @@ fun conf_path is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-buffer.geojson")
    @@ fun out ->
    python_run ~rom ~label:"buffer"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.generate_boundary"
      ~args:[ "--project"; conf_path; "--output"; out ]
      inputs
  in
  let _gedi, _ =
    let earthdata =
      Obuilder_spec.Secret.v "earthdata" ~target:"/home/tmf/app/.env"
    in
    Data.with_input data buffer @@ fun buf is ->
    let rom = Data.collect_inputs is in
    Data.with_output data "gedi" @@ fun gedi ->
    python_run ~label:"GEDI" ~network:[ "host" ]
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.download_gedi_data" ~args:[ buf; gedi ]
      ~secrets:[ earthdata ] ~rom inputs
  in
  let gedi_import, _ =
    Data.with_input data buffer @@ fun gedi is ->
    let rom = Data.collect_inputs is in
    Data.with_output data "import-gedi" @@ fun _ ->
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
      ~script_path:"methods.inputs.import_gedi_data" ~args:[ gedi ] ~rom inputs
  in
  let luc, _ =
    Data.with_input data buffer @@ fun buf ->
    Data.extend_inputs data jrc @@ fun jrc is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-luc.tif") @@ fun out ->
    python_run ~rom ~label:"LUC"
      ~script_path:"methods.inputs.generate_luc_layer" ~args:[ buf; jrc; out ]
      inputs
  in
  let carbon, _ =
    Data.with_input data buffer @@ fun buf ->
    Data.extend_inputs data luc @@ fun luc ->
    Data.extend_inputs data gedi_import @@ fun _ is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-carbon-density.csv")
    @@ fun out ->
    python_run ~rom ~label:"carbon density"
      ~script_path:"methods.inputs.generate_carbon_density"
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
      ~network:[ "host" ] ~args:[ buf; luc; out ] inputs
  in
  let other_projects, _ =
    Data.with_output data "projects/inputs" @@ fun _ ->
    Current_obuilder.run ~pool ~label:"other projects"
      ~shell:[ Obuilder_spec.shell [ "/bin/sh"; "-c" ] ]
      ~snapshot:config_img builder
      [ "rm /home/tmf/app/data/" ^ project_name ]
  in
  let cpc, _ =
    Data.with_input data jrc @@ fun jrc is ->
    let rom = Data.collect_inputs is in
    Data.with_output data "cpc" @@ fun cpc ->
    python_run ~rom ~label:"CPC"
      ~script_path:"methods.inputs.generate_coarsened_propotional_coverage"
      ~args:[ "--jrc"; jrc; "-j"; "20"; "--output"; cpc ]
      inputs
  in
  let matching_area, _ =
    Data.with_input data config_path @@ fun conf_path ->
    Data.extend_inputs data other_projects @@ fun op ->
    Data.extend_inputs data country @@ fun country ->
    Data.extend_inputs data eco_download @@ fun eco is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-matching-area.geojson")
    @@ fun out ->
    python_run ~rom ~label:"matching area"
      ~script_path:"methods.inputs.generate_matching_area"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~pre_symlinks:
        [ Obuilder_spec.run "mkdir projects && mkdir projects/inputs" ]
      ~args:
        [
          conf_path;
          project_config.country_code;
          country;
          eco;
          op;
          out;
        ]
      inputs
  in
  let elevation, _ =
    srtm_elevation ~pool ~builder ~boundaries:config_path
      ~pixel_matching_boundaries:matching_area inputs
  in
  let slope, _ =
    Data.with_input data elevation @@ fun elev is ->
    let rom = Data.collect_inputs is in
    Data.with_output data "slope" @@ fun slope_out ->
    Current_obuilder.run ~label:"slope" ~pool ~snapshot:inputs ~rom builder
      [
        Fmt.str
          "mkdir %s && for i in %s/*; do gdaldem slope $i %s/slope-$(basename \
           $i); done"
          slope_out elev slope_out;
      ]
  in
  let calculate_k, _ =
    Data.with_input data config_path @@ fun conf ->
    Data.extend_inputs data eco @@ fun eco ->
    Data.extend_inputs data access @@ fun access ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data cpc @@ fun cpc ->
    Data.extend_inputs data elevation @@ fun elev ->
    Data.extend_inputs data slope @@ fun slope is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-k.parquet") @@ fun out ->
    python_run ~rom ~label:"calculate k"
      ~script_path:"methods.matching.calculate_k"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:
        [
          "--project";
          conf;
          "--start_year";
          string_of_int project_config.project_start;
          "--jrc";
          jrc;
          "--cpc";
          cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elev;
          "--slope";
          slope;
          "--access";
          access;
          "--output";
          out;
        ]
      matching
  in
  let matches, _ =
    Data.with_input data calculate_k @@ fun calculate_k ->
    Data.extend_inputs data matching_area @@ fun matching_area ->
    Data.extend_inputs data eco @@ fun eco ->
    Data.extend_inputs data access @@ fun access ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data cpc @@ fun cpc ->
    Data.extend_inputs data elevation @@ fun elev ->
    Data.extend_inputs data slope @@ fun slope is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-matches.parquet")
    @@ fun out ->
    python_run ~rom ~label:"potential matchings"
      ~script_path:"methods.matching.find_potential_matches"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:
        [
          "--k";
          calculate_k;
          "--matching";
          matching_area;
          "--start_year";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--cpc";
          cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elev;
          "--slope";
          slope;
          "--access";
          access;
          "-j";
          "20";
          "--output";
          out;
        ]
      matching
  in
  let leakage_zone, _ =
    Data.with_input data config_path @@ fun conf is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-leakage.geojson")
    @@ fun leakage ->
    python_run ~rom ~label:"leakage_zone"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.generate_leakage"
      ~args:[ "--project"; conf; "--output"; leakage ]
      inputs
  in
  let leakage_calculate_k, _ =
    Data.with_input data leakage_zone @@ fun leakage_zone ->
    Data.extend_inputs data eco @@ fun eco ->
    Data.extend_inputs data access @@ fun access ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data cpc @@ fun cpc ->
    Data.extend_inputs data elevation @@ fun elev ->
    Data.extend_inputs data slope @@ fun slope is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-leakage-k.parquet")
    @@ fun out ->
    python_run ~rom ~label:"calculate k (leakage)"
      ~script_path:"methods.matching.calculate_k"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:
        [
          "--project";
          leakage_zone;
          "--start_year";
          string_of_int project_config.project_start;
          "--jrc";
          jrc;
          "--cpc";
          cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elev;
          "--slope";
          slope;
          "--access";
          access;
          "--output";
          out;
        ]
      matching
  in
  let leakage_matching, _ =
    Data.with_input data matching_area @@ fun matching_area ->
    Data.extend_inputs data leakage_calculate_k @@ fun leakage_calculate_k ->
    Data.extend_inputs data eco @@ fun eco ->
    Data.extend_inputs data access @@ fun access ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data cpc @@ fun cpc ->
    Data.extend_inputs data elevation @@ fun elev ->
    Data.extend_inputs data slope @@ fun slope is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-leakage-matches")
    @@ fun out ->
    python_run ~rom ~label:"potential matchings (leakage)"
      ~script_path:"methods.matching.find_potential_matches"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:
        [
          "--k";
          leakage_calculate_k;
          "--matching";
          matching_area;
          "--start_year";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--cpc";
          cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elev;
          "--slope";
          slope;
          "--access";
          access;
          "-j";
          "20";
          "--output";
          out;
        ]
      matching
  in
  let pairs, _ =
    Data.with_input data calculate_k @@ fun calculate_k ->
    Data.extend_inputs data matches @@ fun matches is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "_pairs") @@ fun out ->
    python_run ~rom ~label:"pairs" ~script_path:"methods.matching.find_pairs"
      ~args:
        [
          "--k";
          calculate_k;
          "--s";
          matches;
          "--output";
          out;
          "--seed";
          "42";
          "-j";
          "20";
        ]
      matching
  in
  let leakage_pairs, _ =
    Data.with_input data leakage_calculate_k @@ fun leakage_calculate_k ->
    Data.extend_inputs data leakage_matching @@ fun leakage_matching is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "_leakage_pairs")
    @@ fun out ->
    python_run ~rom ~label:"pairs (leakage)"
      ~script_path:"methods.matching.find_pairs"
      ~args:
        [
          "--k";
          leakage_calculate_k;
          "--s";
          leakage_matching;
          "--output";
          out;
          "--seed";
          "42";
          "-j";
          "20";
        ]
      matching
  in
  let additionality, _ =
    Data.with_input data config_path @@ fun config ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data carbon @@ fun carbon ->
    Data.extend_inputs data pairs @@ fun pairs is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-additionality.csv")
    @@ fun out ->
    python_run ~rom ~label:"additionality"
      ~script_path:"methods.outputs.calculate_additionality"
      ~args:
        [
          "--project";
          config;
          "--project_start";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--density";
          carbon;
          "--matches";
          pairs;
          "--output";
          out;
        ]
      outputs
  in
  let leakage, _ =
    Data.with_input data leakage_zone @@ fun leakage_zone ->
    Data.extend_inputs data config_path @@ fun config ->
    Data.extend_inputs data jrc @@ fun jrc ->
    Data.extend_inputs data carbon @@ fun carbon ->
    Data.extend_inputs data leakage_pairs @@ fun leakage_pairs is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-leakage.csv")
    @@ fun out ->
    python_run ~rom ~label:"leakage"
      ~script_path:"methods.outputs.calculate_leakage"
      ~args:
        [
          "--project";
          config;
          "--leakage_zone";
          leakage_zone;
          "--project_start";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--density";
          carbon;
          "--matches";
          leakage_pairs;
          "--output";
          out;
        ]
      outputs
  in
  let scc, _ =
    let spec =
      Obuilder_spec.stage ~from:"ghcr.io/osgeo/gdal:ubuntu-small-3.6.4"
        Obuilder_spec.
          [
            run "useradd -ms /bin/bash -u 1000 tmf";
            workdir wdir;
            run "chown -R tmf:tmf /home/tmf";
            Obuilder_spec.user_unix ~uid:1000 ~gid:1000;
            run "mkdir %s" output_dir;
            copy [ "." ] ~dst:".";
            run "cp scc/scc.csv %s/scc.csv" output_dir;
          ]
    in
    Data.with_output data "scc.csv" @@ fun _ ->
    Current_obuilder.build ~pool ~label:"scc" spec builder
      (`Git
        (Repos.tmf_data ~gref:"9c72799bb99c5279a676fcaa71976d1512f42d55" ()))
  in
  let _, permanence =
    Data.with_input data additionality @@ fun additionality ->
    Data.extend_inputs data leakage @@ fun leakage ->
    Data.extend_inputs data scc @@ fun scc is ->
    let rom = Data.collect_inputs is in
    Data.with_output data (project_name_no_geojson ^ "-result.json")
    @@ fun out ->
    Current.collapse ~key:"project" ~value:project_name
      ~input:(label project_name outputs)
    @@ python_run ~rom
         ~label:("permanence " ^ String.lowercase_ascii project_name)
         ~script_path:"methods.outputs.calculate_permanence"
         ~args:
           [
             "--additionality";
             additionality;
             "--leakage";
             leakage;
             "--scc";
             scc;
             "--current_year";
             "2021";
             "--output";
             out;
           ]
         outputs
  in
  Current.ignore_value permanence
