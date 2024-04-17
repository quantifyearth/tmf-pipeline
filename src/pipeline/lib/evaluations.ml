open! Import
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile
module Config = Config

let wdir = "/home/tmf/app"
let input_dir = "./inputs"
let output_dir = "./data"
let ( / ) = Filename.concat

let data_spec =
  let open Obuilder_spec in
  stage ~from:(`Image "ghcr.io/osgeo/gdal:ubuntu-small-3.6.4")
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

let other_projects_spec =
  let open Obuilder_spec in
  stage ~from:(`Image "ghcr.io/osgeo/gdal:ubuntu-small-3.6.4")
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
      run "mkdir /home/tmf/app/data/projects";
      copy [ "." ] ~dst:"/home/tmf/app/data/projects";
    ]

module Python = struct
  (* TODO: This should be based on the CI Dockerfile! *)
  let spec =
    let open Obuilder_spec in
    stage ~from:(`Image "ghcr.io/osgeo/gdal:ubuntu-small-3.6.4")
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
    stage ~from:(`Image "ghcr.io/osgeo/gdal:ubuntu-small-3.6.4")
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

  let run ?label ?(args = fun _ -> []) ?(pre = []) ?pre_symlinks ?script_path
      ?rom ?extra_files ?env ?network ?secrets ?(ctx_secrets = "") ~builder
      ~pool ~output snapshot =
    let ctx_secrets = [ ("earthdata", ctx_secrets) ] in
    let arg_output = Filename.concat output_dir output in
    let v =
      Current_obuilder.run ?label builder ~snapshot ?rom ?extra_files ?env
        ?secrets ~ctx_secrets ?network ?pre_symlinks ~pool
        (pre
        @ [
            String.concat " "
              ((("python" :: [ "-m" ]) @ Option.to_list script_path)
              @ args arg_output);
          ])
    in
    (Filename.concat input_dir output, v)
end

module Repos = struct
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

  let tmf_implementation gref =
    Git.clone ~gref ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"

  let tmf_data ?(gref = "1e38b34bf83064f30754320f515b32de5cb4fee2") () =
    (* ALL PROJECTS *)
    (* Git.clone ~gref:"db7a7d1255d4a4860c87646a5443f28da4f05692" ~schedule "git@github.com:carboncredits/tmf-data.git" *)
    Git.clone ~gref ~schedule "git@github.com:carboncredits/tmf-data.git"

  let tmf_other_projects () =
    Git.clone ~gref:"db7a7d1255d4a4860c87646a5443f28da4f05692" ~schedule
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

let make_rom lst =
  List.map (fun img -> Current.map (fun i -> (wdir / "data", wdir, i)) img) lst
  |> Current.list_seq

let jrc ~pool ~builder img =
  Python.run ~output:"jrc" ~pool ~builder
    ~label:"JRC"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_jrc_data"
    ~args:(fun out -> [ out / "zip"; out / "tif" ])
    img

let accessibility_fname = "accessibility.tif"

let accessibility ~pool ~builder ~jrc img =
  let access, access_download =
    Python.run ~pool ~builder ~output:accessibility_fname
      ~label:"accessibility"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ] ~script_path:"methods.inputs.download_accessibility"
      ~args:(fun output -> [ output ])
      img
  in
  let jrc, jrc_data = jrc in
  let rom = make_rom [ access_download; jrc_data ] in
  Python.run ~pool ~builder ~ctx_secrets:"" ~rom ~output:"accessibility_tiles"
    ~label:"accessibility tiles"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.generate_access_tiles"
    ~args:(fun out -> [ access; jrc; out ])
    img

let srtm_zip_dir = "srtm_zip"
let srtm_tif_dir = "srtm_tif"

let srtm_elevation ~pool ~builder ~boundaries ~pixel_matching_boundaries img =
  let bounds, bounds_data = boundaries in
  let pixel_bounds, pixels = pixel_matching_boundaries in
  let rom = make_rom [ bounds_data; pixels ] in
  Python.run ~pool ~builder ~rom ~output:srtm_tif_dir
    ~label:"srtm elev."
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_srtm_data"
    ~args:(fun tif -> [ bounds; pixel_bounds; output_dir / srtm_zip_dir; tif ])
    img

let ecoregions ~pool ~builder ~jrc img =
  let download, ecoregions_download =
    Python.run ~pool ~builder ~output:"ecoregions.geojson"
      ~label:"ecoregions [v]"
        (* Not sure if these env variables are actually used in this part? *)
      ~env:
        [
          ("DATA_PATH", "./data");
          ("USER_PATH", "/home/tmf");
          ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ]
      ~network:[ "host" ] ~script_path:"methods.inputs.download_shapefiles"
      ~args:(fun out -> [ "ecoregion"; out ])
      img
  in
  let jrc, jrc_data = jrc in
  let eco =
    let rom = make_rom [ ecoregions_download; jrc_data ] in
    Python.run ~pool ~builder ~output:"ecoregions.tif"
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
      ~args:(fun output -> [ download; jrc; output ])
      img
  in
  ((download, ecoregions_download), eco)

let countries ~pool ~builder img =
  Python.run ~pool ~builder ~output:"countries.geojson"
    ~label:"countries"
      (* Not sure if these env variables are actually used in this part? *)
    ~env:
      [
        ("DATA_PATH", "./data");
        ("USER_PATH", "/home/tmf");
        ("EARTH_DATA_COOKIE_FILE", "./cookie");
        ("OSM_BOUNDARIES_KEY", "56dc3cd4d5f491f24d6b563b0972dce1");
      ]
    ~network:[ "host" ] ~script_path:"methods.inputs.download_osm_countries"
    ~args:(fun out -> [ out ])
    img

let label l t =
  let open Current.Syntax in
  Current.component "%s" l
  |> let> v = t in
     Current.Primitive.const v

(* For Quantify.earth -- we'd love to decouple this from the
   pipeline itself, but for the sake of speed and results we're putting it
   here. *)
let build_and_publish_data ~pool ~data ~snapshot builder =
  (* First copy all of the data to the output *)
  let area_of_interest = List.map (fun (_, aoi, _) -> aoi) data in
  let pairs = List.map (fun (_, _, pairs) -> pairs) data in
  let rom = make_rom (area_of_interest @ pairs) in
  Current_obuilder.run ~pool ~rom ~label:"quantify.earth" ~snapshot builder
    [ Fmt.str "ls -la %s && cp -r %s %s" input_dir input_dir output_dir ]

let run_command command =
  Logs.info (fun f -> f "Running %s" command);
  Sys.command command

(* No one is happy about this code, but it makes my life easier --patrick *)
let copy_data_out ~(config_img : Current_obuilder.output Current.term)
    ~(pairs_img : Current_obuilder.output Current.term)
    ~(leakage_pairs_data : Current_obuilder.output Current.term)
    ~(carbon_density : Current_obuilder.output Current.term) =
  let+ config_img = config_img
  and+ pairs_img = pairs_img
  and+ leakage_pairs_data = leakage_pairs_data
  and+ carbon_density = carbon_density in
  let path = Fmt.str "/obuilder-zfs/result/%s/rootfs/home/tmf/app/data" in
  let config = path config_img.snapshot in
  let pairs = path pairs_img.snapshot in
  let leakage_pairs = path leakage_pairs_data.snapshot in
  let density = path carbon_density.snapshot in
  let output = "/maps/pf341/results/2024-04" in
  let _ = run_command @@ Fmt.str "rsync -aHq %s/ %s" config output in
  let _ = run_command @@ Fmt.str "rsync -aHq %s/ %s" pairs output in
  let _ = run_command @@ Fmt.str "rsync -aHq %s/ %s" leakage_pairs output in
  let _ = run_command @@ Fmt.str "rsync -aHq %s/ %s" density output in
  ()

let evaluate ~pool ~projects_dir ~project_name ~builder ~inputs ~jrc_input
    ~matching_post_fcc ~matching ~outputs (project_config : Config.t) =
  (* TODO: Move this out of the pipeline and in general provide a generic way
     to handle secrets. *)
  let project_name_no_geojson = Filename.chop_extension project_name in
  let ctx_secrets =
    Bos.OS.File.read (Fpath.v "./secrets/.env") |> Result.get_ok
  in
  let _, jrc_data = jrc ~pool ~builder jrc_input in
  let jrc, jrc_data =
    (input_dir / "jrc/tif/products/tmf_v1/AnnualChange", jrc_data)
  in
  let (eco_download, eco_download_data), (eco, eco_data) =
    ecoregions ~pool ~builder ~jrc:(jrc, jrc_data) inputs
  in
  let country, country_data = countries ~pool ~builder inputs in
  let access, access_data =
    accessibility ~pool ~builder ~jrc:(jrc, jrc_data) inputs
  in
  let python_run = Python.run ~pool ~ctx_secrets ~builder in
  let config_path, config_img =
    let img =
      Current_obuilder.build ~pool ~label:"config" data_spec builder
        (`Dir projects_dir.Current_gitfile.Raw.Git_dir.Value.dir)
    in
    let path, conf_img =
      ( input_dir / project_name,
        Current_obuilder.run ~pool
          ~label:("just project " ^ project_name_no_geojson)
          ~shell:[ Obuilder_spec.shell [ "/bin/sh"; "-c" ] ]
          ~snapshot:img builder
          [
            Fmt.str
              "find /home/tmf/app/data ! -name '%s' -type f -exec rm -f {} +"
              project_name;
          ] )
    in
    (path, conf_img)
  in
  let buffer, buffer_data =
    let rom = make_rom [ config_img ] in
    python_run ~rom ~label:"buffer"
      ~output:(project_name_no_geojson ^ "-buffer.geojson")
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.generate_boundary"
      ~args:(fun out -> [ "--project"; config_path; "--output"; out ])
      inputs
  in
  let _gedi, _ =
    let earthdata =
      Obuilder_spec.Secret.v "earthdata" ~target:"/home/tmf/app/.env"
    in
    let rom = make_rom [ buffer_data ] in
    python_run ~label:"GEDI" ~network:[ "host" ] ~output:"gedi"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.download_gedi_data"
      ~args:(fun gedi -> [ buffer; gedi ])
      ~secrets:[ earthdata ] ~rom inputs
  in
  let _gedi_import, gedi_import_data =
    let rom = make_rom [ buffer_data ] in
    python_run ~label:"import GEDI" ~network:[ "host" ] ~output:"import-gedi"
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
      ~script_path:"methods.inputs.import_gedi_data"
      ~args:(fun _ -> [ buffer ])
      ~rom inputs
  in
  let luc, luc_data =
    let rom = make_rom [ buffer_data; jrc_data ] in
    python_run ~rom ~label:"LUC"
      ~output:(project_name_no_geojson ^ "-luc.tif")
      ~script_path:"methods.inputs.generate_luc_layer"
      ~args:(fun out -> [ buffer; jrc; out ])
      inputs
  in
  let carbon, carbon_data =
    let rom = make_rom [ buffer_data; luc_data; gedi_import_data ] in
    python_run ~rom ~label:"carbon density"
      ~output:(project_name_no_geojson ^ "-carbon-density.csv")
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
      ~network:[ "host" ]
      ~args:(fun out -> [ buffer; luc; out ])
      inputs
  in
  let op, other_projects =
    (* We use a different images to remove more projects than just the one's we're testing. *)
    let img =
      Current.component "Exract Other Projects"
      |>
      let** projects =
        Current_gitfile.directory
          (Repos.tmf_other_projects ())
          (Fpath.v "projects")
      in
      Current_obuilder.build ~pool ~label:"other-projects" other_projects_spec
        builder (`Dir projects.Current_gitfile.Raw.Git_dir.Value.dir)
    in
    ( input_dir / "projects",
      Current_obuilder.run ~pool ~label:"other projects"
        ~shell:[ Obuilder_spec.shell [ "/bin/sh"; "-c" ] ]
        ~snapshot:img builder [] )
  in
  let cpc, cpc_data =
    let rom = make_rom [ jrc_data ] in
    python_run ~rom ~label:"CPC" ~output:"cpc"
      ~script_path:"methods.inputs.generate_coarsened_propotional_coverage"
      ~args:(fun cpc -> [ "--jrc"; jrc; "-j"; "40"; "--output"; cpc ])
      inputs
  in
  let fcc_cpc, fcc_cpc_data =
    let rom = make_rom [ jrc_data; cpc_data ] in
    let output = "fcc-cpcs" in
    python_run ~rom ~label:"FCCise CPC" ~output
      ~script_path:"methods.inputs.generate_fine_circular_coverage"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out -> [ "--jrc"; jrc; "--output"; out ])
      matching
  in
  let leakage_zone, leakage_zone_data =
    let rom = make_rom [ config_img ] in
    let output = project_name_no_geojson ^ "-leakage.geojson" in
    python_run ~rom ~label:"leakage_zone" ~output
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~script_path:"methods.inputs.generate_leakage"
      ~args:(fun leakage -> [ "--project"; config_path; "--output"; leakage ])
      inputs
  in
  let ( project_country_raster,
        project_country_raster_data,
        project_matching_area,
        project_matching_area_data ) =
    let country_list, country_list_data =
      let rom = make_rom [ config_img; country_data ] in
      python_run ~rom ~label:"country codes"
        ~output:(project_name_no_geojson ^ "-country-list.json")
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~script_path:"methods.inputs.generate_country_list"
        ~args:(fun out ->
          [ "--leakage"; config_path; "--countries"; country; "--output"; out ])
        inputs
    in
    let project_matching_area, project_matching_area_data =
      let rom =
        make_rom
          [
            config_img;
            country_list_data;
            other_projects;
            country_data;
            eco_download_data;
          ]
      in
      let output = project_name_no_geojson ^ "-matching-area.geojson" in
      python_run ~rom ~label:"matching area" ~output
        ~script_path:"methods.inputs.generate_matching_area"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~args:(fun out ->
          [
            "--project";
            config_path;
            "--countrycodes";
            country_list;
            "--countries";
            country;
            "--ecoregions";
            eco_download;
            "--projects";
            op;
            "--output";
            out;
          ])
        inputs
    in
    let rom = make_rom [ country_data; jrc_data; project_matching_area_data ] in
    let c, cd =
      python_run ~rom ~label:"country raster"
        ~output:(project_name_no_geojson ^ "-countries.tif")
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~script_path:"methods.inputs.generate_country_raster"
        ~args:(fun out ->
          [
            "--jrc";
            jrc;
            "--matching";
            project_matching_area;
            "--countries";
            country;
            "--output";
            out;
          ])
        inputs
    in
    (c, cd, project_matching_area, project_matching_area_data)
  in
  let ( leakage_country_raster,
        leakage_country_raster_data,
        leakage_matching_area,
        leakage_matching_area_data ) =
    let country_list, country_list_data =
      let rom = make_rom [ leakage_zone_data; country_data ] in
      python_run ~rom ~label:"country codes (leakage)"
        ~output:(project_name_no_geojson ^ "-leakage-country-list.json")
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~script_path:"methods.inputs.generate_country_list"
        ~args:(fun out ->
          [ "--leakage"; leakage_zone; "--countries"; country; "--output"; out ])
        inputs
    in
    let leakage_matching_area, leakage_matching_area_data =
      let rom =
        make_rom
          [
            leakage_zone_data;
            country_list_data;
            other_projects;
            country_data;
            eco_download_data;
          ]
      in
      let output = project_name_no_geojson ^ "-leakage-matching-area.geojson" in
      python_run ~rom ~label:"matching area (leakage)" ~output
        ~script_path:"methods.inputs.generate_matching_area"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~pre_symlinks:
          [ Obuilder_spec.run "mkdir projects && mkdir projects/inputs" ]
        ~args:(fun out ->
          [
            "--project";
            leakage_zone;
            "--countrycodes";
            country_list;
            "--countries";
            country;
            "--ecoregions";
            eco_download;
            "--projects";
            (* MUST INCLUDE THE PROJECT ZONE FOR THIS LEAKAGE ZONE *)
            op;
            "--output";
            out;
          ])
        inputs
    in
    let rom =
      make_rom
        [ config_img; country_data; jrc_data; leakage_matching_area_data ]
    in
    let c, cd =
      python_run ~rom ~label:"country raster (leakage)"
        ~output:(project_name_no_geojson ^ "-leakage-countries.tif")
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~script_path:"methods.inputs.generate_country_raster"
        ~args:(fun out ->
          [
            "--jrc";
            jrc;
            "--matching";
            leakage_matching_area;
            "--countries";
            country;
            "--output";
            out;
          ])
        inputs
    in
    (c, cd, leakage_matching_area, leakage_matching_area_data)
  in
  (* Note: Going forward we assume the leakage matching area is a superset
     of the project matching area. This means for the inputs section we use
     the leakage matching area to pull in the relevant TIFs etc. rather than
     doing it separately for both and duplicating the work. Only in the matching
     stages and beyond do we make a more nuanced decision to pass in the correct
     matching area depending on what we are doing. *)
  let elevation, elevation_data =
    srtm_elevation ~pool ~builder ~boundaries:(config_path, config_img)
      ~pixel_matching_boundaries:
        (leakage_matching_area, leakage_matching_area_data)
      matching (* change back to inputs rather matching image ! *)
  in
  let slope, slope_data =
    let slope, slope_data =
      let rom = make_rom [ elevation_data ] in
      let output = "slopes" in
      python_run ~rom ~label:"calculate slope" ~output
        ~script_path:"methods.inputs.generate_slope"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~args:(fun out -> [ "--input"; elevation; "--output"; out ])
        matching_post_fcc
    in
    let rom = make_rom [ jrc_data; slope_data ] in
    let output = "rescaled-slopes" in
    python_run ~rom ~label:"rescale slopes" ~output
      ~script_path:"methods.inputs.rescale_tiles_to_jrc"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out -> [ "--jrc"; jrc; "--tiles"; slope; "--output"; out ])
      matching
  in
  (* RESCALE TO JRC *)
  let elevation, elevation_data =
    let rom = make_rom [ jrc_data; elevation_data ] in
    let output = "rescaled-elevation" in
    python_run ~rom ~label:"rescale elevation" ~output
      ~script_path:"methods.inputs.rescale_tiles_to_jrc"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out -> [ "--jrc"; jrc; "--tiles"; elevation; "--output"; out ])
      matching
  in
  let calc_k buffer =
    let calculate_k, calculate_k_data =
      let rom =
        make_rom
          [
            config_img;
            eco_data;
            access_data;
            jrc_data;
            elevation_data;
            slope_data;
            cpc_data;
            project_country_raster_data;
          ]
      in
      let label, output =
        match buffer with
        | None -> ("calculate k", project_name_no_geojson ^ "-k.parquet")
        | Some metres ->
            ( Fmt.str "calculate k (%im)" metres,
              project_name_no_geojson ^ "-" ^ string_of_int metres
              ^ "-k.parquet" )
      in
      python_run ~rom ~label ~output ~script_path:"methods.matching.calculate_k"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~args:(fun out ->
          [
            "--project";
            config_path;
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
            elevation;
            "--slope";
            slope;
            "--access";
            access;
            "--countries-raster";
            project_country_raster;
            "--output";
            out;
          ])
        matching
    in
    (calculate_k, calculate_k_data)
  in
  let calculate_k, calculate_k_data = calc_k None in
  let calculate_k_1000, calculate_k_1000_data = calc_k (Some 1000) in
  let matches, matches_data =
    let match_rasters, match_rasters_data =
      let rom =
        make_rom
          [
            calculate_k_1000_data;
            project_matching_area_data;
            eco_data;
            access_data;
            jrc_data;
            fcc_cpc_data;
            elevation_data;
            slope_data;
            project_country_raster_data;
          ]
      in
      let output = project_name_no_geojson ^ "-matches.parquet" in
      python_run ~rom ~label:"potential matchings" ~output
        ~script_path:"methods.matching.find_potential_matches"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~args:(fun out ->
          [
            "--k";
            calculate_k_1000;
            "--matching";
            project_matching_area;
            "--start_year";
            string_of_int project_config.project_start;
            "--evaluation_year";
            "2021";
            "--jrc";
            jrc;
            "--cpc";
            fcc_cpc;
            "--ecoregions";
            eco;
            "--elevation";
            elevation;
            "--slope";
            slope;
            "--access";
            access;
            "--countries-raster";
            project_country_raster;
            "--output";
            out;
          ])
        matching_post_fcc
    in
    let m_raster, m_raster_data =
      let rom = make_rom [ match_rasters_data ] in
      python_run ~rom ~label:"build m raster"
        ~output:(project_name_no_geojson ^ "-matching-rasters")
        ~script_path:"methods.matching.build_m_raster"
        ~args:(fun out ->
          [ "--rasters_directory"; match_rasters; "--output"; out; "-j"; "20" ])
        matching_post_fcc
    in
    let rom =
      make_rom
        [
          m_raster_data;
          project_matching_area_data;
          jrc_data;
          fcc_cpc_data;
          eco_data;
          elevation_data;
          slope_data;
          access_data;
          project_country_raster_data;
        ]
    in
    let output = project_name_no_geojson ^ "-matches.parquet" in
    python_run ~rom ~label:"M (All potential matches)" ~output
      ~script_path:"methods.matching.build_m_table"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out ->
        [
          "--raster";
          m_raster;
          "--matching";
          project_matching_area;
          "--start_year";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--cpc";
          fcc_cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elevation;
          "--slope";
          slope;
          "--access";
          access;
          "--countries-raster";
          project_country_raster;
          "--output";
          out;
        ])
      matching_post_fcc
  in
  let leakage_calculate_k, leakage_calculate_k_data =
    let rom =
      make_rom
        [
          leakage_zone_data;
          eco_data;
          access_data;
          jrc_data;
          cpc_data;
          elevation_data;
          slope_data;
          leakage_country_raster_data;
        ]
    in
    let output = project_name_no_geojson ^ "-leakage-k.parquet" in
    python_run ~rom ~label:"calculate k (leakage)" ~output
      ~script_path:"methods.matching.calculate_k"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out ->
        [
          "--project";
          leakage_zone;
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
          elevation;
          "--slope";
          slope;
          "--access";
          access;
          "--countries-raster";
          leakage_country_raster;
          "--output";
          out;
        ])
      matching
  in
  let leakage_matching, leakage_matching_data =
    let match_rasters, match_rasters_data =
      let rom =
        make_rom
          [
            leakage_matching_area_data;
            leakage_calculate_k_data;
            eco_data;
            access_data;
            jrc_data;
            fcc_cpc_data;
            elevation_data;
            slope_data;
            leakage_country_raster_data;
          ]
      in
      let output = project_name_no_geojson ^ "-leakage-matches" in
      python_run ~rom ~label:"potential matchings (leakage)" ~output
        ~script_path:"methods.matching.find_potential_matches"
        ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
        ~args:(fun out ->
          [
            "--k";
            leakage_calculate_k;
            "--matching";
            leakage_matching_area;
            "--start_year";
            string_of_int project_config.project_start;
            "--evaluation_year";
            "2021";
            "--jrc";
            jrc;
            "--cpc";
            fcc_cpc;
            "--ecoregions";
            eco;
            "--elevation";
            elevation;
            "--slope";
            slope;
            "--countries-raster";
            leakage_country_raster;
            "--access";
            access;
            "--output";
            out;
          ])
        matching_post_fcc
    in
    let leakage_m_raster, leakage_m_raster_data =
      let rom = make_rom [ match_rasters_data ] in
      python_run ~rom ~label:"build m raster (leakage)"
        ~output:(project_name_no_geojson ^ "-leakage-matching-rasters")
        ~script_path:"methods.matching.build_m_raster"
        ~args:(fun out ->
          [ "--rasters_directory"; match_rasters; "--output"; out ])
        matching_post_fcc
    in
    let rom =
      make_rom
        [
          leakage_m_raster_data;
          leakage_matching_area_data;
          jrc_data;
          fcc_cpc_data;
          eco_data;
          elevation_data;
          slope_data;
          access_data;
          project_country_raster_data;
        ]
    in
    let output = project_name_no_geojson ^ "-leakage-matches.parquet" in
    python_run ~rom ~label:"M Leakage (All potential matches)" ~output
      ~script_path:"methods.matching.build_m_table"
      ~env:[ ("PYTHONPATH", wdir ^ ":$PYTHONPATH") ]
      ~args:(fun out ->
        [
          "--raster";
          leakage_m_raster;
          "--matching";
          leakage_matching_area;
          "--start_year";
          string_of_int project_config.project_start;
          "--evaluation_year";
          "2021";
          "--jrc";
          jrc;
          "--cpc";
          fcc_cpc;
          "--ecoregions";
          eco;
          "--elevation";
          elevation;
          "--slope";
          slope;
          "--access";
          access;
          "--countries-raster";
          project_country_raster;
          "--output";
          out;
        ])
      matching_post_fcc
  in
  let pairs, pairs_data =
    let rom = make_rom [ calculate_k_data; matches_data ] in
    let output = project_name_no_geojson ^ "_pairs" in
    python_run ~rom ~label:"pairs" ~script_path:"methods.matching.find_pairs"
      ~output
      ~args:(fun out ->
        [
          "--k";
          calculate_k;
          "--m";
          matches;
          "--start_year";
          string_of_int project_config.project_start;
          "--output";
          out;
          "--seed";
          "42";
          "-j";
          "1";
        ])
      matching_post_fcc
  in
  let leakage_pairs, leakage_pairs_data =
    let rom = make_rom [ leakage_calculate_k_data; leakage_matching_data ] in
    let output = project_name_no_geojson ^ "_leakage_pairs" in
    python_run ~rom ~label:"pairs (leakage)" ~output
      ~script_path:"methods.matching.find_pairs"
      ~args:(fun out ->
        [
          "--k";
          leakage_calculate_k;
          "--m";
          leakage_matching;
          "--start_year";
          string_of_int project_config.project_start;
          "--output";
          out;
          "--seed";
          "42";
          "-j";
          "3";
        ])
      matching_post_fcc
  in
  let additionality =
    let rom = make_rom [ config_img; carbon_data; pairs_data ] in
    let output = project_name_no_geojson ^ "-additionality.csv" in
    Current.collapse ~key:"project" ~value:project_name
      ~input:(label project_name outputs)
    @@ snd
    @@ python_run ~rom ~label:"additionality" ~output
         ~env:[ ("TMF_PARTIALS", output_dir) ]
         ~script_path:"methods.outputs.calculate_additionality"
         ~args:(fun out ->
           [
             "--project";
             config_path;
             "--project_start";
             string_of_int project_config.project_start;
             "--evaluation_year";
             "2021";
             "--density";
             carbon;
             "--matches";
             pairs;
             "--output";
             out;
           ])
         outputs
  in
  let leakage =
    let rom =
      make_rom
        [ config_img; carbon_data; leakage_pairs_data; leakage_zone_data ]
    in
    let output = project_name_no_geojson ^ "-leakage.csv" in
    Current.collapse ~key:"project" ~value:project_name
      ~input:(label project_name outputs)
    @@ snd
    @@ python_run ~rom ~label:"leakage" ~output
         ~env:[ ("TMF_PARTIALS", output_dir) ]
         ~script_path:"methods.outputs.calculate_leakage"
         ~args:(fun out ->
           [
             "--project";
             config_path;
             "--leakage_zone";
             leakage_zone;
             "--project_start";
             string_of_int project_config.project_start;
             "--evaluation_year";
             "2021";
             "--density";
             carbon;
             "--matches";
             leakage_pairs;
             "--output";
             out;
           ])
         outputs
  in
  (* let scc, scc_data =
       let spec =
         Obuilder_spec.stage ~from:(`Image "ghcr.io/osgeo/gdal:ubuntu-small-3.6.4")
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
       ( input_dir / "scc.csv",
         Current_obuilder.build ~pool ~label:"scc" spec builder
           (`Git
             (Repos.tmf_data ~gref:"9c72799bb99c5279a676fcaa71976d1512f42d55" ()))
       )
     in *)
  (* let permanence =
       let rom = make_rom [ additionality_data; leakage_data; scc_data ] in
       let output = project_name_no_geojson ^ "-result.json" in
       Current.collapse ~key:"project" ~value:project_name
         ~input:(label project_name outputs)
       @@ snd
       @@ python_run ~rom ~output
            ~label:("permanence " ^ String.lowercase_ascii project_name)
            ~script_path:"methods.outputs.calculate_permanence"
            ~args:(fun out ->
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
              ])
            outputs
     in *)
  let copy =
    copy_data_out ~config_img ~pairs_img:pairs_data ~leakage_pairs_data
      ~carbon_density:carbon_data
  in
  (additionality, leakage, copy)
