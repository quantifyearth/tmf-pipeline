(* Very niche ocurrent plugins for working directly
   with the 4C evaluations repository *)
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder

let arkdir = "/data"
let wdir = "/home/tmf/app"
let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.( >>= )
let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

let data_spec =
  let open Obuilder_spec in
  (* Use any old small image, but because of obuilder we can't use scratch
     which would be ideal. *)
  stage ~from:"alpine:3.14" [ workdir "/data"; copy [ "." ] ~dst:"/data" ]

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

  let evaluations _token =
    Git.clone ~gref:"main" ~schedule
      "git@github.com:carboncredits/tmf-implementation.git"

  let data _token =
    Git.clone ~gref:"main" ~schedule "git@github.com:carboncredits/tmf-data.git"
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

let evaluate ~project_name ~builder ~config_img img =
  let python_run = Python.run ~builder in
  let buffer =
    let rom =
      Current.list_seq [ Current.map (fun v -> ("/data", wdir, v)) config_img ]
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

(* Fetches files from a git repository, only use for smallish files. *)
module Git_file = struct
  open Lwt.Infix
  module Git = Current_git

  module Raw = struct
    module Git_file = struct
      type t = No_context

      let auto_cancel = true
      let id = "git-file"

      module Key = struct
        type t = { commit : Git.Commit.t; files : Fpath.t list }

        let to_json t =
          `Assoc
            [
              ("commit", `String (Git.Commit.hash t.commit));
              ( "files",
                `List
                  (List.map
                     (fun file -> `String (Fpath.to_string file))
                     t.files) );
            ]

        let digest t = to_json t |> Yojson.Safe.to_string
      end

      let pp ppf t = Yojson.pp ppf (Key.to_json t)

      module Value = struct
        type t = (string * string) list

        let marshal ts =
          `List (List.map (fun (p, v) -> `List [ `String p; `String v ]) ts)
          |> Yojson.Safe.to_string

        let unmarshal s =
          match Yojson.Safe.from_string s with
          | `List lst ->
              List.map
                (function
                  | `List [ `String p; `String c ] -> (p, c)
                  | _ -> failwith "Failed to unmarshal files")
                lst
          | _ -> failwith "Failed to unmarshal files"
      end

      let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

      let build No_context (job : Current.Job.t) (k : Key.t) :
          Value.t Current.or_error Lwt.t =
        Current.Job.start ~level:Harmless job >>= fun () ->
        Current_git.with_checkout ~job k.commit @@ fun dir ->
        let paths = List.map (fun file -> Fpath.(dir // file)) k.files in
        let contents =
          try
            Ok
              (List.map
                 (fun path ->
                   (Fpath.to_string path, Bos.OS.File.read path |> or_raise))
                 paths)
          with Failure msg -> Error (`Msg msg)
        in
        Lwt.return contents
    end

    module Git_dir = struct
      type t = No_context

      let auto_cancel = true
      let id = "git-file"

      module Key = struct
        type t = { commit : Git.Commit.t; dir : Fpath.t }

        let to_json t =
          `Assoc
            [
              ("commit", `String (Git.Commit.hash t.commit));
              ("dir", `String (Fpath.to_string t.dir));
            ]

        let digest t = to_json t |> Yojson.Safe.to_string
      end

      let pp ppf t = Yojson.pp ppf (Key.to_json t)

      module Value = struct
        (* Directory + Digest *)
        type t = { dir : Fpath.t; files : Fpath.t list; digest : string }

        let marshal t =
          let json =
            `Assoc
              [
                ("dir", `String (Fpath.to_string t.dir));
                ( "files",
                  `List
                    (List.map (fun f -> `String (Fpath.to_string f)) t.files) );
                ("digest", `String t.digest);
              ]
          in
          Yojson.to_string json

        let unmarshal s =
          match Yojson.Safe.from_string s with
          | `Assoc
              [
                ("dir", `String dir);
                ("files", `List files);
                ("digest", `String digest);
              ] ->
              let dir = Fpath.of_string dir |> or_raise in
              let files =
                List.map
                  (function
                    | `String s -> Fpath.of_string s |> or_raise
                    | _ -> failwith "Failed to unmarshal files")
                  files
              in
              { dir; files; digest }
          | _ -> failwith "Failed to unmarshal files"
      end

      let or_raise = function Ok v -> v | Error (`Msg m) -> failwith m

      let digest_file path =
        Lwt_io.(open_file ~mode:Input (Fpath.to_string path)) >>= fun file ->
        Lwt.finalize
          (fun () ->
            Lwt_io.read file >|= fun content ->
            Digestif.SHA256.digest_string content |> Digestif.SHA256.to_hex)
          (fun () -> Lwt_io.close file)

      let mv ~cancellable ~job ~src ~dst =
        let cmd = [| "mv"; Fpath.to_string src; Fpath.to_string dst |] in
        Current.Process.exec ~cancellable ~job ("", cmd)

      let build No_context (job : Current.Job.t) (k : Key.t) :
          Value.t Current.or_error Lwt.t =
        let git_dir = Current.state_dir "git-dir" in
        Current.Job.start ~level:Harmless job >>= fun () ->
        Current_git.with_checkout ~job k.commit @@ fun dir ->
        let paths = Bos.OS.Dir.contents Fpath.(dir // k.dir) |> Result.get_ok in
        Current.Job.log job "Directory %a contains %a" Fpath.pp k.dir
          Fmt.(list Fpath.pp)
          paths;
        Lwt_list.map_p
          (fun path -> digest_file path >|= fun d -> (path, d))
          paths
        >>= fun output ->
        let digest =
          List.fold_left (fun acc (_, d) -> Digest.string (acc ^ d)) "" output
          |> Digest.to_hex
        in
        mv ~cancellable:true ~job
          ~src:Fpath.(dir // k.dir)
          ~dst:Fpath.(git_dir // k.dir)
        >>!= fun () ->
        let v =
          Value.
            {
              digest;
              dir = Fpath.(git_dir // k.dir);
              files = List.map fst output;
            }
        in
        Lwt.return (Ok v)
    end
  end

  module GitFileC = Current_cache.Make (Raw.Git_file)

  let raw_git_file ?schedule commit files =
    let key = Raw.Git_file.Key.{ commit; files } in
    GitFileC.get ?schedule No_context key

  let contents ?schedule commit files =
    let open Current.Syntax in
    Current.component "read %a" (Fmt.list Fpath.pp) files
    |> let> commit = commit in
       raw_git_file ?schedule commit files

  module GitDirC = Current_cache.Make (Raw.Git_dir)

  let raw_git_dir ?schedule commit dir =
    let key = Raw.Git_dir.Key.{ commit; dir } in
    GitDirC.get ?schedule No_context key

  let directory_contents ?schedule commit dir =
    let open Current.Syntax in
    Current.component "directory %a" Fpath.pp dir
    |> let> commit = commit in
       raw_git_dir ?schedule commit dir
end
