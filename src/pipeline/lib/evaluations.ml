(* Very niche ocurrent plugins for working directly
   with the 4C evaluations repository *)
open Current.Syntax
module Git = Current_git
module Github = Current_github
module Current_obuilder = Current_obuilder

let arkdir = "/data"
let wdir = "/home/tmf/app"
let ( / ) = Filename.concat

module Python = struct
  let spec =
    let open Obuilder_spec in
    stage ~from:"python:3.10-bullseye"
      [
        (* Install VSCode Server *)
        (* run "apt-get install wget gpg";
        run ~network:[ "host" ] "wget --no-check-certificate -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg";
        run {| install -D -o root -g root -m 644 packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg |};
        run ~network:[ "host" ] {| sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list' |};
        run "rm -f packages.microsoft.gpg";
        run ~network:[ "host" ] "apt install -y apt-transport-https && apt update && apt install -y code"; *)
        (* App specific *)
        workdir wdir;
        run "useradd -ms /bin/bash -u 1000 tmf";
        run "chown -R tmf:tmf /home/tmf";
        Obuilder_spec.user_unix ~uid:1000 ~gid:1000;
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
    Git.clone ~gref:"main" ~schedule
      "git@github.com:carboncredits/tmf-data.git"
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

let evaluate ~scc_values ~project_name ~builder img =
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
      ~extra_files:scc_values
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
        type t = {
          commit : Git.Commit.t;
          files : Fpath.t list;
        }

        let to_json t =
          `Assoc [
            "commit", `String (Git.Commit.hash t.commit);
            "files", `List (List.map (fun file -> (`String (Fpath.to_string file))) t.files)
          ]

        let digest t = to_json t |> Yojson.Safe.to_string
      end

      let pp ppf t = Yojson.pp ppf (Key.to_json t)

      module Value = struct
        type t = (string * string) list

        let marshal ts =
          `List (List.map (fun (p, v) -> `List [ `String p; `String v]) ts)
          |> Yojson.Safe.to_string

        let unmarshal s =
          match Yojson.Safe.from_string s with
          | `List lst ->
            List.map (function `List [ `String p; `String c] -> (p, c) | _ -> failwith "Failed to unmarshal files") lst
          | _ -> failwith "Failed to unmarshal files"
      end

      let or_raise = function
        | Ok v -> v
        | Error (`Msg m) -> failwith m

      let build No_context (job : Current.Job.t)
      (k : Key.t) : Value.t Current.or_error Lwt.t =
      Current.Job.start ~level:Harmless job >>= fun () ->
      Current_git.with_checkout ~job k.commit @@ fun dir ->
      let paths = List.map (fun file -> Fpath.(dir // file)) k.files in
      let contents = 
        try
          Ok (List.map (fun path -> (Fpath.to_string path, Bos.OS.File.read path |> or_raise)) paths)
        with Failure msg -> Error (`Msg msg)
      in
      Lwt.return contents
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
end
