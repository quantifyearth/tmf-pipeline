module KV = struct
  type t = string * string

  let compare (k1, v1) (k2, v2) =
    let a = String.compare k1 k2 in
    if a = 0 then String.compare v1 v2 else a
end

module KVSet = Set.Make (KV)

type entry = { base_dir : string; map : KVSet.t }
type t = entry list
(* Build ID + Arkdir *)

let to_string ts =
  let entry e =
    let a =
      KVSet.to_seq e.map |> List.of_seq
      |> List.map (fun (k, v) -> (k, `String v))
    in
    `Assoc [ ("base_dir", `String e.base_dir); ("map", `Assoc a) ]
  in
  `List (List.map entry ts) |> Yojson.Safe.to_string

let of_string s : t =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string s in
  let entries = to_list json in
  let entry_from_json e =
    let base_dir = member "base_dir" e |> to_string in
    let map =
      member "map" e |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))
    in
    { base_dir; map = KVSet.of_list map }
  in
  List.map entry_from_json entries

let remap_base_dir id entry =
  { entry with base_dir = Filename.concat entry.base_dir id }

let mounts ((id, es) : string * t) =
  let mount id (e : entry) =
    let build_dir = e.base_dir in
    let target = Filename.concat e.base_dir id in
    Obuilder_spec.Rom.of_build ~hash:id ~build_dir target
  in
  let t = List.map (remap_base_dir id) es in
  (t, List.map (mount id) es)

let combine a b = a @ b
