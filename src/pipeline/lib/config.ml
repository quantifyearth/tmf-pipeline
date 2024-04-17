(* Reading project configurations -- https://github.com/carboncredits/tmf-data/blob/main/configurations/configuration-schema.json *)
type t = {
  vcs_id : int;
  country_code : string;
  project_start : int;
  source : string option; [@default None]
  agb : float array option; [@default None]
}
[@@deriving yojson]

type old_t = {
  vcs_id : int;
  country_code : string;
  start_year : int;
  source : string option; [@default None]
  agb : float array option; [@default None]
}
[@@deriving yojson]

let t_of_old_t (t : old_t) : t =
  {
    vcs_id = t.vcs_id;
    country_code = t.country_code;
    project_start = t.start_year;
    source = t.source;
    agb = t.agb;
  }

let of_yojson y =
  match of_yojson y with
  | Ok v -> Ok v
  | Error _ -> Result.map t_of_old_t (old_t_of_yojson y)
