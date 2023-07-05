(* Reading project configurations -- https://github.com/carboncredits/tmf-data/blob/main/configurations/configuration-schema.json *)
type t = {
  vcs_id : int;
  country_code : string;
  agb : float array option; [@default None]
}
[@@deriving yojson]
