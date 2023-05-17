type entry
(** A base directory and key-value map where the values
    are relative paths from the base directory. *)

type t
(** Arkdir *)

val of_string : string -> t
(** Reads the arkdir JSON string *)

val to_string : t -> string

val mounts : string * t -> t * Obuilder_spec.Rom.t list
(** The remapped mounting points along with an updated arkdir.json
    to find the new place of the files. *)

val combine : t -> t -> t
