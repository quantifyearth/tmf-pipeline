module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile

module Repos : sig
  (** Useful 4C repositories *)

  val tmf_implementation : string -> Current_git.Commit.t Current.t
  (** The TMF methodology implementation code at [gref]. *)

  val tmf_data : unit -> Current_git.Commit.t Current.t
  (** The TMF data repository for projection configurations *)
end

module Python : sig
  val spec : Obuilder_spec.t
  (** A python-geo OBuilder specification. *)

  val spec_with_data_dir : Obuilder_spec.t
  (** Like {! spec} but creates a data direction in the working directory *)
end

val data_spec : Obuilder_spec.t
(** A simple OBuilder specification that will copy all of the context data into
    a directory called [/data] in the container. *)

val snapshots_to_rom :
  (Current_obuilder.Raw.Build.Value.t Current.t * string * string) list ->
  Obuilder_spec.Rom.t Current.t list
(** Converts a list of build output, src directory, target directory tuples into a suitable
    read-only mount description for OBuilder. *)

val jrc :
  builder:Current_obuilder.builder ->
  Current_obuilder.Raw.Build.Value.t Current.t ->
  Current_obuilder.Raw.Build.Value.t Current.t
(** [jrc ~builder base] downloads JRC data into a container. *)

val ecoregions :
  builder:Current_obuilder.builder ->
  Current_obuilder.Raw.Build.Value.t Current.t ->
  Current_obuilder.Raw.Build.Value.t Current.t
(** Downloads ecoregion data into a container. *)

val countries :
  builder:Current_obuilder.builder ->
  Current_obuilder.Raw.Build.Value.t Current.t ->
  Current_obuilder.Raw.Build.Value.t Current.t
(** Downloads country data into a container. *)

val evaluate :
  project_name:string ->
  builder:Current_obuilder.builder ->
  jrc:Current_obuilder.Raw.Build.Value.t Current.t ->
  gedi_base_img:Current_obuilder.Raw.Build.Value.t Current.t ->
  config_img:Current_obuilder.Raw.Build.Value.t Current.t ->
  Current_obuilder.Raw.Build.Value.t Current.t ->
  unit Current.t
(** The evaluation pipeline for a particular project. The [config_img] contains the
      configuration file in an OBuilder image and the final build argument contains the
      base image used for the evaluation itself (e.g. a python-geo image). *)
