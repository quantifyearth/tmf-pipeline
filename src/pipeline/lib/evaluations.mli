module Current_obuilder = Current_obuilder
module Current_gitfile = Current_gitfile

module Repos : sig
  (** Useful 4C repositories *)

  val tmf_implementation : unit -> Current_git.Commit.t Current.t
  (** The TMF methodology implementation code *)

  val tmf_data : unit -> Current_git.Commit.t Current.t
  (** The TMF data repository for projection configurations *)
end

module Python : sig
  val spec : Obuilder_spec.t
  (** A python-geo OBuilder specification. *)
end

val data_spec : Obuilder_spec.t
(** A simple OBuilder specification that will copy all of the context data into
    a directory called [/data] in the container. *)

val snapshots_to_rom :
  (Current_obuilder.Raw.Build.Value.t Current.term * string * string) list ->
  Obuilder_spec.Rom.t Current.term list
(** Converts a list of build output, src directory, target directory tuples into a suitable
    read-only mount description for OBuilder. *)

val evaluate :
  project_name:string ->
  builder:Current_obuilder.builder ->
  config_img:Current_obuilder.Raw.Build.Value.t Current.term ->
  Current_obuilder.Raw.Build.Value.t Current.term ->
  unit Current.term
(** The evaluation pipeline for a particular project. The [config_img] contains the
      configuration file in an OBuilder image and the final build argument contains the
      base image used for the evaluation itself (e.g. a python-geo image). *)
