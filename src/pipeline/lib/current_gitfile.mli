module Raw : sig
  module Git_file : sig
    type t = No_context

    val auto_cancel : bool
    val id : string

    module Key : sig
      type t

      val to_json : t -> Yojson.t
      val digest : t -> string
    end

    val pp : Key.t Fmt.t

    module Value : sig
      type t = (Fpath.t * string) list
      (** The path of the file along with the contents of the file. *)

      val marshal : t -> string
      val unmarshal : string -> t
    end
  end

  module Git_dir : sig
    module Value : sig
      type t = { dir : Fpath.t; files : Fpath.t list; digest : string }
      (** The directory path, the contents of that directory and the accumulative digest of those files.
          Note this will be only one-level in the filesystem hierarchy. *)

      val marshal : t -> string
      val unmarshal : string -> t
    end
  end
end

val contents :
  ?schedule:Current_cache.Schedule.t ->
  Current_git.Commit.t Current.term ->
  Fpath.t list ->
  Raw.Git_file.Value.t Current.term
(** [contents git paths] returns the contents of each of the files in [paths]
    from the git repo [git]. *)

val directory_contents :
  ?schedule:Current_cache.Schedule.t ->
  Current_git.Commit.t Current.term ->
  Fpath.t ->
  Raw.Git_dir.Value.t Current.term
(** [directory_contents git dir] returns each of the files (without recursing) in [dir] along
    with the cumulative digest of the directory. *)
