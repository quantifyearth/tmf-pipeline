val engine : Uri.t -> Current_rpc.Engine.t Lwt.t
(** An engine from a capnp address *)

val ui : Current_rpc.Engine.t -> unit Lwt.t
