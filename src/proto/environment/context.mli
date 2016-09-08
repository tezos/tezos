(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

open Hash

include Persist.STORE

val get_genesis_time: t -> Time.t Lwt.t
val get_genesis_block: t -> Block_hash.t Lwt.t
