(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type T = sig
  val hash: Protocol_hash.t
  include Updater.NODE_PROTOCOL
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

type t = (module T)

val mem: Protocol_hash.t -> bool

val get: Protocol_hash.t -> t option
val get_exn: Protocol_hash.t -> t


module Register
    (Env : Updater.Node_protocol_environment_sigs.V1)
    (Proto : Env.Updater.PROTOCOL)
    (Source : sig
       val hash: Protocol_hash.t option
       val sources: Protocol.t
     end) : sig end
