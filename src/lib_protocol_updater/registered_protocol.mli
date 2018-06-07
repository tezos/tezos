(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type T = sig
  module P : sig
    val hash: Protocol_hash.t
    include Tezos_protocol_environment_shell.PROTOCOL
  end
  include (module type of (struct include P end))
  module Block_services :
    (module type of (struct include Block_services.Make(P)(P) end))
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

type t = (module T)

val mem: Protocol_hash.t -> bool

val list: unit -> t list

val get: Protocol_hash.t -> t option
val get_exn: Protocol_hash.t -> t

val list_embedded: unit -> Protocol_hash.t list

val get_embedded_sources: Protocol_hash.t -> Protocol.t option
val get_embedded_sources_exn: Protocol_hash.t -> Protocol.t

module Register_embedded
    (Env : Tezos_protocol_environment_shell.V1)
    (Proto : Env.Updater.PROTOCOL)
    (Source : sig
       val hash: Protocol_hash.t option
       val sources: Protocol.t
     end) : sig end
