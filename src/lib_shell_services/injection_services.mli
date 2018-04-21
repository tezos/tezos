(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

val block:
  #simple ->
  ?async:bool -> ?force:bool -> ?chain:Chain_services.chain ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val operation:
  #simple ->
  ?async:bool -> ?chain:Chain_services.chain ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val protocol:
  #simple ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module S : sig

  val block:
    ([ `POST ], unit,
     unit, < async: bool ;
             force: bool ;
             chain: Chain_services.chain option >, MBytes.t * Operation.t list list,
     Block_hash.t) RPC_service.t

  val operation:
    ([ `POST ], unit,
     unit, < async : bool;
             chain : Chain_services.chain option >, MBytes.t,
     Operation_hash.t) RPC_service.t

  val protocol:
    ([ `POST ], unit,
     unit, < async : bool;
             force : bool >, Protocol.t,
     Protocol_hash.t) RPC_service.t

end
