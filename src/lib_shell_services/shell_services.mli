(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open RPC_context

val forge_block_header:
  #simple ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

val inject_block:
  #simple ->
  ?async:bool -> ?force:bool -> ?chain_id:Chain_id.t ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  #simple ->
  ?async:bool -> ?chain_id:Chain_id.t ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  #simple ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

val bootstrapped:
  #streamed -> ((Block_hash.t * Time.t) Lwt_stream.t * stopper) tzresult Lwt.t

module Monitor : sig

  val valid_blocks:
    #streamed ->
    ?chains:Chain_services.chain list ->
    ?protocols:Protocol_hash.t list ->
    ?next_protocols:Protocol_hash.t list ->
    unit -> ((Chain_id.t * Block_hash.t) Lwt_stream.t * stopper) tzresult Lwt.t

  val heads:
    #streamed ->
    ?next_protocols:Protocol_hash.t list ->
    Chain_services.chain ->
    (Block_hash.t Lwt_stream.t * stopper) tzresult Lwt.t

end

module S : sig

  val forge_block_header:
    ([ `POST ], unit,
     unit, unit, Block_header.t,
     MBytes.t) RPC_service.t

  type inject_block_param = {
    raw: MBytes.t ;
    blocking: bool ;
    force: bool ;
    chain_id: Chain_id.t option ;
    operations: Operation.t list list ;
  }

  val inject_block:
    ([ `POST ], unit,
     unit, unit, inject_block_param,
     Block_hash.t) RPC_service.t

  val inject_operation:
    ([ `POST ], unit,
     unit, unit, (MBytes.t * bool * Chain_id.t option),
     Operation_hash.t) RPC_service.t

  val inject_protocol:
    ([ `POST ], unit,
     unit, unit, (Protocol.t * bool * bool option),
     Protocol_hash.t) RPC_service.t

  val bootstrapped:
    ([ `POST ], unit,
     unit, unit, unit,
     Block_hash.t * Time.t) RPC_service.t

  module Monitor : sig

    val valid_blocks:
      ([ `GET ], unit,
       unit, < chains : Chain_services.chain list;
               next_protocols : Protocol_hash.t list;
               protocols : Protocol_hash.t list >, unit,
       Chain_id.t * Block_hash.t) RPC_service.t

    val heads:
      ([ `GET ], unit,
       unit * Chain_services.chain,
       < next_protocols : Protocol_hash.t list >, unit,
       Block_hash.t) RPC_service.t

  end

end
