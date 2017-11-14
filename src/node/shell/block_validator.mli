(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type block_error =
  | Cannot_parse_operation of Operation_hash.t
  | Invalid_fitness of { expected: Fitness.t ; found: Fitness.t }
  | Non_increasing_timestamp
  | Non_increasing_fitness
  | Invalid_level of { expected: Int32.t ; found: Int32.t }
  | Invalid_proto_level of { expected: int ; found: int }
  | Replayed_operation of Operation_hash.t
  | Outdated_operation of
      { operation: Operation_hash.t;
        originating_block: Block_hash.t }
  | Expired_network of
      { net_id: Net_id.t ;
        expiration: Time.t ;
        timestamp: Time.t ;
      }
  | Unexpected_number_of_validation_passes of int (* uint8 *)

type error +=
  | Invalid_block of
      { block: Block_hash.t ; error: block_error }
  | Unavailable_protocol of
      { block: Block_hash.t ; protocol: Protocol_hash.t }
  | Inconsistent_operations_hash of
      { block: Block_hash.t ;
        expected: Operation_list_list_hash.t ;
        found: Operation_list_list_hash.t }

val create:
  protocol_timeout:float ->
  Distributed_db.t -> t

val validate:
  t ->
  ?canceler:Lwt_utils.Canceler.t ->
  ?peer:P2p.Peer_id.t ->
  ?notify_new_block:(State.Block.t -> unit) ->
  Distributed_db.net_db ->
  Block_hash.t -> Block_header.t -> Operation.t list list ->
  State.Block.t tzresult Lwt.t

val fetch_and_compile_protocol:
  t ->
  ?peer:P2p.Peer_id.t ->
  ?timeout:float ->
  Protocol_hash.t -> State.Registred_protocol.t tzresult Lwt.t

val shutdown: t -> unit Lwt.t
