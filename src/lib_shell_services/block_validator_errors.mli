(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  | Expired_chain of
      { chain_id: Chain_id.t ;
        expiration: Time.t ;
        timestamp: Time.t ;
      }
  | Unexpected_number_of_validation_passes of int (* uint8 *)
  | Too_many_operations of { pass: int; found: int; max: int }
  | Oversized_operation of { operation: Operation_hash.t;
                             size: int; max: int }
  | Unallowed_pass of { operation: Operation_hash.t ;
                        pass: int ;
                        allowed_pass: int list }
  | Cannot_parse_block_header

type error +=
  | Invalid_block of
      { block: Block_hash.t ; error: block_error }
  | Unavailable_protocol of
      { block: Block_hash.t ; protocol: Protocol_hash.t }
  | Inconsistent_operations_hash of
      { block: Block_hash.t ;
        expected: Operation_list_list_hash.t ;
        found: Operation_list_list_hash.t }
