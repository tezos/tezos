(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
        expiration: Time.Protocol.t ;
        timestamp: Time.Protocol.t ;
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
  | Failed_to_checkout_context of Context_hash.t
  | System_error of { errno: Unix.error ;
                      fn: string ;
                      msg: string }
  | Missing_test_protocol of Protocol_hash.t

val invalid_block : Block_hash.t -> block_error -> error
