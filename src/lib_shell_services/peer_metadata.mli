(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** All the (persistent) metadata associated to a peer. *)

type t

val encoding: t Data_encoding.t
val empty : unit -> t



(** the aggregate score function computed from
    the metadata collected for a peer *)
val distributed_db_score : t -> float
val prevalidation_score : t -> float
val score : t -> float

type requests_kind =
  | Branch | Head | Block_header
  | Operations | Protocols
  | Operation_hashes_for_block | Operations_for_block
  | Other

type resource_kind =
  | Block | Operations | Protocol

type advertisement = Head | Branch

type metadata =
  (* Distributed_db *)
  | Received_request of requests_kind
  | Sent_request of requests_kind
  | Failed_request of requests_kind
  | Scheduled_request of requests_kind
  | Received_response of requests_kind
  | Sent_response of requests_kind
  | Unexpected_response
  | Unactivated_chain
  | Inactive_chain
  | Future_block
  | Unadvertised of resource_kind
  | Sent_advertisement of advertisement
  | Received_advertisement of advertisement
  | Outdated_response (* TODO : unused *)
  (* Peer validator *)
  | Valid_blocks | Old_heads
  (* Prevalidation *)
  | Cannot_download | Cannot_parse
  | Refused_by_prefilter
  | Refused_by_postfilter
  | Applied | Branch_delayed
  | Branch_refused
  | Refused | Duplicate | Outdated

(** incr score counters . Used to compute the final score for a peer *)
val incr : t ->   metadata -> unit
val update_requests : t -> requests_kind -> bool -> unit
val update_responses : t -> requests_kind -> bool -> unit


