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

type t = private
  | Implicit of Signature.Public_key_hash.t
  | Originated of Contract_hash.t
type contract = t

include Compare.S with type t := contract

(** {2 Implicit contracts} *)

val implicit_contract : Signature.Public_key_hash.t -> contract

val is_implicit : contract -> Signature.Public_key_hash.t option

(** {2 Originated contracts} *)

(** Originated contracts handles are crafted from the hash of the
    operation that triggered their origination (and nothing else).
    As a single operation can trigger several originations, the
    corresponding handles are forged from a deterministic sequence of
    nonces, initialized with the hash of the operation. *)
type origination_nonce

val originated_contract : origination_nonce -> contract

val originated_contracts : since: origination_nonce -> until: origination_nonce -> contract list

val initial_origination_nonce : Operation_hash.t -> origination_nonce

val incr_origination_nonce : origination_nonce -> origination_nonce

val is_originated : contract -> Contract_hash.t option


(** {2 Human readable notation} *)

type error += Invalid_contract_notation of string (* `Permanent *)

val to_b58check: contract -> string

val of_b58check: string -> contract tzresult

val pp: Format.formatter -> contract -> unit

val pp_short: Format.formatter -> contract -> unit

(** {2 Serializers} *)

val encoding : contract Data_encoding.t

val origination_nonce_encoding : origination_nonce Data_encoding.t

val rpc_arg : contract RPC_arg.arg

module Index : sig
  include Storage_description.INDEX with type t = t
  val contract_prefix: string -> string list
  val pkh_prefix_ed25519: string -> string list
  val pkh_prefix_secp256k1: string -> string list
  val pkh_prefix_p256: string -> string list
end
