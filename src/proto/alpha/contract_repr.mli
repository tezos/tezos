(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

type t = private
  | Default of Ed25519.Public_key_hash.t
  | Originated of Contract_hash.t
type contract = t

include Compare.S with type t := contract

(** {2 Default contracts} *****************************************************)

val default_contract : Ed25519.Public_key_hash.t -> contract

val is_default : contract -> Ed25519.Public_key_hash.t option

(** {2 Originated contracts} **************************************************)

(** Originated contracts handles are crafted from the hash of the
    operation that triggered their origination (and nothing else).
    As a single operation can trigger several originations, the
    corresponding handles are forged from a deterministic sequence of
    nonces, initialized with the hash of the operation. *)
type origination_nonce

val originated_contract : origination_nonce -> contract

val originated_contracts : origination_nonce -> contract list

val initial_origination_nonce : Operation_hash.t -> origination_nonce

val incr_origination_nonce : origination_nonce -> origination_nonce


(** {2 Human readable notation} ***********************************************)

type error += Invalid_contract_notation of string (* `Permanent *)

val to_b58check: contract -> string

val of_b58check: string -> contract tzresult

val pp: Format.formatter -> contract -> unit

val pp_short: Format.formatter -> contract -> unit

(** {2 Serializers} ***********************************************************)

val encoding : contract Data_encoding.t

val origination_nonce_encoding : origination_nonce Data_encoding.t

val arg : contract RPC.Arg.arg
