(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Low level Repr. of Managers' keys *)

(** The public key of the manager of a contract is reveled only after the
    first operation. At Origination time, the manager provides only the hash
    of its public key that is stored in the contract. When the public key
    is actually reveeld, the public key instead of the hash of the key *)
type manager_key =
  | Hash of Signature.Public_key_hash.t
  | Public_key of Signature.Public_key.t

type t = manager_key

val encoding : t Data_encoding.encoding
