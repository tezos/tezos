(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Low level Repr. of Managers' keys *)

type manager_key =
  | Hash of Ed25519.Public_key_hash.t
  | Public_key of Ed25519.Public_key.t

type t = manager_key

open Data_encoding

let hash_case tag =
  case ~tag Ed25519.Public_key_hash.encoding
    (function
      | Hash hash -> Some hash
      | _ -> None)
    (fun hash -> Hash hash)

let pubkey_case tag =
  case ~tag Ed25519.Public_key.encoding
    (function
      | Public_key hash -> Some hash
      | _ -> None)
    (fun hash -> Public_key hash)


let encoding =
  union [
    hash_case 0 ;
    pubkey_case 1 ;
  ]

