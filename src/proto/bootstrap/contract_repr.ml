(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

type descr = {
  manager: Ed25519.public_key_hash ;
  delegate: Ed25519.public_key_hash option ;
  spendable: bool ;
  delegatable: bool ;
  script: Script_repr.t ;
}

type t =
  | Default of Ed25519.public_key_hash
  | Hash of Contract_hash.t
type contract = t

type error += Invalid_contract_notation of string

let to_b48check = function
  | Default pbk -> Ed25519.Public_key_hash.to_b48check pbk
  | Hash h -> Contract_hash.to_b48check h

let of_b48check s =
  try
    match Base48.decode s with
    | Ed25519.Public_key_hash.Hash h -> ok (Default h)
    | Contract_hash.Hash h -> ok (Hash h)
    | _ -> error (Invalid_contract_notation s)
  with _ -> error (Invalid_contract_notation s)

let encoding =
  let open Data_encoding in
  describe
    ~title:
      "A contract handle"
    ~description:
      "A contract notation as given to a RPC or inside scripts. \
       Contract handles can be written 'd<base64 encoded ID>d' \
       for the default contract of some ID (public key hash) or \
       'h<base64 encoded contract ID>h' for a created contract or account, \
       as replied by the contract origination RPC." @@
  splitted
    ~binary:
      (union ~tag_size:`Int8 [
          case ~tag:0 Ed25519.public_key_hash_encoding
            (function Default k -> Some k | _ -> None)
            (fun k -> Default k) ;
          case ~tag:1 Contract_hash.encoding
            (function Hash k -> Some k | _ -> None)
            (fun k -> Hash k) ;
        ])
    ~json:
      (conv
         to_b48check
         (fun s ->
            match of_b48check s with
            | Ok s -> s
            | Error _ -> Json.cannot_destruct "Invalid contract notation.")
         string)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"InvalidContractNotationError"
    ~title: "Invalid contract notation"
    ~description:
      "A malformed contract notation was given to a RPC or by a script. \
       Contract handles can be written 'd<base encoded ID>d' \
       for the default contract of some ID (public key hash) or \
       'h<base encoded contract ID>h' for a created contract or account, \
       as replied by the contract origination RPC."
    (obj1 (req "notation" string))
    (function Invalid_contract_notation loc -> Some loc | _ -> None)
    (fun loc -> Invalid_contract_notation loc)

let default_contract id = Default id

let is_default = function
  | Default m -> Some m
  | Hash _ -> None

let descr_encoding =
  let open Data_encoding in
  conv
    (fun { manager; delegate; spendable; delegatable; script } ->
       (manager, delegate, spendable, delegatable, script))
    (fun (manager, delegate, spendable, delegatable, script) ->
       { manager; delegate; spendable; delegatable; script })
    (obj5
       (req "manager" Ed25519.public_key_hash_encoding)
       (opt "delegate" Ed25519.public_key_hash_encoding)
       (dft "spendable" bool false)
       (dft "delegatable" bool false)
       (req "script" Script_repr.encoding))

let generic_contract ~manager ~delegate ~spendable ~delegatable ~script =
  match delegate, spendable, delegatable, script with
  | Some delegate, true, false, Script_repr.No_script
    when Ed25519.equal_hash manager delegate ->
      default_contract manager
  | _ ->
      let data =
        Data_encoding.Binary.to_bytes
          descr_encoding
          { manager; delegate; spendable; delegatable; script } in
      Hash (Contract_hash.hash_bytes [data])

let arg =
  let construct = to_b48check in
  let destruct hash =
    match of_b48check hash with
    | Error _ -> Error "Cannot parse contract id"
    | Ok contract -> Ok contract in
  RPC.Arg.make
    ~descr: "A contract identifier encoded in b48check."
    ~name: "contract_id"
    ~construct
    ~destruct

let compare l1 l2 =
  match l1, l2 with
  | Default pkh1, Default pkh2 ->
      Ed25519.compare_hash pkh1 pkh2
  | Hash h1, Hash h2 ->
      Contract_hash.compare h1 h2
  | Default _, Hash _ -> -1
  | Hash _, Default _ -> 1
let (=) l1 l2 = Compare.Int.(=) (compare l1 l2) 0
let (<>) l1 l2 = Compare.Int.(<>) (compare l1 l2) 0
let (>) l1 l2 = Compare.Int.(>) (compare l1 l2) 0
let (>=) l1 l2 = Compare.Int.(>=) (compare l1 l2) 0
let (<=) l1 l2 = Compare.Int.(<=) (compare l1 l2) 0
let (<) l1 l2 = Compare.Int.(<) (compare l1 l2) 0
let min l1 l2 = if l1 <= l2 then l1 else l2
let max l1 l2 = if l1 >= l2 then l1 else l2
