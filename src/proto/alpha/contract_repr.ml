(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

type t =
  | Default of Ed25519.Public_key_hash.t
  | Originated of Contract_hash.t
type contract = t

type error += Invalid_contract_notation of string

let to_b58check = function
  | Default pbk -> Ed25519.Public_key_hash.to_b58check pbk
  | Originated h -> Contract_hash.to_b58check h

let of_b58check s =
  match Base58.decode s with
  | Some (Ed25519.Public_key_hash.Hash h) -> ok (Default h)
  | Some (Contract_hash.Hash h) -> ok (Originated h)
  | _ -> error (Invalid_contract_notation s)

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
      (union ~tag_size:`Uint8 [
          case ~tag:0 Ed25519.Public_key_hash.encoding
            (function Default k -> Some k | _ -> None)
            (fun k -> Default k) ;
          case ~tag:1 Contract_hash.encoding
            (function Originated k -> Some k | _ -> None)
            (fun k -> Originated k) ;
        ])
    ~json:
      (conv
         to_b58check
         (fun s ->
            match of_b58check s with
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
  | Originated _ -> None


type origination_nonce =
  { operation_hash: Operation_hash.t ;
    origination_index: int32 }

let origination_nonce_encoding =
  let open Data_encoding in
  conv
    (fun { operation_hash ; origination_index } ->
       (operation_hash, origination_index))
    (fun (operation_hash, origination_index) ->
       { operation_hash ; origination_index }) @@
  obj2
    (req "operation" Operation_hash.encoding)
    (dft "index" int32 0l)

let originated_contract nonce =
  let data =
    Data_encoding.Binary.to_bytes origination_nonce_encoding nonce in
  Originated (Contract_hash.hash_bytes [data])

let originated_contracts ({ origination_index } as origination_nonce) =
  let rec contracts acc origination_index =
    if Compare.Int32.(origination_index < 0l) then
      acc
    else
      let origination_nonce =
        { origination_nonce with origination_index } in
      let acc = originated_contract origination_nonce :: acc in
      contracts acc (Int32.pred origination_index) in
  contracts [] (Int32.pred origination_index)

let initial_origination_nonce operation_hash =
  { operation_hash ; origination_index = 0l }

let incr_origination_nonce nonce =
  let origination_index = Int32.succ nonce.origination_index in
  { nonce with origination_index }

let arg =
  let construct = to_b58check in
  let destruct hash =
    match of_b58check hash with
    | Error _ -> Error "Cannot parse contract id"
    | Ok contract -> Ok contract in
  RPC.Arg.make
    ~descr: "A contract identifier encoded in b58check."
    ~name: "contract_id"
    ~construct
    ~destruct
    ()

let compare l1 l2 =
  match l1, l2 with
  | Default pkh1, Default pkh2 ->
      Ed25519.Public_key_hash.compare pkh1 pkh2
  | Originated h1, Originated h2 ->
      Contract_hash.compare h1 h2
  | Default _, Originated _ -> -1
  | Originated _, Default _ -> 1
let (=) l1 l2 = Compare.Int.(=) (compare l1 l2) 0
let (<>) l1 l2 = Compare.Int.(<>) (compare l1 l2) 0
let (>) l1 l2 = Compare.Int.(>) (compare l1 l2) 0
let (>=) l1 l2 = Compare.Int.(>=) (compare l1 l2) 0
let (<=) l1 l2 = Compare.Int.(<=) (compare l1 l2) 0
let (<) l1 l2 = Compare.Int.(<) (compare l1 l2) 0
let min l1 l2 = if l1 <= l2 then l1 else l2
let max l1 l2 = if l1 >= l2 then l1 else l2
