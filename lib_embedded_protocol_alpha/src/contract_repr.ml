(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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

type error += Invalid_contract_notation of string (* `Permanent *)

let to_b58check = function
  | Default pbk -> Ed25519.Public_key_hash.to_b58check pbk
  | Originated h -> Contract_hash.to_b58check h

let of_b58check s =
  match Base58.decode s with
  | Some (Ed25519.Public_key_hash.Hash h) -> ok (Default h)
  | Some (Contract_hash.Hash h) -> ok (Originated h)
  | _ -> error (Invalid_contract_notation s)

let pp ppf = function
  | Default pbk -> Ed25519.Public_key_hash.pp ppf pbk
  | Originated h -> Contract_hash.pp ppf h

let pp_short ppf = function
  | Default pbk -> Ed25519.Public_key_hash.pp_short ppf pbk
  | Originated h -> Contract_hash.pp_short ppf h

let encoding =
  let open Data_encoding in
  describe
    ~title:
      "A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. \
       Can be a base58 public key hash, representing the default contract \
       of this identity, or a base58 originated contract hash." @@
  splitted
    ~binary:
      (union ~tag_size:`Uint8 [
          case (Tag 0) Ed25519.Public_key_hash.encoding
            (function Default k -> Some k | _ -> None)
            (fun k -> Default k) ;
          case (Tag 1) Contract_hash.encoding
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
    ~id:"contract.invalid_contract_notation"
    ~title: "Invalid contract notation"
    ~pp: (fun ppf x -> Format.fprintf ppf "Invalid contract notation %S" x)
    ~description:
      "A malformed contract notation was given to an RPC or in a script."
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
  RPC_arg.make
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

module Index = struct
  type t = contract
  let path_length =
    assert Compare.Int.(Ed25519.Public_key_hash.path_length =
                        Contract_hash.path_length) ;
    Ed25519.Public_key_hash.path_length + 1
  let to_path c l =
    match c with
    | Default k ->
        "pubkey" :: Ed25519.Public_key_hash.to_path k l
    | Originated h ->
        "originated" :: Contract_hash.to_path h l
  let of_path = function
    | "pubkey" :: key -> begin
        match Ed25519.Public_key_hash.of_path key with
        | None -> None
        | Some h -> Some (Default h)
      end
    | "originated" :: key -> begin
        match Contract_hash.of_path key with
        | None -> None
        | Some h -> Some (Originated h)
      end
    | _ -> None
  let contract_prefix s =
    "originated" :: Contract_hash.prefix_path s
  let pkh_prefix s =
    "pubkey" :: Ed25519.Public_key_hash.prefix_path s
end
