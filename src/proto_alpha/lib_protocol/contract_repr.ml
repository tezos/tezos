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

type t =
  | Implicit of Signature.Public_key_hash.t
  | Originated of Contract_hash.t

include Compare.Make(struct
    type nonrec t = t
    let compare l1 l2 =
      match l1, l2 with
      | Implicit pkh1, Implicit pkh2 ->
          Signature.Public_key_hash.compare pkh1 pkh2
      | Originated h1, Originated h2 ->
          Contract_hash.compare h1 h2
      | Implicit _, Originated _ -> -1
      | Originated _, Implicit _ -> 1
  end)

type contract = t

type error += Invalid_contract_notation of string (* `Permanent *)

let to_b58check = function
  | Implicit pbk -> Signature.Public_key_hash.to_b58check pbk
  | Originated h -> Contract_hash.to_b58check h

let of_b58check s =
  match Base58.decode s with
  | Some (Ed25519.Public_key_hash.Data h) -> ok (Implicit (Signature.Ed25519 h))
  | Some (Secp256k1.Public_key_hash.Data h) -> ok (Implicit (Signature.Secp256k1 h))
  | Some (P256.Public_key_hash.Data h) -> ok (Implicit (Signature.P256 h))
  | Some (Contract_hash.Data h) -> ok (Originated h)
  | _ -> error (Invalid_contract_notation s)

let pp ppf = function
  | Implicit pbk -> Signature.Public_key_hash.pp ppf pbk
  | Originated h -> Contract_hash.pp ppf h

let pp_short ppf = function
  | Implicit pbk -> Signature.Public_key_hash.pp_short ppf pbk
  | Originated h -> Contract_hash.pp_short ppf h

let encoding =
  let open Data_encoding in
  def "contract_id"
    ~title:
      "A contract handle"
    ~description:
      "A contract notation as given to an RPC or inside scripts. \
       Can be a base58 implicit contract hash \
       or a base58 originated contract hash." @@
  splitted
    ~binary:
      (union ~tag_size:`Uint8 [
          case (Tag 0)
            ~title:"Implicit"
            Signature.Public_key_hash.encoding
            (function Implicit k -> Some k | _ -> None)
            (fun k -> Implicit k) ;
          case (Tag 1) (Fixed.add_padding Contract_hash.encoding 1)
            ~title:"Originated"
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

let implicit_contract id = Implicit id

let is_implicit = function
  | Implicit m -> Some m
  | Originated _ -> None

let is_originated = function
  | Implicit _ -> None
  | Originated h -> Some h

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
    Data_encoding.Binary.to_bytes_exn origination_nonce_encoding nonce in
  Originated (Contract_hash.hash_bytes [data])

let originated_contracts
    ~since: { origination_index = first ; operation_hash = first_hash }
    ~until: ({ origination_index = last ; operation_hash = last_hash } as origination_nonce) =
  assert (Operation_hash.equal first_hash last_hash) ;
  let rec contracts acc origination_index =
    if Compare.Int32.(origination_index < first) then
      acc
    else
      let origination_nonce =
        { origination_nonce with origination_index } in
      let acc = originated_contract origination_nonce :: acc in
      contracts acc (Int32.pred origination_index) in
  contracts [] (Int32.pred last)

let initial_origination_nonce operation_hash =
  { operation_hash ; origination_index = 0l }

let incr_origination_nonce nonce =
  let origination_index = Int32.succ nonce.origination_index in
  { nonce with origination_index }

let rpc_arg =
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

module Index = struct
  type t = contract
  let path_length =

    assert Compare.Int.(Signature.Public_key_hash.path_length =
                        1 + Contract_hash.path_length) ;
    Signature.Public_key_hash.path_length
  let to_path c l =
    match c with
    | Implicit k ->
        Signature.Public_key_hash.to_path k l
    | Originated h ->
        "originated" :: Contract_hash.to_path h l
  let of_path = function
    | "originated" :: key -> begin
        match Contract_hash.of_path key with
        | None -> None
        | Some h -> Some (Originated h)
      end
    | key -> begin
        match Signature.Public_key_hash.of_path key with
        | None -> None
        | Some h -> Some (Implicit h)
      end
  let contract_prefix s =
    "originated" :: Contract_hash.prefix_path s
  let pkh_prefix_ed25519 s =
    Ed25519.Public_key_hash.prefix_path s
  let pkh_prefix_secp256k1 s =
    Secp256k1.Public_key_hash.prefix_path s
  let pkh_prefix_p256 s =
    P256.Public_key_hash.prefix_path s

  let rpc_arg = rpc_arg
  let encoding = encoding
  let compare = compare
end
