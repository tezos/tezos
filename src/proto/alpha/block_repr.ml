(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

(** Block header *)

(** Exported type *)
type header = {
  shell: Updater.shell_block ;
  proto: proto_header ;
  signature: Ed25519.signature ;
}

and proto_header = {
  mining_slot: mining_slot ;
  seed_nonce_hash: Nonce_hash.t ;
  proof_of_work_nonce: MBytes.t ;}

and mining_slot = Raw_level_repr.t * Int32.t

let mining_slot_encoding =
  let open Data_encoding in
  tup2 Raw_level_repr.encoding int32

let proto_header_encoding =
  let open Data_encoding in
  conv
    (fun { mining_slot ; seed_nonce_hash ; proof_of_work_nonce } ->
       (mining_slot, seed_nonce_hash, proof_of_work_nonce))
    (fun (mining_slot, seed_nonce_hash, proof_of_work_nonce) ->
       { mining_slot ; seed_nonce_hash ; proof_of_work_nonce })
    (obj3
       (req "slot" mining_slot_encoding)
       (req "seed_nonce_hash" Nonce_hash.encoding)
       (req "proof_of_work_nonce" (Fixed.bytes Constants_repr.proof_of_work_nonce_size)))

let signed_proto_header_encoding =
  let open Data_encoding in
  merge_objs
    proto_header_encoding
    (obj1 (req "signature" Ed25519.signature_encoding))

let unsigned_header_encoding =
  let open Data_encoding in
  merge_objs
    Updater.shell_block_encoding
    proto_header_encoding

(** Constants *)

let max_header_length =
  match Data_encoding.classify signed_proto_header_encoding with
  | `Fixed n -> n
  | `Dynamic | `Variable -> assert false

(** Header parsing entry point  *)

type error +=
  | Cant_parse_proto_header

let parse_header
    ({ shell = { net_id ; predecessor ; timestamp ; fitness ; operations } ;
       proto } : Updater.raw_block) : header tzresult =
  match Data_encoding.Binary.of_bytes signed_proto_header_encoding proto with
  | None -> Error [Cant_parse_proto_header]
  | Some (proto, signature) ->
      let shell =
        { Updater.net_id ; predecessor ; timestamp ; fitness ; operations } in
      Ok { shell ; proto ; signature }

let forge_header shell proto =
  Data_encoding.Binary.to_bytes unsigned_header_encoding (shell, proto)
