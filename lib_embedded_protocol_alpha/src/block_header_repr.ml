(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

(** Block header *)

(** Exported type *)
type t = {
  shell: Block_header.shell_header ;
  proto: proto_header ;
  signature: Ed25519.Signature.t ;
}

and proto_header = {
  priority: int ;
  seed_nonce_hash: Nonce_hash.t ;
  proof_of_work_nonce: MBytes.t ;
}

type block_header = t

type raw = Tezos_data.Block_header.t
type shell_header = Tezos_data.Block_header.shell_header

let raw_encoding = Tezos_data.Block_header.encoding
let shell_header_encoding = Tezos_data.Block_header.shell_header_encoding

let proto_header_encoding =
  let open Data_encoding in
  conv
    (fun { priority ; seed_nonce_hash ; proof_of_work_nonce } ->
       (priority, seed_nonce_hash, proof_of_work_nonce))
    (fun (priority, seed_nonce_hash, proof_of_work_nonce) ->
       { priority ; seed_nonce_hash ; proof_of_work_nonce })
    (obj3
       (req "priority" uint16)
       (req "seed_nonce_hash" Nonce_hash.encoding)
       (req "proof_of_work_nonce"
          (Fixed.bytes Constants_repr.proof_of_work_nonce_size)))

let signed_proto_header_encoding =
  let open Data_encoding in
  merge_objs
    proto_header_encoding
    (obj1 (req "signature" Ed25519.Signature.encoding))

let unsigned_header_encoding =
  let open Data_encoding in
  merge_objs
    Block_header.shell_header_encoding
    proto_header_encoding

let encoding =
  let open Data_encoding in
  conv
    (fun { shell ; proto ; signature } ->
       (shell, (proto, signature)))
    (fun (shell, (proto, signature)) ->
       { shell ; proto ; signature })
    (merge_objs
       Block_header.shell_header_encoding
       signed_proto_header_encoding)

(** Constants *)

let max_header_length =
  match Data_encoding.classify signed_proto_header_encoding with
  | `Fixed n -> n
  | `Dynamic | `Variable -> assert false

(** Header parsing entry point  *)

type error +=
  | Cant_parse_proto_header

let parse
    ({ shell = { level ; proto_level ; predecessor ;
                 timestamp ; fitness ; validation_passes ; operations_hash ;
                 context } ;
       proto } : Block_header.t) : block_header tzresult =
  match Data_encoding.Binary.of_bytes signed_proto_header_encoding proto with
  | None -> Error [Cant_parse_proto_header]
  | Some (proto, signature) ->
      let shell =
        { Block_header.level ; proto_level ; predecessor ;
          timestamp ; fitness ; validation_passes ; operations_hash ;
          context } in
      Ok { shell ; proto ; signature }

let parse_unsigned_proto_header bytes =
  match Data_encoding.Binary.of_bytes proto_header_encoding bytes with
  | None -> Error [Cant_parse_proto_header]
  | Some proto -> Ok proto

let forge_unsigned shell proto =
  Data_encoding.Binary.to_bytes unsigned_header_encoding (shell, proto)

let forge_unsigned_proto_header proto =
  Data_encoding.Binary.to_bytes proto_header_encoding proto

let hash_raw = Block_header.hash
let hash { shell ; proto ; signature } =
  Block_header.hash
    { shell ;
      proto =
        Data_encoding.Binary.to_bytes
          signed_proto_header_encoding
          (proto, signature ) }
