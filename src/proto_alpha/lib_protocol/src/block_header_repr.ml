(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Block header *)

(** Exported type *)
type t = {
  shell: Block_header.shell_header ;
  protocol_data: protocol_data ;
  signature: Signature.t ;
}

and protocol_data = {
  priority: int ;
  seed_nonce_hash: Nonce_hash.t option ;
  proof_of_work_nonce: MBytes.t ;
}

type block_header = t

type raw = Block_header.t
type shell_header = Block_header.shell_header

let raw_encoding = Block_header.encoding
let shell_header_encoding = Block_header.shell_header_encoding

let protocol_data_encoding =
  let open Data_encoding in
  conv
    (fun { priority ; seed_nonce_hash ; proof_of_work_nonce } ->
       (priority, proof_of_work_nonce, seed_nonce_hash))
    (fun (priority, proof_of_work_nonce, seed_nonce_hash) ->
       { priority ; seed_nonce_hash ; proof_of_work_nonce })
    (obj3
       (req "priority" uint16)
       (req "proof_of_work_nonce"
          (Fixed.bytes Constants_repr.proof_of_work_nonce_size))
       (opt "seed_nonce_hash" Nonce_hash.encoding))

let signed_protocol_data_encoding =
  let open Data_encoding in
  merge_objs
    protocol_data_encoding
    (obj1 (req "signature" Signature.encoding))

let unsigned_header_encoding =
  let open Data_encoding in
  merge_objs
    Block_header.shell_header_encoding
    protocol_data_encoding

let encoding =
  let open Data_encoding in
  conv
    (fun { shell ; protocol_data ; signature } ->
       (shell, (protocol_data, signature)))
    (fun (shell, (protocol_data, signature)) ->
       { shell ; protocol_data ; signature })
    (merge_objs
       Block_header.shell_header_encoding
       signed_protocol_data_encoding)

(** Constants *)

let max_header_length =
  let fake = { priority = 0 ;
               proof_of_work_nonce =
                 MBytes.create Constants_repr.proof_of_work_nonce_size ;
               seed_nonce_hash = Some Nonce_hash.zero } in
  Data_encoding.Binary.length
    signed_protocol_data_encoding
    (fake, Signature.zero)


(** Header parsing entry point  *)

type error +=
  | Cant_parse_protocol_data

let parse
    ({ shell = { level ; proto_level ; predecessor ;
                 timestamp ; fitness ; validation_passes ; operations_hash ;
                 context } ;
       protocol_data } : Block_header.t) : block_header tzresult =
  match
    Data_encoding.Binary.of_bytes signed_protocol_data_encoding protocol_data
  with
  | None -> Error [Cant_parse_protocol_data]
  | Some (protocol_data, signature) ->
      let shell =
        { Block_header.level ; proto_level ; predecessor ;
          timestamp ; fitness ; validation_passes ; operations_hash ;
          context } in
      Ok { shell ; protocol_data ; signature }

let parse_unsigned_protocol_data bytes =
  match Data_encoding.Binary.of_bytes protocol_data_encoding bytes with
  | None -> Error [Cant_parse_protocol_data]
  | Some proto -> Ok proto

let forge_unsigned shell proto =
  Data_encoding.Binary.to_bytes_exn unsigned_header_encoding (shell, proto)

let forge_unsigned_protocol_data proto =
  Data_encoding.Binary.to_bytes_exn protocol_data_encoding proto

let hash_raw = Block_header.hash
let hash { shell ; protocol_data ; signature } =
  Block_header.hash
    { shell ;
      protocol_data =
        Data_encoding.Binary.to_bytes_exn
          signed_protocol_data_encoding
          (protocol_data, signature ) }
