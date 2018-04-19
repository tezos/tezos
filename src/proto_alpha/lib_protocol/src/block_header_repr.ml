(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Block header *)

type t = {
  shell: Block_header.shell_header ;
  protocol_data: protocol_data ;
}

and protocol_data = {
  contents: contents ;
  signature: Signature.t ;
}

and contents = {
  priority: int ;
  seed_nonce_hash: Nonce_hash.t option ;
  proof_of_work_nonce: MBytes.t ;
}

type block_header = t

type raw = Block_header.t
type shell_header = Block_header.shell_header

let raw_encoding = Block_header.encoding
let shell_header_encoding = Block_header.shell_header_encoding

let contents_encoding =
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

let protocol_data_encoding =
  let open Data_encoding in
  conv
    (fun { contents ; signature } -> (contents, signature))
    (fun (contents, signature) -> { contents ; signature })
    (merge_objs
       contents_encoding
       (obj1 (req "signature" Signature.encoding)))

let raw { shell ; protocol_data ; } =
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      protocol_data in
  { Block_header.shell ; protocol_data }

let unsigned_encoding =
  let open Data_encoding in
  merge_objs
    Block_header.shell_header_encoding
    contents_encoding

let encoding =
  let open Data_encoding in
  conv
    (fun { shell ; protocol_data } ->
       (shell, protocol_data))
    (fun (shell, protocol_data) ->
       { shell ; protocol_data })
    (merge_objs
       Block_header.shell_header_encoding
       protocol_data_encoding)

type metadata = {
  baker: Signature.Public_key_hash.t ;
  level: Level_repr.t ;
  voting_period_kind: Voting_period_repr.kind ;
}
let metadata_encoding =
  let open Data_encoding in
  conv
    (fun { baker ; level ; voting_period_kind } ->
       (baker, level, voting_period_kind))
    (fun (baker, level, voting_period_kind) ->
       { baker ; level ; voting_period_kind })
    (obj3
       (req "baker" Signature.Public_key_hash.encoding)
       (req "level" Level_repr.encoding)
       (req "voting_period_kind" Voting_period_repr.kind_encoding))

(** Constants *)

let max_header_length =
  let fake = { priority = 0 ;
               proof_of_work_nonce =
                 MBytes.create Constants_repr.proof_of_work_nonce_size ;
               seed_nonce_hash = Some Nonce_hash.zero } in
  Data_encoding.Binary.length
    protocol_data_encoding
    { contents = fake ; signature = Signature.zero}

(** Header parsing entry point  *)

let hash_raw = Block_header.hash
let hash { shell ; protocol_data } =
  Block_header.hash
    { shell ;
      protocol_data =
        Data_encoding.Binary.to_bytes_exn
          protocol_data_encoding
          protocol_data }
