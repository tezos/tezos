(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

val encoding: block_header Data_encoding.encoding
val raw_encoding: raw Data_encoding.t
val contents_encoding: contents Data_encoding.t
val unsigned_encoding: (Block_header.shell_header * contents) Data_encoding.t
val protocol_data_encoding: protocol_data Data_encoding.encoding
val shell_header_encoding: shell_header Data_encoding.encoding

val max_header_length: int
(** The maximum size of block headers in bytes *)

val hash: block_header -> Block_hash.t
val hash_raw: raw -> Block_hash.t
