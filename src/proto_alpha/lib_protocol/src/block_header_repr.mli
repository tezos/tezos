(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

type raw = Block_header.t
type shell_header = Block_header.shell_header

val encoding: block_header Data_encoding.encoding
val raw_encoding: raw Data_encoding.t
val proto_header_encoding: proto_header Data_encoding.encoding
val shell_header_encoding: shell_header Data_encoding.encoding

val max_header_length: int
(** The maximum size of block headers in bytes *)

val parse: Block_header.t -> block_header tzresult
(** Parse the (signed) protocol-specific part of a block header. *)

val parse_unsigned_proto_header: MBytes.t -> proto_header tzresult
(** Parse the (unsigned) protocol-specific part of a block header. *)

val forge_unsigned_proto_header: proto_header -> MBytes.t
(** [forge_header proto_hdr] is the binary serialization
    (using [proto_header_encoding]) of the protocol-specific part
    of a block header, without the signature. *)

val forge_unsigned:
  Block_header.shell_header -> proto_header -> MBytes.t
(** [forge_header shell_hdr proto_hdr] is the binary serialization
    (using [unsigned_header_encoding]) of a block header,
    comprising both the shell and the protocol part of the header,
    without the signature. *)

val hash: block_header -> Block_hash.t
val hash_raw: raw -> Block_hash.t
