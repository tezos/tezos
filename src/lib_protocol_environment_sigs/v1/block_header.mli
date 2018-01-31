(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type shell_header = {
  level: Int32.t ;
  (** The number of preceding block in this chain, i.e. the genesis
      has level 0. *)
  proto_level: int ;
  (** The number of preceding protocol change in the chain (modulo 256),
      i.e the genesis has proto_level 0. *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ;
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
  context: Context_hash.t ;
}

val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  proto: MBytes.t ;
}

include S.HASHABLE with type t := t
                    and type hash := Block_hash.t
