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
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
}

val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  proto: MBytes.t ;
}

include S.HASHABLE with type t := t
                    and type hash := Block_hash.t
val of_bytes_exn: MBytes.t -> t
