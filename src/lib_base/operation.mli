(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type shell_header = {
  branch: Block_hash.t ;
}
val shell_header_encoding: shell_header Data_encoding.t

type t = {
  shell: shell_header ;
  proto: MBytes.t ;
}

include S.HASHABLE with type t := t
                    and type hash := Operation_hash.t
val of_bytes_exn: MBytes.t -> t
