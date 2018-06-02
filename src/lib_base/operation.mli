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

val bounded_encoding: ?max_size:int -> unit -> t Data_encoding.t
val bounded_list_encoding:
  ?max_length:int ->
  ?max_size:int ->
  ?max_operation_size:int ->
  ?max_pass:int ->
  unit -> (Operation_list_list_hash.path * t list) Data_encoding.t
val bounded_hash_list_encoding:
  ?max_length:int ->
  ?max_pass:int ->
  unit -> (Operation_list_list_hash.path * Operation_hash.t list) Data_encoding.t

