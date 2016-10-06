
(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val safe_encode: ?alphabet:string -> string -> string
val safe_decode: ?alphabet:string -> string -> string

type data = ..

val decode: ?alphabet:string -> string -> data
val encode: ?alphabet:string -> data -> string

val decode_partial: ?alphabet:string -> string -> data list Lwt.t

type kind

val register:
  prefix:string ->
  read:(data -> string option) ->
  build:(string -> data) ->
  kind

val register_resolver:
  kind -> (string -> string list Lwt.t) -> unit

module Prefix : sig
  val block_hash: string
  val operation_hash: string
  val protocol_hash: string
  val public_key_hash: string
  val public_key: string
  val secret_key: string
  val signature: string
  val protocol_prefix: string
end
