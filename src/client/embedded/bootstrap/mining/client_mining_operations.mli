(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type operation = {
  hash: Operation_hash.t ;
  content: (Updater.shell_operation * proto_operation) option
}

val monitor:
  Client_commands.context ->
  ?contents:bool -> ?check:bool -> unit ->
  operation list Lwt_stream.t Lwt.t

type valid_endorsement = {
  hash: Operation_hash.t ;
  source: public_key_hash ;
  block: Block_hash.t ;
  slots: int list ;
}

val filter_valid_endorsement:
  Client_commands.context ->
  operation -> valid_endorsement option Lwt.t

val monitor_endorsement:
  Client_commands.context ->
  valid_endorsement Lwt_stream.t Lwt.t
