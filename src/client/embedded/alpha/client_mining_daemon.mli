(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val run:
  Client_commands.context ->
  ?max_priority: int ->
  delay: int ->
  ?min_date: Time.t ->
  public_key_hash list ->
  endorsement:bool ->
  denunciation:bool ->
  mining:bool -> unit tzresult Lwt.t
