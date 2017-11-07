(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val run:
  Client_commands.full_context ->
  ?max_priority: int ->
  delay: int ->
  ?min_date: Time.t ->
  public_key_hash list ->
  endorsement:bool ->
  denunciation:bool ->
  baking:bool -> unit tzresult Lwt.t
