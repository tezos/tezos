(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val run:
  #Proto_alpha.full ->
  ?max_priority: int ->
  delay: int ->
  public_key_hash list ->
  endorsement:bool ->
  denunciation:bool ->
  baking:bool -> unit tzresult Lwt.t
