(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val delegate_commands : unit -> Proto_alpha.full Clic.command list

val baker_commands : unit -> Proto_alpha.full Clic.command list
val endorser_commands : unit -> Proto_alpha.full Clic.command list
val accuser_commands : unit -> Proto_alpha.full Clic.command list
