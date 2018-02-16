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

val tez_sym: string

val init_arg: (string, Proto_alpha.full) Cli_entries.arg
val fee_arg: (Tez.t, Proto_alpha.full) Cli_entries.arg
val arg_arg: (string, Proto_alpha.full) Cli_entries.arg
val source_arg: (string option, Proto_alpha.full) Cli_entries.arg

val delegate_arg: (string option, Proto_alpha.full) Cli_entries.arg
val delegatable_switch: (bool, Proto_alpha.full) Cli_entries.arg
val spendable_switch: (bool, Proto_alpha.full) Cli_entries.arg
val max_priority_arg: (int option, Proto_alpha.full) Cli_entries.arg
val free_baking_switch: (bool, Proto_alpha.full) Cli_entries.arg
val force_switch: (bool, Proto_alpha.full) Cli_entries.arg
val endorsement_delay_arg: (int, Proto_alpha.full) Cli_entries.arg

val no_print_source_flag : (bool, Proto_alpha.full) Cli_entries.arg

val tez_arg :
  default:string ->
  parameter:string ->
  doc:string ->
  (Tez.t, Proto_alpha.full) Cli_entries.arg
val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Cli_entries.params ->
  (Tez.t -> 'a, full) Cli_entries.params

module Daemon : sig
  val baking_switch: (bool, Proto_alpha.full) Cli_entries.arg
  val endorsement_switch: (bool, Proto_alpha.full) Cli_entries.arg
  val denunciation_switch: (bool, Proto_alpha.full) Cli_entries.arg
end

val string_parameter : (string, full) Cli_entries.parameter
