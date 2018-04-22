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

val init_arg: (string, Proto_alpha.full) Clic.arg
val fee_arg: (Tez.t, Proto_alpha.full) Clic.arg
val gas_limit_arg: (Z.t option, Proto_alpha.full) Clic.arg
val storage_limit_arg: (Int64.t option, Proto_alpha.full) Clic.arg
val arg_arg: (string, Proto_alpha.full) Clic.arg
val source_arg: (string option, Proto_alpha.full) Clic.arg

val delegate_arg: (Signature.Public_key_hash.t option, Proto_alpha.full) Clic.arg
val delegatable_switch: (bool, Proto_alpha.full) Clic.arg
val spendable_switch: (bool, Proto_alpha.full) Clic.arg
val max_priority_arg: (int option, Proto_alpha.full) Clic.arg
val free_baking_switch: (bool, Proto_alpha.full) Clic.arg
val force_switch: (bool, Proto_alpha.full) Clic.arg
val minimal_timestamp_switch: (bool, Proto_alpha.full) Clic.arg
val endorsement_delay_arg: (int, Proto_alpha.full) Clic.arg

val no_print_source_flag: (bool, Proto_alpha.full) Clic.arg
val no_confirmation: (bool, Proto_alpha.full) Clic.arg

val tez_arg :
  default:string ->
  parameter:string ->
  doc:string ->
  (Tez.t, Proto_alpha.full) Clic.arg
val tez_param :
  name:string ->
  desc:string ->
  ('a, full) Clic.params ->
  (Tez.t -> 'a, full) Clic.params

module Daemon : sig
  val baking_switch: (bool, Proto_alpha.full) Clic.arg
  val endorsement_switch: (bool, Proto_alpha.full) Clic.arg
  val denunciation_switch: (bool, Proto_alpha.full) Clic.arg
end

val string_parameter : (string, full) Clic.parameter
