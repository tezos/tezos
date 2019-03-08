(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Proto_alpha
open Alpha_context

val tez_sym: string

val init_arg: (string, Proto_alpha.full) Clic.arg
val fee_arg: (Tez.t option, Proto_alpha.full) Clic.arg
val counter_arg: (Z.t option, Proto_alpha.full) Clic.arg
val gas_limit_arg: (Z.t option, Proto_alpha.full) Clic.arg
val storage_limit_arg: (Z.t option, Proto_alpha.full) Clic.arg
val arg_arg: (string option, Proto_alpha.full) Clic.arg
val source_arg: (string option, Proto_alpha.full) Clic.arg

val delegate_arg: (Signature.Public_key_hash.t option, Proto_alpha.full) Clic.arg
val delegatable_switch: (bool, Proto_alpha.full) Clic.arg
val spendable_switch: (bool, Proto_alpha.full) Clic.arg
val max_priority_arg: (int option, Proto_alpha.full) Clic.arg
val minimal_fees_arg: (Tez.tez, Proto_alpha.full) Clic.arg
val minimal_nanotez_per_gas_unit_arg: (Z.t, Proto_alpha.full) Clic.arg
val minimal_nanotez_per_byte_arg: (Z.t, Proto_alpha.full) Clic.arg
val force_low_fee_arg: (bool, Proto_alpha.full) Clic.arg
val fee_cap_arg: (Tez.t, Proto_alpha.full) Clic.arg
val burn_cap_arg: (Tez.t, Proto_alpha.full) Clic.arg
val no_waiting_for_endorsements_arg: (bool, Proto_alpha.full) Clic.arg
val await_endorsements_arg: (bool, Proto_alpha.full) Clic.arg
val force_switch: (bool, Proto_alpha.full) Clic.arg
val minimal_timestamp_switch: (bool, Proto_alpha.full) Clic.arg
val endorsement_delay_arg: (int, Proto_alpha.full) Clic.arg
val preserved_levels_arg: (int, Proto_alpha.full) Clic.arg

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
