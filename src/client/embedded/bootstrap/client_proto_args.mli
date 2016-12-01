(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val block: unit -> Client_node_rpcs.Blocks.block

val tez_sym: string

val init_arg: string * Arg.spec * string
val fee_arg: string * Arg.spec * string
val arg_arg: string * Arg.spec * string
val source_arg: string * Arg.spec * string
val delegate_arg: string * Arg.spec * string
val delegatable_args: (string * Arg.spec * string) list
val spendable_args: (string * Arg.spec * string) list
val max_priority_arg: string * Arg.spec * string
val force_arg: string * Arg.spec * string
val endorsement_delay_arg: string * Arg.spec * string

val tez_param :
  name:string ->
  desc:string ->
  'a Cli_entries.params -> (Tez.t -> 'a) Cli_entries.params

val delegate: string option ref
val source: string option ref
val delegatable: bool ref
val spendable: bool ref
val force: bool ref
val fee: Tez.t ref
val init: string ref
val arg: string option ref
val max_priority: int option ref
val endorsement_delay: int ref

module Daemon : sig
  val mining_arg: string * Arg.spec * string
  val endorsement_arg: string * Arg.spec * string
  val denunciation_arg: string * Arg.spec * string
  val all: bool ref
  val mining: bool ref
  val endorsement: bool ref
  val denunciation: bool ref
end
