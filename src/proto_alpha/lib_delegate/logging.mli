(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val timestamp_tag : Time.t Tag.def
val valid_ops : int Tag.def
val refused_ops : int Tag.def
val bake_priorty_tag : int Tag.def
val fitness_tag : Fitness.t Tag.def
val current_slots_tag : int Tag.def
val future_slots_tag : int Tag.def

val operations_tag : error Preapply_result.t list Tag.def
val bake_op_count_tag : int Tag.def
val endorsement_slot_tag : int Tag.def
val endorsement_slots_tag : int list Tag.def
val denounced_endorsements_slots_tag : int list Tag.def
val denouncement_source_tag : string Tag.def
val level_tag : Proto_alpha.Alpha_context.Raw_level.t Tag.def
val worker_tag : string Tag.def

open Proto_alpha.Alpha_context
val conflicting_endorsements_tag : (Kind.endorsement operation * Kind.endorsement operation) Tag.def
