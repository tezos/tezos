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

let timestamp_tag = Tag.def ~doc:"Timestamp when event occurred" "timestamp" Time.pp_hum
let valid_ops = Tag.def ~doc:"Valid Operations" "valid_ops" Format.pp_print_int
let refused_ops = Tag.def ~doc:"Refused Operations" "refused_ops" Format.pp_print_int
let bake_priorty_tag = Tag.def ~doc:"Baking Priority" "bake_priority" Format.pp_print_int
let fitness_tag = Tag.def ~doc:"Fitness" "fitness" Fitness.pp
let current_slots_tag = Tag.def ~doc:"Number of baking slots that can be baked at this time" "current_slots" Format.pp_print_int
let future_slots_tag = Tag.def ~doc:"Number of baking slots in the foreseeable future but not yet bakeable" "future_slots" Format.pp_print_int

let operations_tag = Tag.def ~doc:"Block Operations" "operations"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "+")
       (fun ppf operations -> Format.fprintf ppf "%d" (List.length operations.Preapply_result.applied)))

let bake_op_count_tag = Tag.def ~doc:"Bake Operation Count" "operation_count" Format.pp_print_int

let endorsement_slot_tag = Tag.def ~doc:"Endorsement Slot" "endorsement_slot" Format.pp_print_int
let endorsement_slots_tag = Tag.def ~doc:"Endorsement Slots" "endorsement_slots" Format.(fun ppf v -> pp_print_int ppf (List.length v))
let denounced_endorsements_slots_tag = Tag.def ~doc:"Endorsement Slots" "denounced_endorsement_slots" Format.(pp_print_list pp_print_int)
let denouncement_source_tag = Tag.def ~doc:"Denounce Source" "source" Format.pp_print_text

let level_tag = Tag.def ~doc:"Level" "level" Raw_level.pp

let worker_tag = Tag.def ~doc:"Worker in which event occurred" "worker" Format.pp_print_text

let conflicting_endorsements_tag = Tag.def ~doc:"Two conflicting endorsements signed by the same key" "conflicting_endorsements" Format.(
    fun ppf (a,b) -> fprintf ppf "%a / %a" Operation_hash.pp (Operation.hash a) Operation_hash.pp (Operation.hash b))
