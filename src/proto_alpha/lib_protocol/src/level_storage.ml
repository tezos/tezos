(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Level_repr

let from_raw c ?offset l =
  let l =
    match offset with
    | None -> l
    | Some o -> Raw_level_repr.(of_int32_exn (Int32.add (to_int32 l) o)) in
  let constants = Raw_context.constants c in
  let first_level = Raw_context.first_level c in
  Level_repr.from_raw
    ~first_level
    ~cycle_length:constants.Constants_repr.cycle_length
    ~voting_period_length:constants.Constants_repr.voting_period_length
    ~blocks_per_commitment:constants.Constants_repr.blocks_per_commitment
    l

let root c =
  Level_repr.root (Raw_context.first_level c)

let succ c l = from_raw c (Raw_level_repr.succ l.level)
let pred c l =
  match Raw_level_repr.pred l.Level_repr.level with
  | None -> None
  | Some l -> Some (from_raw c l)

let current ctxt = Raw_context.current_level ctxt

let previous ctxt =
  let l = current ctxt in
  match pred ctxt l with
  | None -> assert false (* We never validate the Genesis... *)
  | Some p -> p

let first_level_in_cycle ctxt c =
  let constants = Raw_context.constants ctxt in
  let first_level = Raw_context.first_level ctxt in
  from_raw ctxt
    (Raw_level_repr.of_int32_exn
       (Int32.add
          (Raw_level_repr.to_int32 first_level)
          (Int32.mul
             constants.Constants_repr.cycle_length
             (Cycle_repr.to_int32 c))))

let last_level_in_cycle ctxt c =
  match pred ctxt (first_level_in_cycle ctxt (Cycle_repr.succ c)) with
  | None -> assert false
  | Some x -> x

let levels_in_cycle ctxt c =
  let first = first_level_in_cycle ctxt c in
  let rec loop n acc =
    if Cycle_repr.(n.cycle = first.cycle)
    then loop (succ ctxt n) (n :: acc)
    else acc
  in
  loop first []

let levels_with_commitments_in_cycle ctxt c =
  let first = first_level_in_cycle ctxt c in
  let rec loop n acc =
    if Cycle_repr.(n.cycle = first.cycle)
    then
      if n.expected_commitment then
        loop (succ ctxt n) (n :: acc)
      else
        loop (succ ctxt n) acc
    else acc
  in
  loop first []
