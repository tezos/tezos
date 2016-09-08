(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
  let constants = Storage.constants c in
  Level_repr.from_raw
    ~cycle_length:constants.Constants_repr.cycle_length
    ~voting_period_length:constants.Constants_repr.voting_period_length
    l

let succ c l = from_raw c (Raw_level_repr.succ l.level)
let pred c l =
  match Raw_level_repr.pred l.Level_repr.level with
  | None -> None
  | Some l -> Some (from_raw c l)

let current ctxt =
  Storage.Current_level.get ctxt >>=? fun l ->
  return (from_raw ctxt l)

let previous ctxt =
  current ctxt >>=? fun l ->
  match pred ctxt l with
  | None -> assert false (* Context inited with level = 1. *)
  | Some p -> return p

let increment_current ctxt =
  Storage.Current_level.get ctxt >>=? fun l ->
  Storage.Current_level.set ctxt (Raw_level_repr.succ l)


let first_level_in_cycle ctxt c =
  let constants = Storage.constants ctxt in
  from_raw ctxt
    (Raw_level_repr.of_int32_exn
       (Int32.mul constants.Constants_repr.cycle_length (Cycle_repr.to_int32 c)))

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

let init ctxt =
  Storage.Current_level.init ctxt Raw_level_repr.(succ root)


