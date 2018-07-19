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
    ~blocks_per_cycle:constants.Constants_repr.blocks_per_cycle
    ~blocks_per_voting_period:constants.Constants_repr.blocks_per_voting_period
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
             constants.Constants_repr.blocks_per_cycle
             (Cycle_repr.to_int32 c))))

let last_level_in_cycle ctxt c =
  match pred ctxt (first_level_in_cycle ctxt (Cycle_repr.succ c)) with
  | None -> assert false
  | Some x -> x

let levels_in_cycle ctxt cycle =
  let first = first_level_in_cycle ctxt cycle in
  let rec loop n acc =
    if Cycle_repr.(n.cycle = first.cycle)
    then loop (succ ctxt n) (n :: acc)
    else acc
  in
  loop first []

let levels_in_current_cycle ctxt ?(offset = 0l) () =
  let current_cycle = Cycle_repr.to_int32 (current ctxt).cycle in
  let cycle = Int32.add current_cycle offset in
  if Compare.Int32.(cycle < 0l) then
    []
  else
    let cycle = Cycle_repr.of_int32_exn cycle in
    levels_in_cycle ctxt cycle

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


let last_allowed_fork_level c =
  let level = Raw_context.current_level c in
  let preserved_cycles = Constants_storage.preserved_cycles c in
  match Cycle_repr.sub level.cycle preserved_cycles with
  | None -> Raw_level_repr.root
  | Some cycle -> (first_level_in_cycle c cycle).level
