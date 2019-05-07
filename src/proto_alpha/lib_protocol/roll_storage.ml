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

open Misc

type error +=
  | Consume_roll_change (* `Permanent *)
  | No_roll_for_delegate (* `Permanent *)
  | No_roll_snapshot_for_cycle of Cycle_repr.t (* `Permanent *)
  | Unregistered_delegate of Signature.Public_key_hash.t (* `Permanent *)

let () =
  let open Data_encoding in
  (* Consume roll change *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.consume_roll_change"
    ~title:"Consume roll change"
    ~description:"Change is not enough to consume a roll."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Not enough change to consume a roll.")
    empty
    (function Consume_roll_change -> Some () | _ -> None)
    (fun () -> Consume_roll_change) ;
  (* No roll for delegate *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.no_roll_for_delegate"
    ~title:"No roll for delegate"
    ~description:"Delegate has no roll."
    ~pp:(fun ppf () -> Format.fprintf ppf "Delegate has no roll.")
    empty
    (function No_roll_for_delegate -> Some () | _ -> None)
    (fun () -> No_roll_for_delegate) ;
  (* No roll snapshot for cycle *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.no_roll_snapshot_for_cycle"
    ~title:"No roll snapshot for cycle"
    ~description:"A snapshot of the rolls distribution does not exist for this cycle."
    ~pp:(fun ppf c ->
        Format.fprintf ppf
          "A snapshot of the rolls distribution does not exist for cycle %a" Cycle_repr.pp c)
    (obj1 (req "cycle" Cycle_repr.encoding))
    (function No_roll_snapshot_for_cycle c-> Some c | _ -> None)
    (fun c -> No_roll_snapshot_for_cycle c) ;
  (* Unregistered delegate *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.unregistered_delegate"
    ~title:"Unregistered delegate"
    ~description:"A contract cannot be delegated to an unregistered delegate"
    ~pp:(fun ppf k->
        Format.fprintf ppf "The provided public key (with hash %a) is \
                           \ not registered as valid delegate key."
          Signature.Public_key_hash.pp k)
    (obj1 (req "hash" Signature.Public_key_hash.encoding))
    (function Unregistered_delegate k -> Some k | _ -> None)
    (fun k -> Unregistered_delegate k)

let get_contract_delegate c contract =
  Storage.Contract.Delegate.get_option c contract

let delegate_pubkey ctxt delegate =
  Storage.Contract.Manager.get_option ctxt
    (Contract_repr.implicit_contract delegate) >>=? function
  | None | Some (Manager_repr.Hash _) ->
      fail (Unregistered_delegate delegate)
  | Some (Manager_repr.Public_key pk) ->
      return pk

let clear_cycle c cycle =
  Storage.Roll.Snapshot_for_cycle.get c cycle >>=? fun index ->
  Storage.Roll.Snapshot_for_cycle.delete c cycle >>=? fun c ->
  Storage.Roll.Last_for_snapshot.delete (c, cycle) index >>=? fun c ->
  Storage.Roll.Owner.delete_snapshot c (cycle, index) >>= fun c ->
  return c

let fold ctxt ~f init =
  Storage.Roll.Next.get ctxt >>=? fun last ->
  let rec loop ctxt roll acc =
    acc >>=? fun acc ->
    if Roll_repr.(roll = last) then
      return acc
    else
      Storage.Roll.Owner.get_option ctxt roll >>=? function
      | None ->
          loop ctxt (Roll_repr.succ roll) (return acc)
      | Some delegate ->
          loop ctxt (Roll_repr.succ roll) (f roll delegate acc) in
  loop ctxt Roll_repr.first (return init)

let snapshot_rolls_for_cycle ctxt cycle =
  Storage.Roll.Snapshot_for_cycle.get ctxt cycle >>=? fun index ->
  Storage.Roll.Snapshot_for_cycle.set ctxt cycle (index + 1) >>=? fun ctxt ->
  Storage.Roll.Owner.snapshot ctxt (cycle, index) >>=? fun ctxt ->
  Storage.Roll.Next.get ctxt >>=? fun last ->
  Storage.Roll.Last_for_snapshot.init (ctxt, cycle) index last >>=? fun ctxt ->
  return ctxt

let freeze_rolls_for_cycle ctxt cycle =
  Storage.Roll.Snapshot_for_cycle.get ctxt cycle >>=? fun max_index ->
  Storage.Seed.For_cycle.get ctxt cycle >>=? fun seed ->
  let rd = Seed_repr.initialize_new seed [MBytes.of_string "roll_snapshot"] in
  let seq = Seed_repr.sequence rd 0l in
  let selected_index =
    Seed_repr.take_int32 seq (Int32.of_int max_index) |> fst |> Int32.to_int in
  Storage.Roll.Snapshot_for_cycle.set ctxt cycle selected_index >>=? fun ctxt ->
  fold_left_s
    (fun ctxt index ->
       if Compare.Int.(index = selected_index) then
         return ctxt
       else
         Storage.Roll.Owner.delete_snapshot ctxt (cycle, index) >>= fun ctxt ->
         Storage.Roll.Last_for_snapshot.delete (ctxt, cycle) index >>=? fun ctxt ->
         return ctxt
    )
    ctxt
    Misc.(0 --> (max_index - 1)) >>=? fun ctxt ->
  return ctxt

(* Roll selection *)

module Random = struct

  let int32_to_bytes i =
    let b = MBytes.create 4 in
    MBytes.set_int32 b 0 i;
    b

  let level_random seed use level =
    let position = level.Level_repr.cycle_position in
    Seed_repr.initialize_new seed
      [MBytes.of_string ("level "^use^":");
       int32_to_bytes position]

  let owner c kind level offset =
    let cycle = level.Level_repr.cycle in
    Seed_storage.for_cycle c cycle >>=? fun random_seed ->
    let rd = level_random random_seed kind level in
    let sequence = Seed_repr.sequence rd (Int32.of_int offset) in
    Storage.Roll.Snapshot_for_cycle.get c cycle >>=? fun index ->
    Storage.Roll.Last_for_snapshot.get (c, cycle) index >>=? fun bound ->
    let rec loop sequence =
      let roll, sequence = Roll_repr.random sequence ~bound in
      Storage.Roll.Owner.Snapshot.get_option c ((cycle, index), roll) >>=? function
      | None ->
          loop sequence
      | Some delegate ->
          return delegate in
    Storage.Roll.Owner.snapshot_exists c (cycle, index) >>= fun snapshot_exists ->
    fail_unless snapshot_exists (No_roll_snapshot_for_cycle cycle) >>=? fun () ->
    loop sequence

end

let baking_rights_owner c level ~priority =
  Random.owner c "baking" level priority

let endorsement_rights_owner c level ~slot =
  Random.owner c "endorsement" level slot

let traverse_rolls ctxt head =
  let rec loop acc roll =
    Storage.Roll.Successor.get_option ctxt roll >>=? function
    | None -> return (List.rev acc)
    | Some next -> loop (next :: acc) next in
  loop [head] head

let get_rolls ctxt delegate =
  Storage.Roll.Delegate_roll_list.get_option ctxt delegate >>=? function
  | None -> return_nil
  | Some head_roll -> traverse_rolls ctxt head_roll

let get_change c delegate =
  Storage.Roll.Delegate_change.get_option c delegate >>=? function
  | None -> return Tez_repr.zero
  | Some change -> return change

module Delegate = struct

  let fresh_roll c =
    Storage.Roll.Next.get c >>=? fun roll ->
    Storage.Roll.Next.set c (Roll_repr.succ roll) >>=? fun c ->
    return (roll, c)

  let get_limbo_roll c =
    Storage.Roll.Limbo.get_option c >>=? function
    | None ->
        fresh_roll c >>=? fun (roll, c) ->
        Storage.Roll.Limbo.init c roll >>=? fun c ->
        return (roll, c)
    | Some roll ->
        return (roll, c)

  let consume_roll_change c delegate =
    let tokens_per_roll = Constants_storage.tokens_per_roll c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    trace Consume_roll_change
      (Lwt.return Tez_repr.(change -? tokens_per_roll)) >>=? fun new_change ->
    Storage.Roll.Delegate_change.set c delegate new_change

  let recover_roll_change c delegate =
    let tokens_per_roll = Constants_storage.tokens_per_roll c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    Lwt.return Tez_repr.(change +? tokens_per_roll) >>=? fun new_change ->
    Storage.Roll.Delegate_change.set c delegate new_change

  let pop_roll_from_delegate c delegate =
    recover_roll_change c delegate >>=? fun c ->
    (* beginning:
       delegate : roll -> successor_roll -> ...
       limbo : limbo_head -> ...
    *)
    Storage.Roll.Limbo.get_option c >>=? fun limbo_head ->
    Storage.Roll.Delegate_roll_list.get_option c delegate >>=? function
    | None -> fail No_roll_for_delegate
    | Some roll ->
        Storage.Roll.Owner.delete c roll >>=? fun c ->
        Storage.Roll.Successor.get_option c roll >>=? fun successor_roll ->
        Storage.Roll.Delegate_roll_list.set_option c delegate successor_roll >>= fun c ->
        (* delegate : successor_roll -> ...
           roll ------^
           limbo : limbo_head -> ... *)
        Storage.Roll.Successor.set_option c roll limbo_head >>= fun c ->
        (* delegate : successor_roll -> ...
           roll ------v
           limbo : limbo_head -> ... *)
        Storage.Roll.Limbo.init_set c roll >>= fun c ->
        (* delegate : successor_roll -> ...
           limbo : roll -> limbo_head -> ... *)
        return (roll, c)

  let create_roll_in_delegate c delegate delegate_pk =
    consume_roll_change c delegate >>=? fun c ->

    (* beginning:
       delegate : delegate_head -> ...
       limbo : roll -> limbo_successor -> ...
    *)
    Storage.Roll.Delegate_roll_list.get_option c delegate >>=? fun delegate_head ->
    get_limbo_roll c >>=? fun (roll, c) ->
    Storage.Roll.Owner.init c roll delegate_pk >>=? fun c ->
    Storage.Roll.Successor.get_option c roll >>=? fun limbo_successor ->
    Storage.Roll.Limbo.set_option c limbo_successor >>= fun c ->
    (* delegate : delegate_head -> ...
       roll ------v
       limbo : limbo_successor -> ... *)
    Storage.Roll.Successor.set_option c roll delegate_head >>= fun c ->
    (* delegate : delegate_head -> ...
       roll ------^
       limbo : limbo_successor -> ... *)
    Storage.Roll.Delegate_roll_list.init_set c delegate roll >>= fun c ->
    (* delegate : roll -> delegate_head -> ...
       limbo : limbo_successor -> ... *)
    return c

  let ensure_inited c delegate =
    Storage.Roll.Delegate_change.mem c delegate >>= function
    | true -> return c
    | false ->
        Storage.Roll.Delegate_change.init c delegate Tez_repr.zero

  let is_inactive c delegate =
    Storage.Contract.Inactive_delegate.mem c
      (Contract_repr.implicit_contract delegate) >>= fun inactive ->
    if inactive then
      return inactive
    else
      Storage.Contract.Delegate_desactivation.get_option c
        (Contract_repr.implicit_contract delegate) >>=? function
      | Some last_active_cycle ->
          let { Level_repr.cycle = current_cycle } = Raw_context.current_level c in
          return Cycle_repr.(last_active_cycle < current_cycle)
      | None ->
          (* This case is only when called from `set_active`, when creating
             a contract. *)
          return false

  let add_amount c delegate amount =
    ensure_inited c delegate >>=? fun c ->
    let tokens_per_roll = Constants_storage.tokens_per_roll c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    Lwt.return Tez_repr.(amount +? change) >>=? fun change ->
    Storage.Roll.Delegate_change.set c delegate change >>=? fun c ->
    delegate_pubkey c delegate >>=? fun delegate_pk ->
    let rec loop c change =
      if Tez_repr.(change < tokens_per_roll) then
        return c
      else
        Lwt.return Tez_repr.(change -? tokens_per_roll) >>=? fun  change ->
        create_roll_in_delegate c delegate delegate_pk >>=? fun c ->
        loop c change in
    is_inactive c delegate >>=? fun inactive ->
    if inactive then
      return c
    else
      loop c change >>=? fun c ->
      Storage.Roll.Delegate_roll_list.get_option c delegate >>=? fun rolls ->
      match rolls with
      | None ->
          return c
      | Some _ ->
          Storage.Active_delegates_with_rolls.add c delegate >>= fun c ->
          return c

  let remove_amount c delegate amount =
    let tokens_per_roll = Constants_storage.tokens_per_roll c in
    let rec loop c change =
      if Tez_repr.(amount <= change)
      then return (c, change)
      else
        pop_roll_from_delegate c delegate >>=? fun (_, c) ->
        Lwt.return Tez_repr.(change +? tokens_per_roll) >>=? fun change ->
        loop c change in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    is_inactive c delegate >>=? fun inactive ->
    begin
      if inactive then
        return (c, change)
      else
        loop c change >>=? fun (c, change) ->
        Storage.Roll.Delegate_roll_list.get_option c delegate >>=? fun rolls ->
        match rolls with
        | None ->
            Storage.Active_delegates_with_rolls.del c delegate >>= fun c ->
            return (c, change)
        | Some _ ->
            return (c, change)
    end >>=? fun (c, change) ->
    Lwt.return Tez_repr.(change -? amount) >>=? fun change ->
    Storage.Roll.Delegate_change.set c delegate change

  let set_inactive ctxt delegate =
    ensure_inited ctxt delegate >>=? fun ctxt ->
    let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
    Storage.Roll.Delegate_change.get ctxt delegate >>=? fun change ->
    Storage.Contract.Inactive_delegate.add ctxt
      (Contract_repr.implicit_contract delegate) >>= fun ctxt ->
    Storage.Active_delegates_with_rolls.del ctxt delegate >>= fun ctxt ->
    let rec loop ctxt change =
      Storage.Roll.Delegate_roll_list.get_option ctxt delegate >>=? function
      | None -> return (ctxt, change)
      | Some _roll ->
          pop_roll_from_delegate ctxt delegate >>=? fun (_, ctxt) ->
          Lwt.return Tez_repr.(change +? tokens_per_roll) >>=? fun change ->
          loop ctxt change in
    loop ctxt change >>=? fun (ctxt, change) ->
    Storage.Roll.Delegate_change.set ctxt delegate change >>=? fun ctxt ->
    return ctxt

  let set_active ctxt delegate =
    is_inactive ctxt delegate >>=? fun inactive ->
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    (* When the delegate is new or inactive, she will become active in
       `1+preserved_cycles`, and we allow `preserved_cycles` for the
       delegate to start baking. When the delegate is active, we only
       give her at least `preserved_cycles` after the current cycle
       before to be deactivated.  *)
    Storage.Contract.Delegate_desactivation.get_option ctxt
      (Contract_repr.implicit_contract delegate) >>=? fun current_expiration ->
    let expiration = match current_expiration with
      | None ->
          Cycle_repr.add current_cycle (1+2*preserved_cycles)
      | Some current_expiration ->
          let delay =
            if inactive then (1+2*preserved_cycles) else 1+preserved_cycles in
          let updated =
            Cycle_repr.add current_cycle delay in
          Cycle_repr.max current_expiration updated in
    Storage.Contract.Delegate_desactivation.init_set ctxt
      (Contract_repr.implicit_contract delegate)
      expiration >>= fun ctxt ->
    if not inactive then
      return ctxt
    else begin
      ensure_inited ctxt delegate >>=? fun ctxt ->
      let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
      Storage.Roll.Delegate_change.get ctxt delegate >>=? fun change ->
      Storage.Contract.Inactive_delegate.del ctxt
        (Contract_repr.implicit_contract delegate) >>= fun ctxt ->
      delegate_pubkey ctxt delegate >>=? fun delegate_pk ->
      let rec loop ctxt change =
        if Tez_repr.(change < tokens_per_roll) then
          return ctxt
        else
          Lwt.return Tez_repr.(change -? tokens_per_roll) >>=? fun  change ->
          create_roll_in_delegate ctxt delegate delegate_pk >>=? fun ctxt ->
          loop ctxt change in
      loop ctxt change >>=? fun ctxt ->
      Storage.Roll.Delegate_roll_list.get_option ctxt delegate >>=? fun rolls ->
      match rolls with
      | None ->
          return ctxt
      | Some _ ->
          Storage.Active_delegates_with_rolls.add ctxt delegate >>= fun ctxt ->
          return ctxt
    end

end

module Contract = struct

  let add_amount c contract amount =
    get_contract_delegate c contract >>=? function
    | None -> return c
    | Some delegate ->
        Delegate.add_amount c delegate amount

  let remove_amount c contract amount =
    get_contract_delegate c contract >>=? function
    | None -> return c
    | Some delegate ->
        Delegate.remove_amount c delegate amount

end

let init ctxt =
  Storage.Roll.Next.init ctxt Roll_repr.first

let init_first_cycles ctxt =
  let preserved = Constants_storage.preserved_cycles ctxt in
  (* Precompute rolls for cycle (0 --> preserved_cycles) *)
  List.fold_left
    (fun ctxt c ->
       ctxt >>=? fun ctxt ->
       let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
       Storage.Roll.Snapshot_for_cycle.init ctxt cycle 0 >>=? fun ctxt ->
       snapshot_rolls_for_cycle ctxt cycle >>=? fun ctxt ->
       freeze_rolls_for_cycle ctxt cycle)
    (return ctxt) (0 --> preserved) >>=? fun ctxt ->
  let cycle = Cycle_repr.of_int32_exn (Int32.of_int (preserved + 1)) in
  (* Precomputed a snapshot for cycle (preserved_cycles + 1) *)
  Storage.Roll.Snapshot_for_cycle.init ctxt cycle 0 >>=? fun ctxt ->
  snapshot_rolls_for_cycle ctxt cycle >>=? fun ctxt ->
  (* Prepare storage for storing snapshots for cycle (preserved_cycles+2) *)
  let cycle = Cycle_repr.of_int32_exn (Int32.of_int (preserved + 2)) in
  Storage.Roll.Snapshot_for_cycle.init ctxt cycle 0 >>=? fun ctxt ->
  return ctxt

let snapshot_rolls ctxt =
  let current_level = Raw_context.current_level ctxt in
  let preserved = Constants_storage.preserved_cycles ctxt in
  let cycle = Cycle_repr.add current_level.cycle (preserved+2) in
  snapshot_rolls_for_cycle ctxt cycle

let cycle_end ctxt last_cycle =
  let preserved = Constants_storage.preserved_cycles ctxt in
  begin
    match Cycle_repr.sub last_cycle preserved with
    | None -> return ctxt
    | Some cleared_cycle ->
        clear_cycle ctxt cleared_cycle
  end >>=? fun ctxt ->
  let frozen_roll_cycle = Cycle_repr.add last_cycle (preserved+1) in
  freeze_rolls_for_cycle ctxt frozen_roll_cycle >>=? fun ctxt ->
  Storage.Roll.Snapshot_for_cycle.init
    ctxt (Cycle_repr.succ (Cycle_repr.succ frozen_roll_cycle)) 0 >>=? fun ctxt ->
  return ctxt
