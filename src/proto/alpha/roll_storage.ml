(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Consume_roll_change
  | No_roll_in_contract
  | Deleted_contract_owning_rolls

let get_contract_delegate c contract =
  match Contract_repr.is_default contract with
  | Some manager -> return (Some manager)
  | None -> Storage.Contract.Delegate.get_option c contract

let clear_cycle c cycle =
  Storage.Roll.Last_for_cycle.get c cycle >>=? fun last ->
  Storage.Roll.Last_for_cycle.delete c cycle >>=? fun c ->
  let rec loop c roll =
    if Roll_repr.(roll = last) then
      return c
    else
      Storage.Roll.Owner_for_cycle.delete c (cycle, roll) >>=? fun c ->
      loop c (Roll_repr.succ roll) in
  loop c Roll_repr.first

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
      | Some contract ->
          loop ctxt (Roll_repr.succ roll) (f roll contract acc) in
  loop ctxt Roll_repr.first (return init)

let freeze_rolls_for_cycle ctxt cycle =
  fold ctxt (ctxt, Roll_repr.first)
    ~f:(fun _roll contract (ctxt, promoted_roll as acc) ->
        get_contract_delegate ctxt contract >>=? function
        | None -> return acc
        | Some delegate ->
            Storage.Roll.Owner_for_cycle.init
              ctxt (cycle, promoted_roll) delegate >>=? fun ctxt ->
            return (ctxt, Roll_repr.succ promoted_roll))
  >>=? fun (ctxt, last_promoted_roll) ->
  Storage.Roll.Last_for_cycle.init ctxt cycle last_promoted_roll

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
    Storage.Roll.Last_for_cycle.get c cycle >>=? fun bound ->
    let roll, _ = Roll_repr.random sequence bound in
    Storage.Roll.Owner_for_cycle.get c (cycle, roll)

end

let mining_rights_owner c level ~priority =
  Random.owner c "mining" level priority

let endorsement_rights_owner c level ~slot =
  Random.owner c "endorsement" level slot

module Contract = struct

  let fresh_roll c =
    Storage.Roll.Next.get c >>=? fun roll ->
    Storage.Roll.Next.set c (Roll_repr.succ roll) >>=? fun c ->
    return (roll, c)

  let get_limbo_roll c =
    Storage.Roll.Limbo.get c >>=? function
    | None ->
        fresh_roll c >>=? fun (roll, c) ->
        Storage.Roll.Limbo.set c (Some roll) >>=? fun c ->
        return (roll, c)
    | Some roll ->
        return (roll, c)

  let consume_roll_change c contract =
    Storage.Roll.Contract_change.get c contract >>=? fun change ->
    trace Consume_roll_change
      (Lwt.return Tez_repr.(change -? Constants_repr.roll_value)) >>=? fun new_change ->
    Storage.Roll.Contract_change.set c contract new_change

  let recover_roll_change c contract =
    Storage.Roll.Contract_change.get c contract >>=? fun change ->
    Lwt.return Tez_repr.(change +? Constants_repr.roll_value) >>=? fun new_change ->
    Storage.Roll.Contract_change.set c contract new_change

  let pop_roll_from_contract c contract =
    recover_roll_change c contract >>=? fun c ->
    (* beginning:
       contract : roll -> successor_roll -> ...
       limbo : limbo_head -> ...
    *)
    Storage.Roll.Limbo.get c >>=? fun limbo_head ->
    Storage.Roll.Contract_roll_list.get c contract >>=? function
    | None -> fail No_roll_in_contract
    | Some roll ->
        Storage.Roll.Owner.delete c roll >>=? fun c ->
        Storage.Roll.Successor.get c roll >>=? fun successor_roll ->
        Storage.Roll.Contract_roll_list.set c contract successor_roll >>=? fun c ->
        (* contract : successor_roll -> ...
           roll ------^
           limbo : limbo_head -> ... *)
        Storage.Roll.Successor.set c roll limbo_head >>=? fun c ->
        (* contract : successor_roll -> ...
           roll ------v
           limbo : limbo_head -> ... *)
        Storage.Roll.Limbo.set c (Some roll) >>=? fun c ->
        (* contract : successor_roll -> ...
           limbo : roll -> limbo_head -> ... *)
        Lwt.return (Ok (roll, c))

  let create_roll_in_contract c contract =
    consume_roll_change c contract >>=? fun c ->

    (* beginning:
       contract : contract_head -> ...
       limbo : roll -> limbo_successor -> ...
    *)
    Storage.Roll.Contract_roll_list.get c contract >>=? fun contract_head ->
    get_limbo_roll c >>=? fun (roll, c) ->
    Storage.Roll.Owner.init c roll contract >>=? fun c ->
    Storage.Roll.Successor.get c roll >>=? fun limbo_successor ->
    Storage.Roll.Limbo.set c limbo_successor >>=? fun c ->
    (* contract : contract_head -> ...
       roll ------v
       limbo : limbo_successor -> ... *)
    Storage.Roll.Successor.set c roll contract_head >>=? fun c ->
    (* contract : contract_head -> ...
       roll ------^
       limbo : limbo_successor -> ... *)
    Storage.Roll.Contract_roll_list.set c contract (Some roll)
  (* contract : roll -> contract_head -> ...
     limbo : limbo_successor -> ... *)

  let init c contract =
    Storage.Roll.Contract_change.init c contract Tez_repr.zero

  let add_amount c contract amount =
    Storage.Roll.Contract_change.get c contract >>=? fun change ->
    Lwt.return Tez_repr.(amount +? change) >>=? fun change ->
    Storage.Roll.Contract_change.set c contract change >>=? fun c ->
    let rec loop c change =
      if Tez_repr.(change < Constants_repr.roll_value) then
        return c
      else
        Lwt.return Tez_repr.(change -? Constants_repr.roll_value) >>=? fun  change ->
        create_roll_in_contract c contract >>=? fun c ->
        loop c change in
    loop c change

  let remove_amount c contract amount =
    let rec loop c change =
      if Tez_repr.(amount <= change)
      then return (c, change)
      else
        pop_roll_from_contract c contract >>=? fun (_, c) ->
        Lwt.return Tez_repr.(change +? Constants_repr.roll_value) >>=? fun change ->
        loop c change in
    Storage.Roll.Contract_change.get c contract >>=? fun change ->
    loop c change >>=? fun (c, change) ->
    Lwt.return Tez_repr.(change -? amount) >>=? fun change ->
    Storage.Roll.Contract_roll_list.mem c contract >>= fun rolls ->
    if Tez_repr.(change = zero) && not rolls then
      Storage.Roll.Contract_change.delete c contract
    else
      Storage.Roll.Contract_change.set c contract change

  let assert_empty c contract =
    Storage.Roll.Contract_change.mem c contract >>= fun change ->
    fail_unless (not change) Deleted_contract_owning_rolls

end

let init c =
  Storage.Roll.Next.init c Roll_repr.first
