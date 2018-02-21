(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Consume_roll_change
  | No_roll_for_delegate
  | No_roll_snapshot_for_cycle of Cycle_repr.t
  | Unregistred_delegate of Ed25519.Public_key_hash.t (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"contract.manager.unregistred_delegate"
    ~title:"Unregistred delegate"
    ~description:"A contract cannot be delegated to an unregistred delegate"
    ~pp:(fun ppf (k) ->
        Format.fprintf ppf "The provided public key (with hash %a) is \
                           \ not registred as valid delegate key."
          Ed25519.Public_key_hash.pp k)
    Data_encoding.(obj1 (req "hash" Ed25519.Public_key_hash.encoding))
    (function Unregistred_delegate (k) -> Some (k) | _ -> None)
    (fun (k) -> Unregistred_delegate (k))

let get_contract_delegate c contract =
  Storage.Contract.Delegate.get_option c contract

let delegate_pubkey ctxt delegate =
  Storage.Contract.Manager.get_option ctxt
    (Contract_repr.implicit_contract delegate) >>=? function
  | None | Some (Manager_repr.Hash _) ->
      fail (Unregistred_delegate delegate)
  | Some (Manager_repr.Public_key pk) ->
      return pk

let clear_cycle c cycle =
  Storage.Roll.Last_for_cycle.delete c cycle >>=? fun c ->
  Storage.Roll.Owner.delete_snapshot c cycle >>= fun c ->
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

let freeze_rolls_for_cycle ctxt cycle =
  Storage.Roll.Owner.snapshot ctxt cycle >>=? fun ctxt ->
  Storage.Roll.Next.get ctxt >>=? fun last ->
  Storage.Roll.Last_for_cycle.init ctxt cycle last

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
    let rec loop sequence =
      let roll, sequence = Roll_repr.random sequence ~bound in
      Storage.Roll.Owner.Snapshot.get_option c (cycle, roll) >>=? function
      | None ->
          loop sequence
      | Some delegate ->
          return delegate in
    Storage.Roll.Owner.snapshot_exists c cycle >>= fun snapshot_exists ->
    fail_unless snapshot_exists (No_roll_snapshot_for_cycle cycle) >>=? fun () ->
    loop sequence

end

let baking_rights_owner c level ~priority =
  Random.owner c "baking" level priority

let endorsement_rights_owner c level ~slot =
  Random.owner c "endorsement" level slot

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
    let roll_value = Raw_context.roll_value c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    trace Consume_roll_change
      (Lwt.return Tez_repr.(change -? roll_value)) >>=? fun new_change ->
    Storage.Roll.Delegate_change.set c delegate new_change

  let recover_roll_change c delegate =
    let roll_value = Raw_context.roll_value c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    Lwt.return Tez_repr.(change +? roll_value) >>=? fun new_change ->
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

  let add_amount c delegate amount =
    ensure_inited c delegate >>=? fun c ->
    let roll_value = Raw_context.roll_value c in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    Lwt.return Tez_repr.(amount +? change) >>=? fun change ->
    Storage.Roll.Delegate_change.set c delegate change >>=? fun c ->
    delegate_pubkey c delegate >>=? fun delegate_pk ->
    let rec loop c change =
      if Tez_repr.(change < roll_value) then
        return c
      else
        Lwt.return Tez_repr.(change -? roll_value) >>=? fun  change ->
        create_roll_in_delegate c delegate delegate_pk >>=? fun c ->
        loop c change in
    loop c change

  let remove_amount c delegate amount =
    let roll_value = Raw_context.roll_value c in
    let rec loop c change =
      if Tez_repr.(amount <= change)
      then return (c, change)
      else
        pop_roll_from_delegate c delegate >>=? fun (_, c) ->
        Lwt.return Tez_repr.(change +? roll_value) >>=? fun change ->
        loop c change in
    Storage.Roll.Delegate_change.get c delegate >>=? fun change ->
    loop c change >>=? fun (c, change) ->
    Lwt.return Tez_repr.(change -? amount) >>=? fun change ->
    Storage.Roll.Delegate_roll_list.mem c delegate >>= fun rolls ->
    if Tez_repr.(change = zero) && not rolls then
      Storage.Roll.Delegate_change.delete c delegate
    else
      Storage.Roll.Delegate_change.set c delegate change

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

let value = Raw_context.roll_value

let init c =
  Storage.Roll.Next.init c Roll_repr.first
