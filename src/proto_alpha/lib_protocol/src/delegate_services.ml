(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type info = {
  balance: Tez.t ;
  frozen_balance: Tez.t ;
  frozen_balances: Delegate.frozen_balance Cycle.Map.t ;
  delegated_balance: Tez.t ;
  delegated_contracts: Contract_hash.t list ;
  deactivated: bool ;
  grace_period: Cycle.t ;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun { balance ; frozen_balance ; frozen_balances ; delegated_balance ;
           delegated_contracts ; deactivated ; grace_period } ->
      (balance, frozen_balance, frozen_balances, delegated_balance,
       delegated_contracts, deactivated, grace_period))
    (fun (balance, frozen_balance, frozen_balances, delegated_balance,
          delegated_contracts, deactivated, grace_period) ->
      { balance ; frozen_balance ; frozen_balances ; delegated_balance ;
        delegated_contracts ; deactivated ; grace_period })
    (obj7
       (req "balance" Tez.encoding)
       (req "frozen_balance" Tez.encoding)
       (req "frozen_balances" Delegate.frozen_balances_encoding)
       (req "delegated_balance" Tez.encoding)
       (req "delegated_contracts" (list Contract_hash.encoding))
       (req "deactivated" bool)
       (req "grace_period" Cycle.encoding))

module S = struct

  let path = RPC_path.(open_root / "delegate")

  open Data_encoding

  type list_query = {
    active: bool ;
    inactive: bool ;
  }
  let list_query :list_query RPC_query.t =
    let open RPC_query in
    query (fun active inactive -> { active ; inactive })
    |+ flag "active" (fun t -> t.active)
    |+ flag "inactive" (fun t -> t.inactive)
    |> seal

  let list_delegate =
    RPC_service.get_service
      ~description:
        "List all registred delegates."
      ~query: list_query
      ~output: (list Signature.Public_key_hash.encoding)
      path

  let path = RPC_path.(path /: Signature.Public_key_hash.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:
        "Everything about a delegate."
      ~query: RPC_query.empty
      ~output: info_encoding
      path

  let balance =
    RPC_service.get_service
      ~description:
        "Returns the full balance of a given delegate, \
         including the frozen balances."
      ~query: RPC_query.empty
      ~output: Tez.encoding
      RPC_path.(path / "balance")

  let frozen_balance =
    RPC_service.get_service
      ~description:
        "Returns the total frozen balances of a given delegate, \
         this includes the frozen deposits, rewards and fees."
      ~query: RPC_query.empty
      ~output: Tez.encoding
      RPC_path.(path / "change")

  let frozen_balances =
    RPC_service.get_service
      ~description:
        "Returns the amount of frozen tokens associated to a given delegate."
      ~query: RPC_query.empty
      ~output: Delegate.frozen_balances_encoding
      RPC_path.(path / "frozen_balances")

  let delegated_balance =
    RPC_service.get_service
      ~description:
        "Returns the total amount of token delegated to a given delegate. \
         This includes the balance of all the contracts that delegates \
         to it, but also the balance of the delegate itself and its frozen \
         fees and deposits. The rewards do not count in the delegated balance \
         until they are unfrozen."
      ~query: RPC_query.empty
      ~output: Tez.encoding
      RPC_path.(path / "delegated_balance")

  let delegated_contracts =
    RPC_service.get_service
      ~description:
        "Returns the list of contract that delegates to a given delegate."
      ~query: RPC_query.empty
      ~output: (list Contract_hash.encoding)
      RPC_path.(path / "delegated_contracts")

  let deactivated =
    RPC_service.get_service
      ~description:
        "Returns whether the delegate is currently tagged as deactivated or not."
      ~query: RPC_query.empty
      ~output: bool
      RPC_path.(path / "deactivated")

  let grace_period =
    RPC_service.get_service
      ~description:
        "Returns the cycle by the end of which the delegate might be \
         deactivated, whether should she failed to execute any delegate \
         action until then. \
         A deactivated delegate might be reactivated \
         (without loosing any rolls) by simply re-register as a delegate. \
         For deactivated delegate this value contains the cycle by which \
         they were deactivated."
      ~query: RPC_query.empty
      ~output: Cycle.encoding
      RPC_path.(path / "grace_period")

end

let () =
  let open Services_registration in
  register0 S.list_delegate begin fun ctxt q () ->
    Delegate.list ctxt >>= fun delegates ->
    if q.active && q.inactive then
      return delegates
    else if q.active then
      Lwt_list.filter_p
        (fun pkh -> Delegate.deactivated ctxt pkh >|= not)
        delegates >>= return
    else if q.inactive then
      Lwt_list.filter_p
        (fun pkh -> Delegate.deactivated ctxt pkh)
        delegates >>= return
    else
      return []
  end ;
  register1 S.info begin fun ctxt pkh () () ->
    Delegate.full_balance ctxt pkh >>=? fun balance ->
    Delegate.frozen_balance ctxt pkh >>=? fun frozen_balance ->
    Delegate.frozen_balances ctxt pkh >>= fun frozen_balances ->
    Delegate.delegated_balance ctxt pkh >>=? fun delegated_balance ->
    Delegate.get_delegated_contracts ctxt pkh >>= fun delegated_contracts ->
    Delegate.deactivated ctxt pkh >>= fun deactivated ->
    Delegate.grace_period ctxt pkh >>=? fun grace_period ->
    return {
      balance ; frozen_balance ; frozen_balances ; delegated_balance ;
      delegated_contracts ; deactivated ; grace_period
    }
  end ;
  register1 S.balance begin fun ctxt pkh () () ->
    Delegate.full_balance ctxt pkh
  end ;
  register1 S.frozen_balance begin fun ctxt pkh () () ->
    Delegate.frozen_balance ctxt pkh
  end ;
  register1 S.frozen_balances begin fun ctxt pkh () () ->
    Delegate.frozen_balances ctxt pkh >>= return
  end ;
  register1 S.delegated_balance begin fun ctxt pkh () () ->
    Delegate.delegated_balance ctxt pkh
  end ;
  register1 S.delegated_contracts begin fun ctxt pkh () () ->
    Delegate.get_delegated_contracts ctxt pkh >>= return
  end ;
  register1 S.deactivated begin fun ctxt pkh () () ->
    Delegate.deactivated ctxt pkh >>= return
  end ;
  register1 S.grace_period begin fun ctxt pkh () () ->
    Delegate.grace_period ctxt pkh
  end

let list ctxt block ?(active = true) ?(inactive = false) () =
  RPC_context.make_call0 S.list_delegate ctxt block { active ; inactive } ()

let info ctxt block pkh =
  RPC_context.make_call1 S.info ctxt block pkh () ()

let balance ctxt block pkh =
  RPC_context.make_call1 S.balance ctxt block pkh () ()

let frozen_balance ctxt block pkh =
  RPC_context.make_call1 S.frozen_balance ctxt block pkh () ()

let frozen_balances ctxt block pkh =
  RPC_context.make_call1 S.frozen_balances ctxt block pkh () ()

let delegated_balance ctxt block pkh =
  RPC_context.make_call1 S.delegated_balance ctxt block pkh () ()

let delegated_contracts ctxt block pkh =
  RPC_context.make_call1 S.delegated_contracts ctxt block pkh () ()

let deactivated ctxt block pkh =
  RPC_context.make_call1 S.deactivated ctxt block pkh () ()

let grace_period ctxt block pkh =
  RPC_context.make_call1 S.grace_period ctxt block pkh () ()

let requested_levels ~default ctxt cycles levels =
  match levels, cycles with
  | [], [] ->
      return [default]
  | levels, cycles ->
      (* explicitly fail when requested levels or cycle are in the past...
         or too far in the future... *)
      (* check_levels levels >>=? fun () -> *)
      (* check_cycles levels >>=? fun () -> *)
      let levels =
        List.sort_uniq
          Level.compare
          (List.concat (List.map (Level.from_raw ctxt) levels ::
                        List.map (Level.levels_in_cycle ctxt) cycles)) in
      map_p
        (fun level ->
           let current_level = Level.current ctxt in
           if Level.(level <= current_level) then
             return (level, None)
           else
             Baking.earlier_predecessor_timestamp
               ctxt level >>=? fun timestamp ->
             return (level, Some timestamp))
        levels

module Baking_rights = struct

  type t = {
    level: Raw_level.t ;
    delegate: Signature.Public_key_hash.t ;
    priority: int ;
    timestamp: Timestamp.t option ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { level ; delegate ; priority ; timestamp } ->
         (level, delegate, priority, timestamp))
      (fun (level, delegate, priority, timestamp) ->
         { level ; delegate ; priority ; timestamp })
      (obj4
         (req "level" Raw_level.encoding)
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "priority" uint16)
         (opt "timestamp" Timestamp.encoding))

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "baking_rights")

    type baking_rights_query = {
      levels: Raw_level.t list ;
      cycles: Cycle.t list ;
      delegates: Signature.Public_key_hash.t list ;
      max_priority: int option ;
      all: bool ;
    }

    let baking_rights_query =
      let open RPC_query in
      query (fun levels cycles delegates max_priority all ->
          { levels ; cycles ; delegates ; max_priority ; all })
      |+ multi_field "level" Raw_level.arg (fun t -> t.levels)
      |+ multi_field "cycle" Cycle.arg (fun t -> t.cycles)
      |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t -> t.delegates)
      |+ opt_field "max_priority" RPC_arg.int (fun t -> t.max_priority)
      |+ flag "all" (fun t -> t.all)
      |> seal

    let baking_rights =
      RPC_service.get_service
        ~description: "...FIXME..."
        ~query: baking_rights_query
        ~output: (list encoding)
        custom_root

  end

  let baking_priorities ctxt max_prio (level, pred_timestamp) =
    Baking.baking_priorities ctxt level >>=? fun contract_list ->
    let rec loop l acc priority =
      if Compare.Int.(priority >= max_prio) then
        return (List.rev acc)
      else
        let Misc.LCons (pk, next) = l in
        let delegate = Signature.Public_key.hash pk in
        begin
          match pred_timestamp with
          | None -> return None
          | Some pred_timestamp ->
              Baking.minimal_time ctxt priority pred_timestamp >>=? fun t ->
              return (Some t)
        end>>=? fun timestamp ->
        let acc =
          { level = level.level ; delegate ; priority ; timestamp } :: acc in
        next () >>=? fun l ->
        loop l acc (priority+1) in
    loop contract_list [] 0

  let remove_duplicated_delegates rights =
    List.rev @@ fst @@
    List.fold_left
      (fun (acc, previous) r ->
         if Signature.Public_key_hash.Set.mem r.delegate previous then
           (acc, previous)
         else
           (r :: acc,
            Signature.Public_key_hash.Set.add r.delegate previous))
      ([], Signature.Public_key_hash.Set.empty)
      rights

  let () =
    let open Services_registration in
    register0 S.baking_rights begin fun ctxt q () ->
      requested_levels
        ~default:
          (Level.succ ctxt (Level.current ctxt), Some (Timestamp.current ctxt))
        ctxt q.cycles q.levels >>=? fun levels ->
      let max_priority =
        match q.max_priority with
        | None -> 64
        | Some max -> max in
      map_p (baking_priorities ctxt max_priority) levels >>=? fun rights ->
      let rights =
        if q.all then
          rights
        else
          List.map remove_duplicated_delegates rights in
      let rights = List.concat rights in
      match q.delegates with
      | [] -> return rights
      | _ :: _ as delegates ->
          let is_requested p =
            List.exists (Signature.Public_key_hash.equal p.delegate) delegates in
          return (List.filter is_requested rights)
    end

  let get ctxt
      ?(levels = []) ?(cycles = []) ?(delegates = []) ?(all = false)
      ?max_priority block =
    RPC_context.make_call0 S.baking_rights ctxt block
      { levels ; cycles ; delegates ; max_priority ; all }
      ()

end

module Endorsing_rights = struct

  type t = {
    level: Raw_level.t ;
    delegate: Signature.Public_key_hash.t ;
    slots: int list ;
    estimated_time: Time.t option ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { level ; delegate ; slots ; estimated_time } ->
         (level, delegate, slots, estimated_time))
      (fun (level, delegate, slots, estimated_time) ->
         { level ; delegate ; slots ; estimated_time })
      (obj4
         (req "level" Raw_level.encoding)
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "slots" (list uint16))
         (opt "estimated_time" Timestamp.encoding))

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "endorsing_rights")

    type endorsing_rights_query = {
      levels: Raw_level.t list ;
      cycles: Cycle.t list ;
      delegates: Signature.Public_key_hash.t list ;
    }

    let endorsing_rights_query =
      let open RPC_query in
      query (fun levels cycles delegates ->
          { levels ; cycles ; delegates })
      |+ multi_field "level" Raw_level.arg (fun t -> t.levels)
      |+ multi_field "cycle" Cycle.arg (fun t -> t.cycles)
      |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t -> t.delegates)
      |> seal

    let endorsing_rights =
      RPC_service.get_service
        ~description: "...FIXME..."
        ~query: endorsing_rights_query
        ~output: (list encoding)
        custom_root

  end

  let endorsement_slots ctxt (level, estimated_time) =
    let max_slot = Constants.endorsers_per_block ctxt in
    Baking.endorsement_priorities ctxt level >>=? fun contract_list ->
    let build (delegate, slots) = {
      level = level.level ; delegate ; slots ; estimated_time
    } in
    let rec loop l map slot =
      if Compare.Int.(slot >= max_slot) then
        return (List.map build (Signature.Public_key_hash.Map.bindings map))
      else
        let Misc.LCons (pk, next) = l in
        let delegate = Signature.Public_key.hash pk in
        let slots =
          match Signature.Public_key_hash.Map.find_opt delegate map with
          | None -> [slot]
          | Some slots -> slot :: slots in
        let map = Signature.Public_key_hash.Map.add delegate slots map in
        next () >>=? fun l ->
        loop l map (slot+1) in
    loop contract_list Signature.Public_key_hash.Map.empty 0

  let () =
    let open Services_registration in
    register0 S.endorsing_rights begin fun ctxt q () ->
      requested_levels
        ~default: (Level.current ctxt, Some (Timestamp.current ctxt))
        ctxt q.cycles q.levels >>=? fun levels ->
      map_p (endorsement_slots ctxt) levels >>=? fun rights ->
      let rights = List.concat rights in
      match q.delegates with
      | [] -> return rights
      | _ :: _ as delegates ->
          let is_requested p =
            List.exists (Signature.Public_key_hash.equal p.delegate) delegates in
          return (List.filter is_requested rights)
    end

  let get ctxt
      ?(levels = []) ?(cycles = []) ?(delegates = []) block =
    RPC_context.make_call0 S.endorsing_rights ctxt block
      { levels ; cycles ; delegates }
      ()

end

let endorsement_rights ctxt level =
  Endorsing_rights.endorsement_slots ctxt (level, None) >>=? fun l ->
  return (List.map (fun { Endorsing_rights.delegate ; _ } -> delegate) l)

let baking_rights ctxt max_priority =
  let max = match max_priority with None -> 64 | Some m -> m in
  let level = Level.current ctxt in
  Baking_rights.baking_priorities ctxt max (level, None) >>=? fun l ->
  return (level.level,
          List.map
            (fun { Baking_rights.delegate ; timestamp ; _ } ->
               (delegate, timestamp)) l)

