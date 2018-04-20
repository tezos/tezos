(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

let slots_range_encoding =
  let open Data_encoding in
  (obj3
     (opt "max_priority" int31)
     (opt "first_level" Raw_level.encoding)
     (opt "last_level" Raw_level.encoding))

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


module Baker = struct

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "rights" / "baking")

    let slot_encoding =
      (obj3
         (req "level" Raw_level.encoding)
         (req "priority" int31)
         (req "timestamp" Timestamp.encoding))

    let rights =
      RPC_service.post_service
        ~description:
          "List delegates allowed to bake for the next level, \
           ordered by priority."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "baking_rights"
                       (list
                          (obj2
                             (req "delegate" Signature.Public_key_hash.encoding)
                             (req "timestamp" Timestamp.encoding)))))
        custom_root

    let rights_for_level =
      RPC_service.post_service
        ~description:
          "List delegates allowed to bake for a given level, \
           ordered by priority."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Signature.Public_key_hash.encoding)))
        RPC_path.(custom_root / "level" /: Raw_level.arg)

    (* let levels = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List level for which we might computed baking rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "levels" (list Raw_level.encoding))) *)
    (* RPC_path.(custom_root / "level") *)

    let rights_for_delegate =
      RPC_service.post_service
        ~description: "Future baking rights for a given delegate."
        ~query: RPC_query.empty
        ~input: slots_range_encoding
        ~output: (Data_encoding.list slot_encoding)
        RPC_path.(custom_root / "delegate" /: Signature.Public_key_hash.rpc_arg)


    (* let delegates = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List delegates with baking rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "delegates" *)
    (* (list Signature.Public_key_hash.encoding))) *)
    (* RPC_path.(custom_root / "delegate") *)

  end

  module I = struct

    let default_max_baking_priority ctxt arg =
      let default = Constants.first_free_baking_slot ctxt in
      match arg with
      | None -> 2 * default
      | Some m -> m

    let baking_rights_for_level ctxt level max =
      let max = default_max_baking_priority ctxt max in
      Baking.baking_priorities ctxt level >>=? fun contract_list ->
      let rec loop l n =
        match n with
        | 0 -> return []
        | n ->
            let Misc.LCons (h, t) = l in
            t () >>=? fun t ->
            loop t (pred n) >>=? fun t ->
            return (Signature.Public_key.hash h :: t)
      in
      loop contract_list max >>=? fun prio ->
      return (level.level, prio)

    let baking_rights ctxt () max =
      let level = Level.succ ctxt (Level.current ctxt) in
      baking_rights_for_level ctxt level max >>=? fun (raw_level, slots) ->
      begin
        Lwt_list.filter_map_p (fun x -> x) @@
        List.mapi
          (fun prio c ->
             let timestamp = Timestamp.current ctxt in
             Baking.minimal_time ctxt prio timestamp >>= function
             | Error _ -> Lwt.return None
             | Ok minimal_timestamp -> Lwt.return (Some (c, minimal_timestamp)))
          slots
      end >>= fun timed_slots ->
      return (raw_level, timed_slots)

    let baking_rights_for_delegate
        ctxt contract () (max_priority, min_level, max_level) =
      let max_priority = default_max_baking_priority ctxt max_priority in
      let current_level = Level.succ ctxt (Level.current ctxt) in
      let min_level = match min_level with
        | None -> current_level
        | Some l -> Level.from_raw ctxt l in
      let max_level =
        match max_level with
        | Some max_level -> Level.from_raw ctxt max_level
        | None ->
            Level.last_level_in_cycle ctxt @@
            current_level.cycle in
      let rec loop level =
        if Level.(>) level max_level
        then return []
        else
          loop (Level.succ ctxt level) >>=? fun t ->
          Baking.first_baking_priorities
            ctxt ~max_priority contract level >>=? fun priorities ->
          let raw_level = level.level in
          Error_monad.map_s
            (fun priority ->
               let timestamp = Timestamp.current ctxt in
               Baking.minimal_time ctxt priority timestamp >>=? fun time ->
               return (raw_level, priority, time))
            priorities >>=? fun priorities ->
          return (priorities @ t)
      in
      loop min_level

  end

  let () =
    let open Services_registration in
    register0 S.rights I.baking_rights ;
    register1 S.rights_for_level begin fun ctxt raw_level () max ->
      let level = Level.from_raw ctxt raw_level in
      I.baking_rights_for_level ctxt level max
    end;
    register1 S.rights_for_delegate I.baking_rights_for_delegate

  let rights ctxt ?max_priority block =
    RPC_context.make_call0 S.rights ctxt block () max_priority

  let rights_for_level ctxt ?max_priority block level =
    RPC_context.make_call1 S.rights_for_level ctxt block level () max_priority

  let rights_for_delegate ctxt ?max_priority ?first_level ?last_level block delegate =
    RPC_context.make_call1 S.rights_for_delegate ctxt block delegate ()
      (max_priority, first_level, last_level)

end

module Endorser = struct

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "rights" / "endorsement")

    let slot_encoding =
      (obj2
         (req "level" Raw_level.encoding)
         (req "priority" int31))

    let rights =
      RPC_service.post_service
        ~description:
          "List delegates allowed to endorse for the current block."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Signature.Public_key_hash.encoding)))
        custom_root

    let rights_for_level =
      RPC_service.post_service
        ~description:
          "List delegates allowed to endorse blocks for a given level."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Signature.Public_key_hash.encoding)))
        RPC_path.(custom_root / "level" /: Raw_level.arg)

    (* let levels = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List level for which we might computed endorsement rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "levels" (list Raw_level.encoding))) *)
    (* RPC_path.(custom_root / "level") *)

    let rights_for_delegate =
      RPC_service.post_service
        ~description: "Compute endorsement rights for a given delegate."
        ~query: RPC_query.empty
        ~input: slots_range_encoding
        ~output: (Data_encoding.list slot_encoding)
        RPC_path.(custom_root / "delegate" /: Signature.Public_key_hash.rpc_arg)

    (* let delegates = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List delegates with endorsement rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "delegates" *)
    (* (list Signature.Public_key_hash.encoding))) *)
    (* RPC_path.(custom_root / "delegate") *)

  end

  module I = struct

    let default_max_endorsement_priority ctxt arg =
      let default = Constants.endorsers_per_block ctxt in
      match arg with
      | None -> default
      | Some m -> m

    let endorsement_rights ctxt level max =
      let max = default_max_endorsement_priority ctxt max in
      Baking.endorsement_priorities ctxt level >>=? fun contract_list ->
      let rec loop l n =
        match n with
        | 0 -> return []
        | n ->
            let Misc.LCons (h, t) = l in
            t () >>=? fun t ->
            loop t (pred n) >>=? fun t ->
            return (Signature.Public_key.hash h :: t)
      in
      loop contract_list max >>=? fun prio ->
      return (level.level, prio)

    let endorsement_rights_for_delegate
        ctxt contract () (max_priority, min_level, max_level) =
      let current_level = Level.current ctxt in
      let max_priority = default_max_endorsement_priority ctxt max_priority in
      let min_level = match min_level with
        | None -> current_level
        | Some l -> Level.from_raw ctxt l in
      let max_level =
        match max_level with
        | None -> min_level
        | Some l -> Level.from_raw ctxt l in
      let rec loop level =
        if Level.(>) level max_level
        then return []
        else
          loop (Level.succ ctxt level) >>=? fun t ->
          Baking.first_endorsement_slots
            ctxt ~max_priority contract level >>=? fun slots ->
          let raw_level = level.level in
          let slots = List.rev_map (fun slot -> (raw_level, slot)) slots in
          return (List.rev_append slots t)
      in
      loop min_level

  end

  let () =
    let open Services_registration in
    register0 S.rights begin fun ctxt () max ->
      let level = Level.current ctxt in
      I.endorsement_rights ctxt level max
    end ;
    register1 S.rights_for_level begin fun ctxt raw_level () max ->
      let level = Level.from_raw ctxt raw_level in
      I.endorsement_rights ctxt level max
    end ;
    register1 S.rights_for_delegate I.endorsement_rights_for_delegate

  let rights ctxt ?max_priority block =
    RPC_context.make_call0 S.rights ctxt block () max_priority

  let rights_for_level ctxt ?max_priority block level =
    RPC_context.make_call1 S.rights_for_level ctxt block level () max_priority

  let rights_for_delegate ctxt ?max_priority ?first_level ?last_level block delegate =
    RPC_context.make_call1 S.rights_for_delegate ctxt block delegate ()
      (max_priority, first_level, last_level)

end


let baking_rights = Baker.I.baking_rights
let endorsement_rights = Endorser.I.endorsement_rights
