(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Main Entry Points *)

open Tezos_context

type error += Bad_endorsement (* TODO: doc *)
type error += Insert_coin (* TODO: doc *)
type error += Contract_not_delegatable (* TODO: doc *)
type error += Unimplemented
type error += Invalid_voting_period

let apply_delegate_operation_content
    ctxt delegate pred_block block_priority = function
  | Endorsement { block ; slot } ->
      fail_unless
        (Block_hash.equal block pred_block) Bad_endorsement >>=? fun () ->
      Mining.check_signing_rights ctxt slot delegate >>=? fun () ->
      Fitness.increase ctxt >>=? fun ctxt ->
      Mining.pay_endorsement_bond ctxt delegate >>=? fun (ctxt, bond) ->
      Mining.endorsement_reward ~block_priority >>=? fun reward ->
      Level.current ctxt >>=? fun { cycle = current_cycle } ->
      Lwt.return Tez.(reward +? bond) >>=? fun full_reward ->
      Reward.record ctxt delegate current_cycle full_reward
  | Proposals { period ; proposals } ->
      Level.current ctxt >>=? fun level ->
      fail_unless Voting_period.(level.voting_period = period)
        Invalid_voting_period >>=? fun () ->
      Amendment.record_proposals ctxt delegate proposals
  | Ballot { period ; proposal ; ballot } ->
      Level.current ctxt >>=? fun level ->
      fail_unless Voting_period.(level.voting_period = period)
        Invalid_voting_period >>=? fun () ->
      Amendment.record_ballot ctxt delegate proposal ballot

let rec is_reject = function
  | [] -> false
  | Script_interpreter.Reject _ :: _ -> true
  | _ :: err -> is_reject err

type error += Non_scripted_contract_with_parameter
type error += Scripted_contract_without_paramater

let apply_manager_operation_content ctxt accept_failing_script source = function
  | Transaction { amount ; parameters ; destination } -> begin
      Contract.spend ctxt source amount >>=? fun ctxt ->
      Contract.credit ctxt destination amount >>=? fun ctxt ->
      Contract.get_script ctxt destination >>=? function
      | No_script -> begin
          match parameters with
          | None | Some (Prim (_, "Unit", [])) -> return ctxt
          | Some _ -> fail Non_scripted_contract_with_parameter
        end
      | Script { code ; storage } ->
          match parameters with
          | None -> fail Scripted_contract_without_paramater
          | Some parameters ->
              Script_interpreter.execute
                source destination ctxt storage code amount parameters
                (Constants.instructions_per_transaction ctxt)
              >>= function
              | Ok (storage_res, _res, _steps, ctxt) ->
                  (* TODO: pay for the steps and the storage diff:
                     update_script_storage checks the storage cost *)
                  Contract.update_script_storage
                    ctxt destination storage_res >>=? fun ctxt ->
                  return ctxt
              | Error err ->
                  if accept_failing_script && is_reject err then
                    return ctxt
                  else
                    Lwt.return (Error err)
    end
  | Origination { manager ; delegate ; script ;
                  spendable ; delegatable ; credit } -> begin
      match script with
      | No_script -> return ()
      | Script { code ; storage } ->
          Script_ir_translator.parse_script ctxt storage code >>=? fun _ ->
          let storage_fee = Script.storage_cost storage in
          let code_fee = Script.code_cost code in
          Lwt.return Tez.(code_fee +? storage_fee) >>=? fun script_fee ->
          Lwt.return Tez.(script_fee +? Constants.origination_burn) >>=? fun total_fee ->
          fail_unless Tez.(credit > total_fee) Insert_coin >>=? fun () ->
          return ()
    end >>=? fun () ->
      Contract.spend ctxt source credit >>=? fun ctxt ->
      Lwt.return Tez.(credit -? Constants.origination_burn) >>=? fun balance ->
      Contract.originate ctxt
        ~manager ~delegate ~balance
        ~script ~spendable ~delegatable >>=? fun (ctxt, _) ->
      return ctxt
  | Issuance { asset = (asset, key); amount } ->
      Contract.issue ctxt source asset key amount
      (* TODO: pay for the storage diff *)
  | Delegation delegate ->
      Contract.is_delegatable ctxt source >>=? fun delegatable ->
      fail_unless delegatable Contract_not_delegatable >>=? fun () ->
      Contract.set_delegate ctxt source delegate

let check_signature_and_update_public_key ctxt id public_key op =
  begin
    match public_key with
    | None -> return ctxt
    | Some public_key ->
        Public_key.set ctxt id public_key
  end >>=? fun ctxt ->
  Public_key.get ctxt id >>=? fun public_key ->
  Operation.check_signature public_key op >>=? fun () ->
  return ctxt

(* TODO document parameters *)
let apply_sourced_operation
    ctxt accept_failing_script miner_contract pred_block block_prio operation ops =
  match ops with
  | Manager_operations { source ; public_key ; fee ; counter ; operations = contents } ->
      Contract.get_manager ctxt source >>=? fun manager ->
      check_signature_and_update_public_key
        ctxt manager public_key operation >>=? fun ctxt ->
      Contract.check_counter_increment
        ctxt source counter >>=? fun () ->
      Contract.increment_counter ctxt source >>=? fun ctxt ->
      Contract.spend ctxt source fee >>=? fun ctxt ->
      (match miner_contract with
       | None -> return ctxt
       | Some contract ->
           Contract.credit ctxt contract fee) >>=? fun ctxt ->
      fold_left_s (fun ctxt content ->
          apply_manager_operation_content ctxt accept_failing_script source content)
        ctxt contents >>=? fun ctxt ->
      return ctxt
  | Delegate_operations { source ; operations = contents } ->
      let delegate = Ed25519.hash source in
      check_signature_and_update_public_key
        ctxt delegate (Some source) operation >>=? fun ctxt ->
    (* TODO, see how to extract the public key hash after this operation to
       pass it to apply_delegate_operation_content *)
    fold_left_s (fun ctxt content ->
          apply_delegate_operation_content
            ctxt delegate pred_block block_prio content)
        ctxt contents >>=? fun ctxt ->
      return ctxt

let apply_anonymous_operation ctxt miner_contract kind =
  match kind with
  | Seed_nonce_revelation { level ; nonce } ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun (ctxt, delegate_to_reward,
                                              reward_amount) ->
      Reward.record ctxt
        delegate_to_reward level.cycle reward_amount >>=? fun ctxt ->
      (match miner_contract with
       | None -> return ctxt
       | Some contract ->
           Contract.credit ctxt contract Constants.seed_nonce_revelation_tip)

let apply_operation
    ctxt accept_failing_script miner_contract pred_block block_prio operation =
  match operation.contents with
  | Anonymous_operations ops ->
      fold_left_s
        (fun ctxt -> apply_anonymous_operation ctxt miner_contract)
        ctxt ops
  | Sourced_operations op ->
      apply_sourced_operation
        ctxt accept_failing_script miner_contract pred_block block_prio
        operation op

let may_start_new_cycle ctxt =
  Mining.dawn_of_a_new_cycle ctxt >>=? function
  | None -> return ctxt
  | Some new_cycle ->
      let last_cycle =
        match Cycle.pred new_cycle with
        | None -> assert false
        | Some last_cycle -> last_cycle in
      Seed.clear_cycle ctxt last_cycle >>=? fun ctxt ->
      Seed.compute_for_cycle ctxt (Cycle.succ new_cycle) >>=? fun ctxt ->
      Roll.clear_cycle ctxt last_cycle >>=? fun ctxt ->
      Roll.freeze_rolls_for_cycle ctxt (Cycle.succ new_cycle) >>=? fun ctxt ->
      Timestamp.get_current ctxt >>=? fun timestamp ->
      Lwt.return (Timestamp.(timestamp +? (Constants.time_before_reward ctxt)))
      >>=? fun reward_date ->
      Reward.set_reward_time_for_cycle
        ctxt last_cycle reward_date >>=? fun ctxt ->
      return ctxt

let apply_main ctxt accept_failing_script block operations =
  (* read only checks *)
  Mining.check_proof_of_work_stamp ctxt block >>=? fun () ->
  Mining.check_fitness_gap ctxt block >>=? fun () ->
  Mining.check_mining_rights ctxt block >>=? fun delegate_pkh ->
  Mining.check_signature ctxt block delegate_pkh >>=? fun () ->
  (* automatic bonds payment *)
  Mining.pay_mining_bond ctxt block delegate_pkh >>=? fun ctxt ->
  (* set timestamp *)
  Timestamp.set_current ctxt block.shell.timestamp >>=? fun ctxt ->
  (* do effectful stuff *)
  Fitness.increase ctxt >>=? fun ctxt ->
  let priority = snd block.proto.mining_slot in
  fold_left_s (fun ctxt operation ->
      apply_operation
        ctxt accept_failing_script
        (Some (Contract.default_contract delegate_pkh))
        block.shell.predecessor priority operation)
    ctxt operations >>=? fun ctxt ->
  (* end of level (from this point nothing should fail) *)
  let reward =
    Mining.base_mining_reward ctxt
      ~priority:(snd block.proto.mining_slot) in
  Nonce.record_hash ctxt
    delegate_pkh reward block.proto.seed_nonce_hash >>=? fun ctxt ->
  Reward.pay_due_rewards ctxt >>=? fun ctxt ->
  Level.increment_current ctxt >>=? fun ctxt ->
  (* end of cycle *)
  may_start_new_cycle ctxt >>=? fun ctxt ->
  Amendment.may_start_new_voting_cycle ctxt >>=? fun ctxt ->
  return ctxt

type error += Internal_error of string

let apply ctxt accept_failing_script block operations =
  (init ctxt >>=? fun ctxt ->
   get_prevalidation ctxt >>= function
   | true ->
       fail (Internal_error "we should not call `apply` after `preapply`!")
   | false ->
       apply_main ctxt accept_failing_script block operations >>=? fun ctxt ->
       finalize ctxt)

let empty_result =
  { Updater.applied = [];
    refused = Operation_hash_map.empty;
    branch_refused = Operation_hash_map.empty;
    branch_delayed = Operation_hash_map.empty;
  }

let compare_operations op1 op2 =
  match op1.contents, op2.contents with
  | Anonymous_operations _, Anonymous_operations _ -> 0
  | Anonymous_operations _, Sourced_operations _ -> 1
  | Sourced_operations _, Anonymous_operations _ -> -1
  | Sourced_operations op1, Sourced_operations op2 ->
      match op1, op2 with
      | Delegate_operations _, Manager_operations _ -> -1
      | Manager_operations _, Delegate_operations _ -> 1
      | Delegate_operations _, Delegate_operations _ -> 0
      | Manager_operations op1, Manager_operations op2 -> begin
          (* Manager operations with smaller counter are pre-validated first. *)
          Int32.compare op1.counter op2.counter
        end

let merge_result r r' =
  let open Updater in
  let merge _key a b =
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | _, Some y -> Some y in
  { applied = r.applied @ r'.applied ;
    refused = Operation_hash_map.merge merge r.refused r'.refused ;
    branch_refused =
      Operation_hash_map.merge merge r.branch_refused r'.branch_refused ;
    branch_delayed = r'.branch_delayed ;
  }

let prevalidate ctxt pred_block sort operations =
  let operations =
    if sort then List.sort compare_operations operations else operations in
  let rec loop ctxt operations =
    (Lwt_list.fold_left_s
       (fun (ctxt, r) op ->
          apply_operation ctxt false None pred_block 0l op >>= function
          | Ok ctxt ->
              let applied = op.hash :: r.Updater.applied in
              Lwt.return (ctxt, { r with Updater.applied} )
          | Error errors ->
              match classify_errors errors with
              | `Branch ->
                  let branch_refused =
                    Operation_hash_map.add op.hash errors r.Updater.branch_refused in
                  Lwt.return (ctxt, { r with Updater.branch_refused })
              | `Permanent ->
                  let refused =
                    Operation_hash_map.add op.hash errors r.Updater.refused in
                  Lwt.return (ctxt, { r with Updater.refused })
              | `Temporary ->
                  let branch_delayed =
                    Operation_hash_map.add op.hash errors r.Updater.branch_delayed in
                  Lwt.return (ctxt, { r with Updater.branch_delayed }))
       (ctxt, empty_result)
       operations >>= fun (ctxt, r) ->
     match r.Updater.applied with
     | _ :: _ when sort ->
         let rechecked_operations =
           List.filter
             (fun op -> Operation_hash_map.mem op.hash r.Updater.branch_delayed)
             operations in
         loop ctxt rechecked_operations >>=? fun (ctxt, r') ->
         return (ctxt, merge_result r r')
     | _ ->
         return (ctxt, r)) in
  loop ctxt operations

let preapply ctxt pred_block timestamp sort operations =
  let result =
    init ctxt >>=? fun ctxt ->
    begin
      get_prevalidation ctxt >>= function
      | true -> return ctxt
      | false ->
          set_prevalidation ctxt >>= fun ctxt ->
          Fitness.increase ctxt >>=? fun ctxt ->
          return ctxt
    end >>=? fun ctxt ->
    Timestamp.set_current ctxt timestamp >>=? fun ctxt ->
    prevalidate ctxt pred_block sort operations >>=? fun (ctxt, r) ->
    (* TODO should accept failing script in the last round ?
            or: what should we export to let the miner decide *)
    finalize ctxt >>=? fun ctxt ->
    return (ctxt, r) in
  (* "Reify" errors into options. *)
  result >>|? function  (ctxt, r) ->
    (ctxt, { r with Updater.applied = List.rev r.Updater.applied })
