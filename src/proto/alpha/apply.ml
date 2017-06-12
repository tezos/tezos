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

type error += Wrong_voting_period of Voting_period.t * Voting_period.t (* `Temporary *)
type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t (* `Temporary *)

let () =
  register_error_kind
    `Temporary
    ~id:"operation.wrong_endorsement_predecessor"
    ~title:"Wrong endorsement predecessor"
    ~description:"Trying to include an endorsement in a block \
                  that is not the successor of the endorsed one"
    ~pp:(fun ppf (e, p) ->
        Format.fprintf ppf "Wrong predecessor %a, expected %a"
          Block_hash.pp p Block_hash.pp e)
    Data_encoding.(obj2
                     (req "expected" Block_hash.encoding)
                     (req "provided" Block_hash.encoding))
    (function Wrong_endorsement_predecessor (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_endorsement_predecessor (e, p)) ;
  register_error_kind
    `Temporary
    ~id:"operation.wrong_voting_period"
    ~title:"Wrong voting period"
    ~description:"Trying to onclude a proposal or ballot \
                  meant for another voting period"
    ~pp:(fun ppf (e, p) ->
        Format.fprintf ppf "Wrong voting period %a, current is %a"
          Voting_period.pp p Voting_period.pp e)
    Data_encoding.(obj2
                     (req "current" Voting_period.encoding)
                     (req "provided" Voting_period.encoding))
    (function Wrong_voting_period (e, p) -> Some (e, p) | _ -> None)
    (fun (e, p) -> Wrong_voting_period (e, p))

let apply_delegate_operation_content
    ctxt delegate pred_block block_priority = function
  | Endorsement { block ; slot } ->
      fail_unless
        (Block_hash.equal block pred_block)
        (Wrong_endorsement_predecessor (pred_block, block)) >>=? fun () ->
      Mining.check_signing_rights ctxt slot delegate >>=? fun () ->
      let ctxt = Fitness.increase ctxt in
      Mining.pay_endorsement_bond ctxt delegate >>=? fun (ctxt, bond) ->
      Mining.endorsement_reward ~block_priority >>=? fun reward ->
      let { cycle = current_cycle } : Level.t = Level.current ctxt in
      Lwt.return Tez.(reward +? bond) >>=? fun full_reward ->
      Reward.record ctxt delegate current_cycle full_reward
  | Proposals { period ; proposals } ->
      let level = Level.current ctxt in
      fail_unless Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period)) >>=? fun () ->
      Amendment.record_proposals ctxt delegate proposals
  | Ballot { period ; proposal ; ballot } ->
      let level = Level.current ctxt in
      fail_unless Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period)) >>=? fun () ->
      Amendment.record_ballot ctxt delegate proposal ballot

type error += Non_scripted_contract_with_parameter
type error += Scripted_contract_without_paramater

let apply_manager_operation_content
    ctxt origination_nonce source = function
  | Transaction { amount ; parameters ; destination } -> begin
      Contract.spend ctxt source amount >>=? fun ctxt ->
      Contract.credit ctxt destination amount >>=? fun ctxt ->
      Contract.get_script ctxt destination >>=? function
      | None -> begin
          match parameters with
          | None | Some (Prim (_, "Unit", [])) ->
              return (ctxt, origination_nonce, None)
          | Some _ -> fail Non_scripted_contract_with_parameter
        end
      | Some { code ; storage } ->
          match parameters with
          | None -> fail Scripted_contract_without_paramater
          | Some parameters ->
              Script_interpreter.execute
                origination_nonce
                source destination ctxt storage code amount parameters
                (Constants.instructions_per_transaction ctxt)
              >>= function
              | Ok (storage_res, _res, _steps, ctxt, origination_nonce) ->
                  (* TODO: pay for the steps and the storage diff:
                     update_script_storage checks the storage cost *)
                  Contract.update_script_storage_and_fees
                    ctxt destination
                    Script_interpreter.dummy_storage_fee storage_res >>=? fun ctxt ->
                  return (ctxt, origination_nonce, None)
              | Error err ->
                  return (ctxt, origination_nonce, Some err)
    end
  | Origination { manager ; delegate ; script ;
                  spendable ; delegatable ; credit } ->
      begin match script with
        | None -> return None
        | Some ({ Script.storage ; code } as script) ->
            Script_ir_translator.parse_script ctxt storage code >>=? fun _ ->
            return (Some (script, (Script_interpreter.dummy_code_fee, Script_interpreter.dummy_storage_fee)))
      end >>=? fun script ->
      Contract.spend ctxt source Constants.origination_burn >>=? fun ctxt ->
      Contract.spend ctxt source credit >>=? fun ctxt ->
      Contract.originate ctxt
        origination_nonce
        ~manager ~delegate ~balance:credit
        ?script
        ~spendable ~delegatable >>=? fun (ctxt, _, origination_nonce) ->
      return (ctxt, origination_nonce, None)
  | Delegation delegate ->
      Contract.set_delegate ctxt source delegate >>=? fun ctxt ->
      return (ctxt, origination_nonce, None)

let check_signature_and_update_public_key ctxt id public_key op =
  begin
    match public_key with
    | None -> return ctxt
    | Some public_key ->
        Public_key.reveal ctxt id public_key
  end >>=? fun ctxt ->
  Public_key.get ctxt id >>=? fun public_key ->
  Operation.check_signature public_key op >>=? fun () ->
  return ctxt

let apply_sourced_operation
    ctxt miner_contract pred_block block_prio
    operation origination_nonce ops =
  match ops with
  | Manager_operations { source ; public_key ; fee ; counter ; operations = contents } ->
      Contract.must_exist ctxt source >>=? fun () ->
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
      fold_left_s (fun (ctxt, origination_nonce, err) content ->
          match err with
          | Some _ -> return (ctxt, origination_nonce, err)
          | None ->
              Contract.must_exist ctxt source >>=? fun () ->
              apply_manager_operation_content
                ctxt origination_nonce source content)
        (ctxt, origination_nonce, None) contents
  | Delegate_operations { source ; operations = contents } ->
      let delegate = Ed25519.Public_key.hash source in
      check_signature_and_update_public_key
        ctxt delegate (Some source) operation >>=? fun ctxt ->
    (* TODO, see how to extract the public key hash after this operation to
       pass it to apply_delegate_operation_content *)
    fold_left_s (fun ctxt content ->
          apply_delegate_operation_content
            ctxt delegate pred_block block_prio content)
        ctxt contents >>=? fun ctxt ->
      return (ctxt, origination_nonce, None)
  | Dictator_operation (Activate hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      activate ctxt hash >>= fun ctxt ->
      return (ctxt, origination_nonce, None)
  | Dictator_operation (Activate_testnet hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      let expiration = (* in two days maximum... *)
        Time.add (Timestamp.current ctxt) (Int64.mul 48L 3600L) in
      fork_test_network ctxt hash expiration >>= fun ctxt ->
      return (ctxt, origination_nonce, None)

let apply_anonymous_operation ctxt miner_contract origination_nonce kind =
  match kind with
  | Seed_nonce_revelation { level ; nonce } ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce
      >>=? fun (ctxt, delegate_to_reward, reward_amount) ->
      Reward.record ctxt
        delegate_to_reward level.cycle reward_amount >>=? fun ctxt ->
      begin
        match miner_contract with
        | None -> return (ctxt, origination_nonce)
        | Some contract ->
            Contract.credit
              ctxt contract Constants.seed_nonce_revelation_tip >>=? fun ctxt ->
            return (ctxt, origination_nonce)
      end
  | Faucet { id = manager } ->
      (* Free tez for all! *)
      begin
        match miner_contract with
        | None -> return None
        | Some contract -> Contract.get_delegate_opt ctxt contract
      end >>=? fun delegate ->
      Contract.originate ctxt
        origination_nonce
        ~manager ~delegate ~balance:Constants.faucet_credit ?script:None
        ~spendable:true ~delegatable:true >>=? fun (ctxt, _, origination_nonce) ->
      return (ctxt, origination_nonce)

let apply_operation
    ctxt miner_contract pred_block block_prio operation =
  match operation.contents with
  | Anonymous_operations ops ->
      let origination_nonce = Contract.initial_origination_nonce operation.hash in
      fold_left_s
        (fun (ctxt, origination_nonce) ->
           apply_anonymous_operation ctxt miner_contract origination_nonce)
        (ctxt, origination_nonce) ops >>=? fun (ctxt, origination_nonce) ->
      return (ctxt, Contract.originated_contracts origination_nonce, None)
  | Sourced_operations op ->
      let origination_nonce = Contract.initial_origination_nonce operation.hash in
      apply_sourced_operation
        ctxt miner_contract pred_block block_prio
        operation origination_nonce op >>=? fun (ctxt, origination_nonce, err) ->
      return (ctxt, Contract.originated_contracts origination_nonce, err)

let may_start_new_cycle ctxt =
  Mining.dawn_of_a_new_cycle ctxt >>=? function
  | None -> return ctxt
  | Some last_cycle ->
      let new_cycle = Cycle.succ last_cycle in
      Bootstrap.refill ctxt >>=? fun ctxt ->
      Seed.clear_cycle ctxt last_cycle >>=? fun ctxt ->
      Seed.compute_for_cycle ctxt (Cycle.succ new_cycle) >>=? fun ctxt ->
      Roll.clear_cycle ctxt last_cycle >>=? fun ctxt ->
      Roll.freeze_rolls_for_cycle ctxt (Cycle.succ new_cycle) >>=? fun ctxt ->
      let timestamp = Timestamp.current ctxt in
      Lwt.return (Timestamp.(timestamp +? (Constants.time_before_reward ctxt)))
      >>=? fun reward_date ->
      Reward.set_reward_time_for_cycle
        ctxt last_cycle reward_date >>=? fun ctxt ->
      return ctxt

let begin_full_construction ctxt pred_timestamp proto_header =
  Lwt.return
    (Block_header.parse_unsigned_proto_header
       proto_header) >>=? fun proto_header ->
  Mining.check_mining_rights
    ctxt proto_header pred_timestamp >>=? fun miner ->
  Mining.pay_mining_bond ctxt proto_header miner >>=? fun ctxt ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, proto_header, miner)

let begin_partial_construction ctxt =
  let ctxt = Fitness.increase ctxt in
  return ctxt

let begin_application ctxt block_header pred_timestamp =
  Mining.check_proof_of_work_stamp ctxt block_header >>=? fun () ->
  Mining.check_fitness_gap ctxt block_header >>=? fun () ->
  Mining.check_mining_rights
    ctxt block_header.proto pred_timestamp >>=? fun miner ->
  Mining.check_signature ctxt block_header miner >>=? fun () ->
  Mining.pay_mining_bond ctxt block_header.proto miner >>=? fun ctxt ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, miner)

let finalize_application ctxt block_proto_header miner =
  (* end of level (from this point nothing should fail) *)
  let priority = block_proto_header.Block_header.priority in
  let reward = Mining.base_mining_reward ctxt ~priority in
  Nonce.record_hash ctxt
    miner reward block_proto_header.seed_nonce_hash >>=? fun ctxt ->
  Reward.pay_due_rewards ctxt >>=? fun ctxt ->
  (* end of cycle *)
  may_start_new_cycle ctxt >>=? fun ctxt ->
  Amendment.may_start_new_voting_cycle ctxt >>=? fun ctxt ->
  return ctxt

let compare_operations op1 op2 =
  match op1.contents, op2.contents with
  | Anonymous_operations _, Anonymous_operations _ -> 0
  | Anonymous_operations _, Sourced_operations _ -> -1
  | Sourced_operations _, Anonymous_operations _ -> 1
  | Sourced_operations op1, Sourced_operations op2 ->
      match op1, op2 with
      | Delegate_operations _, (Manager_operations _ | Dictator_operation _) -> -1
      | Manager_operations _, Dictator_operation _ -> -1
      | Dictator_operation _, Manager_operations _ -> 1
      | (Manager_operations _ | Dictator_operation _), Delegate_operations _ -> 1
      | Delegate_operations _, Delegate_operations _ -> 0
      | Dictator_operation _, Dictator_operation _ -> 0
      | Manager_operations op1, Manager_operations op2 -> begin
          (* Manager operations with smaller counter are pre-validated first. *)
          Int32.compare op1.counter op2.counter
        end
