(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Main Entry Points *)

open Alpha_context

type error += Wrong_voting_period of Voting_period.t * Voting_period.t (* `Temporary *)
type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t (* `Temporary *)
type error += Duplicate_endorsement of int (* `Branch *)
type error += Bad_contract_parameter of Contract.t * Script.expr option * Script.expr option (* `Permanent *)
type error += Too_many_faucet
type error += Invalid_endorsement_level
type error += Invalid_commitment of { expected: bool }


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
    (fun (e, p) -> Wrong_voting_period (e, p));
  register_error_kind
    `Permanent
    ~id:"badContractParameter"
    ~title:"Contract supplied an invalid parameter"
    ~description:"Either no parameter was supplied to a contract, \
                  a parameter was passed to an account, \
                  or a parameter was supplied of the wrong type"
    Data_encoding.(obj3
                     (req "contract" Contract.encoding)
                     (opt "expectedType" Script.expr_encoding)
                     (opt "providedArgument" Script.expr_encoding))
    (function Bad_contract_parameter (c, expected, supplied) ->
       Some (c, expected, supplied) | _ -> None)
    (fun (c, expected, supplied) -> Bad_contract_parameter (c, expected, supplied)) ;
  register_error_kind
    `Branch
    ~id:"operation.duplicate_endorsement"
    ~title:"Duplicate endorsement"
    ~description:"Two endorsements received for the same slot"
    ~pp:(fun ppf k ->
        Format.fprintf ppf "Duplicate endorsement for slot %d." k)
    Data_encoding.(obj1 (req "slot" uint16))
    (function Duplicate_endorsement k -> Some k | _ -> None)
    (fun k -> Duplicate_endorsement k);
  register_error_kind
    `Temporary
    ~id:"operation.too_many_faucet"
    ~title:"Too many faucet"
    ~description:"Trying to include a faucet operation in a block \
                 \ with more than 5 faucet operations."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Too many faucet operation.")
    Data_encoding.unit
    (function Too_many_faucet -> Some () | _ -> None)
    (fun () -> Too_many_faucet) ;
  register_error_kind
    `Temporary
    ~id:"operation.invalid_endorsement_level"
    ~title:"Unpexpected level in endorsement"
    ~description:"The level of an endorsement is inconsistent with the \
                 \ provided block hash."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Unpexpected level in endorsement.")
    Data_encoding.unit
    (function Invalid_endorsement_level -> Some () | _ -> None)
    (fun () -> Invalid_endorsement_level) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_commitment"
    ~title:"Invalid commitment in block header"
    ~description:"The block header has invalid commitment."
    ~pp:(fun ppf expected ->
        if expected then
          Format.fprintf ppf "Missing seed's nonce commitment in block header."
        else
          Format.fprintf ppf "Unexpected seed's nonce commitment in block header.")
    Data_encoding.(obj1 (req "expected "bool))
    (function Invalid_commitment { expected } -> Some expected | _ -> None)
    (fun expected -> Invalid_commitment { expected })

let apply_consensus_operation_content ctxt
    pred_block block_priority operation = function
  | Endorsements { block ; level ; slots } ->
      begin
        match Level.pred ctxt (Level.current ctxt) with
        | None -> failwith ""
        | Some lvl -> return lvl
      end >>=? fun ({ level = current_level ;_ } as lvl) ->
      fail_unless
        (Block_hash.equal block pred_block)
        (Wrong_endorsement_predecessor (pred_block, block)) >>=? fun () ->
      fail_unless
        Raw_level.(level = current_level)
        Invalid_endorsement_level >>=? fun () ->
      fold_left_s (fun ctxt slot ->
          fail_when
            (endorsement_already_recorded ctxt slot)
            (Duplicate_endorsement slot) >>=? fun () ->
          return (record_endorsement ctxt slot))
        ctxt slots >>=? fun ctxt ->
      Baking.check_endorsements_rights ctxt lvl slots >>=? fun delegate ->
      Operation.check_signature delegate operation >>=? fun () ->
      let delegate = Ed25519.Public_key.hash delegate in
      let ctxt = Fitness.increase ~gap:(List.length slots) ctxt in
      Baking.freeze_endorsement_bond ctxt delegate >>=? fun ctxt ->
      Baking.endorsement_reward ~block_priority >>=? fun reward ->
      Delegate.freeze_rewards ctxt delegate reward >>=? fun ctxt ->
      return ctxt

let apply_amendment_operation_content ctxt delegate = function
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

let apply_manager_operation_content
    ctxt origination_nonce source = function
  | Reveal _ -> return (ctxt, origination_nonce, None)
  | Transaction { amount ; parameters ; destination } -> begin
      Contract.spend ctxt source amount >>=? fun ctxt ->
      Contract.credit ctxt destination amount >>=? fun ctxt ->
      Contract.get_script ctxt destination >>=? function
      | None -> begin
          match parameters with
          | None ->
              return (ctxt, origination_nonce, None)
          | Some arg ->
              match Micheline.root arg with
              | Prim (_, D_Unit, [], _) ->
                  return (ctxt, origination_nonce, None)
              | _ -> fail (Bad_contract_parameter (destination, None, parameters))
        end
      | Some script ->
          let call_contract argument =
            Script_interpreter.execute
              origination_nonce
              source destination ctxt script amount argument
              (Gas.of_int (Constants.max_gas ctxt))
            >>= function
            | Ok (storage_res, _res, _steps, ctxt, origination_nonce, maybe_big_map_diff) ->
                (* TODO: pay for the steps and the storage diff:
                   update_script_storage checks the storage cost *)
                Contract.update_script_storage
                  ctxt destination
                  storage_res
                  (match maybe_big_map_diff with
                   | None -> None
                   | Some map ->
                       Some (Script_ir_translator.to_serializable_big_map map)) >>=? fun ctxt ->
                Fees.update_script_storage ctxt ~source
                  destination Script_interpreter.dummy_storage_fee >>=? fun ctxt ->
                return (ctxt, origination_nonce, None)
            | Error err ->
                return (ctxt, origination_nonce, Some err) in
          Lwt.return (Script_ir_translator.parse_toplevel script.code) >>=? fun (arg_type, _, _, _) ->
          let arg_type = Micheline.strip_locations arg_type in
          match parameters, Micheline.root arg_type with
          | None, Prim (_, T_unit, _, _) ->
              call_contract (Micheline.strip_locations (Prim (0, Script.D_Unit, [], None)))
          | Some parameters, _ -> begin
              Script_ir_translator.typecheck_data ctxt (parameters, arg_type) >>= function
              | Ok () -> call_contract parameters
              | Error errs ->
                  let err = Bad_contract_parameter (destination, Some arg_type, Some parameters) in
                  return (ctxt, origination_nonce, Some ((err :: errs)))
            end
          | None, _ -> fail (Bad_contract_parameter (destination, Some arg_type, None))
    end
  | Origination { manager ; delegate ; script ;
                  spendable ; delegatable ; credit } ->
      begin match script with
        | None -> return (None, None)
        | Some script ->
            Script_ir_translator.parse_script ctxt script >>=? fun _ ->
            Script_ir_translator.erase_big_map_initialization ctxt script >>=? fun (script, big_map_diff) ->
            return (Some (script, (Script_interpreter.dummy_code_fee, Script_interpreter.dummy_storage_fee)),
                    big_map_diff)
      end >>=? fun (script, big_map) ->
      Contract.spend ctxt source credit >>=? fun ctxt ->
      Contract.originate ctxt
        origination_nonce
        ~manager ~delegate ~balance:credit
        ?script
        ~spendable ~delegatable >>=? fun (ctxt, contract, origination_nonce) ->
      Fees.origination_burn ctxt ~source contract >>=? fun ctxt ->
      begin match big_map with
        | None -> return ctxt
        | Some diff ->
            fold_left_s (fun ctxt (key, value) ->
                match value with
                | None -> Contract.Big_map.remove ctxt contract key
                | Some v ->
                    Contract.Big_map.set ctxt contract key v)
              ctxt diff
      end >>=? fun ctxt ->
      return (ctxt, origination_nonce, None)
  | Delegation delegate ->
      Delegate.set ctxt source delegate >>=? fun ctxt ->
      return (ctxt, origination_nonce, None)

let apply_sourced_operation
    ctxt pred_block block_prio
    operation origination_nonce ops =
  match ops with
  | Manager_operations { source ; fee ; counter ; operations = contents } ->
      let public_key =
        List.fold_left (fun acc op ->
            match op with
            | Reveal pk -> Some pk
            | _ -> acc) None contents in
      Contract.must_exist ctxt source >>=? fun () ->
      Contract.update_manager_key ctxt source public_key >>=? fun (ctxt,public_key) ->
      Operation.check_signature public_key operation >>=? fun () ->
      Contract.check_counter_increment ctxt source counter >>=? fun () ->
      Contract.increment_counter ctxt source >>=? fun ctxt ->
      Contract.spend ctxt source fee >>=? fun ctxt ->
      fold_left_s (fun (ctxt, origination_nonce, err) content ->
          match err with
          | Some _ -> return (ctxt, origination_nonce, err)
          | None ->
              Contract.must_exist ctxt source >>=? fun () ->
              apply_manager_operation_content
                ctxt origination_nonce source content)
        (ctxt, origination_nonce, None) contents
      >>=? fun (ctxt, origination_nonce, err) ->
      return (ctxt, origination_nonce, err, fee, Tez.zero)
  | Consensus_operation content ->
      apply_consensus_operation_content ctxt
        pred_block block_prio operation content >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero, Tez.zero)
  | Amendment_operation { source ; operation = content } ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate operation >>=? fun () ->
      (* TODO, see how to extract the public key hash after this operation to
         pass it to apply_delegate_operation_content *)
      apply_amendment_operation_content ctxt source content >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero, Tez.zero)
  | Dictator_operation (Activate hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      activate ctxt hash >>= fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero, Tez.zero)
  | Dictator_operation (Activate_testchain hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      let expiration = (* in two days maximum... *)
        Time.add (Timestamp.current ctxt) (Int64.mul 48L 3600L) in
      fork_test_chain ctxt hash expiration >>= fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero, Tez.zero)

let apply_anonymous_operation ctxt delegate origination_nonce kind =
  match kind with
  | Seed_nonce_revelation { level ; nonce } ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun ctxt ->
      return (ctxt, origination_nonce,
              Tez.zero, Constants.seed_nonce_revelation_tip)
  | Faucet { id = manager ; _ } ->
      (* Free tez for all! *)
      if Compare.Int.(faucet_count ctxt < 5) then
        let ctxt = incr_faucet_count ctxt in
        Contract.originate ctxt
          origination_nonce
          ~manager ~delegate ~balance:Constants.faucet_credit ?script:None
          ~spendable:true ~delegatable:true >>=? fun (ctxt, _, origination_nonce) ->
        return (ctxt, origination_nonce, Tez.zero, Tez.zero)
      else
        fail Too_many_faucet

let apply_operation
    ctxt delegate pred_block block_prio operation =
  match operation.contents with
  | Anonymous_operations ops ->
      let origination_nonce = Contract.initial_origination_nonce operation.hash in
      fold_left_s
        (fun (ctxt, origination_nonce, fees, rewards) op ->
           apply_anonymous_operation ctxt delegate origination_nonce op
           >>=? fun (ctxt, origination_nonce, fee, reward) ->
           return (ctxt, origination_nonce,
                   fees >>? Tez.(+?) fee,
                   rewards >>? Tez.(+?) reward))
        (ctxt, origination_nonce, Ok Tez.zero, Ok Tez.zero) ops
      >>=? fun (ctxt, origination_nonce, fees, rewards) ->
      return (ctxt, Contract.originated_contracts origination_nonce, None,
              fees, rewards)
  | Sourced_operations op ->
      let origination_nonce = Contract.initial_origination_nonce operation.hash in
      apply_sourced_operation
        ctxt pred_block block_prio
        operation origination_nonce op >>=? fun (ctxt, origination_nonce, err,
                                                 fees, rewards) ->
      return (ctxt, Contract.originated_contracts origination_nonce, err,
              Ok fees, Ok rewards)

let may_start_new_cycle ctxt =
  Baking.dawn_of_a_new_cycle ctxt >>=? function
  | None -> return ctxt
  | Some last_cycle ->
      Seed.cycle_end ctxt last_cycle >>=? fun (ctxt, unrevealed) ->
      Roll.cycle_end ctxt last_cycle >>=? fun ctxt ->
      Delegate.cycle_end ctxt last_cycle unrevealed >>=? fun ctxt ->
      return ctxt

let begin_full_construction ctxt pred_timestamp protocol_data =
  Lwt.return
    (Block_header.parse_unsigned_protocol_data
       protocol_data) >>=? fun protocol_data ->
  Baking.check_baking_rights
    ctxt protocol_data pred_timestamp >>=? fun delegate_pk ->
  let delegate_pkh = Ed25519.Public_key.hash delegate_pk in
  Baking.freeze_baking_bond ctxt protocol_data delegate_pkh >>=? fun (ctxt, bond) ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, protocol_data, delegate_pk, bond)

let begin_partial_construction ctxt =
  let ctxt = Fitness.increase ctxt in
  return ctxt

let begin_application ctxt block_header pred_timestamp =
  let current_level = Alpha_context.Level.current ctxt in
  Baking.check_proof_of_work_stamp ctxt block_header >>=? fun () ->
  Baking.check_fitness_gap ctxt block_header >>=? fun () ->
  Baking.check_baking_rights
    ctxt block_header.protocol_data pred_timestamp >>=? fun delegate_pk ->
  Baking.check_signature block_header delegate_pk >>=? fun () ->
  let has_commitment =
    match block_header.protocol_data.seed_nonce_hash with
    | None -> false
    | Some _ -> true in
  fail_unless
    Compare.Bool.(has_commitment = current_level.expected_commitment)
    (Invalid_commitment
       { expected = current_level.expected_commitment }) >>=? fun () ->
  let delegate_pkh = Ed25519.Public_key.hash delegate_pk in
  Baking.freeze_baking_bond ctxt
    block_header.protocol_data delegate_pkh >>=? fun (ctxt, bond) ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, delegate_pk, bond)

let finalize_application ctxt protocol_data delegate bond fees rewards =
  (* end of level (from this point nothing should fail) *)
  Lwt.return Tez.(rewards +? Constants.baking_reward) >>=? fun rewards ->
  Delegate.freeze_fees ctxt delegate fees >>=? fun ctxt ->
  Delegate.freeze_rewards ctxt delegate rewards >>=? fun ctxt ->
  begin
    match protocol_data.Block_header.seed_nonce_hash with
    | None -> return ctxt
    | Some nonce_hash ->
        Nonce.record_hash ctxt
          { nonce_hash ; delegate ; bond ; rewards ; fees }
  end >>=? fun ctxt ->
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
      | Consensus_operation _, (Amendment_operation _ | Manager_operations _ | Dictator_operation _) -> -1
      | (Amendment_operation _ | Manager_operations _ | Dictator_operation _), Consensus_operation _ -> 1
      | Amendment_operation _, (Manager_operations _ | Dictator_operation _) -> -1
      | (Manager_operations _ | Dictator_operation _), Amendment_operation _ -> 1
      | Manager_operations _, Dictator_operation _ -> -1
      | Dictator_operation _, Manager_operations _ -> 1
      | Consensus_operation _, Consensus_operation _ -> 0
      | Amendment_operation _, Amendment_operation _ -> 0
      | Manager_operations op1, Manager_operations op2 ->
          (* Manager operations with smaller counter are pre-validated first. *)
          Int32.compare op1.counter op2.counter
      | Dictator_operation _, Dictator_operation _ -> 0
