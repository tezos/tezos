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
type error += Invalid_endorsement_level
type error += Invalid_commitment of { expected: bool }

type error += Invalid_double_endorsement_evidence (* `Permanent *)
type error += Inconsistent_double_endorsement_evidence
  of { delegate1: Signature.Public_key_hash.t ; delegate2: Signature.Public_key_hash.t } (* `Permanent *)
type error += Unrequired_double_endorsement_evidence (* `Branch*)
type error += Too_early_double_endorsement_evidence
  of { level: Raw_level.t ; current: Raw_level.t } (* `Temporary *)
type error += Outdated_double_endorsement_evidence
  of { level: Raw_level.t ; last: Raw_level.t } (* `Permanent *)

type error += Invalid_double_baking_evidence
  of { level1: Int32.t ; level2: Int32.t } (* `Permanent *)
type error += Inconsistent_double_baking_evidence
  of { delegate1: Signature.Public_key_hash.t ; delegate2: Signature.Public_key_hash.t } (* `Permanent *)
type error += Unrequired_double_baking_evidence (* `Branch*)
type error += Too_early_double_baking_evidence
  of { level: Raw_level.t ; current: Raw_level.t } (* `Temporary *)
type error += Outdated_double_baking_evidence
  of { level: Raw_level.t ; last: Raw_level.t } (* `Permanent *)
type error += Invalid_activation of { pkh : Ed25519.Public_key_hash.t }
type error += Wrong_activation_secret
type error += Multiple_revelation

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
    ~id:"operation.invalid_endorsement_level"
    ~title:"Unexpected level in endorsement"
    ~description:"The level of an endorsement is inconsistent with the \
                 \ provided block hash."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Unexpected level in endorsement.")
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
    (fun expected -> Invalid_commitment { expected }) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_double_endorsement_evidence"
    ~title:"Invalid double endorsement evidence"
    ~description:"A double-endorsement evidence is malformed"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Malformed double-endorsement evidence")
    Data_encoding.empty
    (function Invalid_double_endorsement_evidence -> Some () | _ -> None)
    (fun () -> Invalid_double_endorsement_evidence) ;
  register_error_kind
    `Permanent
    ~id:"block.inconsistent_double_endorsement_evidence"
    ~title:"Inconsistent double endorsement evidence"
    ~description:"A double-endorsement evidence is inconsistent \
                 \ (two distinct delegates)"
    ~pp:(fun ppf (delegate1, delegate2) ->
        Format.fprintf ppf
          "Inconsistent double-endorsement evidence \
          \ (distinct delegate: %a and %a)"
          Signature.Public_key_hash.pp_short delegate1
          Signature.Public_key_hash.pp_short delegate2)
    Data_encoding.(obj2
                     (req "delegate1" Signature.Public_key_hash.encoding)
                     (req "delegate2" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_double_endorsement_evidence { delegate1 ; delegate2 } ->
          Some (delegate1, delegate2)
      | _ -> None)
    (fun (delegate1, delegate2) ->
       Inconsistent_double_endorsement_evidence { delegate1 ; delegate2 }) ;
  register_error_kind
    `Branch
    ~id:"block.unrequired_double_endorsement_evidence"
    ~title:"Unrequired double endorsement evidence"
    ~description:"A double-endorsement evidence is unrequired"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "A valid double-endorsement operation cannot \
                           \ be applied: the associated delegate \
                           \ has previously been denunciated in this cycle.")
    Data_encoding.empty
    (function Unrequired_double_endorsement_evidence -> Some () | _ -> None)
    (fun () -> Unrequired_double_endorsement_evidence) ;
  register_error_kind
    `Temporary
    ~id:"block.too_early_double_endorsement_evidence"
    ~title:"Too early double endorsement evidence"
    ~description:"A double-endorsement evidence is in the future"
    ~pp:(fun ppf (level, current) ->
        Format.fprintf ppf
          "A double-endorsement evidence is in the future \
          \ (current level: %a, endorsement level: %a)"
          Raw_level.pp current
          Raw_level.pp level)
    Data_encoding.(obj2
                     (req "level" Raw_level.encoding)
                     (req "current" Raw_level.encoding))
    (function
      | Too_early_double_endorsement_evidence { level ; current } ->
          Some (level, current)
      | _ -> None)
    (fun (level, current) ->
       Too_early_double_endorsement_evidence { level ; current }) ;
  register_error_kind
    `Permanent
    ~id:"block.outdated_double_endorsement_evidence"
    ~title:"Outdated double endorsement evidence"
    ~description:"A double-endorsement evidence is outdated."
    ~pp:(fun ppf (level, last) ->
        Format.fprintf ppf
          "A double-endorsement evidence is outdated \
          \ (last acceptable level: %a, endorsement level: %a)"
          Raw_level.pp last
          Raw_level.pp level)
    Data_encoding.(obj2
                     (req "level" Raw_level.encoding)
                     (req "last" Raw_level.encoding))
    (function
      | Outdated_double_endorsement_evidence { level ; last } ->
          Some (level, last)
      | _ -> None)
    (fun (level, last) ->
       Outdated_double_endorsement_evidence { level ; last }) ;
  register_error_kind
    `Permanent
    ~id:"block.invalid_double_baking_evidence"
    ~title:"Invalid double baking evidence"
    ~description:"A double-baking evidence is inconsistent \
                 \ (two distinct level)"
    ~pp:(fun ppf (level1, level2) ->
        Format.fprintf ppf
          "Inconsistent double-baking evidence (levels: %ld and %ld)"
          level1 level2)
    Data_encoding.(obj2
                     (req "level1" int32)
                     (req "level2" int32))
    (function
      | Invalid_double_baking_evidence { level1 ; level2 } -> Some (level1, level2)
      | _ -> None)
    (fun (level1, level2) -> Invalid_double_baking_evidence { level1 ; level2 }) ;
  register_error_kind
    `Permanent
    ~id:"block.inconsistent_double_baking_evidence"
    ~title:"Inconsistent double baking evidence"
    ~description:"A double-baking evidence is inconsistent \
                 \ (two distinct delegates)"
    ~pp:(fun ppf (delegate1, delegate2) ->
        Format.fprintf ppf
          "Inconsistent double-baking evidence \
          \ (distinct delegate: %a and %a)"
          Signature.Public_key_hash.pp_short delegate1
          Signature.Public_key_hash.pp_short delegate2)
    Data_encoding.(obj2
                     (req "delegate1" Signature.Public_key_hash.encoding)
                     (req "delegate2" Signature.Public_key_hash.encoding))
    (function
      | Inconsistent_double_baking_evidence { delegate1 ; delegate2 } ->
          Some (delegate1, delegate2)
      | _ -> None)
    (fun (delegate1, delegate2) ->
       Inconsistent_double_baking_evidence { delegate1 ; delegate2 }) ;
  register_error_kind
    `Branch
    ~id:"block.unrequired_double_baking_evidence"
    ~title:"Unrequired double baking evidence"
    ~description:"A double-baking evidence is unrequired"
    ~pp:(fun ppf () ->
        Format.fprintf ppf "A valid double-baking operation cannot \
                           \ be applied: the associated delegate \
                           \ has previously been denunciated in this cycle.")
    Data_encoding.empty
    (function Unrequired_double_baking_evidence -> Some () | _ -> None)
    (fun () -> Unrequired_double_baking_evidence) ;
  register_error_kind
    `Temporary
    ~id:"block.too_early_double_baking_evidence"
    ~title:"Too early double baking evidence"
    ~description:"A double-baking evidence is in the future"
    ~pp:(fun ppf (level, current) ->
        Format.fprintf ppf
          "A double-baking evidence is in the future \
          \ (current level: %a, baking level: %a)"
          Raw_level.pp current
          Raw_level.pp level)
    Data_encoding.(obj2
                     (req "level" Raw_level.encoding)
                     (req "current" Raw_level.encoding))
    (function
      | Too_early_double_baking_evidence { level ; current } ->
          Some (level, current)
      | _ -> None)
    (fun (level, current) ->
       Too_early_double_baking_evidence { level ; current }) ;
  register_error_kind
    `Permanent
    ~id:"block.outdated_double_baking_evidence"
    ~title:"Outdated double baking evidence"
    ~description:"A double-baking evidence is outdated."
    ~pp:(fun ppf (level, last) ->
        Format.fprintf ppf
          "A double-baking evidence is outdated \
          \ (last acceptable level: %a, baking level: %a)"
          Raw_level.pp last
          Raw_level.pp level)
    Data_encoding.(obj2
                     (req "level" Raw_level.encoding)
                     (req "last" Raw_level.encoding))
    (function
      | Outdated_double_baking_evidence { level ; last } ->
          Some (level, last)
      | _ -> None)
    (fun (level, last) ->
       Outdated_double_baking_evidence { level ; last }) ;
  register_error_kind
    `Permanent
    ~id:"operation.invalid_activation"
    ~title:"Invalid activation"
    ~description:"The given key has already been activated or the given \
                  key does not correspond to any preallocated contract"
    ~pp:(fun ppf pkh ->
        Format.fprintf ppf "Invalid activation. The public key %a does \
                            not match any commitment."
          Ed25519.Public_key_hash.pp pkh
      )
    Data_encoding.(obj1 (req "pkh" Ed25519.Public_key_hash.encoding))
    (function Invalid_activation { pkh } -> Some pkh | _ -> None)
    (fun pkh -> Invalid_activation { pkh } ) ;
  register_error_kind
    `Permanent
    ~id:"operation.wrong_activation_secret"
    ~title:"Wrong activation secret"
    ~description:"The submitted activation key does not match the \
                  registered key."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Wrong activation secret.")
    Data_encoding.unit
    (function Wrong_activation_secret -> Some () | _ -> None)
    (fun () -> Wrong_activation_secret) ;
  register_error_kind
    `Permanent
    ~id:"block.multiple_revelation"
    ~title:"Multiple revelations were included in a manager operation"
    ~description:"A manager operation should not contain more than one revelation"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Multiple revelations were included in a manager operation")
    Data_encoding.empty
    (function Multiple_revelation -> Some () | _ -> None)
    (fun () -> Multiple_revelation)

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
      let delegate = Signature.Public_key.hash delegate in
      let ctxt = Fitness.increase ~gap:(List.length slots) ctxt in
      Baking.freeze_endorsement_deposit ctxt delegate >>=? fun ctxt ->
      Baking.endorsement_reward ctxt ~block_priority >>=? fun reward ->
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
  | Reveal _ -> return (ctxt, origination_nonce, None, Tez.zero)
  | Transaction { amount ; parameters ; destination ; gas_limit } ->
      Lwt.return (Gas.set_limit ctxt gas_limit) >>=? fun ctxt ->
      begin
        Contract.spend ctxt source amount >>=? fun ctxt ->
        Contract.credit ctxt destination amount >>=? fun ctxt ->
        Contract.get_script ctxt destination >>=? fun (ctxt, script) -> match script with
        | None -> begin
            match parameters with
            | None ->
                return (ctxt, origination_nonce, None, Tez.zero)
            | Some arg ->
                match Micheline.root arg with
                | Prim (_, D_Unit, [], _) ->
                    return (ctxt, origination_nonce, None, Tez.zero)
                | _ -> fail (Bad_contract_parameter (destination, None, parameters))
          end
        | Some script ->
            let call_contract ctxt parameter =
              Script_interpreter.execute
                ctxt origination_nonce
                ~source ~self:(destination, script) ~amount ~parameter
              >>= function
              | Ok { ctxt ; origination_nonce ; storage ; big_map_diff ; return_value = _ } ->
                  Contract.update_script_storage
                    ctxt destination storage big_map_diff >>=? fun ctxt ->
                  Fees.update_script_storage
                    ctxt ~source destination >>=? fun (ctxt, fees) ->
                  return (ctxt, origination_nonce, None, fees)
              | Error err ->
                  return (ctxt, origination_nonce, Some err, Tez.zero) in
            Lwt.return @@ Script_ir_translator.parse_toplevel script.code >>=? fun (arg_type, _, _, _) ->
            let arg_type = Micheline.strip_locations arg_type in
            match parameters, Micheline.root arg_type with
            | None, Prim (_, T_unit, _, _) ->
                call_contract ctxt (Micheline.strip_locations (Prim (0, Script.D_Unit, [], None)))
            | Some parameters, _ -> begin
                Script_ir_translator.typecheck_data ctxt (parameters, arg_type) >>= function
                | Ok ctxt -> call_contract ctxt parameters
                | Error errs ->
                    let err = Bad_contract_parameter (destination, Some arg_type, Some parameters) in
                    return (ctxt, origination_nonce, Some ((err :: errs)), Tez.zero)
              end
            | None, _ -> fail (Bad_contract_parameter (destination, Some arg_type, None))
      end
  | Origination { manager ; delegate ; script ;
                  spendable ; delegatable ; credit ; gas_limit } ->
      Lwt.return (Gas.set_limit ctxt gas_limit) >>=? fun ctxt ->
      begin match script with
        | None -> return (None, ctxt)
        | Some script ->
            Script_ir_translator.parse_script ctxt script >>=? fun (_, ctxt) ->
            Script_ir_translator.erase_big_map_initialization ctxt script >>=? fun (script, big_map_diff, ctxt) ->
            return (Some (script, big_map_diff), ctxt)
      end >>=? fun (script, ctxt) ->
      Contract.spend ctxt source credit >>=? fun ctxt ->
      Contract.originate ctxt
        origination_nonce
        ~manager ~delegate ~balance:credit
        ?script
        ~spendable ~delegatable >>=? fun (ctxt, contract, origination_nonce) ->
      Fees.origination_burn ctxt ~source contract >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)
  | Delegation delegate ->
      Delegate.set ctxt source delegate >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)

let apply_sourced_operation
    ctxt pred_block block_prio
    operation origination_nonce ops =
  match ops with
  | Manager_operations { source ; fee ; counter ; operations = contents } ->
      let revealed_public_keys =
        List.fold_left (fun acc op ->
            match op with
            | Reveal pk -> pk :: acc
            | _ -> acc) [] contents in
      Contract.must_be_allocated ctxt source >>=? fun () ->
      Contract.check_counter_increment ctxt source counter >>=? fun () ->
      begin
        match revealed_public_keys with
        | [] -> return ctxt
        | [pk] ->
            Contract.reveal_manager_key ctxt source pk
        | _ :: _ :: _ ->
            fail Multiple_revelation
      end >>=? fun ctxt ->
      Contract.get_manager_key ctxt source >>=? fun public_key ->
      Operation.check_signature public_key operation >>=? fun () ->
      Contract.increment_counter ctxt source >>=? fun ctxt ->
      Contract.spend ctxt source fee >>=? fun ctxt ->
      add_fees ctxt fee >>=? fun ctxt ->
      fold_left_s (fun (ctxt, origination_nonce, err, storage_fees) content ->
          match err with
          | Some _ -> return (ctxt, origination_nonce, err, Tez.zero)
          | None ->
              Contract.must_exist ctxt source >>=? fun () ->
              apply_manager_operation_content
                ctxt origination_nonce source content
              >>=? fun (ctxt, origination_nonce, err, operation_storage_fees)  ->
              Lwt.return Tez.(storage_fees +? operation_storage_fees) >>=? fun storage_fees ->
              return (ctxt, origination_nonce, err, storage_fees))
        (ctxt, origination_nonce, None, Tez.zero) contents
      >>=? fun (ctxt, origination_nonce, err,storage_fees) ->
      return (ctxt, origination_nonce, err, storage_fees)
  | Consensus_operation content ->
      apply_consensus_operation_content ctxt
        pred_block block_prio operation content >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)
  | Amendment_operation { source ; operation = content } ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate operation >>=? fun () ->
      (* TODO, see how to extract the public key hash after this operation to
         pass it to apply_delegate_operation_content *)
      apply_amendment_operation_content ctxt source content >>=? fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)
  | Dictator_operation (Activate hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      activate ctxt hash >>= fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)
  | Dictator_operation (Activate_testchain hash) ->
      let dictator_pubkey = Constants.dictator_pubkey ctxt in
      Operation.check_signature dictator_pubkey operation >>=? fun () ->
      let expiration = (* in two days maximum... *)
        Time.add (Timestamp.current ctxt) (Int64.mul 48L 3600L) in
      fork_test_chain ctxt hash expiration >>= fun ctxt ->
      return (ctxt, origination_nonce, None, Tez.zero)

let apply_anonymous_operation ctxt _delegate origination_nonce kind =
  match kind with
  | Seed_nonce_revelation { level ; nonce } ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun ctxt ->
      let seed_nonce_revelation_tip =
        Constants.seed_nonce_revelation_tip ctxt in
      add_rewards ctxt seed_nonce_revelation_tip >>=? fun ctxt ->
      return (ctxt, origination_nonce)
  | Double_endorsement_evidence { op1 ; op2 } -> begin
      match op1.contents, op2.contents with
      | Sourced_operations (Consensus_operation (Endorsements e1)),
        Sourced_operations (Consensus_operation (Endorsements e2))
        when Raw_level.(e1.level = e2.level) &&
             not (Block_hash.equal e1.block e2.block) ->
          let level = Level.from_raw ctxt e1.level in
          let oldest_level = Level.last_allowed_fork_level ctxt in
          fail_unless Level.(level < Level.current ctxt)
            (Too_early_double_endorsement_evidence
               { level = level.level ;
                 current = (Level.current ctxt).level }) >>=? fun () ->
          fail_unless Raw_level.(oldest_level <= level.level)
            (Outdated_double_endorsement_evidence
               { level = level.level ;
                 last = oldest_level }) >>=? fun () ->
          (* Whenever a delegate might have multiple endorsement slots for
             given level, she should not endorse different block with different
             slots. Hence, we don't check that [e1.slots] and [e2.slots]
             intersect. *)
          Baking.check_endorsements_rights ctxt level e1.slots >>=? fun delegate1 ->
          Operation.check_signature delegate1 op1 >>=? fun () ->
          Baking.check_endorsements_rights ctxt level e2.slots >>=? fun delegate2 ->
          Operation.check_signature delegate2 op2 >>=? fun () ->
          fail_unless
            (Signature.Public_key.equal delegate1 delegate2)
            (Inconsistent_double_endorsement_evidence
               { delegate1 = Signature.Public_key.hash delegate1 ;
                 delegate2 = Signature.Public_key.hash delegate2 }) >>=? fun () ->
          let delegate = Signature.Public_key.hash delegate1 in
          Delegate.has_frozen_balance ctxt delegate level.cycle >>=? fun valid ->
          fail_unless valid Unrequired_double_endorsement_evidence >>=? fun () ->
          Delegate.punish ctxt delegate level.cycle >>=? fun (ctxt, burned) ->
          let reward =
            match Tez.(burned /? 2L) with
            | Ok v -> v
            | Error _ -> Tez.zero in
          add_rewards ctxt reward >>=? fun ctxt ->
          return (ctxt, origination_nonce)
      | _, _ -> fail Invalid_double_endorsement_evidence
    end
  | Double_baking_evidence { bh1 ; bh2 } ->
      fail_unless Compare.Int32.(bh1.shell.level = bh2.shell.level)
        (Invalid_double_baking_evidence
           { level1 = bh1.shell.level ;
             level2 = bh2.shell.level }) >>=? fun () ->
      Lwt.return (Raw_level.of_int32 bh1.shell.level) >>=? fun raw_level ->
      let oldest_level = Level.last_allowed_fork_level ctxt in
      fail_unless Raw_level.(raw_level < (Level.current ctxt).level)
        (Too_early_double_baking_evidence
           { level = raw_level ;
             current = (Level.current ctxt).level }) >>=? fun () ->
      fail_unless Raw_level.(oldest_level <= raw_level)
        (Outdated_double_baking_evidence
           { level = raw_level ;
             last = oldest_level }) >>=? fun () ->
      let level = Level.from_raw ctxt raw_level in
      Roll.baking_rights_owner
        ctxt level ~priority:bh1.protocol_data.priority >>=? fun delegate1 ->
      Baking.check_signature bh1 delegate1 >>=? fun () ->
      Roll.baking_rights_owner
        ctxt level ~priority:bh2.protocol_data.priority >>=? fun delegate2 ->
      Baking.check_signature bh2 delegate2 >>=? fun () ->
      fail_unless
        (Signature.Public_key.equal delegate1 delegate2)
        (Inconsistent_double_baking_evidence
           { delegate1 = Signature.Public_key.hash delegate1 ;
             delegate2 = Signature.Public_key.hash delegate2 }) >>=? fun () ->
      let delegate = Signature.Public_key.hash delegate1 in
      Delegate.has_frozen_balance ctxt delegate level.cycle >>=? fun valid ->
      fail_unless valid Unrequired_double_baking_evidence >>=? fun () ->
      Delegate.punish ctxt delegate level.cycle >>=? fun (ctxt, burned) ->
      let reward =
        match Tez.(burned /? 2L) with
        | Ok v -> v
        | Error _ -> Tez.zero in
      add_rewards ctxt reward >>=? fun ctxt ->
      return (ctxt, origination_nonce)
  | Activation { id = pkh ; secret } ->
      let h_pkh = Unclaimed_public_key_hash.of_ed25519_pkh pkh in
      Commitment.get_opt ctxt h_pkh >>=? function
      | None -> fail (Invalid_activation { pkh })
      | Some { blinded_public_key_hash = blinded_pkh ; amount } ->
          let submitted_bpkh = Blinded_public_key_hash.of_ed25519_pkh secret pkh in
          fail_unless
            Blinded_public_key_hash.(blinded_pkh = submitted_bpkh)
            Wrong_activation_secret >>=? fun () ->
          Commitment.delete ctxt h_pkh >>=? fun ctxt ->
          Contract.(credit ctxt (implicit_contract (Signature.Ed25519 pkh)) amount) >>=? fun ctxt ->
          return (ctxt, origination_nonce)

type operation_result =
  { ctxt : context ;
    gas : Gas.t ;
    origination_nonce : Contract.origination_nonce ;
    ignored_error : error list option ;
    fees : Tez.t ;
    rewards : Tez.t ;
    storage_fees : Tez.t }

let apply_operation
    ctxt delegate pred_block block_prio hash operation =
  begin match operation.contents with
    | Anonymous_operations ops ->
        let origination_nonce = Contract.initial_origination_nonce hash in
        fold_left_s
          (fun (ctxt, origination_nonce) op ->
             apply_anonymous_operation ctxt delegate origination_nonce op)
          (ctxt, origination_nonce) ops
        >>=? fun (ctxt, origination_nonce) ->
        return (ctxt, origination_nonce, None, Tez.zero)
    | Sourced_operations op ->
        let origination_nonce = Contract.initial_origination_nonce hash in
        apply_sourced_operation
          ctxt pred_block block_prio
          operation origination_nonce op
  end >>=? fun (ctxt, origination_nonce, ignored_error, storage_fees) ->
  let gas = Gas.level ctxt in
  let ctxt = Gas.set_unlimited ctxt in
  return { ctxt ; gas ; origination_nonce ; ignored_error ; storage_fees ;
           fees = Alpha_context.get_fees ctxt ;
           rewards = Alpha_context.get_rewards ctxt }

let may_snapshot_roll ctxt =
  let level = Alpha_context.Level.current ctxt in
  let blocks_per_roll_snapshot = Constants.blocks_per_roll_snapshot ctxt in
  if Compare.Int32.equal
      (Int32.rem level.cycle_position blocks_per_roll_snapshot)
      (Int32.pred blocks_per_roll_snapshot)
  then
    Alpha_context.Roll.snapshot_rolls ctxt >>=? fun ctxt ->
    return ctxt
  else
    return ctxt

let may_start_new_cycle ctxt =
  Baking.dawn_of_a_new_cycle ctxt >>=? function
  | None -> return ctxt
  | Some last_cycle ->
      Seed.cycle_end ctxt last_cycle >>=? fun (ctxt, unrevealed) ->
      Roll.cycle_end ctxt last_cycle >>=? fun ctxt ->
      Delegate.cycle_end ctxt last_cycle unrevealed >>=? fun ctxt ->
      Bootstrap.cycle_end ctxt last_cycle >>=? fun ctxt ->
      return ctxt

let begin_full_construction ctxt pred_timestamp protocol_data =
  Lwt.return
    (Block_header.parse_unsigned_protocol_data
       protocol_data) >>=? fun protocol_data ->
  Baking.check_baking_rights
    ctxt protocol_data pred_timestamp >>=? fun delegate_pk ->
  let delegate_pkh = Signature.Public_key.hash delegate_pk in
  Baking.freeze_baking_deposit ctxt protocol_data delegate_pkh >>=? fun (ctxt, deposit) ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, protocol_data, delegate_pk, deposit)

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
  let delegate_pkh = Signature.Public_key.hash delegate_pk in
  Baking.freeze_baking_deposit ctxt
    block_header.protocol_data delegate_pkh >>=? fun (ctxt, deposit) ->
  let ctxt = Fitness.increase ctxt in
  return (ctxt, delegate_pk, deposit)

let finalize_application ctxt protocol_data delegate =
  let block_reward = Constants.block_reward ctxt in
  add_rewards ctxt block_reward >>=? fun ctxt ->
  (* end of level (from this point nothing should fail) *)
  let fees = Alpha_context.get_fees ctxt in
  Delegate.freeze_fees ctxt delegate fees >>=? fun ctxt ->
  let rewards = Alpha_context.get_rewards ctxt in
  Delegate.freeze_rewards ctxt delegate rewards >>=? fun ctxt ->
  begin
    match protocol_data.Block_header.seed_nonce_hash with
    | None -> return ctxt
    | Some nonce_hash ->
        Nonce.record_hash ctxt
          { nonce_hash ; delegate ; rewards ; fees }
  end >>=? fun ctxt ->
  (* end of cycle *)
  may_snapshot_roll ctxt >>=? fun ctxt ->
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
