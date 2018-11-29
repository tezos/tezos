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

(** Tezos Protocol Implementation - Main Entry Points *)

open Alpha_context

type error += Wrong_voting_period of Voting_period.t * Voting_period.t (* `Temporary *)
type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t (* `Temporary *)
type error += Duplicate_endorsement of Signature.Public_key_hash.t (* `Branch *)
type error += Invalid_endorsement_level
type error += Invalid_commitment of { expected: bool }
type error += Internal_operation_replay of packed_internal_operation

type error += Invalid_double_endorsement_evidence (* `Permanent *)
type error += Inconsistent_double_endorsement_evidence
  of { delegate1: Signature.Public_key_hash.t ; delegate2: Signature.Public_key_hash.t } (* `Permanent *)
type error += Unrequired_double_endorsement_evidence (* `Branch*)
type error += Too_early_double_endorsement_evidence
  of { level: Raw_level.t ; current: Raw_level.t } (* `Temporary *)
type error += Outdated_double_endorsement_evidence
  of { level: Raw_level.t ; last: Raw_level.t } (* `Permanent *)

type error += Invalid_double_baking_evidence
  of { hash1: Block_hash.t ;
       level1: Int32.t ;
       hash2: Block_hash.t ;
       level2: Int32.t } (* `Permanent *)
type error += Inconsistent_double_baking_evidence
  of { delegate1: Signature.Public_key_hash.t ; delegate2: Signature.Public_key_hash.t } (* `Permanent *)
type error += Unrequired_double_baking_evidence (* `Branch*)
type error += Too_early_double_baking_evidence
  of { level: Raw_level.t ; current: Raw_level.t } (* `Temporary *)
type error += Outdated_double_baking_evidence
  of { level: Raw_level.t ; last: Raw_level.t } (* `Permanent *)
type error += Invalid_activation of { pkh : Ed25519.Public_key_hash.t }
type error += Multiple_revelation
type error += Gas_quota_exceeded_init_deserialize (* Permanent *)

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
    `Branch
    ~id:"operation.duplicate_endorsement"
    ~title:"Duplicate endorsement"
    ~description:"Two endorsements received from same delegate"
    ~pp:(fun ppf k ->
        Format.fprintf ppf "Duplicate endorsement from delegate %a (possible replay attack)."
          Signature.Public_key_hash.pp_short k)
    Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
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
    Data_encoding.(obj1 (req "expected" bool))
    (function Invalid_commitment { expected } -> Some expected | _ -> None)
    (fun expected -> Invalid_commitment { expected }) ;
  register_error_kind
    `Permanent
    ~id:"internal_operation_replay"
    ~title:"Internal operation replay"
    ~description:"An internal operation was emitted twice by a script"
    ~pp:(fun ppf (Internal_operation { nonce ; _ }) ->
        Format.fprintf ppf "Internal operation %d was emitted twice by a script" nonce)
    Operation.internal_operation_encoding
    (function Internal_operation_replay op -> Some op | _ -> None)
    (fun op -> Internal_operation_replay op) ;
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
    ~pp:(fun ppf (hash1, level1, hash2, level2) ->
        Format.fprintf ppf
          "Invalid double-baking evidence (hash: %a and %a, levels: %ld and %ld)"
          Block_hash.pp hash1 Block_hash.pp hash2
          level1 level2)
    Data_encoding.(obj4
                     (req "hash1" Block_hash.encoding)
                     (req "level1" int32)
                     (req "hash2" Block_hash.encoding)
                     (req "level2" int32))
    (function
      | Invalid_double_baking_evidence { hash1 ; level1 ; hash2 ; level2 } ->
          Some (hash1, level1, hash2, level2)
      | _ -> None)
    (fun (hash1, level1, hash2, level2) ->
       Invalid_double_baking_evidence { hash1 ; level1 ; hash2 ; level2 }) ;
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
    ~description:"The given key and secret do not correspond to any \
                  existing preallocated contract"
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
    ~id:"block.multiple_revelation"
    ~title:"Multiple revelations were included in a manager operation"
    ~description:"A manager operation should not contain more than one revelation"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Multiple revelations were included in a manager operation")
    Data_encoding.empty
    (function Multiple_revelation -> Some () | _ -> None)
    (fun () -> Multiple_revelation) ;
  register_error_kind
    `Permanent
    ~id:"gas_exhausted.init_deserialize"
    ~title:"Not enough gas for initial deserialization of script expresions"
    ~description:"Gas limit was not high enough to deserialize the \
                  transaction parameters or origination script code or \
                  initial storage, making the operation impossible to \
                  parse within the provided gas bounds."
    Data_encoding.empty
    (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
    (fun () -> Gas_quota_exceeded_init_deserialize)

open Apply_results

let apply_manager_operation_content :
  type kind.
  ( Alpha_context.t -> Script_ir_translator.unparsing_mode -> payer:Contract.t -> source:Contract.t ->
    internal:bool -> kind manager_operation ->
    (context * kind successful_manager_operation_result * packed_internal_operation list) tzresult Lwt.t ) =
  fun ctxt mode ~payer ~source ~internal operation ->
    let before_operation =
      (* This context is not used for backtracking. Only to compute
         gas consumption and originations for the operation result. *)
      ctxt in
    Contract.must_exist ctxt source >>=? fun () ->
    let spend =
      (* Ignore the spendable flag for smart contracts. *)
      if internal then Contract.spend_from_script else Contract.spend in
    let set_delegate =
      (* Ignore the delegatable flag for smart contracts. *)
      if internal then Delegate.set_from_script else Delegate.set in
    Lwt.return (Gas.consume ctxt Michelson_v1_gas.Cost_of.manager_operation) >>=? fun ctxt ->
    match operation with
    | Reveal _ ->
        return (* No-op: action already performed by `precheck_manager_contents`. *)
          (ctxt, (Reveal_result { consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt } : kind successful_manager_operation_result), [])
    | Transaction { amount ; parameters ; destination } -> begin
        spend ctxt source amount >>=? fun ctxt ->
        begin match Contract.is_implicit destination with
          | None -> return (ctxt, [], false)
          | Some _ ->
              Contract.allocated ctxt destination >>=? function
              | true -> return (ctxt, [], false)
              | false ->
                  Fees.origination_burn ctxt >>=? fun (ctxt, origination_burn) ->
                  return (ctxt, [ Delegate.Contract payer, Delegate.Debited origination_burn ], true)
        end >>=? fun (ctxt, maybe_burn_balance_update, allocated_destination_contract) ->
        Contract.credit ctxt destination amount >>=? fun ctxt ->
        Contract.get_script ctxt destination >>=? fun (ctxt, script) ->
        match script with
        | None -> begin
            match parameters with
            | None -> return ctxt
            | Some arg ->
                Script.force_decode ctxt arg >>=? fun (arg, ctxt) -> (* see [note] *)
                (* [note]: for toplevel ops, cost is nil since the
                   lazy value has already been forced at precheck, so
                   we compute and consume the full cost again *)
                let cost_arg = Script.deserialized_cost arg in
                Lwt.return (Gas.consume ctxt cost_arg) >>=? fun ctxt ->
                match Micheline.root arg with
                | Prim (_, D_Unit, [], _) ->
                    (* Allow [Unit] parameter to non-scripted contracts. *)
                    return ctxt
                | _ -> fail (Script_interpreter.Bad_contract_parameter destination)
          end >>=? fun ctxt ->
            let result =
              Transaction_result
                { storage = None ;
                  big_map_diff = None;
                  balance_updates =
                    Delegate.cleanup_balance_updates
                      ([ Delegate.Contract source, Delegate.Debited amount ;
                         Contract destination, Credited amount ]
                       @ maybe_burn_balance_update) ;
                  originated_contracts = [] ;
                  consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt ;
                  storage_size = Z.zero ;
                  paid_storage_size_diff = Z.zero ;
                  allocated_destination_contract ;
                } in
            return (ctxt, result, [])
        | Some script ->
            begin match parameters with
              | None ->
                  (* Forge a [Unit] parameter that will be checked by [execute]. *)
                  let unit = Micheline.strip_locations (Prim (0, Script.D_Unit, [], [])) in
                  return (ctxt, unit)
              | Some parameters ->
                  Script.force_decode ctxt parameters >>=? fun (arg, ctxt) -> (* see [note] *)
                  let cost_arg = Script.deserialized_cost arg in
                  Lwt.return (Gas.consume ctxt cost_arg) >>=? fun ctxt ->
                  return (ctxt, arg)
            end >>=? fun (ctxt, parameter) ->
            Script_interpreter.execute
              ctxt mode
              ~source ~payer ~self:(destination, script) ~amount ~parameter
            >>=? fun { ctxt ; storage ; big_map_diff ; operations } ->
            Contract.update_script_storage
              ctxt destination storage big_map_diff >>=? fun ctxt ->
            Fees.record_paid_storage_space
              ctxt destination >>=? fun (ctxt, new_size, paid_storage_size_diff, fees) ->
            Contract.originated_from_current_nonce
              ~since: before_operation
              ~until: ctxt >>=? fun originated_contracts ->
            let result =
              Transaction_result
                { storage = Some storage ;
                  big_map_diff;
                  balance_updates =
                    Delegate.cleanup_balance_updates
                      [ Contract payer, Debited fees ;
                        Contract source, Debited amount ;
                        Contract destination, Credited amount ] ;
                  originated_contracts ;
                  consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt ;
                  storage_size = new_size ;
                  paid_storage_size_diff ;
                  allocated_destination_contract } in
            return (ctxt, result, operations)
      end
    | Origination { manager ; delegate ; script ; preorigination ;
                    spendable ; delegatable ; credit } ->
        begin match script with
          | None -> return (None, ctxt)
          | Some script ->
              Script.force_decode ctxt script.storage >>=? fun (unparsed_storage, ctxt) -> (* see [note] *)
              Lwt.return (Gas.consume ctxt (Script.deserialized_cost unparsed_storage)) >>=? fun ctxt ->
              Script.force_decode ctxt script.code >>=? fun (unparsed_code, ctxt) -> (* see [note] *)
              Lwt.return (Gas.consume ctxt (Script.deserialized_cost unparsed_code)) >>=? fun ctxt ->
              Script_ir_translator.parse_script ctxt script >>=? fun (_, ctxt) ->
              Script_ir_translator.erase_big_map_initialization ctxt Optimized script >>=? fun (script, big_map_diff, ctxt) ->
              return (Some (script, big_map_diff), ctxt)
        end >>=? fun (script, ctxt) ->
        spend ctxt source credit >>=? fun ctxt ->
        begin match preorigination with
          | Some contract ->
              assert internal ;
              (* The preorigination field is only used to early return
                 the address of an originated contract in Michelson.
                 It cannot come from the outside. *)
              return (ctxt, contract)
          | None ->
              Contract.fresh_contract_from_current_nonce ctxt
        end >>=? fun (ctxt, contract) ->
        Contract.originate ctxt contract
          ~manager ~delegate ~balance:credit
          ?script
          ~spendable ~delegatable >>=? fun ctxt ->
        Fees.origination_burn ctxt >>=? fun (ctxt, origination_burn) ->
        Fees.record_paid_storage_space ctxt contract >>=? fun (ctxt, size, paid_storage_size_diff, fees) ->
        let result =
          Origination_result
            { balance_updates =
                Delegate.cleanup_balance_updates
                  [ Contract payer, Debited fees ;
                    Contract payer, Debited origination_burn ;
                    Contract source, Debited credit ;
                    Contract contract, Credited credit ] ;
              originated_contracts = [ contract ] ;
              consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt ;
              storage_size = size ;
              paid_storage_size_diff } in
        return (ctxt, result, [])
    | Delegation delegate ->
        set_delegate ctxt source delegate >>=? fun ctxt ->
        return (ctxt, Delegation_result { consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt }, [])

let apply_internal_manager_operations ctxt mode ~payer ops =
  let rec apply ctxt applied worklist =
    match worklist with
    | [] -> Lwt.return (`Success ctxt, List.rev applied)
    | (Internal_operation
         ({ source ; operation ; nonce } as op)) :: rest ->
        begin
          if internal_nonce_already_recorded ctxt nonce then
            fail (Internal_operation_replay (Internal_operation op))
          else
            let ctxt = record_internal_nonce ctxt nonce in
            apply_manager_operation_content
              ctxt mode ~source ~payer ~internal:true operation
        end >>= function
        | Error errors ->
            let result =
              Internal_operation_result (op, Failed (manager_kind op.operation, errors)) in
            let skipped =
              List.rev_map
                (fun (Internal_operation op) ->
                   Internal_operation_result (op, Skipped (manager_kind op.operation)))
                rest in
            Lwt.return (`Failure, List.rev (skipped @ (result :: applied)))
        | Ok (ctxt, result, emitted) ->
            apply ctxt
              (Internal_operation_result (op, Applied result) :: applied)
              (rest @ emitted) in
  apply ctxt [] ops

let precheck_manager_contents
    (type kind) ctxt chain_id raw_operation (op : kind Kind.manager contents)
  : context tzresult Lwt.t =
  let Manager_operation { source ; fee ; counter ; operation ; gas_limit ; storage_limit } = op in
  Lwt.return (Gas.check_limit ctxt gas_limit) >>=? fun () ->
  let ctxt = Gas.set_limit ctxt gas_limit in
  Lwt.return (Fees.check_storage_limit ctxt storage_limit) >>=? fun () ->
  Contract.must_be_allocated ctxt source >>=? fun () ->
  Contract.check_counter_increment ctxt source counter >>=? fun () ->
  begin
    match operation with
    | Reveal pk ->
        Contract.reveal_manager_key ctxt source pk
    | Transaction { parameters = Some arg ; _ } ->
        (* Fail quickly if not enough gas for minimal deserialization cost *)
        Lwt.return @@ record_trace Gas_quota_exceeded_init_deserialize @@
        Gas.check_enough ctxt (Script.minimal_deserialize_cost arg) >>=? fun () ->
        (* Fail if not enough gas for complete deserialization cost *)
        trace Gas_quota_exceeded_init_deserialize @@
        Script.force_decode ctxt arg >>|? fun (_arg, ctxt) -> ctxt
    | Origination { script = Some script ; _ } ->
        (* Fail quickly if not enough gas for minimal deserialization cost *)
        Lwt.return @@ record_trace Gas_quota_exceeded_init_deserialize @@
        (Gas.consume ctxt (Script.minimal_deserialize_cost script.code) >>? fun ctxt ->
         Gas.check_enough ctxt (Script.minimal_deserialize_cost script.storage)) >>=? fun () ->
        (* Fail if not enough gas for complete deserialization cost *)
        trace Gas_quota_exceeded_init_deserialize @@
        Script.force_decode ctxt script.code >>=? fun (_code, ctxt) ->
        trace Gas_quota_exceeded_init_deserialize @@
        Script.force_decode ctxt script.storage >>|? fun (_storage, ctxt) ->
        ctxt
    | _ -> return ctxt
  end >>=? fun ctxt ->
  Contract.get_manager_key ctxt source >>=? fun public_key ->
  (* Currently, the `raw_operation` only contains one signature, so
     all operations are required to be from the same manager. This may
     change in the future, allowing several managers to group-sign a
     sequence of transactions.  *)
  Operation.check_signature public_key chain_id raw_operation >>=? fun () ->
  Contract.increment_counter ctxt source >>=? fun ctxt ->
  Contract.spend ctxt source fee >>=? fun ctxt ->
  add_fees ctxt fee >>=? fun ctxt ->
  return ctxt

let apply_manager_contents
    (type kind) ctxt mode (op : kind Kind.manager contents)
  : ([ `Success of context | `Failure ] *
     kind manager_operation_result *
     packed_internal_operation_result list) Lwt.t =
  let Manager_operation
      { source ; operation ; gas_limit ; storage_limit } = op in
  let ctxt = Gas.set_limit ctxt gas_limit in
  let ctxt = Fees.start_counting_storage_fees ctxt in
  apply_manager_operation_content ctxt mode
    ~source ~payer:source ~internal:false operation >>= function
  | Ok (ctxt, operation_results, internal_operations) -> begin
      apply_internal_manager_operations
        ctxt mode ~payer:source internal_operations >>= function
      | (`Success ctxt, internal_operations_results) -> begin
          Fees.burn_storage_fees ctxt ~storage_limit ~payer:source >>= function
          | Ok ctxt ->
              Lwt.return
                (`Success ctxt, Applied operation_results, internal_operations_results)
          | Error errors ->
              Lwt.return
                (`Failure, Backtracked (operation_results, Some errors), internal_operations_results)
        end
      | (`Failure, internal_operations_results) ->
          Lwt.return
            (`Failure, Applied operation_results, internal_operations_results)
    end
  | Error errors ->
      Lwt.return
        (`Failure, Failed (manager_kind operation, errors), [])

let skipped_operation_result
  : type kind. kind manager_operation -> kind manager_operation_result
  = function operation ->
  match operation with
  | Reveal _ ->
      Applied ( Reveal_result { consumed_gas = Z.zero } : kind successful_manager_operation_result )
  | _ -> Skipped (manager_kind operation)

let rec mark_skipped
  : type kind.
    baker : Signature.Public_key_hash.t -> Level.t -> kind Kind.manager contents_list ->
  kind Kind.manager contents_result_list = fun ~baker level -> function
  | Single (Manager_operation { source ; fee ; operation } ) ->
      Single_result
        (Manager_operation_result
           { balance_updates =
               Delegate.cleanup_balance_updates
                 [ Contract source, Debited fee ;
                   Fees (baker, level.cycle), Credited fee ] ;
             operation_result = skipped_operation_result operation ;
             internal_operation_results = [] })
  | Cons (Manager_operation { source ; fee ; operation } , rest) ->
      Cons_result
        (Manager_operation_result {
            balance_updates =
              Delegate.cleanup_balance_updates
                [ Contract source, Debited fee ;
                  Fees (baker, level.cycle), Credited fee ] ;
            operation_result = skipped_operation_result operation ;
            internal_operation_results = [] },
         mark_skipped ~baker level rest)

let rec precheck_manager_contents_list
  : type kind.
    Alpha_context.t -> Chain_id.t -> _ Operation.t -> kind Kind.manager contents_list ->
    context tzresult Lwt.t =
  fun ctxt chain_id raw_operation contents_list ->
    match contents_list with
    | Single (Manager_operation _ as op) ->
        precheck_manager_contents ctxt chain_id raw_operation op
    | Cons (Manager_operation _ as op, rest) ->
        precheck_manager_contents ctxt chain_id raw_operation op >>=? fun ctxt ->
        precheck_manager_contents_list ctxt chain_id raw_operation rest

let rec apply_manager_contents_list_rec
  : type kind.
    Alpha_context.t -> Script_ir_translator.unparsing_mode ->
    public_key_hash -> kind Kind.manager contents_list ->
    ([ `Success of context | `Failure ] *
     kind Kind.manager contents_result_list) Lwt.t =
  fun ctxt mode baker contents_list ->
    let level = Level.current ctxt in
    match contents_list with
    | Single (Manager_operation { source ; fee ; _ } as op) -> begin
        apply_manager_contents ctxt mode op
        >>= fun (ctxt_result, operation_result, internal_operation_results) ->
        let result =
          Manager_operation_result {
            balance_updates =
              Delegate.cleanup_balance_updates
                [ Contract source, Debited fee ;
                  Fees (baker, level.cycle), Credited fee ] ;
            operation_result ;
            internal_operation_results ;
          } in
        Lwt.return (ctxt_result, Single_result (result))
      end
    | Cons (Manager_operation { source ; fee ; _ } as op, rest) ->
        apply_manager_contents ctxt mode op >>= function
        | (`Failure, operation_result, internal_operation_results) ->
            let result =
              Manager_operation_result {
                balance_updates =
                  Delegate.cleanup_balance_updates
                    [ Contract source, Debited fee ;
                      Fees (baker, level.cycle), Credited fee ] ;
                operation_result ;
                internal_operation_results ;
              } in
            Lwt.return (`Failure, Cons_result (result, mark_skipped ~baker level rest))
        | (`Success ctxt, operation_result, internal_operation_results) ->
            let result =
              Manager_operation_result {
                balance_updates =
                  Delegate.cleanup_balance_updates
                    [ Contract source, Debited fee ;
                      Fees (baker, level.cycle), Credited fee ] ;
                operation_result ;
                internal_operation_results ;
              } in
            apply_manager_contents_list_rec ctxt mode baker rest >>= fun (ctxt_result, results) ->
            Lwt.return (ctxt_result, Cons_result (result, results))

let mark_backtracked results =
  let rec mark_contents_list
    : type kind. kind Kind.manager contents_result_list -> kind Kind.manager contents_result_list
    = function
      | Single_result (Manager_operation_result op) ->
          Single_result (Manager_operation_result
                           { balance_updates =
                               op.balance_updates ;
                             operation_result =
                               mark_manager_operation_result op.operation_result ;
                             internal_operation_results =
                               List.map mark_internal_operation_results op.internal_operation_results})
      | Cons_result (Manager_operation_result op, rest) ->
          Cons_result (Manager_operation_result
                         { balance_updates =
                             op.balance_updates ;
                           operation_result =
                             mark_manager_operation_result op.operation_result ;
                           internal_operation_results =
                             List.map mark_internal_operation_results op.internal_operation_results},
                       mark_contents_list rest)
  and mark_internal_operation_results (Internal_operation_result (kind, result)) =
    (Internal_operation_result (kind, mark_manager_operation_result result))
  and mark_manager_operation_result
    : type kind. kind manager_operation_result -> kind manager_operation_result
    = function
      | Failed _ | Skipped _ | Backtracked _ as result -> result
      | Applied (Reveal_result _) as result -> result
      | Applied result -> Backtracked (result, None) in
  mark_contents_list results

let apply_manager_contents_list ctxt mode baker contents_list =
  apply_manager_contents_list_rec ctxt mode baker contents_list >>= fun (ctxt_result, results) ->
  match ctxt_result with
  | `Failure -> Lwt.return (ctxt (* backtracked *), mark_backtracked results)
  | `Success ctxt -> Lwt.return (ctxt, results)

let apply_contents_list
    (type kind) ctxt ~partial chain_id mode pred_block baker
    (operation : kind operation)
    (contents_list : kind contents_list)
  : (context * kind contents_result_list) tzresult Lwt.t =
  match contents_list with
  | Single (Endorsement { level }) ->
      let block = operation.shell.branch in
      fail_unless
        (Block_hash.equal block pred_block)
        (Wrong_endorsement_predecessor (pred_block, block)) >>=? fun () ->
      let current_level = (Level.current ctxt).level in
      fail_unless
        Raw_level.(succ level = current_level)
        Invalid_endorsement_level >>=? fun () ->
      Baking.check_endorsement_rights ctxt chain_id operation >>=? fun (delegate, slots, used) ->
      if used then fail (Duplicate_endorsement delegate)
      else
        let ctxt = record_endorsement ctxt delegate in
        let gap = List.length slots in
        let ctxt = Fitness.increase ~gap ctxt in
        Lwt.return
          Tez.(Constants.endorsement_security_deposit ctxt *?
               Int64.of_int gap) >>=? fun deposit ->
        begin
          if partial then
            Delegate.freeze_deposit ctxt delegate deposit
          else
            add_deposit ctxt delegate deposit
        end >>=? fun ctxt ->
        Global.get_last_block_priority ctxt >>=? fun block_priority ->
        Baking.endorsement_reward ctxt ~block_priority gap >>=? fun reward ->
        Delegate.freeze_rewards ctxt delegate reward >>=? fun ctxt ->
        let level = Level.from_raw ctxt level in
        return (ctxt, Single_result
                  (Endorsement_result
                     { balance_updates = Delegate.cleanup_balance_updates
                           [ Contract (Contract.implicit_contract delegate), Debited deposit;
                             Deposits (delegate, level.cycle), Credited deposit;
                             Rewards (delegate, level.cycle), Credited reward; ] ;
                       delegate ; slots }))
  | Single (Seed_nonce_revelation { level ; nonce }) ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun ctxt ->
      let seed_nonce_revelation_tip =
        Constants.seed_nonce_revelation_tip ctxt in
      add_rewards ctxt seed_nonce_revelation_tip >>=? fun ctxt ->
      return (ctxt, Single_result
                (Seed_nonce_revelation_result
                   [ Rewards (baker, level.cycle), Credited seed_nonce_revelation_tip ]))
  | Single (Double_endorsement_evidence { op1 ; op2 }) -> begin
      match op1.protocol_data.contents, op2.protocol_data.contents with
      | Single (Endorsement e1),
        Single (Endorsement e2)
        when Raw_level.(e1.level = e2.level) &&
             not (Block_hash.equal op1.shell.branch op2.shell.branch) ->
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
          Baking.check_endorsement_rights ctxt chain_id op1 >>=? fun (delegate1, _, _) ->
          Baking.check_endorsement_rights ctxt chain_id op2 >>=? fun (delegate2, _, _) ->
          fail_unless
            (Signature.Public_key_hash.equal delegate1 delegate2)
            (Inconsistent_double_endorsement_evidence
               { delegate1 ; delegate2 }) >>=? fun () ->
          Delegate.has_frozen_balance ctxt delegate1 level.cycle >>=? fun valid ->
          fail_unless valid Unrequired_double_endorsement_evidence >>=? fun () ->
          Delegate.punish ctxt delegate1 level.cycle >>=? fun (ctxt, balance) ->
          Lwt.return Tez.(balance.deposit +? balance.fees) >>=? fun burned ->
          let reward =
            match Tez.(burned /? 2L) with
            | Ok v -> v
            | Error _ -> Tez.zero in
          add_rewards ctxt reward >>=? fun ctxt ->
          let current_cycle = (Level.current ctxt).cycle in
          return (ctxt, Single_result
                    (Double_endorsement_evidence_result
                       (Delegate.cleanup_balance_updates [
                           Deposits (delegate1, level.cycle), Debited balance.deposit ;
                           Fees (delegate1, level.cycle), Debited balance.fees ;
                           Rewards (delegate1, level.cycle), Debited balance.rewards ;
                           Rewards (baker, current_cycle), Credited reward ])))
      | _, _ -> fail Invalid_double_endorsement_evidence
    end
  | Single (Double_baking_evidence { bh1 ; bh2 }) ->
      let hash1 = Block_header.hash bh1 in
      let hash2 = Block_header.hash bh2 in
      fail_unless
        (Compare.Int32.(bh1.shell.level = bh2.shell.level) &&
         not (Block_hash.equal hash1 hash2))
        (Invalid_double_baking_evidence
           { hash1 ;
             level1 = bh1.shell.level ;
             hash2 ;
             level2 = bh2.shell.level ;
           }) >>=? fun () ->
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
        ctxt level ~priority:bh1.protocol_data.contents.priority >>=? fun delegate1 ->
      Baking.check_signature bh1 chain_id delegate1 >>=? fun () ->
      Roll.baking_rights_owner
        ctxt level ~priority:bh2.protocol_data.contents.priority >>=? fun delegate2 ->
      Baking.check_signature bh2 chain_id delegate2 >>=? fun () ->
      fail_unless
        (Signature.Public_key.equal delegate1 delegate2)
        (Inconsistent_double_baking_evidence
           { delegate1 = Signature.Public_key.hash delegate1 ;
             delegate2 = Signature.Public_key.hash delegate2 }) >>=? fun () ->
      let delegate = Signature.Public_key.hash delegate1 in
      Delegate.has_frozen_balance ctxt delegate level.cycle >>=? fun valid ->
      fail_unless valid Unrequired_double_baking_evidence >>=? fun () ->
      Delegate.punish ctxt delegate level.cycle >>=? fun (ctxt, balance) ->
      Lwt.return Tez.(balance.deposit +? balance.fees) >>=? fun burned ->
      let reward =
        match Tez.(burned /? 2L) with
        | Ok v -> v
        | Error _ -> Tez.zero in
      add_rewards ctxt reward >>=? fun ctxt ->
      let current_cycle = (Level.current ctxt).cycle in
      return (ctxt, Single_result
                (Double_baking_evidence_result
                   (Delegate.cleanup_balance_updates [
                       Deposits (delegate, level.cycle), Debited balance.deposit ;
                       Fees (delegate, level.cycle), Debited balance.fees ;
                       Rewards (delegate, level.cycle), Debited balance.rewards ;
                       Rewards (baker, current_cycle), Credited reward ; ])))
  | Single (Activate_account { id = pkh ; activation_code }) -> begin
      let blinded_pkh =
        Blinded_public_key_hash.of_ed25519_pkh activation_code pkh in
      Commitment.get_opt ctxt blinded_pkh >>=? function
      | None -> fail (Invalid_activation { pkh })
      | Some amount ->
          Commitment.delete ctxt blinded_pkh >>=? fun ctxt ->
          let contract = Contract.implicit_contract (Signature.Ed25519 pkh) in
          Contract.(credit ctxt contract amount) >>=? fun ctxt ->
          return (ctxt, Single_result (Activate_account_result
                                         [ Contract contract, Credited amount ]))
    end
  | Single (Proposals { source ; period ; proposals }) ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation >>=? fun () ->
      let level = Level.current ctxt in
      fail_unless Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period)) >>=? fun () ->
      Amendment.record_proposals ctxt source proposals >>=? fun ctxt ->
      return (ctxt, Single_result Proposals_result)
  | Single (Ballot { source ; period ; proposal ; ballot }) ->
      Roll.delegate_pubkey ctxt source >>=? fun delegate ->
      Operation.check_signature delegate chain_id operation >>=? fun () ->
      let level = Level.current ctxt in
      fail_unless Voting_period.(level.voting_period = period)
        (Wrong_voting_period (level.voting_period, period)) >>=? fun () ->
      Amendment.record_ballot ctxt source proposal ballot >>=? fun ctxt ->
      return (ctxt, Single_result Ballot_result)
  | Single (Manager_operation _) as op ->
      precheck_manager_contents_list ctxt chain_id operation op >>=? fun ctxt ->
      apply_manager_contents_list ctxt mode baker op >>= fun (ctxt, result) ->
      return (ctxt, result)
  | Cons (Manager_operation _, _) as op ->
      precheck_manager_contents_list ctxt chain_id operation op >>=? fun ctxt ->
      apply_manager_contents_list ctxt mode baker op >>= fun (ctxt, result) ->
      return (ctxt, result)

let apply_operation ctxt ~partial chain_id mode pred_block baker hash operation =
  let ctxt = Contract.init_origination_nonce ctxt hash in
  apply_contents_list
    ctxt ~partial chain_id mode pred_block baker operation
    operation.protocol_data.contents >>=? fun (ctxt, result) ->
  let ctxt = Gas.set_unlimited ctxt in
  let ctxt = Contract.unset_origination_nonce ctxt in
  return (ctxt, { contents = result })

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
  | None -> return (ctxt, [], [])
  | Some last_cycle ->
      Seed.cycle_end ctxt last_cycle >>=? fun (ctxt, unrevealed) ->
      Roll.cycle_end ctxt last_cycle >>=? fun ctxt ->
      Delegate.cycle_end ctxt last_cycle unrevealed >>=? fun (ctxt, update_balances, deactivated) ->
      Bootstrap.cycle_end ctxt last_cycle >>=? fun ctxt ->
      return (ctxt, update_balances, deactivated)

let begin_full_construction ctxt pred_timestamp protocol_data =
  Baking.check_baking_rights
    ctxt protocol_data pred_timestamp >>=? fun delegate_pk ->
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None -> assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level >>=? fun rights ->
      let ctxt = init_endorsements ctxt rights in
      return (ctxt, protocol_data, delegate_pk)

let begin_partial_construction ctxt =
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None -> assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level >>=? fun rights ->
      let ctxt = init_endorsements ctxt rights in
      return ctxt

let begin_application ctxt chain_id block_header pred_timestamp =
  let current_level = Alpha_context.Level.current ctxt in
  Baking.check_proof_of_work_stamp ctxt block_header >>=? fun () ->
  Baking.check_fitness_gap ctxt block_header >>=? fun () ->
  Baking.check_baking_rights
    ctxt block_header.protocol_data.contents pred_timestamp >>=? fun delegate_pk ->
  Baking.check_signature block_header chain_id delegate_pk >>=? fun () ->
  let has_commitment =
    match block_header.protocol_data.contents.seed_nonce_hash with
    | None -> false
    | Some _ -> true in
  fail_unless
    Compare.Bool.(has_commitment = current_level.expected_commitment)
    (Invalid_commitment
       { expected = current_level.expected_commitment }) >>=? fun () ->
  let ctxt = Fitness.increase ctxt in
  match Level.pred ctxt (Level.current ctxt) with
  | None -> assert false (* genesis *)
  | Some pred_level ->
      Baking.endorsement_rights ctxt pred_level >>=? fun rights ->
      let ctxt = init_endorsements ctxt rights in
      return (ctxt, delegate_pk)

let finalize_application ctxt protocol_data delegate =
  let deposit = Constants.block_security_deposit ctxt in
  add_deposit ctxt delegate deposit >>=? fun ctxt ->
  let reward = (Constants.block_reward ctxt) in
  add_rewards ctxt reward >>=? fun ctxt ->
  Signature.Public_key_hash.Map.fold
    (fun delegate deposit ctxt ->
       ctxt >>=? fun ctxt ->
       Delegate.freeze_deposit ctxt delegate deposit)
    (get_deposits ctxt)
    (return ctxt) >>=? fun ctxt ->
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
  Alpha_context.Global.set_last_block_priority
    ctxt protocol_data.priority >>=? fun ctxt ->
  (* end of cycle *)
  may_snapshot_roll ctxt >>=? fun ctxt ->
  may_start_new_cycle ctxt >>=? fun (ctxt, balance_updates, deactivated) ->
  Amendment.may_start_new_voting_cycle ctxt >>=? fun ctxt ->
  let cycle = (Level.current ctxt).cycle in
  let balance_updates =
    Delegate.(cleanup_balance_updates
                ([ Contract (Contract.implicit_contract delegate), Debited deposit ;
                   Deposits (delegate, cycle), Credited deposit ;
                   Rewards (delegate, cycle), Credited reward ] @ balance_updates)) in
  let consumed_gas = Z.sub (Constants.hard_gas_limit_per_block ctxt) (Alpha_context.Gas.block_level ctxt) in
  Alpha_context.Vote.get_current_period_kind ctxt >>=? fun voting_period_kind ->
  let receipt = Apply_results.{ baker = delegate ;
                                level = Level.current ctxt;
                                voting_period_kind ;
                                nonce_hash = protocol_data.seed_nonce_hash ;
                                consumed_gas ;
                                deactivated ;
                                balance_updates } in
  return (ctxt, receipt)
