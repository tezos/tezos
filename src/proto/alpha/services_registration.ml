(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

type rpc_context = {
  block_hash: Block_hash.t ;
  block_header: Block_header.raw ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> Operation.raw list list Lwt.t ;
  context: Tezos_context.t ;
}

let rpc_init
    ({ block_hash ; block_header ;
       operation_hashes ; operations ; context } : Updater.rpc_context) =
  let level = Int32.succ block_header.shell.level in
  let timestamp = block_header.shell.timestamp in
  let fitness = block_header.shell.fitness in
  Tezos_context.init ~level ~timestamp ~fitness context >>=? fun context ->
  return { block_hash ; block_header ; operation_hashes ; operations ; context }

let rpc_services = ref (RPC.empty : Updater.rpc_context RPC.directory)

let register0_fullctxt s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun ctxt () ->
         ( rpc_init ctxt >>=? fun ctxt ->
           f ctxt ) >>= RPC.Answer.return)
let register0 s f = register0_fullctxt s (fun { context } -> f context)

let register1_fullctxt s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun ctxt arg ->
         ( rpc_init ctxt >>=? fun ctxt ->
           f ctxt arg ) >>= RPC.Answer.return)
let register1 s f = register1_fullctxt s (fun { context } x -> f context x)
let register1_noctxt s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun _ arg -> f arg >>= RPC.Answer.return)

let register2_fullctxt s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun (ctxt, arg1) arg2 ->
         ( rpc_init ctxt >>=? fun ctxt ->
           f ctxt arg1 arg2 ) >>= RPC.Answer.return)
let register2 s f = register2_fullctxt s (fun { context } x y -> f context x y)


(*-- Operations --------------------------------------------------------------*)

let () =
  register0_fullctxt
    Services.operations
    (fun { operation_hashes ; operations } ->
       operation_hashes () >>= fun operation_hashes ->
       operations () >>= fun operations ->
       map2_s
         (map2_s (fun x y -> Lwt.return (Operation.parse x y)))
         operation_hashes operations)

let () =
  register0_fullctxt
    Services.header
    (fun { block_header } ->
       Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
       return block_header) ;
  register0_fullctxt
    Services.Header.priority
    (fun { block_header } ->
       Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
       return block_header.proto.priority) ;
  register0_fullctxt
    Services.Header.seed_nonce_hash
    (fun { block_header } ->
       Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
       return block_header.proto.seed_nonce_hash)


(*-- Constants ---------------------------------------------------------------*)

let cycle_length ctxt =
  return @@ Constants.cycle_length ctxt

let () = register0 Services.Constants.cycle_length cycle_length

let voting_period_length ctxt =
  return @@ Constants.voting_period_length ctxt

let () =
  register0
    Services.Constants.voting_period_length
    voting_period_length

let time_before_reward ctxt =
  return @@ Constants.time_before_reward ctxt

let () = register0 Services.Constants.time_before_reward time_before_reward

let slot_durations ctxt =
  return @@ Constants.slot_durations ctxt

let () = register0 Services.Constants.slot_durations slot_durations

let first_free_mining_slot ctxt =
  return @@ Constants.first_free_mining_slot ctxt

let () =
  register0 Services.Constants.first_free_mining_slot first_free_mining_slot

let max_signing_slot ctxt =
  return @@ Constants.max_signing_slot ctxt

let () = register0 Services.Constants.max_signing_slot max_signing_slot

let instructions_per_transaction ctxt =
  return @@ Constants.instructions_per_transaction ctxt

let () =
  register0
    Services.Constants.instructions_per_transaction instructions_per_transaction

let proof_of_work_threshold ctxt =
  return @@ Constants.proof_of_work_threshold ctxt

let () =
  register0 Services.Constants.proof_of_work_threshold proof_of_work_threshold

let () =
  register1_noctxt Services.Constants.errors
    (fun () ->
       Lwt.return (Data_encoding.Json.(schema (error_encoding ()))))

(*-- Context -----------------------------------------------------------------*)

type error += Unexpected_level_in_context

let level ctxt =
  let level = Level.current ctxt in
  match Level.pred ctxt level with
  | None -> fail Unexpected_level_in_context
  | Some level -> return level

let () = register0 Services.Context.level level

let next_level ctxt =
  return (Level.current ctxt)

let () = register0 Services.Context.next_level next_level

let () =
  register0 Services.Context.voting_period_kind Vote.get_current_period_kind


(*-- Context.Nonce -----------------------------------------------------------*)

let nonce ctxt raw_level () =
  let level = Level.from_raw ctxt raw_level in
  Nonce.get ctxt level >>= function
  | Ok (Revealed nonce) -> return (Services.Context.Nonce.Revealed nonce)
  | Ok (Unrevealed { nonce_hash }) ->
      return (Services.Context.Nonce.Missing nonce_hash)
  | Error _ -> return Services.Context.Nonce.Forgotten

let () = register2 Services.Context.Nonce.get nonce

let nonce_hash ctxt =
  level ctxt >>=? fun level ->
  Nonce.get ctxt level >>=? function
  | Unrevealed { nonce_hash } -> return nonce_hash
  | _ -> assert false

let () = register0 Services.Context.Nonce.hash nonce_hash

(*-- Context.Key -------------------------------------------------------------*)

let get_key ctxt hash () =
  Public_key.get ctxt hash >>=? fun pk ->
  return (hash, pk)

let () = register2 Services.Context.Key.get get_key
let () = register0 Services.Context.Key.list Public_key.list

(*-- Context.Contract --------------------------------------------------------*)

let () =
  register0 Services.Context.Contract.list Contract.list

let () =
  let register2 s f =
    rpc_services :=
      RPC.register !rpc_services (s RPC.Path.root)
        (fun (ctxt, contract) arg ->
           ( rpc_init ctxt >>=? fun { context = ctxt } ->
             Contract.exists ctxt contract >>=? function
             | true -> f ctxt contract arg
             | false -> raise Not_found ) >>= RPC.Answer.return) in
  let register2' s f = register2 s (fun ctxt a1 () -> f ctxt a1) in
  register2' Services.Context.Contract.balance Contract.get_balance ;
  register2' Services.Context.Contract.manager Contract.get_manager ;
  register2' Services.Context.Contract.delegate Contract.get_delegate_opt ;
  register2' Services.Context.Contract.counter Contract.get_counter ;
  register2' Services.Context.Contract.spendable Contract.is_spendable ;
  register2' Services.Context.Contract.delegatable Contract.is_delegatable ;
  register2' Services.Context.Contract.script Contract.get_script ;
  register2' Services.Context.Contract.get (fun ctxt contract ->
      Contract.get_balance ctxt contract >>=? fun balance ->
      Contract.get_manager ctxt contract >>=? fun manager ->
      Contract.get_delegate_opt ctxt contract >>=? fun delegate ->
      Contract.get_counter ctxt contract >>=? fun counter ->
      Contract.is_delegatable ctxt contract >>=? fun delegatable ->
      Contract.is_spendable ctxt contract >>=? fun spendable ->
      Contract.get_script ctxt contract >>=? fun script ->
      return { Services.Context.Contract.manager ; balance ;
               spendable ; delegate = (delegatable, delegate) ;
               script ; counter }) ;
  ()

(*-- Helpers -----------------------------------------------------------------*)

let minimal_timestamp ctxt prio =
  let prio = match prio with None -> 0 | Some p -> p in
  Mining.minimal_time ctxt prio

let () = register1
           Services.Helpers.minimal_timestamp
           (fun ctxt slot ->
              let timestamp = Tezos_context.Timestamp.current ctxt in
             minimal_timestamp ctxt slot timestamp)

let () =
  (* ctxt accept_failing_script miner_contract pred_block block_prio operation *)
  register1 Services.Helpers.apply_operation
    (fun ctxt (pred_block, hash, forged_operation, signature) ->
       match Data_encoding.Binary.of_bytes
               Operation.unsigned_operation_encoding
               forged_operation with
       | None -> Error_monad.fail Operation.Cannot_parse_operation
       | Some (shell, contents) ->
           let operation = { hash ; shell ; contents ; signature } in
           let level = Tezos_context.Level.current ctxt in
           Mining.mining_priorities ctxt level >>=? fun (Misc.LCons (miner_pkh, _)) ->
           let miner_contract = Contract.default_contract miner_pkh in
           let block_prio = 0 in
           Apply.apply_operation
             ctxt (Some miner_contract) pred_block block_prio operation
           >>=? function
           | (_ctxt, _, Some script_err) -> Lwt.return (Error script_err)
           | (_ctxt, contracts, None) -> Lwt.return (Ok contracts)) ;
  let run_parameters ctxt (script, storage, input, amount, contract, origination_nonce) =
    let amount =
      match amount with
      | Some amount -> amount
      | None ->
          match Tez.of_cents 100_00L with
          | Some tez -> tez
          | None -> Tez.zero in
    let contract =
      match contract with
      | Some contract -> contract
      | None ->
          Contract.default_contract
            (List.hd (Bootstrap.accounts ctxt)).Bootstrap.public_key_hash in
    let storage : Script.storage =
      { storage ; storage_type = (script : Script.code).storage_type } in
    let qta =
      Constants.instructions_per_transaction ctxt in
    let origination_nonce =
      match origination_nonce with
      | Some origination_nonce -> origination_nonce
      | None ->
          Contract.initial_origination_nonce
            (Operation_hash.hash_string [ "FAKE " ; "FAKE" ; "FAKE" ]) in
    (script, storage, input, amount, contract, qta, origination_nonce) in
  register1 Services.Helpers.run_code
    (fun ctxt parameters ->
       let (script, storage, input, amount, contract, qta, origination_nonce) =
         run_parameters ctxt parameters in
       Script_interpreter.execute
         origination_nonce
         contract (* transaction initiator *)
         contract (* script owner *)
         ctxt storage script amount input
         qta >>=? fun (sto, ret, _qta, _ctxt, _) ->
       Error_monad.return (sto, ret)) ;
  register1 Services.Helpers.trace_code
    (fun ctxt parameters ->
       let (script, storage, input, amount, contract, qta, origination_nonce) =
         run_parameters ctxt parameters in
       Script_interpreter.trace
         origination_nonce
         contract (* transaction initiator *)
         contract (* script owner *)
         ctxt storage script amount input
         qta >>=? fun ((sto, ret, _qta, _ctxt, _), trace) ->
       Error_monad.return (sto, ret, trace))

let () =
  register1 Services.Helpers.typecheck_code
    Script_ir_translator.typecheck_code

let () =
  register1 Services.Helpers.typecheck_data
    Script_ir_translator.typecheck_data

let () =
  register1 Services.Helpers.hash_data
    (fun _ctxt expr -> return (Script.hash_expr expr))

let compute_level ctxt raw offset =
  return (Level.from_raw ctxt ?offset raw)

let () = register2 Services.Helpers.level compute_level

let levels ctxt cycle () =
  let levels = Level.levels_in_cycle ctxt cycle in
  let first = List.hd (List.rev levels) in
  let last = List.hd levels in
  return (first.level, last.level)

let () = register2 Services.Helpers.levels levels


(*-- Helpers.Rights ----------------------------------------------------------*)

let default_max_mining_priority ctxt arg =
  let default = Constants.first_free_mining_slot ctxt in
  match arg with
  | None -> 2 * default
  | Some m -> m

let mining_rights ctxt level max =
  let max = default_max_mining_priority ctxt max in
  Mining.mining_priorities ctxt level >>=? fun contract_list ->
  let rec loop l n =
    match n with
    | 0 -> return []
    | n ->
        let Misc.LCons (h, t) = l in
        t () >>=? fun t ->
        loop t (pred n) >>=? fun t ->
        return (h :: t)
  in
  loop contract_list max >>=? fun prio ->
  return (level.level, prio)

let () =
  register1 Services.Helpers.Rights.mining_rights
    (fun ctxt max ->
       let level = Level.current ctxt in
       mining_rights ctxt level max >>=? fun (raw_level, slots) ->
       begin
         Lwt_list.filter_map_p (fun x -> x) @@
         List.mapi
           (fun prio c ->
             let timestamp = Timestamp.current ctxt in
              Mining.minimal_time ctxt prio timestamp >>= function
              | Error _ -> Lwt.return None
              | Ok minimal_timestamp -> Lwt.return (Some (c, minimal_timestamp)))
           slots
       end >>= fun timed_slots ->
       return (raw_level, timed_slots))

let () =
  register2 Services.Helpers.Rights.mining_rights_for_level
    (fun ctxt raw_level max ->
       let level = Level.from_raw ctxt raw_level in
       mining_rights ctxt level max)

let mining_rights_for_delegate
    ctxt contract (max_priority, min_level, max_level) =
  let max_priority = default_max_mining_priority ctxt max_priority in
  let current_level = Level.current ctxt in
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
      Mining.first_mining_priorities
        ctxt ~max_priority contract level >>=? fun priorities ->
      let raw_level = level.level in
      Error_monad.map_s
        (fun priority ->
           let timestamp = Timestamp.current ctxt in
           Mining.minimal_time ctxt priority timestamp >>=? fun time ->
           return (raw_level, priority, time))
        priorities >>=? fun priorities ->
      return (priorities @ t)
  in
  loop min_level

let () =
  register2 Services.Helpers.Rights.mining_rights_for_delegate
    mining_rights_for_delegate

let default_max_endorsement_priority ctxt arg =
  let default = Constants.max_signing_slot ctxt in
  match arg with
  | None -> default
  | Some m -> m

let endorsement_rights ctxt level max =
  let max = default_max_endorsement_priority ctxt max in
  Mining.endorsement_priorities ctxt level >>=? fun contract_list ->
  let rec loop l n =
    match n with
    | 0 -> return []
    | n ->
        let Misc.LCons (h, t) = l in
        t () >>=? fun t ->
        loop t (pred n) >>=? fun t ->
        return (h :: t)
  in
  loop contract_list max >>=? fun prio ->
  return (level.level, prio)

let () =
  register1 Services.Helpers.Rights.endorsement_rights
    (fun ctxt max ->
       let level = Level.current ctxt in
       endorsement_rights ctxt (Level.succ ctxt level) max) ;
  register2 Services.Helpers.Rights.endorsement_rights_for_level
    (fun ctxt raw_level max ->
       let level = Level.from_raw ctxt raw_level in
       endorsement_rights ctxt level max)

let endorsement_rights_for_delegate
    ctxt contract (max_priority, min_level, max_level) =
  let current_level = Level.current ctxt in
  let max_priority = default_max_endorsement_priority ctxt max_priority in
  let min_level = match min_level with
    | None -> Level.succ ctxt current_level
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
      Mining.first_endorsement_slots
        ctxt ~max_priority contract level >>=? fun slots ->
      let raw_level = level.level in
      let slots = List.rev_map (fun slot -> (raw_level, slot)) slots in
      return (List.rev_append slots t)
  in
  loop min_level

let () =
  register2 Services.Helpers.Rights.endorsement_rights_for_delegate
    endorsement_rights_for_delegate

(*-- Helpers.Forge -----------------------------------------------------------*)

let operation_public_key ctxt = function
  | None -> return None
  | Some public_key ->
      let hash = Ed25519.Public_key.hash public_key in
      Public_key.get_option ctxt hash >>=? function
      | None -> return (Some public_key)
      | Some _ -> return None

let forge_operations _ctxt (shell, proto) =
  return (Operation.forge shell proto)

let () = register1 Services.Helpers.Forge.operations forge_operations

let forge_block_proto_header _ctxt
    (priority, seed_nonce_hash, proof_of_work_nonce) : MBytes.t tzresult Lwt.t =
  return (Block_header.forge_unsigned_proto_header
            { priority ; seed_nonce_hash ; proof_of_work_nonce })

let () =
  register1 Services.Helpers.Forge.block_proto_header forge_block_proto_header

(*-- Helpers.Parse -----------------------------------------------------------*)

let dummy_hash = Operation_hash.hash_bytes []

let check_signature ctxt signature shell contents =
  match contents with
  | Anonymous_operations _ -> return ()
  | Sourced_operations (Manager_operations op) ->
      begin
        match op.public_key with
        | Some key -> return key
        | None ->
            Contract.get_manager ctxt op.source >>=? fun manager ->
            Public_key.get ctxt manager
      end >>=? fun public_key ->
      Operation.check_signature public_key
        { signature ; shell ; contents ; hash = dummy_hash }
  | Sourced_operations (Delegate_operations { source }) ->
      Operation.check_signature source
        { signature ; shell ; contents ; hash = dummy_hash }
  | Sourced_operations (Dictator_operation _) ->
     let key = Constants.dictator_pubkey ctxt in
     Operation.check_signature key
       { signature ; shell ; contents ; hash = dummy_hash }

let parse_operations ctxt (operations, check) =
  map_s begin fun raw ->
    begin
      Lwt.return
        (Operation.parse (Tezos_data.Operation.hash raw) raw) >>=? fun op ->
      begin match check with
        | Some true -> check_signature ctxt op.signature op.shell op.contents
        | Some false | None -> return ()
      end >>|? fun () -> op
    end
  end operations

let () = register1 Services.Helpers.Parse.operations parse_operations

let parse_block _ctxt raw_block =
  Lwt.return (Block_header.parse raw_block) >>=? fun { proto } ->
  return proto

let () = register1 Services.Helpers.Parse.block parse_block

(*****)

let rpc_services = !rpc_services
