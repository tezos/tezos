(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

let rpc_services = ref (RPC.empty : Context.t RPC.directory)
let register0 s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun ctxt () ->
         ( Tezos_context.init ctxt >>=? fun ctxt ->
           f ctxt ) >>= RPC.Answer.return)
let register1 s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun ctxt arg ->
         ( Tezos_context.init ctxt >>=? fun ctxt ->
           f ctxt arg ) >>= RPC.Answer.return)
let register2 s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun (ctxt, arg1) arg2 ->
         ( Tezos_context.init ctxt >>=? fun ctxt ->
           f ctxt arg1 arg2 ) >>= RPC.Answer.return)
let register1_noctxt s f =
  rpc_services :=
    RPC.register !rpc_services (s RPC.Path.root)
      (fun _ arg -> f arg >>= RPC.Answer.return)

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

let () =
  register1_noctxt Services.Constants.bootstrap
    (fun () -> Lwt.return Bootstrap.accounts)

(*-- Context -----------------------------------------------------------------*)

let level ctxt =
  Level.current ctxt >>=? fun level ->
  match Level.pred ctxt level with
  | None -> fail (Apply.Internal_error "unexpected level in context")
  | Some level -> return level

let () = register0 Services.Context.level level

let next_level ctxt =
  Level.current ctxt

let () = register0 Services.Context.next_level next_level

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
           ( Tezos_context.init ctxt >>=? fun ctxt ->
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
  register2' Services.Context.Contract.assets Contract.get_assets ;
  register2' Services.Context.Contract.get (fun ctxt contract ->
      Contract.get_balance ctxt contract >>=? fun balance ->
      Contract.get_manager ctxt contract >>=? fun manager ->
      Contract.get_delegate_opt ctxt contract >>=? fun delegate ->
      Contract.get_counter ctxt contract >>=? fun counter ->
      Contract.is_delegatable ctxt contract >>=? fun delegatable ->
      Contract.is_spendable ctxt contract >>=? fun spendable ->
      Contract.get_script ctxt contract >>=? fun script ->
      Contract.get_assets ctxt contract >>=? fun assets ->
      return { Services.Context.Contract.manager ; balance ;
               spendable ; delegate = (delegatable, delegate) ;
               script ; assets ; counter }) ;
  ()

(*-- Helpers -----------------------------------------------------------------*)

let minimal_timestamp ctxt prio =
  let prio = match prio with None -> 0l | Some p -> Int32.of_int p in
  Mining.minimal_time ctxt prio

let () = register1 Services.Helpers.minimal_timestamp minimal_timestamp

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
           Tezos_context.Level.current ctxt >>=? fun level ->
           Mining.mining_priorities ctxt level >>=? fun (Misc.LCons (miner_pkh, _)) ->
           let miner_contract = Contract.default_contract miner_pkh in
           let block_prio = 0l in
           Apply.apply_operation ctxt false (Some miner_contract) pred_block block_prio operation
           >>=? fun (_ctxt, contracts) ->
           Error_monad.return contracts) ;
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
            (List.hd Bootstrap.accounts).Bootstrap.public_key_hash in
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
  | None -> Int32.mul 2l default
  | Some m -> Int32.of_int m

let mining_rights ctxt level max =
  let max = Int32.to_int (default_max_mining_priority ctxt max) in
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
       Level.current ctxt >>=? fun level ->
       mining_rights ctxt level max >>=? fun (raw_level, slots) ->
       begin
         Lwt_list.filter_map_p (fun x -> x) @@
         List.mapi
           (fun prio c ->
              Mining.minimal_time
                ctxt (Int32.of_int prio) >>= function
              | Error _ -> Lwt.return None
              | Ok timestamp -> Lwt.return (Some (c, timestamp)))
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
  Level.current ctxt >>=? fun current_level ->
  let max_level =
    match max_level with
    | None ->
        Level.last_level_in_cycle ctxt @@
        Cycle.succ current_level.cycle
    | Some l -> Level.from_raw ctxt l in
  let min_level = match min_level with
    | None -> current_level
    | Some l -> Level.from_raw ctxt l in
  let rec loop level =
    if Level.(>) level max_level
    then return []
    else
      loop (Level.succ ctxt level) >>=? fun t ->
      Mining.first_mining_priorities
        ctxt ~max_priority contract level >>=? fun priorities ->
      let raw_level = level.level in
      Lwt_list.map_p
        (fun priority ->
           Mining.minimal_time ctxt priority >>= function
           | Ok time -> Lwt.return (raw_level, Int32.to_int priority, Some time)
               | Error _ -> Lwt.return (raw_level, Int32.to_int priority, None))
        priorities >>= fun priorities ->
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
       Level.current ctxt >>=? fun level ->
       endorsement_rights ctxt (Level.succ ctxt level) max) ;
  register2 Services.Helpers.Rights.endorsement_rights_for_level
    (fun ctxt raw_level max ->
       let level = Level.from_raw ctxt raw_level in
       endorsement_rights ctxt level max)

let endorsement_rights_for_delegate
    ctxt contract (max_priority, min_level, max_level) =
  let max_priority =
    Int32.of_int @@
    default_max_endorsement_priority ctxt max_priority in
  Level.current ctxt >>=? fun current_level ->
  let max_level =
    match max_level with
    | None ->
        Level.last_level_in_cycle ctxt @@
        Cycle.succ (Cycle.succ current_level.cycle)
    | Some l -> Level.from_raw ctxt l in
  let min_level = match min_level with
    | None -> Level.succ ctxt current_level
    | Some l -> Level.from_raw ctxt l in
  let rec loop level =
    if Level.(>) level max_level
    then return []
    else
      loop (Level.succ ctxt level) >>=? fun t ->
      Mining.first_endorsement_slots
        ctxt ~max_priority contract level >>=? fun slots ->
      let raw_level = level.level in
      let slots =
        List.rev_map
          (fun slot -> (raw_level, Int32.to_int slot, None))
          slots in
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
      let hash = Ed25519.hash public_key in
      Public_key.get_option ctxt hash >>=? function
      | None -> return (Some public_key)
      | Some _ -> return None

let forge_operations _ctxt (shell, proto) =
  return (Operation.forge shell proto)

let () = register1 Services.Helpers.Forge.operations forge_operations

let forge_block _ctxt
    (net_id, predecessor, timestamp, fitness, operations,
     raw_level, priority, seed_nonce_hash, proof_of_work_nonce) : MBytes.t tzresult Lwt.t =
  let priority = Int32.of_int priority in
  let mining_slot = (raw_level, priority) in
  return (Block.forge_header
            { net_id ; predecessor ; timestamp ; fitness ; operations }
            { mining_slot ; seed_nonce_hash ; proof_of_work_nonce })

let () = register1 Services.Helpers.Forge.block forge_block

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

let parse_operations ctxt (shell, bytes, check) =
  Operation.parse_proto bytes >>=? fun (proto, signature) ->
  begin
    match check with
    | Some true -> check_signature ctxt signature shell proto
    | Some false | None -> return ()
  end >>=? fun () ->
  return proto

let () = register1 Services.Helpers.Parse.operations parse_operations

(*****)

let rpc_services = !rpc_services
