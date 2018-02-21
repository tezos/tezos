(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

module S = struct

  open Data_encoding

  let custom_root = RPC_path.(open_root / "helpers")

  let minimal_timestamp =
    RPC_service.post_service
      ~description: "Minimal timestamp for the next block."
      ~query: RPC_query.empty
      ~input: (obj1 (opt "priority" int31))
      ~output: (obj1 (req "timestamp" Timestamp.encoding))
      RPC_path.(custom_root / "minimal_timestamp")

  let run_code_input_encoding =
    (obj6
       (req "script" Script.expr_encoding)
       (req "storage" Script.expr_encoding)
       (req "input" Script.expr_encoding)
       (req "amount" Tez.encoding)
       (opt "contract" Contract.encoding)
       (opt "origination_nonce" Contract.origination_nonce_encoding))

  let run_code =
    RPC_service.post_service
      ~description: "Run a piece of code in the current context"
      ~query: RPC_query.empty
      ~input: run_code_input_encoding
      ~output: (obj3
                  (req "storage" Script.expr_encoding)
                  (req "output" Script.expr_encoding)
                  (opt "big_map_diff" (list (tup2 Script.expr_encoding (option Script.expr_encoding)))))
      RPC_path.(custom_root / "run_code")

  let apply_operation =
    RPC_service.post_service
      ~description: "Applies an operation in the current context"
      ~query: RPC_query.empty
      ~input: (obj4
                 (req "pred_block" Block_hash.encoding)
                 (req "operation_hash" Operation_hash.encoding)
                 (req "forged_operation" bytes)
                 (opt "signature" Ed25519.Signature.encoding))
      ~output: (obj1 (req "contracts" (list Contract.encoding)))
      RPC_path.(custom_root / "apply_operation")


  let trace_code =
    RPC_service.post_service
      ~description: "Run a piece of code in the current context, \
                     keeping a trace"
      ~query: RPC_query.empty
      ~input: run_code_input_encoding
      ~output: (obj4
                  (req "storage" Script.expr_encoding)
                  (req "output" Script.expr_encoding)
                  (req "trace"
                     (list @@ obj3
                        (req "location" Script.location_encoding)
                        (req "gas" Gas.encoding)
                        (req "stack" (list (Script.expr_encoding)))))
                  (opt "big_map_diff" (list (tup2 Script.expr_encoding (option Script.expr_encoding)))))
      RPC_path.(custom_root / "trace_code")

  let typecheck_code =
    RPC_service.post_service
      ~description: "Typecheck a piece of code in the current context"
      ~query: RPC_query.empty
      ~input: Script.expr_encoding
      ~output: Script_tc_errors_registration.type_map_enc
      RPC_path.(custom_root / "typecheck_code")

  let typecheck_data =
    RPC_service.post_service
      ~description: "Check that some data expression is well formed \
                     and of a given type in the current context"
      ~query: RPC_query.empty
      ~input: (obj2
                 (req "data" Script.expr_encoding)
                 (req "type" Script.expr_encoding))
      ~output: empty
      RPC_path.(custom_root / "typecheck_data")

  let hash_data =
    RPC_service.post_service
      ~description: "Computes the hash of some data expression \
                     using the same algorithm as script instruction H"
      ~input: (obj2 (req "data" Script.expr_encoding)
                 (req "type" Script.expr_encoding))
      ~output: (obj1 (req "hash" string))
      ~query: RPC_query.empty
      RPC_path.(custom_root / "hash_data")

  let level =
    RPC_service.post_service
      ~description: "..."
      ~query: RPC_query.empty
      ~input: (obj1 (opt "offset" int32))
      ~output: Level.encoding
      RPC_path.(custom_root / "level" /: Raw_level.arg)

  let levels =
    RPC_service.post_service
      ~description: "Levels of a cycle"
      ~query: RPC_query.empty
      ~input: empty
      ~output: (describe ~title: "levels of a cycle"
                  (obj2
                     (req "first" Raw_level.encoding)
                     (req "last" Raw_level.encoding)))
      RPC_path.(custom_root / "levels" /: Cycle.arg)

end

module I = struct

  let apply_operation ctxt () (pred_block, hash, forged_operation, signature) =
    (* ctxt accept_failing_script baker_contract pred_block block_prio operation *)
    match Data_encoding.Binary.of_bytes
            Operation.unsigned_operation_encoding
            forged_operation with
    | None -> Error_monad.fail Operation.Cannot_parse_operation
    | Some (shell, contents) ->
        let operation = { hash ; shell ; contents ; signature } in
        let level = Alpha_context.Level.current ctxt in
        Baking.baking_priorities ctxt level >>=? fun (Misc.LCons (baker_pkh, _)) ->
        let baker_contract = Contract.implicit_contract baker_pkh in
        let block_prio = 0 in
        Apply.apply_operation
          ctxt (Some baker_contract) pred_block block_prio operation
        >>=? function
        | (_ctxt, _, Some script_err) -> Lwt.return (Error script_err)
        | (_ctxt, contracts, None) -> Lwt.return (Ok contracts)


  let run_parameters ctxt (script, storage, input, amount, contract, origination_nonce) =
    let contract =
      match contract with
      | Some contract -> contract
      | None ->
          Contract.implicit_contract
            (List.hd (Bootstrap.accounts ctxt)).Bootstrap.public_key_hash in
    let max_gas =
      Constants.max_gas ctxt in
    let origination_nonce =
      match origination_nonce with
      | Some origination_nonce -> origination_nonce
      | None ->
          Contract.initial_origination_nonce
            (Operation_hash.hash_string [ "FAKE " ; "FAKE" ; "FAKE" ]) in
    (script, storage, input, amount, contract, max_gas, origination_nonce)

end

let () =
  let open Services_registration in
  register0 S.minimal_timestamp begin fun ctxt () slot ->
    let timestamp = Alpha_context.Timestamp.current ctxt in
    let slot = match slot with None -> 0 | Some p -> p in
    Baking.minimal_time ctxt slot timestamp
  end ;
  register0 S.apply_operation I.apply_operation ;
  register0 S.run_code begin fun ctxt () parameters ->
    let (code, storage, input, amount, contract, gas, origination_nonce) =
      I.run_parameters ctxt parameters in
    Script_interpreter.execute
      origination_nonce
      contract (* transaction initiator *)
      contract (* script owner *)
      ctxt { storage ; code } amount input
      (Gas.of_int gas) >>=? fun (sto, ret, _gas, _ctxt, _, maybe_big_map_diff) ->
    return (sto, ret,
            Option.map maybe_big_map_diff
              ~f:Script_ir_translator.to_printable_big_map)
  end ;
  register0 S.trace_code begin fun ctxt () parameters ->
    let (code, storage, input, amount, contract, gas, origination_nonce) =
      I.run_parameters ctxt parameters in
    Script_interpreter.trace
      origination_nonce
      contract (* transaction initiator *)
      contract (* script owner *)
      ctxt { storage ; code } amount input
      (Gas.of_int gas) >>=? fun ((sto, ret, _gas, _ctxt, _, maybe_big_map_diff), trace) ->
    return (sto, ret, trace,
            Option.map maybe_big_map_diff
              ~f:Script_ir_translator.to_printable_big_map)
  end ;
  register0 S.typecheck_code begin fun ctxt () ->
    Script_ir_translator.typecheck_code ctxt
  end ;
  register0 S.typecheck_data begin fun ctxt () ->
    Script_ir_translator.typecheck_data ctxt
  end ;
  register0 S.hash_data begin fun ctxt () (expr, typ) ->
    let open Script_ir_translator in
    Lwt.return @@ parse_ty false (Micheline.root typ) >>=? fun (Ex_ty typ, _) ->
    parse_data ctxt typ (Micheline.root expr) >>=? fun data ->
    return (Script_ir_translator.hash_data typ data)
  end ;
  register1 S.level begin fun ctxt raw () offset ->
    return (Level.from_raw ctxt ?offset raw)
  end ;
  register1 S.levels begin fun ctxt cycle () () ->
    let levels = Level.levels_in_cycle ctxt cycle in
    let first = List.hd (List.rev levels) in
    let last = List.hd levels in
    return (first.level, last.level)
  end

let minimal_time ctxt ?priority block =
  RPC_context.make_call0 S.minimal_timestamp ctxt block () priority

let run_code ctxt block code (storage, input, amount) =
  RPC_context.make_call0 S.run_code ctxt
    block () (code, storage, input, amount, None, None)

let apply_operation ctxt block pred_block hash forged_operation signature =
  RPC_context.make_call0 S.apply_operation ctxt
    block () (pred_block, hash, forged_operation, signature)

let trace_code ctxt block code (storage, input, amount) =
  RPC_context.make_call0 S.trace_code ctxt
    block () (code, storage, input, amount, None, None)

let typecheck_code ctxt block =
  RPC_context.make_call0 S.typecheck_code ctxt block ()

let typecheck_data ctxt block =
  RPC_context.make_call0 S.typecheck_data ctxt block ()

let hash_data ctxt block =
  RPC_context.make_call0 S.hash_data ctxt block ()

let level ctxt block ?offset lvl =
  RPC_context.make_call1 S.level ctxt block lvl () offset

let levels ctxt block cycle =
  RPC_context.make_call1 S.levels ctxt block cycle () ()

module Forge = struct

  module S = struct

    open Data_encoding

    let custom_root = RPC_path.(open_root / "helpers" / "forge")

    let operations =
      RPC_service.post_service
        ~description:"Forge an operation"
        ~query: RPC_query.empty
        ~input: Operation.unsigned_operation_encoding
        ~output:
          (obj1
             (req "operation" @@
              describe ~title: "hex encoded operation" bytes))
        RPC_path.(custom_root / "forge" / "operations" )

    let empty_proof_of_work_nonce =
      MBytes.of_string
        (String.make Constants_repr.proof_of_work_nonce_size  '\000')

    let protocol_data =
      RPC_service.post_service
        ~description: "Forge the protocol-specific part of a block header"
        ~query: RPC_query.empty
        ~input:
          (obj3
             (req "priority" uint16)
             (req "nonce_hash" Nonce_hash.encoding)
             (dft "proof_of_work_nonce"
                (Fixed.bytes
                   Alpha_context.Constants.proof_of_work_nonce_size)
                empty_proof_of_work_nonce))
        ~output: (obj1 (req "protocol_data" bytes))
        RPC_path.(custom_root / "forge" / "protocol_data")

  end

  let () =
    let open Services_registration in
    register0_noctxt S.operations begin fun () (shell, proto) ->
      return (Operation.forge shell proto)
    end ;
    register0_noctxt S.protocol_data begin fun ()
      (priority, seed_nonce_hash, proof_of_work_nonce) ->
      return (Block_header.forge_unsigned_protocol_data
                { priority ; seed_nonce_hash ; proof_of_work_nonce })
    end

  module Manager = struct

    let operations ctxt
        block ~branch ~source ?sourcePubKey ~counter ~fee operations =
      let ops =
        Manager_operations { source ; public_key = sourcePubKey ;
                             counter ; operations ; fee } in
      (RPC_context.make_call0 S.operations ctxt block
         () ({ branch }, Sourced_operations ops))

    let transaction ctxt
        block ~branch ~source ?sourcePubKey ~counter
        ~amount ~destination ?parameters ~fee ()=
      operations ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
        Alpha_context.[Transaction { amount ; parameters ; destination }]

    let origination ctxt
        block ~branch
        ~source ?sourcePubKey ~counter
        ~managerPubKey ~balance
        ?(spendable = true)
        ?(delegatable = true)
        ?delegatePubKey ?script ~fee () =
      operations ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
        Alpha_context.[
          Origination { manager = managerPubKey ;
                        delegate = delegatePubKey ;
                        script ;
                        spendable ;
                        delegatable ;
                        credit = balance }
        ]

    let delegation ctxt
        block ~branch ~source ?sourcePubKey ~counter ~fee delegate =
      operations ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
        Alpha_context.[Delegation delegate]

  end

  module Delegate = struct

    let operations ctxt
        block ~branch ~source operations =
      let ops = Delegate_operations { source ; operations } in
      (RPC_context.make_call0 S.operations ctxt block
         () ({ branch }, Sourced_operations ops))

    let endorsement ctxt
        b ~branch ~source ~block ~slot () =
      operations ctxt b ~branch ~source
        Alpha_context.[Endorsement { block ; slot }]

    let proposals ctxt
        b ~branch ~source ~period ~proposals () =
      operations ctxt b ~branch ~source
        Alpha_context.[Proposals { period ; proposals }]

    let ballot ctxt
        b ~branch ~source ~period ~proposal ~ballot () =
      operations ctxt b ~branch ~source
        Alpha_context.[Ballot { period ; proposal ; ballot }]

  end

  module Dictator = struct

    let operation ctxt
        block ~branch operation =
      let op = Dictator_operation operation in
      (RPC_context.make_call0 S.operations ctxt block
         () ({ branch }, Sourced_operations op))

    let activate ctxt
        b ~branch hash =
      operation ctxt b ~branch (Activate hash)

    let activate_testchain ctxt
        b ~branch hash =
      operation ctxt b ~branch (Activate_testchain hash)

  end

  module Anonymous = struct

    let operations ctxt block ~branch operations =
      (RPC_context.make_call0 S.operations ctxt block
         () ({ branch }, Anonymous_operations operations))

    let seed_nonce_revelation ctxt
        block ~branch ~level ~nonce () =
      operations ctxt block ~branch [Seed_nonce_revelation { level ; nonce }]

    let faucet ctxt
        block ~branch ~id ~nonce () =
      operations ctxt block ~branch [Faucet { id ; nonce }]

  end

  let empty_proof_of_work_nonce =
    MBytes.of_string
      (String.make Constants_repr.proof_of_work_nonce_size  '\000')

  let protocol_data ctxt
      block
      ~priority ~seed_nonce_hash
      ?(proof_of_work_nonce = empty_proof_of_work_nonce) () =
    RPC_context.make_call0 S.protocol_data
      ctxt block () (priority, seed_nonce_hash, proof_of_work_nonce)

end

module Parse = struct

  module S = struct

    open Data_encoding

    let custom_root = RPC_path.(open_root / "helpers" / "parse")

    let operations =
      RPC_service.post_service
        ~description:"Parse operations"
        ~query: RPC_query.empty
        ~input:
          (obj2
             (req "operations" (list (dynamic_size Operation.raw_encoding)))
             (opt "check_signature" bool))
        ~output: (list (dynamic_size Operation.encoding))
        RPC_path.(custom_root / "parse" / "operations" )

    let block =
      RPC_service.post_service
        ~description:"Parse a block"
        ~query: RPC_query.empty
        ~input: Block_header.raw_encoding
        ~output: Block_header.protocol_data_encoding
        RPC_path.(custom_root / "parse" / "block" )

  end

  module I = struct

    let check_signature ctxt signature shell contents =
      match contents with
      | Anonymous_operations _ -> return ()
      | Sourced_operations (Manager_operations op) ->
          begin
            match op.public_key with
            | Some key -> return key
            | None ->
                Contract.get_manager ctxt op.source >>=? fun manager ->
                Delegates_pubkey.get ctxt manager
          end >>=? fun public_key ->
          Operation.check_signature public_key
            { signature ; shell ; contents ; hash = Operation_hash.zero }
      | Sourced_operations (Delegate_operations { source ; _ }) ->
          Operation.check_signature source
            { signature ; shell ; contents ; hash = Operation_hash.zero }
      | Sourced_operations (Dictator_operation _) ->
          let key = Constants.dictator_pubkey ctxt in
          Operation.check_signature key
            { signature ; shell ; contents ; hash = Operation_hash.zero }

  end

  let () =
    let open Services_registration in
    register0 S.operations begin fun ctxt () (operations, check) ->
      map_s begin fun raw ->
        Lwt.return (Operation.parse (Operation.hash_raw raw) raw) >>=? fun op ->
        begin match check with
          | Some true -> I.check_signature ctxt op.signature op.shell op.contents
          | Some false | None -> return ()
        end >>|? fun () -> op
      end operations
    end ;
    register0_noctxt S.block begin fun () raw_block ->
      Lwt.return (Block_header.parse raw_block) >>=? fun { protocol_data ; _ } ->
      return protocol_data
    end

  let operations ctxt block ?check operations =
    RPC_context.make_call0
      S.operations ctxt block () (operations, check)
  let block ctxt block shell protocol_data =
    RPC_context.make_call0
      S.block ctxt block () ({ shell ; protocol_data } : Block_header.raw)

end

