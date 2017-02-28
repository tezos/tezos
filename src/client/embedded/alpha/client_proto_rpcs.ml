(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let string_of_errors exns =
  Format.asprintf "  @[<v>%a@]" pp_print_error exns

let handle_error cctxt = function
  | Ok res -> Lwt.return res
  | Error exns ->
      pp_print_error Format.err_formatter exns ;
      cctxt.Client_commands.error "%s" "cannot continue"

type block = [
  | `Genesis
  | `Head of int | `Prevalidation
  | `Test_head of int | `Test_prevalidation
  | `Hash of Block_hash.t
]

let call_service1 cctxt s block a1 =
  Client_node_rpcs.call_service1 cctxt
    (s Node_rpc_services.Blocks.proto_path) block a1
let call_error_service1 cctxt s block a1 =
  call_service1 cctxt s block a1 >|= wrap_error
let call_service2 cctxt s block a1 a2 =
  Client_node_rpcs.call_service2 cctxt
    (s Node_rpc_services.Blocks.proto_path) block a1 a2
let call_error_service2 cctxt s block a1 a2 =
  call_service2 cctxt s block a1 a2 >|= wrap_error

module Constants = struct
  let errors cctxt block =
    call_service1 cctxt Services.Constants.errors block ()
  let cycle_length cctxt block =
    call_error_service1 cctxt Services.Constants.cycle_length block ()
  let voting_period_length cctxt block =
    call_error_service1 cctxt Services.Constants.voting_period_length block ()
  let time_before_reward cctxt block =
    call_error_service1 cctxt Services.Constants.time_before_reward block ()
  let slot_durations cctxt block =
    call_error_service1 cctxt Services.Constants.slot_durations block ()
  let first_free_mining_slot cctxt block =
    call_error_service1 cctxt Services.Constants.first_free_mining_slot block ()
  let max_signing_slot cctxt block =
    call_error_service1 cctxt Services.Constants.max_signing_slot block ()
  let instructions_per_transaction cctxt block =
    call_error_service1 cctxt Services.Constants.instructions_per_transaction block ()
  let stamp_threshold cctxt block =
    call_error_service1 cctxt Services.Constants.proof_of_work_threshold block ()
end

module Context = struct

  let level cctxt block =
    call_error_service1 cctxt Services.Context.level block ()
  let next_level cctxt block =
    call_error_service1 cctxt Services.Context.next_level block ()

  module Nonce = struct

    type nonce_info = Services.Context.Nonce.nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten

    let get cctxt block level =
      call_error_service2 cctxt Services.Context.Nonce.get block level ()

    let hash cctxt block =
      call_error_service1 cctxt Services.Context.Nonce.hash block ()

  end

  module Key = struct

    let get cctxt block pk_h =
      call_error_service2 cctxt Services.Context.Key.get block pk_h ()

    let list cctxt block =
      call_error_service1 cctxt Services.Context.Key.list block ()

  end

  module Contract = struct
    let list cctxt b =
      call_error_service1 cctxt Services.Context.Contract.list b ()
    type info = Services.Context.Contract.info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t ;
      assets: Asset.Map.t ;
      counter: int32 ;
    }
    let get cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.get b c ()
    let balance cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.balance b c ()
    let manager cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.manager b c ()
    let delegate cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.delegate b c ()
    let counter cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.counter b c ()
    let spendable cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.spendable b c ()
    let delegatable cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.delegatable b c ()
    let script cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.script b c ()
    let assets cctxt b c =
      call_error_service2 cctxt Services.Context.Contract.assets b c ()
  end

end

module Helpers = struct

  let minimal_time cctxt block ?prio () =
    call_error_service1 cctxt Services.Helpers.minimal_timestamp block prio

  let typecheck_code cctxt =
    call_error_service1 cctxt Services.Helpers.typecheck_code

  let apply_operation cctxt block pred_block hash forged_operation signature =
    call_error_service1 cctxt Services.Helpers.apply_operation
      block (pred_block, hash, forged_operation, signature)

  let run_code cctxt block code (storage, input) =
    call_error_service1 cctxt Services.Helpers.run_code
      block (code, storage, input, None, None, None)

  let trace_code cctxt block code (storage, input) =
    call_error_service1 cctxt Services.Helpers.trace_code
      block (code, storage, input, None, None, None)

  let typecheck_data cctxt =
    call_error_service1 cctxt Services.Helpers.typecheck_data

  let hash_data cctxt =
    call_error_service1 cctxt Services.Helpers.hash_data

  let level cctxt block ?offset lvl =
    call_error_service2 cctxt Services.Helpers.level block lvl offset

  let levels cctxt block cycle =
    call_error_service2 cctxt Services.Helpers.levels block cycle ()

  module Rights = struct
    type slot = Raw_level.t * int * Time.t option
    let mining_rights_for_delegate cctxt
        b c ?max_priority ?first_level ?last_level () =
      call_error_service2 cctxt Services.Helpers.Rights.mining_rights_for_delegate
      b c (max_priority, first_level, last_level)
    let endorsement_rights_for_delegate cctxt
        b c ?max_priority ?first_level ?last_level () =
    call_error_service2 cctxt Services.Helpers.Rights.endorsement_rights_for_delegate
      b c (max_priority, first_level, last_level)
  end

  module Forge = struct

    let script_of_option = function
      | None -> Script.No_script
      | Some (code, storage) -> Script { code ; storage }

    open Operation

    module Manager = struct
      let operations cctxt
          block ~net ~source ?sourcePubKey ~counter ~fee operations =
        let ops =
          Manager_operations { source ; public_key = sourcePubKey ;
                               counter ; operations ; fee } in
        (call_error_service1 cctxt Services.Helpers.Forge.operations block
           ({net_id=net}, Sourced_operations ops))
      let transaction cctxt
          block ~net ~source ?sourcePubKey ~counter
          ~amount ~destination ?parameters ~fee ()=
        operations cctxt block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Transaction { amount ; parameters ; destination }]
      let origination cctxt
          block ~net
          ~source ?sourcePubKey ~counter
          ~managerPubKey ~balance
          ?(spendable = true)
          ?(delegatable = true)
          ?delegatePubKey ?script ~fee () =
        let script = script_of_option script in
        operations cctxt block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[
            Origination { manager = managerPubKey ;
                          delegate = delegatePubKey ;
                          script ;
                          spendable ;
                          delegatable ;
                          credit = balance }
          ]
      let issuance cctxt
          block ~net ~source ?sourcePubKey ~counter ~assetType ~quantity ~fee ()=
        operations cctxt block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Issuance { asset = assetType ; amount = quantity }]
      let delegation cctxt
          block ~net ~source ?sourcePubKey ~counter ~fee delegate =
        operations cctxt block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Delegation delegate]
    end
    module Delegate = struct
      let operations cctxt
          block ~net ~source operations =
        let ops = Delegate_operations { source ; operations } in
        (call_error_service1 cctxt Services.Helpers.Forge.operations block
           ({net_id=net}, Sourced_operations ops))
      let endorsement cctxt
          b ~net ~source ~block ~slot () =
        operations cctxt b ~net ~source
          Tezos_context.[Endorsement { block ; slot }]
    end
    module Anonymous = struct
      let operations cctxt block ~net operations =
        (call_error_service1 cctxt Services.Helpers.Forge.operations block
           ({net_id=net}, Anonymous_operations operations))
      let seed_nonce_revelation cctxt
          block ~net ~level ~nonce () =
        operations cctxt block ~net [Seed_nonce_revelation { level ; nonce }]
      let faucet cctxt
          block ~net ~id () =
        let nonce = Sodium.Random.Bigbytes.generate 16 in
        operations cctxt block ~net [Faucet { id ; nonce }]
    end
    let block cctxt
        block ~net ~predecessor ~timestamp ~fitness ~operations
        ~level ~priority ~seed_nonce_hash ~proof_of_work_nonce () =
      call_error_service1 cctxt Services.Helpers.Forge.block block
        (net, predecessor, timestamp, fitness, operations,
         level, priority, seed_nonce_hash, proof_of_work_nonce)
  end

  module Parse = struct
    let operations cctxt
        block ?check shell bytes =
      call_error_service1 cctxt Services.Helpers.Parse.operations block (shell, bytes, check)
  end

end
(* type slot = *)
      (* raw_level * int * timestamp option *)
    (* let mining_possibilities *)
        (* b c ?max_priority ?first_level ?last_level () = *)
      (* call_error_service2 Services.Helpers.Context.Contract.mining_possibilities *)
        (* b c (max_priority, first_level, last_level) *)
    (* (\* let endorsement_possibilities b c ?max_priority ?first_level ?last_level () = *\) *)
      (* call_error_service2 Services.Helpers.Context.Contract.endorsement_possibilities *)
        (* b c (max_priority, first_level, last_level) *)
