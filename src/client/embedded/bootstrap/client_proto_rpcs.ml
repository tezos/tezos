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

let handle_error = function
  | Ok res -> Lwt.return res
  | Error exns ->
      pp_print_error Format.err_formatter exns ;
      Cli_entries.error "cannot continue"

type net = State.net_id = Net of Block_hash.t
type block = [
  | `Genesis
  | `Head of int | `Prevalidation
  | `Test_head of int | `Test_prevalidation
  | `Hash of Block_hash.t
]

let call_service1 s block a1 =
  Client_node_rpcs.call_service1
    (s Node_rpc_services.Blocks.proto_path) block a1
let call_error_service1 s block a1 =
  call_service1 s block a1 >|= wrap_error
let call_service2 s block a1 a2 =
  Client_node_rpcs.call_service2
    (s Node_rpc_services.Blocks.proto_path) block a1 a2
let call_error_service2 s block a1 a2 =
  call_service2 s block a1 a2 >|= wrap_error

module Constants = struct
  let bootstrap block = call_service1 Services.Constants.bootstrap block ()
  let errors block = call_service1 Services.Constants.errors block ()
  let cycle_length block =
    call_error_service1 Services.Constants.cycle_length block ()
  let voting_period_length block =
    call_error_service1 Services.Constants.voting_period_length block ()
  let time_before_reward block =
    call_error_service1 Services.Constants.time_before_reward block ()
  let time_between_slots block =
    call_error_service1 Services.Constants.time_between_slots block ()
  let first_free_mining_slot block =
    call_error_service1 Services.Constants.first_free_mining_slot block ()
  let max_signing_slot block =
    call_error_service1 Services.Constants.max_signing_slot block ()
  let instructions_per_transaction block =
    call_error_service1 Services.Constants.instructions_per_transaction block ()
  let stamp_threshold block =
    call_error_service1 Services.Constants.proof_of_work_threshold block ()
end

module Context = struct

  let level block = call_error_service1 Services.Context.level block ()
  let next_level block = call_error_service1 Services.Context.next_level block ()

  module Nonce = struct

    type nonce_info = Services.Context.Nonce.nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten

    let get block level =
      call_error_service2 Services.Context.Nonce.get block level ()

    let hash block =
      call_error_service1 Services.Context.Nonce.hash block ()

  end

  module Key = struct

    let get block pk_h =
      call_error_service2 Services.Context.Key.get block pk_h ()

    let list block =
      call_error_service1 Services.Context.Key.list block ()

  end

  module Contract = struct
    let list b =
      call_error_service1 Services.Context.Contract.list b ()
    type info = Services.Context.Contract.info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t ;
      assets: Asset.Map.t ;
      counter: int32 ;
    }
    let get b c =
      call_error_service2 Services.Context.Contract.get b c ()
    let balance b c =
      call_error_service2 Services.Context.Contract.balance b c ()
    let manager b c =
      call_error_service2 Services.Context.Contract.manager b c ()
    let delegate b c =
      call_error_service2 Services.Context.Contract.delegate b c ()
    let counter b c =
      call_error_service2 Services.Context.Contract.counter b c ()
    let spendable b c =
      call_error_service2 Services.Context.Contract.spendable b c ()
    let delegatable b c =
      call_error_service2 Services.Context.Contract.delegatable b c ()
    let script b c =
      call_error_service2 Services.Context.Contract.script b c ()
    let assets b c =
      call_error_service2 Services.Context.Contract.assets b c ()
  end

end

module Helpers = struct

  let minimal_time block ?prio () =
    call_error_service1 Services.Helpers.minimal_timestamp block prio

  let typecheck_code = call_error_service1 Services.Helpers.typecheck_code

  let typecheck_tagged_data = call_error_service1 Services.Helpers.typecheck_tagged_data

  let typecheck_untagged_data = call_error_service1 Services.Helpers.typecheck_untagged_data

  let hash_data = call_error_service1 Services.Helpers.hash_data

  let level block ?offset lvl =
    call_error_service2 Services.Helpers.level block lvl offset

  let levels block cycle =
    call_error_service2 Services.Helpers.levels block cycle ()

  module Rights = struct
    type slot = Raw_level.t * int * Time.t option
    let mining_rights_for_delegate
        b c ?max_priority ?first_level ?last_level () =
      call_error_service2 Services.Helpers.Rights.mining_rights_for_delegate
      b c (max_priority, first_level, last_level)
    let endorsement_rights_for_delegate
        b c ?max_priority ?first_level ?last_level () =
    call_error_service2 Services.Helpers.Rights.endorsement_rights_for_delegate
      b c (max_priority, first_level, last_level)
  end

  module Forge = struct

    let script_of_option = function
      | None -> Script.No_script
      | Some (code, storage) -> Script { code ; storage }

    open Operation

    module Manager = struct
      let operations
          block ~net ~source ?sourcePubKey ~counter ~fee operations =
        let ops =
          Manager_operations { source ; public_key = sourcePubKey ;
                               counter ; operations ; fee } in
        (call_error_service1 Services.Helpers.Forge.operations block
           ({net_id=net}, Sourced_operations ops))
        >>=? fun (bytes, contracts) ->
        return (bytes, match contracts with None -> [] | Some l -> l)
      let transaction
          block ~net ~source ?sourcePubKey ~counter
          ~amount ~destination ?parameters ~fee ()=
        operations block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Transaction { amount ; parameters ; destination }]
        >>=? fun (bytes, contracts) ->
        assert (contracts = []) ;
        return bytes
      let origination
          block ~net
          ~source ?sourcePubKey ~counter
          ~managerPubKey ~balance
          ?(spendable = true)
          ?(delegatable = true)
          ?delegatePubKey ?script ~fee () =
        let script = script_of_option script in
        operations block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[
            Origination { manager = managerPubKey ;
                          delegate = delegatePubKey ;
                          script ;
                          spendable ;
                          delegatable ;
                          credit = balance }
          ]
        >>=? fun (bytes, contracts) ->
        match contracts with
        | [contract] -> return (contract, bytes)
        | _ -> assert false
      let issuance
          block ~net ~source ?sourcePubKey ~counter ~assetType ~quantity ~fee ()=
        operations block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Issuance { asset = assetType ; amount = quantity }]
        >>=? fun (bytes, contracts) ->
        assert (contracts = []) ;
        return bytes
      let delegation
          block ~net ~source ?sourcePubKey ~counter ~fee delegate =
        operations block ~net ~source ?sourcePubKey ~counter ~fee
          Tezos_context.[Delegation delegate]
        >>=? fun (bytes, contracts) ->
        assert (contracts = []) ;
        return bytes
    end
    module Delegate = struct
      let operations
          block ~net ~source operations =
        let ops = Delegate_operations { source ; operations } in
        (call_error_service1 Services.Helpers.Forge.operations block
           ({net_id=net}, Sourced_operations ops))
        >>=? fun (hash, _contracts) ->
        return hash
      let endorsement b ~net ~source ~block ~slot () =
        operations b ~net ~source
          Tezos_context.[Endorsement { block ; slot }]
    end
    module Anonymous = struct
      let operations block ~net operations =
        (call_error_service1 Services.Helpers.Forge.operations block
           ({net_id=net}, Anonymous_operations operations))
        >>=? fun (hash, _contracts) ->
        return hash
      let seed_nonce_revelation
          block ~net ~level ~nonce () =
        operations block ~net [Seed_nonce_revelation { level ; nonce }]
    end
    let block
        block ~net ~predecessor ~timestamp ~fitness ~operations
        ~level ~priority ~seed_nonce_hash ~proof_of_work_nonce () =
      call_error_service1 Services.Helpers.Forge.block block
        (net, predecessor, timestamp, fitness, operations,
         level, priority, seed_nonce_hash, proof_of_work_nonce)
  end

  module Parse = struct
    let operations block ?check shell bytes =
      call_error_service1 Services.Helpers.Parse.operations block (shell, bytes, check)
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
