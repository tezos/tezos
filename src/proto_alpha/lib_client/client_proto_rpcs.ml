(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

let make_call1 cctxt s=
  RPC_context.make_call1 (s (Block_services.S.proto_path ())) cctxt
let make_call2 cctxt s =
  RPC_context.make_call2 (s (Block_services.S.proto_path ())) cctxt
let make_call3 cctxt s =
  RPC_context.make_call3 (s (Block_services.S.proto_path ())) cctxt

let make_opt_call2 cctxt s block a1 q i =
  make_call2 cctxt s block a1 q i >>= function
  | Ok v -> return (Some v)
  | Error [RPC_context.Not_found _] -> return None
  | Error _ as err -> Lwt.return err

type block = Block_services.block

let header cctxt block =
  make_call1 cctxt Services.header block () ()

module Header = struct
  let priority cctxt block =
    make_call1 cctxt Services.Header.priority block () ()
  let seed_nonce_hash cctxt block =
    make_call1 cctxt Services.Header.seed_nonce_hash block () ()
end

module Constants = struct
  let errors cctxt block =
    make_call1 cctxt Services.Constants.errors block () ()
  let cycle_length cctxt block =
    make_call1 cctxt Services.Constants.cycle_length block () ()
  let voting_period_length cctxt block =
    make_call1 cctxt Services.Constants.voting_period_length block () ()
  let time_before_reward cctxt block =
    make_call1 cctxt Services.Constants.time_before_reward block () ()
  let slot_durations cctxt block =
    make_call1 cctxt Services.Constants.slot_durations block () ()
  let first_free_baking_slot cctxt block =
    make_call1 cctxt Services.Constants.first_free_baking_slot block () ()
  let max_signing_slot cctxt block =
    make_call1 cctxt Services.Constants.max_signing_slot block () ()
  let instructions_per_transaction cctxt block =
    make_call1 cctxt Services.Constants.max_gas block () ()
  let stamp_threshold cctxt block =
    make_call1 cctxt Services.Constants.proof_of_work_threshold block () ()
end

module Context = struct

  let level cctxt block =
    make_call1 cctxt Services.Context.level block () ()

  let next_level cctxt block =
    make_call1 cctxt Services.Context.next_level block () ()

  let voting_period_kind cctxt block =
    make_call1 cctxt Services.Context.voting_period_kind block () ()

  module Nonce = struct

    type nonce_info = Services.Context.Nonce.nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten

    let get cctxt block level =
      make_call2 cctxt Services.Context.Nonce.get block level () ()

    let hash cctxt block =
      make_call1 cctxt Services.Context.Nonce.hash block () ()

  end

  module Key = struct

    let get cctxt block pk_h =
      make_call2 cctxt Services.Context.Key.get block pk_h () ()

    let list cctxt block =
      make_call1 cctxt Services.Context.Key.list block () ()

  end

  module Contract = struct
    let list cctxt b =
      make_call1 cctxt Services.Context.Contract.list b () ()
    type info = Services.Context.Contract.info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t option ;
      counter: int32 ;
    }
    let get cctxt b c =
      make_call2 cctxt Services.Context.Contract.get b c () ()
    let balance cctxt b c =
      make_call2 cctxt Services.Context.Contract.balance b c () ()
    let manager cctxt b c =
      make_call2 cctxt Services.Context.Contract.manager b c () ()
    let delegate cctxt b c =
      make_opt_call2 cctxt Services.Context.Contract.delegate b c () ()
    let counter cctxt b c =
      make_call2 cctxt Services.Context.Contract.counter b c () ()
    let spendable cctxt b c =
      make_call2 cctxt Services.Context.Contract.spendable b c () ()
    let delegatable cctxt b c =
      make_call2 cctxt Services.Context.Contract.delegatable b c () ()
    let script cctxt b c =
      make_opt_call2 cctxt Services.Context.Contract.script b c () ()
    let storage cctxt b c =
      make_opt_call2 cctxt Services.Context.Contract.storage b c () ()
  end

end

module Helpers = struct

  let minimal_time cctxt block ?prio () =
    make_call1 cctxt Services.Helpers.minimal_timestamp block () prio

  let typecheck_code cctxt block =
    make_call1 cctxt Services.Helpers.typecheck_code block ()

  let apply_operation cctxt block pred_block hash forged_operation signature =
    make_call1 cctxt Services.Helpers.apply_operation
      block () (pred_block, hash, forged_operation, signature)

  let run_code cctxt block code (storage, input, amount) =
    make_call1 cctxt Services.Helpers.run_code
      block () (code, storage, input, amount, None, None)

  let trace_code cctxt block code (storage, input, amount) =
    make_call1 cctxt Services.Helpers.trace_code
      block () (code, storage, input, amount, None, None)

  let typecheck_data cctxt block =
    make_call1 cctxt Services.Helpers.typecheck_data block ()

  let hash_data cctxt block =
    make_call1 cctxt Services.Helpers.hash_data block ()

  let level cctxt block ?offset lvl =
    make_call2 cctxt Services.Helpers.level block lvl () offset

  let levels cctxt block cycle =
    make_call2 cctxt Services.Helpers.levels block cycle () ()

  module Rights = struct
    type baking_slot = Raw_level.t * int * Time.t
    type endorsement_slot = Raw_level.t * int
    let baking_rights_for_delegate cctxt
        b c ?max_priority ?first_level ?last_level () =
      make_call2 cctxt Services.Helpers.Rights.baking_rights_for_delegate
        b c () (max_priority, first_level, last_level)
    let endorsement_rights_for_delegate cctxt
        b c ?max_priority ?first_level ?last_level () =
      make_call2 cctxt Services.Helpers.Rights.endorsement_rights_for_delegate
        b c () (max_priority, first_level, last_level)
  end

  module Forge = struct

    module Manager = struct
      let operations cctxt
          block ~branch ~source ?sourcePubKey ~counter ~fee operations =
        let ops =
          Manager_operations { source ; public_key = sourcePubKey ;
                               counter ; operations ; fee } in
        (make_call1 cctxt Services.Helpers.Forge.operations block
           () ({ branch }, Sourced_operations ops))
      let transaction cctxt
          block ~branch ~source ?sourcePubKey ~counter
          ~amount ~destination ?parameters ~fee ()=
        operations cctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          Alpha_context.[Transaction { amount ; parameters ; destination }]
      let origination cctxt
          block ~branch
          ~source ?sourcePubKey ~counter
          ~managerPubKey ~balance
          ?(spendable = true)
          ?(delegatable = true)
          ?delegatePubKey ?script ~fee () =
        operations cctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          Alpha_context.[
            Origination { manager = managerPubKey ;
                          delegate = delegatePubKey ;
                          script ;
                          spendable ;
                          delegatable ;
                          credit = balance }
          ]
      let delegation cctxt
          block ~branch ~source ?sourcePubKey ~counter ~fee delegate =
        operations cctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          Alpha_context.[Delegation delegate]
    end
    module Delegate = struct
      let operations cctxt
          block ~branch ~source operations =
        let ops = Delegate_operations { source ; operations } in
        (make_call1 cctxt Services.Helpers.Forge.operations block
           () ({ branch }, Sourced_operations ops))
      let endorsement cctxt
          b ~branch ~source ~block ~slot () =
        operations cctxt b ~branch ~source
          Alpha_context.[Endorsement { block ; slot }]
      let proposals cctxt
          b ~branch ~source ~period ~proposals () =
        operations cctxt b ~branch ~source
          Alpha_context.[Proposals { period ; proposals }]
      let ballot cctxt
          b ~branch ~source ~period ~proposal ~ballot () =
        operations cctxt b ~branch ~source
          Alpha_context.[Ballot { period ; proposal ; ballot }]
    end
    module Dictator = struct
      let operation cctxt
          block ~branch operation =
        let op = Dictator_operation operation in
        (make_call1 cctxt Services.Helpers.Forge.operations block
           () ({ branch }, Sourced_operations op))
      let activate cctxt
          b ~branch hash =
        operation cctxt b ~branch (Activate hash)
      let activate_testnet cctxt
          b ~branch hash =
        operation cctxt b ~branch (Activate_testnet hash)
    end
    module Anonymous = struct
      let operations cctxt block ~branch operations =
        (make_call1 cctxt Services.Helpers.Forge.operations block
           () ({ branch }, Anonymous_operations operations))
      let seed_nonce_revelation cctxt
          block ~branch ~level ~nonce () =
        operations cctxt block ~branch [Seed_nonce_revelation { level ; nonce }]
      let faucet cctxt
          block ~branch ~id () =
        let nonce = Rand.generate 16 in
        operations cctxt block ~branch [Faucet { id ; nonce }]
    end
    let empty_proof_of_work_nonce =
      MBytes.of_string
        (String.make Constants_repr.proof_of_work_nonce_size  '\000')
    let block_proto_header cctxt
        block
        ~priority ~seed_nonce_hash
        ?(proof_of_work_nonce = empty_proof_of_work_nonce) () =
      make_call1 cctxt Services.Helpers.Forge.block_proto_header
        block () (priority, seed_nonce_hash, proof_of_work_nonce)
  end

  module Parse = struct
    let operations cctxt block ?check operations =
      make_call1 cctxt
        Services.Helpers.Parse.operations block () (operations, check)
    let block cctxt block shell proto =
      make_call1 cctxt
        Services.Helpers.Parse.block block
        () ({ shell ; proto } : Block_header.raw)
  end

end
(* type slot = *)
(* raw_level * int * timestamp option *)
(* let baking_possibilities *)
(* b c ?max_priority ?first_level ?last_level () = *)
(* make_call2 Services.Helpers.Context.Contract.baking_possibilities *)
(* b c (max_priority, first_level, last_level) *)
(* (\* let endorsement_possibilities b c ?max_priority ?first_level ?last_level () = *\) *)
(* make_call2 Services.Helpers.Context.Contract.endorsement_possibilities *)
(* b c (max_priority, first_level, last_level) *)
