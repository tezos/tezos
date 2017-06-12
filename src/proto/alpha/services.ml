(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding
open Tezos_context

let error_encoding =
  let open Data_encoding in
  describe
    ~description:
      "The full list of error is available with \
       the global RPC `/errors`"
    (conv
       (fun exn -> `A (List.map json_of_error exn))
       (function `A exns -> List.map error_of_json exns | _ -> [])
       json)

let wrap_tzerror encoding =
  let open Data_encoding in
  union [
    case
      (obj1 (req "ok" encoding))
      (function Ok x -> Some x | _ -> None)
      (fun x -> Ok x) ;
    case
      (obj1 (req "error" error_encoding))
      (function Error x -> Some x | _ -> None)
      (fun x -> Error x) ;
  ]


let operations custom_root =
  RPC.service
    ~description: "All the operations of the block (fully decoded)."
    ~input: empty
    ~output: (wrap_tzerror @@
              (list (list (dynamic_size Operation.encoding))))
    RPC.Path.(custom_root / "operations")

let header custom_root =
  RPC.service
    ~description: "The header of the block (fully decoded)."
    ~input: empty
    ~output: (wrap_tzerror Block_header.encoding)
    RPC.Path.(custom_root / "header")

module Header = struct

  let priority custom_root =
    RPC.service
      ~description: "Mining priority of the block."
      ~input: empty
      ~output: (wrap_tzerror uint16)
      RPC.Path.(custom_root / "header" / "priority")

  let seed_nonce_hash custom_root =
    RPC.service
      ~description: "Hash of the seed nonce of the block."
      ~input: empty
      ~output: (wrap_tzerror Nonce_hash.encoding)
      RPC.Path.(custom_root / "header" / "seed_nonce_hash")

end

module Constants = struct

  let cycle_length custom_root =
    RPC.service
      ~description: "Cycle length"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "cycle length" int32)
      RPC.Path.(custom_root / "constants" / "cycle_length")

  let voting_period_length custom_root =
    RPC.service
      ~description: "Length of the voting period"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "voting period length" int32)
      RPC.Path.(custom_root / "constants" / "voting_period_length")

  let time_before_reward custom_root =
    RPC.service
      ~description: "Time before reward"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "time before reward" Period.encoding)
      RPC.Path.(custom_root / "constants" / "time_before_reward")

  let slot_durations custom_root =
    RPC.service
      ~description: "Slot durations"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "time between slots" (list Period.encoding))
      RPC.Path.(custom_root / "constants" / "time_between_slots")

  let first_free_mining_slot custom_root =
    RPC.service
      ~description: "First free mining slot"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "first free mining slot" uint16)
      RPC.Path.(custom_root / "constants" / "first_free_mining_slot")

  let max_signing_slot custom_root =
    RPC.service
      ~description: "Max signing slot"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "max signing slot" uint16)
      RPC.Path.(custom_root / "constants" / "max_signing_slot")

  let instructions_per_transaction custom_root =
    RPC.service
      ~description: "Instructions per transaction"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "instructions per transaction" int31)
      RPC.Path.(custom_root / "constants" / "instructions_per_transaction")

  let proof_of_work_threshold custom_root =
    RPC.service
      ~description: "Stamp threshold"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "proof_of_work threshold" int64)
      RPC.Path.(custom_root / "constants" / "proof_of_work_threshold")

  let errors custom_root =
    RPC.service
      ~description: "Schema for all the RPC errors from this protocol version"
      ~input: empty
      ~output: json_schema
      RPC.Path.(custom_root / "constants" / "errors")

end

module Context = struct

  let level custom_root =
    RPC.service
      ~description: "Detailled level information for the current block"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "detailled level info" Level.encoding)
      RPC.Path.(custom_root / "context" / "level")

  let next_level custom_root =
    RPC.service
      ~description: "Detailled level information for the next block"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "detailled level info" Level.encoding)
      RPC.Path.(custom_root / "context" / "next_level")

  let voting_period_kind custom_root =
    RPC.service
      ~description: "Voting period kind for the current block"
      ~input: empty
      ~output:
        (wrap_tzerror @@
          (obj1
            (req "voting_period_kind" Voting_period.kind_encoding)))
      RPC.Path.(custom_root / "context" / "voting_period_kind")


  module Nonce = struct

    type nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten

    let nonce_encoding =
      union [
        case
          (obj1 (req "nonce" Nonce.encoding))
          (function Revealed nonce -> Some nonce | _ -> None)
          (fun nonce -> Revealed nonce) ;
        case
          (obj1 (req "hash" Nonce_hash.encoding))
          (function Missing nonce -> Some nonce | _ -> None)
          (fun nonce -> Missing nonce) ;
        case
          empty
          (function Forgotten -> Some () | _ -> None)
          (fun () -> Forgotten) ;
      ]

  let get custom_root =
    RPC.service
      ~description: "Info about the nonce of a previous block."
      ~input: empty
      ~output: (wrap_tzerror nonce_encoding)
      RPC.Path.(custom_root / "context" / "nonce" /: Raw_level.arg)

  let hash custom_root =
    RPC.service
      ~description: "Hash of the current block's nonce."
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "nonce hash" Nonce_hash.encoding)
      RPC.Path.(custom_root / "context" / "nonce")

  end

  module Key = struct

    let public_key_hash_arg =
      let construct = Ed25519.Public_key_hash.to_b58check in
      let destruct hash =
        match Ed25519.Public_key_hash.of_b58check_opt hash with
        | None -> Error "Cannot parse public key hash"
        | Some public_key_hash -> Ok public_key_hash in
      RPC.Arg.make
        ~descr:"A public key hash"
        ~name: "public_key_hash"
        ~construct
        ~destruct
        ()

    let pk_encoding =
      (obj2
         (req "hash" Ed25519.Public_key_hash.encoding)
         (req "public_key" Ed25519.Public_key.encoding))

    let list custom_root =
      RPC.service
        ~description: "List the known public keys"
        ~input: empty
        ~output: (wrap_tzerror @@ list pk_encoding)
        RPC.Path.(custom_root / "context" / "keys")

    let get custom_root =
      RPC.service
        ~description: "Fetch the stored public key"
        ~input: empty
        ~output: (wrap_tzerror @@ pk_encoding)
        RPC.Path.(custom_root / "context" / "keys" /: public_key_hash_arg )

  end

  (*-- Contracts ---------------------------------------------------------------*)

  module Contract = struct

    let balance custom_root =
      RPC.service
        ~description: "Access the balance of a contract."
        ~input: empty
        ~output: (wrap_tzerror Tez.encoding)
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "balance")

    let manager custom_root =
      RPC.service
        ~description: "Access the manager of a contract."
        ~input: empty
        ~output: (wrap_tzerror Ed25519.Public_key_hash.encoding)
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "manager")

    let delegate custom_root =
      RPC.service
        ~description: "Access the delegate of a contract, if any."
        ~input: empty
        ~output: (wrap_tzerror (option Ed25519.Public_key_hash.encoding))
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "delegate")

    let counter custom_root =
      RPC.service
        ~description: "Access the counter of a contract, if any."
        ~input: empty
        ~output: (wrap_tzerror int32)
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "counter")

    let spendable custom_root =
      RPC.service
        ~description: "Tells if the contract tokens can be spent by the manager."
        ~input: empty
        ~output: (wrap_tzerror bool)
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "spendable")

    let delegatable custom_root =
      RPC.service
        ~description: "Tells if the contract delegate can be changed."
        ~input: empty
        ~output: (wrap_tzerror bool)
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "delegatable")

    let script custom_root =
      RPC.service
        ~description: "Access the code and data of the contract."
        ~input: empty
        ~output: (wrap_tzerror (option Script.encoding))
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg / "script")

    type info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t option ;
      counter: int32 ;
    }

    let get custom_root =
      RPC.service
        ~description: "Access the complete status of a contract."
        ~input: empty
        ~output:
          (wrap_tzerror @@
           conv
             (fun {manager;balance;spendable;delegate;script;counter} ->
                (manager,balance,spendable,delegate,script,counter))
             (fun (manager,balance,spendable,delegate,script,counter) ->
                {manager;balance;spendable;delegate;script;counter}) @@
           obj6
             (req "manager" Ed25519.Public_key_hash.encoding)
             (req "balance" Tez.encoding)
             (req "spendable" bool)
             (req "delegate" @@ obj2
                (req "setable" bool)
                (opt "value" Ed25519.Public_key_hash.encoding))
             (opt "script" Script.encoding)
             (req "counter" int32))
        RPC.Path.(custom_root / "context" / "contracts" /: Contract.arg)

    let list custom_root =
      RPC.service
        ~description:
          "All existing contracts (including non-empty default contracts)."
        ~input: empty
        ~output: (wrap_tzerror @@ list Contract.encoding)
        RPC.Path.(custom_root / "context" / "contracts")

  end

end

(*-- Helpers ----------------------------------------------------------------*)

module Helpers = struct

  let minimal_timestamp custom_root =
    RPC.service
      ~description: "Minimal timestamp for the next block."
      ~input: (obj1 (opt "priority" int31))
      ~output: (wrap_tzerror @@
                obj1 (req "timestamp" Timestamp.encoding))
      RPC.Path.(custom_root / "helpers" / "minimal_timestamp")

  let run_code_input_encoding =
    (obj6
       (req "script" Script.code_encoding)
       (req "storage" Script.expr_encoding)
       (req "input" Script.expr_encoding)
       (opt "amount" Tez.encoding)
       (opt "contract" Contract.encoding)
       (opt "origination_nonce" Contract.origination_nonce_encoding))

  let run_code custom_root =
    RPC.service
      ~description: "Run a piece of code in the current context"
      ~input: run_code_input_encoding
      ~output: (wrap_tzerror
                  (obj2
                     (req "storage" Script.expr_encoding)
                     (req "output" Script.expr_encoding)))
      RPC.Path.(custom_root / "helpers" / "run_code")

  let apply_operation custom_root =
    RPC.service
      ~description: "Applies an operation in the current context"
      ~input: (obj4
                 (req "pred_block" Block_hash.encoding)
                 (req "operation_hash" Operation_hash.encoding)
                 (req "forged_operation" bytes)
                 (opt "signature" Ed25519.Signature.encoding))
      ~output: (wrap_tzerror
                  (obj1 (req "contracts" (list Contract.encoding))))
      RPC.Path.(custom_root / "helpers" / "apply_operation")


  let trace_code custom_root =
    RPC.service
      ~description: "Run a piece of code in the current context, \
                     keeping a trace"
      ~input: run_code_input_encoding
      ~output: (wrap_tzerror
                  (obj3
                     (req "storage" Script.expr_encoding)
                     (req "output" Script.expr_encoding)
                     (req "trace"
                        (list @@ obj3
                           (req "location" Script.location_encoding)
                           (req "gas" int31)
                           (req "stack" (list (Script.expr_encoding)))))))
      RPC.Path.(custom_root / "helpers" / "trace_code")

  let typecheck_code custom_root =
    RPC.service
      ~description: "Typecheck a piece of code in the current context"
      ~input: Script.code_encoding
      ~output: (wrap_tzerror Script_ir_translator.type_map_enc)
      RPC.Path.(custom_root / "helpers" / "typecheck_code")

  let typecheck_data custom_root =
    RPC.service
      ~description: "Check that some data expression is well formed \
                     and of a given type in the current context"
      ~input: (obj2
                 (req "data" Script.expr_encoding)
                 (req "type" Script.expr_encoding))
      ~output: (wrap_tzerror empty)
      RPC.Path.(custom_root / "helpers" / "typecheck_data")

  let hash_data custom_root =
    RPC.service
      ~description: "Computes the hash of some data expression \
                     using the same algorithm as script instruction H"
      ~input: (obj1 (req "data" Script.expr_encoding))
      ~output: (wrap_tzerror @@
                obj1 (req "hash" string))
      RPC.Path.(custom_root / "helpers" / "hash_data")

  let level custom_root =
    RPC.service
      ~description: "..."
      ~input: (obj1
                 (opt "offset" int32))
      ~output: (wrap_tzerror @@
                describe ~title: "block level and cycle information" Level.encoding)
      RPC.Path.(custom_root / "helpers" / "level" /: Raw_level.arg)

  let levels custom_root =
    RPC.service
      ~description: "Levels of a cycle"
      ~input: empty
      ~output: (wrap_tzerror @@
                describe ~title: "levels of a cycle"
                  (obj2
                     (req "first" Raw_level.encoding)
                     (req "last" Raw_level.encoding)))
      RPC.Path.(custom_root / "helpers" / "levels" /: Cycle.arg)

  module Rights = struct

    let slots_range_encoding =
      (obj3
         (opt "max_priority" int31)
         (opt "first_level" Raw_level.encoding)
         (opt "last_level" Raw_level.encoding))

    let endorsement_slot_encoding =
      (obj2
         (req "level" Raw_level.encoding)
         (req "priority" int31))

    let mining_slot_encoding =
      (obj3
         (req "level" Raw_level.encoding)
         (req "priority" int31)
         (req "timestamp" Timestamp.encoding))

    let mining_rights custom_root =
      RPC.service
        ~description:
          "List gelegates allowed to mine for the next level, \
           ordered by priority."
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (wrap_tzerror @@
                  obj2
                    (req "level" Raw_level.encoding)
                    (req "mining_rights"
                       (list
                          (obj2
                             (req "delegate" Ed25519.Public_key_hash.encoding)
                             (req "timestamp" Timestamp.encoding)))))
        RPC.Path.(custom_root / "helpers" / "rights" / "mining")

    let mining_rights_for_level custom_root =
      RPC.service
        ~description:
          "List delegate allowed to mine for a given level, \
           ordered by priority."
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (wrap_tzerror @@
                  obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "mining" / "level" /: Raw_level.arg )

    let mining_levels custom_root =
      RPC.service
        ~description:
          "List level for which we might computed mining rights."
        ~input: empty
        ~output: (wrap_tzerror @@
                  obj1 (req "levels" (list Raw_level.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "mining" / "level"  )

    let mining_rights_for_delegate custom_root =
      RPC.service
        ~description: "Future mining rights for a given delegate."
        ~input: slots_range_encoding
        ~output: (wrap_tzerror (Data_encoding.list mining_slot_encoding))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "mining" / "delegate" /: Context.Key.public_key_hash_arg )

    let mining_delegates custom_root =
      RPC.service
        ~description:
          "List delegates with mining rights."
        ~input: empty
        ~output: (wrap_tzerror @@
                  obj1 (req "delegates"
                          (list Ed25519.Public_key_hash.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "mining" / "delegate"  )

    let endorsement_rights custom_root =
      RPC.service
        ~description:
          "List delegates allowed to endorse for the current block."
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (wrap_tzerror @@
                  obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights" / "endorsement")

    let endorsement_rights_for_level custom_root =
      RPC.service
        ~description:
          "List delegates allowed to endorse blocks for a given level."
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (wrap_tzerror @@
                  obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "endorsement" / "level" /: Raw_level.arg )

    let endorsement_levels custom_root =
      RPC.service
        ~description:
          "List level for which we might computed endorsement rights."
        ~input: empty
        ~output: (wrap_tzerror @@
                  obj1 (req "levels" (list Raw_level.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "endorsement" / "level"  )

    let endorsement_rights_for_delegate custom_root =
      RPC.service
        ~description: "Compute endorsement rights for a given delegate."
        ~input: slots_range_encoding
        ~output: (wrap_tzerror @@ Data_encoding.list endorsement_slot_encoding)
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "endorsement" / "delegate" /: Context.Key.public_key_hash_arg )

    let endorsement_delegates custom_root =
      RPC.service
        ~description:
          "List delegates with endorsement rights."
        ~input: empty
        ~output: (wrap_tzerror @@
                  obj1 (req "delegates"
                          (list Ed25519.Public_key_hash.encoding)))
        RPC.Path.(custom_root / "helpers" / "rights"
                  / "endorsement" / "delegate"  )

  end

  module Forge = struct

    let operations custom_root =
      RPC.service
        ~description:"Forge an operation"
        ~input: Operation.unsigned_operation_encoding
        ~output:
          (wrap_tzerror @@
           (obj1
              (req "operation" @@
               describe ~title: "hex encoded operation" bytes)))
        RPC.Path.(custom_root / "helpers" / "forge" / "operations" )

    let empty_proof_of_work_nonce =
      MBytes.of_string
        (String.make Constants_repr.proof_of_work_nonce_size  '\000')

    let block_proto_header custom_root =
      RPC.service
        ~description: "Forge the protocol-specific part of a block header"
        ~input:
          (obj3
             (req "priority" uint16)
             (req "nonce_hash" Nonce_hash.encoding)
             (dft "proof_of_work_nonce"
                (Fixed.bytes
                   Tezos_context.Constants.proof_of_work_nonce_size)
                empty_proof_of_work_nonce))
        ~output: (wrap_tzerror bytes)
        RPC.Path.(custom_root / "helpers" / "forge" / "block_proto_header")

  end

  module Parse = struct

    let operations custom_root =
      RPC.service
        ~description:"Parse operations"
        ~input:
          (obj2
             (req "operations" (list (dynamic_size Operation.raw_encoding)))
             (opt "check_signature" bool))
        ~output:
          (wrap_tzerror (list (dynamic_size Operation.encoding)))
        RPC.Path.(custom_root / "helpers" / "parse" / "operations" )

    let block custom_root =
      RPC.service
        ~description:"Parse a block"
        ~input: Block_header.raw_encoding
        ~output: (wrap_tzerror Block_header.proto_header_encoding)
        RPC.Path.(custom_root / "helpers" / "parse" / "block" )

  end

end
