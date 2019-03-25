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

open Proto_alpha
module Proto_Nonce = Nonce (* Renamed otherwise is masked by Alpha_context *)
open Alpha_context

(* This type collects a block and the context that results from its application *)
type t = {
  hash : Block_hash.t ;
  header : Block_header.t ;
  operations : Operation.packed list ;
  context : Tezos_protocol_environment_memory.Context.t ;
}
type block = t

let rpc_context block = {
  Alpha_environment.Updater.block_hash = block.hash ;
  block_header = block.header.shell ;
  context = block.context ;
}

let rpc_ctxt =
  new Alpha_environment.proto_rpc_context_of_directory
    rpc_context Proto_alpha.rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

let get_next_baker_by_priority priority block =
  Alpha_services.Delegate.Baking_rights.get rpc_ctxt
    ~all:true
    ~max_priority:(priority+1) block >>=? fun bakers ->
  let { Alpha_services.Delegate.Baking_rights.delegate = pkh ;
        timestamp; _ } = List.find (fun { Alpha_services.Delegate.Baking_rights.priority = p ; _ } -> p = priority)  bakers in
  return (pkh, priority, Option.unopt_exn (Failure "") timestamp)

let get_next_baker_by_account pkh block =
  Alpha_services.Delegate.Baking_rights.get rpc_ctxt
    ~delegates:[pkh]
    ~max_priority:256 block >>=? fun bakers ->
  let { Alpha_services.Delegate.Baking_rights.delegate = pkh ;
        timestamp ; priority ; _ } = List.hd bakers in
  return (pkh, priority, Option.unopt_exn (Failure "") timestamp)

let get_next_baker_excluding excludes block =
  Alpha_services.Delegate.Baking_rights.get rpc_ctxt
    ~max_priority:256 block >>=? fun bakers ->
  let { Alpha_services.Delegate.Baking_rights.delegate = pkh ;
        timestamp ; priority ; _ } =
    List.find
      (fun { Alpha_services.Delegate.Baking_rights.delegate ; _ } ->
         not (List.mem delegate excludes))
      bakers in
  return (pkh, priority, Option.unopt_exn (Failure "") timestamp)

let dispatch_policy = function
  | By_priority p -> get_next_baker_by_priority p
  | By_account a -> get_next_baker_by_account a
  | Excluding al -> get_next_baker_excluding al

let get_next_baker ?(policy = By_priority 0) = dispatch_policy policy

module Forge = struct

  type header = {
    baker : public_key_hash ;   (* the signer of the block *)
    shell : Block_header.shell_header ;
    contents : Block_header.contents ;
  }

  let default_proof_of_work_nonce =
    MBytes.create Constants.proof_of_work_nonce_size

  let make_contents
      ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ~priority ~seed_nonce_hash () =
    Block_header.{ priority ;
                   proof_of_work_nonce ;
                   seed_nonce_hash }

  let make_shell
      ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.{
      level ;
      predecessor ;
      timestamp ;
      fitness ;
      operations_hash ;
      (* We don't care of the following values, only the shell validates them. *)
      proto_level = 0 ;
      validation_passes = 0 ;
      context = Context_hash.zero ;
    }

  let set_seed_nonce_hash seed_nonce_hash { baker ; shell ; contents } =
    { baker ; shell ; contents = { contents with seed_nonce_hash } }

  let set_baker baker header = { header with baker }

  let sign_header { baker ; shell ; contents } =
    Account.find baker >>=? fun delegate ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents) in
    let signature =
      Signature.sign ~watermark:Signature.(Block_header Chain_id.zero) delegate.sk unsigned_bytes in
    Block_header.{ shell ; protocol_data = { contents ; signature } } |>
    return

  let forge_header
      ?(policy = By_priority 0)
      ?(operations = []) pred =
    dispatch_policy policy pred >>=? fun (pkh, priority, timestamp) ->
    let level = Int32.succ pred.header.shell.level in
    begin
      match Fitness_repr.to_int64 pred.header.shell.fitness with
      | Ok old_fitness ->
          return (Fitness_repr.from_int64
                    (Int64.add (Int64.of_int 1) old_fitness))
      | Error _ -> assert false
    end >>=? fun fitness ->
    begin
      Alpha_services.Helpers.current_level ~offset:1l (rpc_ctxt) pred >>|? function
      | { expected_commitment = true ; _ } -> Some (fst (Proto_Nonce.generate ()))
      | { expected_commitment = false ; _ } -> None
    end >>=? fun seed_nonce_hash ->
    let hashes = List.map Operation.hash_packed operations in
    let operations_hash = Operation_list_list_hash.compute
        [Operation_list_hash.compute hashes] in
    let shell = make_shell ~level ~predecessor:pred.hash
        ~timestamp ~fitness ~operations_hash in
    let contents = make_contents ~priority ~seed_nonce_hash () in
    return { baker = pkh ; shell ; contents }

  (* compatibility only, needed by incremental *)
  let contents
      ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?(priority = 0) ?seed_nonce_hash () =
    {
      Block_header.priority ;
      proof_of_work_nonce ;
      seed_nonce_hash ;
    }

end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = [ "protocol_parameters" ]

let check_constants_consistency constants =
  let open Constants_repr in
  let { blocks_per_cycle ; blocks_per_commitment ;
        blocks_per_roll_snapshot ; _ } = constants in
  Error_monad.unless (blocks_per_commitment <= blocks_per_cycle)
    (fun () -> failwith "Inconsistent constants : blocks per commitment must be \
                         less than blocks per cycle") >>=? fun () ->
  Error_monad.unless (blocks_per_cycle >= blocks_per_roll_snapshot)
    (fun () -> failwith "Inconsistent constants : blocks per cycle \
                         must be superior than blocks per roll snapshot") >>=?
  return

let initial_context
    constants
    header
    commitments
    initial_accounts
    security_deposit_ramp_up_cycles
    no_reward_cycles
  =
  let bootstrap_accounts =
    List.map (fun (Account.{ pk ; pkh ; _ }, amount) ->
        Parameters_repr.{ public_key_hash = pkh ; public_key = Some pk ; amount }
      ) initial_accounts
  in
  let json =
    Data_encoding.Json.construct
      Parameters_repr.encoding
      Parameters_repr.{
        bootstrap_accounts ;
        bootstrap_contracts = [] ;
        commitments ;
        constants ;
        security_deposit_ramp_up_cycles ;
        no_reward_cycles ;
      }
  in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment_memory.Context.(
    set empty ["version"] (MBytes.of_string "genesis")
  ) >>= fun ctxt ->
  Tezos_protocol_environment_memory.Context.(
    set ctxt protocol_param_key proto_params
  ) >>= fun ctxt ->
  Main.init ctxt header
  >|= Alpha_environment.wrap_error >>=? fun { context; _ } ->
  return context

let genesis
    ?(preserved_cycles = Constants_repr.default.preserved_cycles)
    ?(blocks_per_cycle = Constants_repr.default.blocks_per_cycle)
    ?(blocks_per_commitment = Constants_repr.default.blocks_per_commitment)
    ?(blocks_per_roll_snapshot = Constants_repr.default.blocks_per_roll_snapshot)
    ?(blocks_per_voting_period = Constants_repr.default.blocks_per_voting_period)
    ?(time_between_blocks = Constants_repr.default.time_between_blocks)
    ?(endorsers_per_block = Constants_repr.default.endorsers_per_block)
    ?(hard_gas_limit_per_operation = Constants_repr.default.hard_gas_limit_per_operation)
    ?(hard_gas_limit_per_block = Constants_repr.default.hard_gas_limit_per_block)
    ?(proof_of_work_threshold = Int64.(neg one))
    ?(tokens_per_roll = Constants_repr.default.tokens_per_roll)
    ?(michelson_maximum_type_size = Constants_repr.default.michelson_maximum_type_size)
    ?(seed_nonce_revelation_tip = Constants_repr.default.seed_nonce_revelation_tip)
    ?(origination_size = Constants_repr.default.origination_size)
    ?(block_security_deposit = Constants_repr.default.block_security_deposit)
    ?(endorsement_security_deposit = Constants_repr.default.endorsement_security_deposit)
    ?(block_reward = Constants_repr.default.block_reward)
    ?(endorsement_reward = Constants_repr.default.endorsement_reward)
    ?(cost_per_byte = Constants_repr.default.cost_per_byte)
    ?(hard_storage_limit_per_operation = Constants_repr.default.hard_storage_limit_per_operation)
    ?(commitments = [])
    ?(security_deposit_ramp_up_cycles = None)
    ?(no_reward_cycles = None)
    (initial_accounts : (Account.t * Tez_repr.t) list)
  =
  if initial_accounts = [] then
    Pervasives.failwith "Must have one account with a roll to bake";

  (* Check there is at least one roll *)
  begin try
      let open Test_utils in
      fold_left_s (fun acc (_, amount) ->
          Alpha_environment.wrap_error @@
          Tez_repr.(+?) acc amount >>?= fun acc ->
          if acc >= tokens_per_roll then
            raise Exit
          else return acc
        ) Tez_repr.zero initial_accounts >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll"
    with Exit -> return_unit
  end >>=? fun () ->

  let constants : Constants_repr.parametric = {
    preserved_cycles ;
    blocks_per_cycle ;
    blocks_per_commitment ;
    blocks_per_roll_snapshot ;
    blocks_per_voting_period ;
    time_between_blocks ;
    endorsers_per_block ;
    hard_gas_limit_per_operation ;
    hard_gas_limit_per_block ;
    proof_of_work_threshold ;
    tokens_per_roll ;
    michelson_maximum_type_size ;
    seed_nonce_revelation_tip ;
    origination_size ;
    block_security_deposit ;
    endorsement_security_deposit ;
    block_reward ;
    endorsement_reward ;
    cost_per_byte ;
    hard_storage_limit_per_operation ;
  } in
  check_constants_consistency constants >>=? fun () ->

  let hash =
    Block_hash.of_b58check_exn "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let shell = Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp:Time.Protocol.epoch
      ~fitness: (Fitness_repr.from_int64 0L)
      ~operations_hash: Operation_list_list_hash.zero in
  let contents = Forge.make_contents
      ~priority:0
      ~seed_nonce_hash:None () in
  initial_context
    constants
    shell
    commitments
    initial_accounts
    security_deposit_ramp_up_cycles
    no_reward_cycles
  >>=? fun context ->
  let block =
    { hash ;
      header = {
        shell = shell ;
        protocol_data = {
          contents = contents ;
          signature = Signature.zero ;
        } ;
      };
      operations = [] ;
      context ;
    }
  in
  return block

(********* Baking *************)

let apply header ?(operations = []) pred =
  begin
    let open Alpha_environment.Error_monad in
    Proto_alpha.Main.begin_application
      ~chain_id: Chain_id.zero
      ~predecessor_context: pred.context
      ~predecessor_fitness: pred.header.shell.fitness
      ~predecessor_timestamp: pred.header.shell.timestamp
      header >>=? fun vstate ->
    fold_left_s
      (fun vstate op ->
         Proto_alpha.apply_operation vstate op >>=? fun (state, _result) ->
         return state)
      vstate operations >>=? fun vstate ->
    Proto_alpha.Main.finalize_block vstate >>=? fun (validation, _result) ->
    return validation.context
  end >|= Alpha_environment.wrap_error >>|? fun context ->
  let hash = Block_header.hash header in
  { hash ; header ; operations ; context }

let bake ?policy ?operation ?operations pred =
  let operations =
    match operation,operations with
    | Some op, Some ops -> Some (op::ops)
    | Some op, None -> Some [op]
    | None, Some ops -> Some ops
    | None, None -> None
  in
  Forge.forge_header ?policy ?operations pred >>=? fun header ->
  Forge.sign_header header >>=? fun header ->
  apply header ?operations pred

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants b =
  Alpha_services.Constants.all rpc_ctxt b

let bake_n ?policy n b =
  Error_monad.fold_left_s
    (fun b _ -> bake ?policy b) b (1 -- n)

let bake_until_cycle_end ?policy b =
  get_constants b >>=? fun Constants.{ parametric = { blocks_per_cycle ; _ } ; _ } ->
  let current_level = b.header.shell.level in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end ?policy n b =
  Error_monad.fold_left_s
    (fun b _ -> bake_until_cycle_end ?policy b) b (1 -- n)

let bake_until_cycle ?policy cycle (b:t) =
  get_constants b >>=? fun Constants.{ parametric = { blocks_per_cycle ; _ } ; _ } ->
  let rec loop (b:t) =
    let current_cycle =
      let current_level = b.header.shell.level in
      let current_cycle = Int32.div current_level blocks_per_cycle in
      current_cycle
    in
    if Int32.equal (Cycle.to_int32 cycle) current_cycle then
      return b
    else
      bake_until_cycle_end ?policy b >>=? fun b ->
      loop b
  in
  loop b
