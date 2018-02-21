(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Error_monad

type shell_header = Block_header.shell_header
type tezos_header = Block_header.t
type protocol_data = Proto_alpha.Alpha_context.Block_header.protocol_data
type operation_header = Operation.shell_header

type init_block = {
  pred_block_hash : Block_hash.t ;
  pred_shell_header : shell_header ;
  protocol_data : protocol_data ;
  op_header : operation_header ;
  sourced_operations : (Proto_alpha.Main.operation * Helpers_account.t) list ;
  operation_hashs : Operation_hash.t list ;
  protocol_data_bytes : MBytes.t ;
  timestamp : Time.t ;
  level : Int32.t ;
  context : Tezos_protocol_environment_memory.Context.t
}

type result = {
  tezos_header : tezos_header ;
  hash : Block_hash.t ;
  level : Int32.t ;
  validation : Tezos_protocol_environment_memory.validation_result ;
  tezos_context : Proto_alpha.Alpha_context.t
}

let get_op_header_res (res : result) : operation_header = {
  branch = res.hash
}

let get_protocol_data priority : protocol_data = {
  priority ;
  proof_of_work_nonce = Helpers_crypto.generate_proof_of_work_nonce ();
  seed_nonce_hash = Proto_alpha.Alpha_context.Nonce.hash @@ Helpers_crypto.generate_seed_nonce ()
}

let get_op_header pbh : operation_header = {
  branch = pbh
}


let make_sourced_operation op_header (proto_operation, source) =
  Helpers_operation.main_of_proto source op_header proto_operation >>? fun (a, b) ->
  ok ((a, source), b)


let init (pred_shell_header : shell_header) pred_block_hash
    level priority src_protops context =
  let op_header : operation_header =
    get_op_header pred_block_hash in
  Helpers_assert.tmp_map (make_sourced_operation op_header) src_protops >>? fun src_ops_hashs ->
  let (sourced_operations, operation_hashs) = List.split src_ops_hashs in
  let protocol_data = get_protocol_data priority in
  let protocol_data_bytes =
    Proto_alpha.Alpha_context.Block_header.forge_unsigned_protocol_data
      protocol_data
  in
  let timestamp =
    Time.add
      pred_shell_header.timestamp
    @@ Int64.mul 60L @@ Int64.of_int (priority + 1)
  in
  ok {
    pred_block_hash ;
    pred_shell_header ;
    protocol_data ;
    op_header ;
    protocol_data_bytes ;
    sourced_operations ;
    operation_hashs ;
    timestamp ;
    level ;
    context
  }


let init_of_result ?(priority = 15) ~(res : result) ~ops =
  init
    res.tezos_header.shell
    res.hash
    res.level
    priority
    ops
    res.validation.context


let get_level opt_msg =
  let msg = Option.unopt ~default: "level 1" opt_msg in
  let parts = String.split_on_char ',' msg in
  let level_part = List.hd parts in
  let parts = String.split_on_char ' ' level_part in
  let level_str = List.nth parts 1 in
  Int32.of_int @@ int_of_string level_str


let get_header_hash
    (init_block : init_block)
    (validation_result : Tezos_protocol_environment_memory.validation_result)
  : result tzresult Lwt.t
  =
  let op_hashs = init_block.operation_hashs in
  let hash = Operation_list_list_hash.compute
      [Operation_list_hash.compute op_hashs] in
  let level = Int32.succ init_block.pred_shell_header.level in
  let timestamp = init_block.timestamp in
  let shell_header = {
    init_block.pred_shell_header with
    level ;
    predecessor = init_block.pred_block_hash ;
    operations_hash = hash ;
    timestamp ;
    fitness = validation_result.fitness
  } in
  let tezos_header : tezos_header = {
    shell = shell_header ;
    protocol_data = init_block.protocol_data_bytes
  } in
  Proto_alpha.Alpha_context.init
    validation_result.context
    ~level
    ~timestamp
    ~fitness: validation_result.fitness
  >>=? fun tezos_context ->
  let hash = Block_header.hash tezos_header in
  return {
    tezos_header ;
    hash ;
    validation = validation_result ;
    level ;
    tezos_context
  }


let begin_construction_pre (init_block: init_block) =
  Proto_alpha.Main.begin_construction
    ~predecessor_context: init_block.context
    ~predecessor_timestamp: init_block.pred_shell_header.timestamp
    ~predecessor_level: init_block.level
    ~predecessor_fitness: init_block.pred_shell_header.fitness
    ~predecessor: init_block.pred_block_hash
    ~timestamp: init_block.timestamp
    ~protocol_data: init_block.protocol_data_bytes
    ()


let make init_block =
  let (operations,_) = List.split init_block.sourced_operations in
  begin_construction_pre init_block >>=? fun vs ->
  Proto_alpha.Error_monad.fold_left_s
    Main.apply_operation
    vs
    operations
  >>=? Main.finalize_block >>=? get_header_hash init_block


let make_init psh pbh lvl prio ops ctxt =
  Lwt.return @@ init psh pbh lvl prio ops ctxt >>=? make


let of_res ?priority ?(ops =[]) ~(res: result) () =
  Lwt.return @@ init_of_result ?priority ~res ~ops >>=? make


let endorsement
    psh pbh level priority src ctxt slot =
  make_init
    psh pbh (Alpha_context.Raw_level.to_int32 level) priority
    [Helpers_operation.endorsement_full pbh ~slot level, src]
    ctxt


let empty psh pbh level prio ctxt =
  make_init psh pbh level prio [] ctxt
