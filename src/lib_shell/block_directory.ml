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

let rec read_partial_context context path depth =
  (* non tail-recursive *)
  if depth = 0 then
    Lwt.return Block_services.Cut
  else
    (* try to read as file *)
    Context.get context path >>= function
    | Some v ->
        Lwt.return (Block_services.Key v)
    | None ->
        (* try to read as directory *)
        Context.fold context path ~init:[] ~f: begin fun k acc ->
          match k with
          | `Key k | `Dir k ->
              read_partial_context context k (depth-1) >>= fun v ->
              let k = List.nth k ((List.length k)-1) in
              Lwt.return ((k,v)::acc)
        end >>= fun l ->
        Lwt.return (Block_services.Dir (List.rev l))

let build_raw_header_rpc_directory (module Proto : Block_services.PROTO) =

  let dir : (State.Chain.t * Block_hash.t * Block_header.t) RPC_directory.t ref =
    ref RPC_directory.empty in

  let register0 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst0 s)
        (fun block p q -> f block p q) in

  let module Block_services = Block_services.Make(Proto)(Proto) in
  let module S = Block_services.S in

  register0 S.hash begin fun (_, hash, _) () () ->
    return hash
  end ;

  (* block header *)

  register0 S.header begin fun (chain_state, hash, header) () () ->
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_data_encoding
        header.protocol_data in
    return { Block_services.hash ; chain_id = State.Chain.id chain_state ;
             shell = header.shell ; protocol_data }
  end ;
  register0 S.raw_header begin fun (_, _, header) () () ->
    return (Data_encoding.Binary.to_bytes_exn Block_header.encoding header)
  end ;
  register0 S.Header.shell_header begin fun (_, _, header) () () ->
    return header.shell
  end ;
  register0 S.Header.protocol_data begin fun (_, _, header) () () ->
    return
      (Data_encoding.Binary.of_bytes_exn
         Proto.block_header_data_encoding
         header.protocol_data)
  end ;
  register0 S.Header.raw_protocol_data begin fun (_, _, header) () () ->
    return header.protocol_data
  end ;

  (* helpers *)

  register0 S.Helpers.Forge.block_header begin fun _block () header ->
    return (Data_encoding.Binary.to_bytes_exn Block_header.encoding header)
  end ;

  register0 S.protocols begin fun (chain_state, _, header) () () ->
    State.Block.header_of_hash chain_state header.shell.predecessor >>= function
    | None ->
        State.Chain.get_level_indexed_protocol chain_state header >>= fun next_protocol_hash ->
        return {
          Tezos_shell_services.Block_services.current_protocol = next_protocol_hash ;
          next_protocol = next_protocol_hash ;
        }

    | Some pred_header ->
        State.Chain.get_level_indexed_protocol chain_state pred_header >>= fun protocol_hash ->
        State.Chain.get_level_indexed_protocol chain_state header >>= fun next_protocol_hash ->
        return {
          Tezos_shell_services.Block_services.current_protocol = protocol_hash ;
          next_protocol = next_protocol_hash ;
        }
  end ;

  !dir

let build_raw_rpc_directory
    (module Proto : Block_services.PROTO)
    (module Next_proto : Registered_protocol.T) =

  let dir : State.Block.block RPC_directory.t ref =
    ref RPC_directory.empty in

  let merge d = dir := RPC_directory.merge d !dir in
  let register0 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst0 s)
        (fun block p q -> f block p q) in
  let register1 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst1 s)
        (fun (block, a) p q -> f block a p q) in
  let register2 s f =
    dir :=
      RPC_directory.register !dir (RPC_service.subst2 s)
        (fun ((block, a), b) p q -> f block a b p q) in

  let module Block_services = Block_services.Make(Proto)(Next_proto) in
  let module S = Block_services.S in

  register0 S.live_blocks begin fun block () () ->
    State.Block.max_operations_ttl block >>=? fun max_op_ttl ->
    Chain_traversal.live_blocks
      block
      max_op_ttl >>=? fun (live_blocks, _) ->
    return live_blocks
  end ;

  (* block metadata *)

  let metadata block =
    State.Block.metadata block >>=? fun metadata ->
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_metadata_encoding
        metadata in
    State.Block.max_operations_ttl block >>=? fun max_operations_ttl ->
    State.Block.test_chain block >>= fun (test_chain_status, _) ->
    return {
      Block_services.protocol_data ;
      test_chain_status ;
      max_operations_ttl ;
      max_operation_data_length = Next_proto.max_operation_data_length ;
      max_block_header_length = Next_proto.max_block_length ;
      operation_list_quota =
        List.map
          (fun { Tezos_protocol_environment_shell.max_size; max_op } ->
             { Tezos_shell_services.Block_services.max_size ; max_op } )
          Next_proto.validation_passes ;
    } in

  register0 S.metadata begin fun block () () ->
    metadata block
  end ;

  (* operations *)

  let convert chain_id (op : Operation.t) metadata : Block_services.operation =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding
        op.proto in
    let receipt =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_receipt_encoding
        metadata in
    { Block_services.chain_id ;
      hash = Operation.hash op ;
      shell = op.shell ;
      protocol_data ;
      receipt ;
    } in

  let operations block =
    State.Block.all_operations block >>= fun ops ->
    State.Block.all_operations_metadata block >>= fun metadata ->
    let chain_id = State.Block.chain_id block in
    return (List.map2 (List.map2 (convert chain_id)) ops metadata) in

  register0 S.Operations.operations begin fun block () () ->
    operations block
  end ;

  register1 S.Operations.operations_in_pass begin fun block i () () ->
    let chain_id = State.Block.chain_id block in
    try
      State.Block.operations block i >>= fun (ops, _path) ->
      State.Block.operations_metadata block i >>= fun metadata ->
      return (List.map2 (convert chain_id) ops metadata)
    with _ -> Lwt.fail Not_found
  end ;

  register2 S.Operations.operation begin fun block i j () () ->
    let chain_id = State.Block.chain_id block in
    begin try
        State.Block.operations block i >>= fun (ops, _path) ->
        State.Block.operations_metadata block i >>= fun metadata ->
        Lwt.return (List.nth ops j, List.nth metadata j)
      with _ -> Lwt.fail Not_found end >>= fun (op, md) ->
    return (convert chain_id op md)
  end ;

  (* operation_hashes *)

  register0 S.Operation_hashes.operation_hashes begin fun block () () ->
    State.Block.all_operation_hashes block >>= return
  end ;

  register1 S.Operation_hashes.operation_hashes_in_pass begin fun block i () () ->
    State.Block.operation_hashes block i >>= fun (ops, _) ->
    return ops
  end ;

  register2 S.Operation_hashes.operation_hash begin fun block i j () () ->
    begin
      try
        State.Block.operation_hashes block i >>= fun (ops, _) ->
        Lwt.return (List.nth ops j)
      with _ -> Lwt.fail Not_found end >>= fun op ->
    return op
  end ;

  (* context *)

  register1 S.Context.read begin fun block path q () ->
    let depth = Option.unopt ~default:max_int q#depth in
    fail_unless (depth >= 0)
      (Tezos_shell_services.Block_services.Invalid_depth_arg depth) >>=? fun () ->
    State.Block.context block >>= fun context ->
    Context.mem context path >>= fun mem ->
    Context.dir_mem context path >>= fun dir_mem ->
    if not (mem || dir_mem) then
      Lwt.fail Not_found
    else
      read_partial_context context path depth >>= fun dir ->
      return dir
  end ;

  (* info *)

  register0 S.info begin fun block () () ->
    (* TODO allow to send partial information *)
    let chain_id = State.Block.chain_id block in
    let hash = State.Block.hash block in
    let header = State.Block.header block in
    let shell = header.shell in
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.block_header_data_encoding
        header.protocol_data in
    metadata block >>=? fun metadata ->
    operations block >>=? fun operations ->
    return { Block_services.hash ; chain_id ;
             header = { shell ; protocol_data } ;
             metadata ; operations }
  end ;

  (* helpers *)

  register0 S.Helpers.Preapply.block begin fun block q p ->
    let timestamp =
      match q#timestamp with
      | None -> Time.now ()
      | Some time -> time in
    let protocol_data =
      Data_encoding.Binary.to_bytes_exn
        Next_proto.block_header_data_encoding
        p.protocol_data in
    let operations =
      List.map
        (List.map
           (fun op ->
              let proto =
                Data_encoding.Binary.to_bytes_exn
                  Next_proto.operation_data_encoding
                  op.Next_proto.protocol_data in
              { Operation.shell = op.shell ; proto }))
        p.operations in
    Prevalidation.preapply
      ~predecessor:block
      ~timestamp
      ~protocol_data
      operations
  end ;

  register0 S.Helpers.Preapply.operations begin fun block () ops ->
    State.Block.context block >>= fun ctxt ->
    let predecessor = State.Block.hash block in
    let header = State.Block.shell_header block in
    Next_proto.begin_construction
      ~chain_id: (State.Block.chain_id block)
      ~predecessor_context:ctxt
      ~predecessor_timestamp:header.timestamp
      ~predecessor_level:header.level
      ~predecessor_fitness:header.fitness
      ~predecessor
      ~timestamp:(Time.now ()) () >>=? fun state ->
    fold_left_s
      (fun (state, acc) op ->
         Next_proto.apply_operation state op >>=? fun (state, result) ->
         return (state, (op.protocol_data, result) :: acc))
      (state, []) ops >>=? fun (state, acc) ->
    Next_proto.finalize_block state >>=? fun _ ->
    return (List.rev acc)
  end ;

  register1 S.Helpers.complete begin fun block prefix () () ->
    State.Block.context block >>= fun ctxt ->
    Base58.complete prefix >>= fun l1 ->
    Next_proto.complete_b58prefix ctxt prefix >>= fun l2 ->
    return (l1 @ l2)
  end ;

  (* merge protocol rpcs... *)

  merge
    (RPC_directory.map
       (fun block ->
          let chain_state = State.Block.chain_state  block in
          let hash = State.Block.hash block in
          let header = State.Block.header block in
          Lwt.return (chain_state, hash, header))
       (build_raw_header_rpc_directory (module Proto))) ;

  merge
    (RPC_directory.map
       (fun block ->
          State.Block.context block >|= fun context ->
          { Tezos_protocol_environment_shell.
            block_hash = State.Block.hash block ;
            block_header = State.Block.shell_header block ;
            context })
       Next_proto.rpc_services) ;

  !dir

let get_protocol hash =
  match Registered_protocol.get hash with
  | None -> raise Not_found
  | Some protocol -> protocol

let get_directory block =
  State.Block.get_rpc_directory block >>= function
  | Some dir -> Lwt.return dir
  | None ->
      State.Block.protocol_hash block >>= fun next_protocol_hash ->
      let next_protocol = get_protocol next_protocol_hash in
      State.Block.predecessor block >>= function
      | None ->
          Lwt.return (build_raw_rpc_directory
                        (module Block_services.Fake_protocol)
                        next_protocol)
      | Some pred ->
          State.Block.protocol_hash pred >>= fun protocol_hash ->
          let (module Proto) = get_protocol protocol_hash in
          State.Block.get_rpc_directory block >>= function
          | Some dir -> Lwt.return dir
          | None ->
              let dir = build_raw_rpc_directory (module Proto) next_protocol in
              State.Block.set_rpc_directory block dir >>= fun () ->
              Lwt.return dir

let get_block chain_state = function
  | `Genesis ->
      Chain.genesis chain_state >>= begin function
        | Some b -> Lwt.return_some b
        | None -> Lwt.fail Not_found
      end
  | `Head n ->
      Chain.head chain_state >>= fun head ->
      if n < 0 then
        Lwt.fail Not_found
      else if n = 0 then
        Lwt.return_some head
      else
        State.Block.read_predecessor
          chain_state ~pred:n ~below_save_point:true
          (State.Block.hash head)
  | `Alias (_, n) | `Hash (_, n) as b ->
      begin match b with
        | `Alias (`Checkpoint, _) ->
            State.Chain.checkpoint chain_state >>= fun checkpoint ->
            Lwt.return (Block_header.hash checkpoint)
        | `Alias (`Save_point, _) ->
            State.Chain.save_point chain_state >>= fun (_, save_point) ->
            Lwt.return save_point
        | `Alias (`Caboose, _) ->
            State.Chain.caboose chain_state >>= fun (_, caboose) ->
            Lwt.return caboose
        | `Hash (h, _) -> Lwt.return h
      end >>= fun hash ->
      if n < 0 then
        State.Block.read_exn chain_state hash >>= fun block ->
        Chain.head chain_state >>= fun head ->
        let head_level = State.Block.level head in
        let block_level = State.Block.level block in
        let target =
          Int32.(to_int (sub head_level (sub block_level (of_int n)))) in
        if target < 0 then
          Lwt.fail Not_found
        else
          State.Block.read_predecessor
            chain_state ~pred:target ~below_save_point:true
            (State.Block.hash head)
      else
        State.Block.read_predecessor
          chain_state ~pred:n ~below_save_point:true hash
  | `Level i ->
      Chain.head chain_state >>= fun head ->
      let target = Int32.(to_int (sub (State.Block.level head) i)) in
      if target < 0 then
        Lwt.fail Not_found
      else
        State.Block.read_predecessor
          chain_state ~pred:target ~below_save_point:true
          (State.Block.hash head)


let fake_rpc_directory =
  build_raw_header_rpc_directory (module Block_services.Fake_protocol)

let build_rpc_directory chain_state block =
  get_block chain_state block >>= function
  | Some block ->
      get_directory block >>= fun dir ->
      Lwt.return (RPC_directory.map (fun _ -> Lwt.return block) dir)
  | None -> Lwt.fail Not_found
