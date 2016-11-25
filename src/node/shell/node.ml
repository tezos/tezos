(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Worker

let (>|=) = Lwt.(>|=)

let inject_operation validator ?force bytes =
  let t =
    match Store.Operation.of_bytes bytes with
    | None -> failwith "Can't parse the operation"
    | Some operation ->
        Validator.get validator operation.shell.net_id >>=? fun net_validator ->
        let pv = Validator.prevalidator net_validator in
        Prevalidator.inject_operation pv ?force operation in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_protocol state ?force:_ proto =
  let proto_bytes = Store.Protocol.to_bytes proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation = Updater.compile hash proto >>= function
    | false -> Lwt.fail_with (Format.asprintf "Invalid protocol %a: compilation failed" Protocol_hash.pp_short hash)
    | true ->
        State.Protocol.store state proto_bytes >>= function
        | Ok None -> Lwt.fail_with "Previously registred protocol"
        | t -> t >|? ignore |> Lwt.return
  in
  Lwt.return (hash, validation)

let process_operation state validator bytes =
  State.Operation.store state bytes >>= function
  | Error _ | Ok None -> Lwt.return_unit
  | Ok (Some (hash, op)) ->
      lwt_log_info "process Operation %a (net: %a)"
        Operation_hash.pp_short hash
        Store.pp_net_id op.Store.shell.net_id >>= fun () ->
      Validator.get validator op.shell.net_id >>= function
      | Error _ -> Lwt.return_unit
      | Ok net_validator ->
          let prevalidator = Validator.prevalidator net_validator in
          Prevalidator.register_operation prevalidator hash ;
          Lwt.return_unit

let process_protocol state _validator bytes =
  State.Protocol.store state bytes >>= function
  | Error _ | Ok None -> Lwt.return_unit
  | Ok (Some (hash, _proto)) ->
      (* TODO: Store only pending protocols... *)
      lwt_log_info "process Protocol %a" Protocol_hash.pp_short hash

let process_block state validator bytes =
  State.Block.store state bytes >>= function
  | Error _ | Ok None -> Lwt.return_unit
  | Ok (Some (hash, block)) ->
      lwt_log_notice "process Block %a (net: %a)"
        Block_hash.pp_short hash
        Store.pp_net_id block.Store.shell.net_id >>= fun () ->
      lwt_debug "process Block %a (predecessor %a)"
        Block_hash.pp_short hash
        Block_hash.pp_short block.shell.predecessor >>= fun () ->
      lwt_debug "process Block %a (timestamp %a)"
        Block_hash.pp_short hash
        Time.pp_hum block.shell.timestamp >>= fun () ->
      Validator.notify_block validator hash block >>= fun () ->
      Lwt.return_unit

let inject_block state validator ?(force = false) bytes =
  let hash = Block_hash.hash_bytes [bytes] in
  let validation =
    State.Block.store state bytes >>=? function
    | None -> failwith "Previously registred block."
    | Some (hash, block) ->
        lwt_log_notice "inject Block %a"
          Block_hash.pp_short hash >>= fun () ->
        Lwt.return (State.Net.get state block.Store.shell.net_id) >>=? fun net ->
        State.Net.Blockchain.head net >>= fun head ->
        if force
        || Fitness.compare head.fitness block.shell.fitness <= 0 then
          Validator.get validator block.shell.net_id >>=? fun net ->
          Validator.fetch_block net hash >>=? fun _ ->
          return ()
        else
          failwith "Fitness is below the current one" in
  Lwt.return (hash, validation)

let process state validator msg =
  let open Tezos_p2p in
  match msg with

  | Discover_blocks (net_id, blocks) ->
      lwt_log_info "process Discover_blocks" >>= fun () ->
      if not (State.Net.is_active state net_id) then
        Lwt.return_nil
      else begin
        match State.Net.get state net_id with
        | Error _ -> Lwt.return_nil
        | Ok net ->
            State.Block.prefetch state net_id blocks ;
            State.Net.Blockchain.find_new net blocks 50 >>= function
            | Ok new_block_hashes ->
                Lwt.return [Block_inventory (net_id, new_block_hashes)]
            | Error _ -> Lwt.return_nil
      end

  | Block_inventory (net_id, blocks) ->
      lwt_log_info "process Block_inventory" >>= fun () ->
      if State.Net.is_active state net_id then
        State.Block.prefetch state net_id blocks ;
      Lwt.return_nil

  | Get_blocks blocks ->
      lwt_log_info "process Get_blocks" >>= fun () ->
      Lwt_list.map_p (State.Block.raw_read state) blocks >>= fun blocks ->
      let cons_block acc = function
        | Some b -> Block b :: acc
        | None -> acc in
      Lwt.return (List.fold_left cons_block [] blocks)

  | Block block ->
      lwt_log_info "process Block" >>= fun () ->
      process_block state validator block >>= fun _ ->
      Lwt.return_nil

  | Current_operations net_id ->
      lwt_log_info "process Current_operations" >>= fun () ->
      if not (State.Net.is_active state net_id) then
        Lwt.return_nil
      else begin
        Validator.get validator net_id >>= function
        | Error _ ->
            Lwt.return_nil
        | Ok net_validator ->
            let pv = Validator.prevalidator net_validator in
            let mempool = (fst (Prevalidator.operations pv)).applied in
            Lwt.return [Operation_inventory (net_id, mempool)]
      end

  | Operation_inventory (net_id, ops) ->
      lwt_log_info "process Operation_inventory" >>= fun () ->
      if State.Net.is_active state net_id then
        State.Operation.prefetch state net_id ops ;
      Lwt.return_nil

  | Get_operations ops ->
      lwt_log_info "process Get_operations" >>= fun () ->
      Lwt_list.map_p (State.Operation.raw_read state) ops >>= fun ops ->
      let cons_operation acc = function
        | Some op -> Operation op :: acc
        | None -> acc in
      Lwt.return (List.fold_left cons_operation [] ops)

  | Operation content ->
      lwt_log_info "process Operation" >>= fun () ->
      process_operation state validator content >>= fun () ->
      Lwt.return_nil

  | Get_protocols protos ->
      lwt_log_info "process Get_protocols" >>= fun () ->
      Lwt_list.map_p (State.Protocol.raw_read state) protos >>= fun protos ->
      let cons_protocol acc = function
        | Some proto -> Protocol proto :: acc
        | None -> acc in
      Lwt.return (List.fold_left cons_protocol [] protos)

  | Protocol content ->
      lwt_log_info "process Protocol" >>= fun () ->
      process_protocol state validator content >>= fun () ->
      Lwt.return_nil


type t = {
  state: State.t ;
  validator: Validator.worker ;
  global_net: State.Net.t ;
  global_validator: Validator.t ;
  inject_block:
    ?force:bool -> MBytes.t -> (Block_hash.t * unit tzresult Lwt.t) Lwt.t ;
  inject_operation:
    ?force:bool -> MBytes.t -> (Operation_hash.t * unit tzresult Lwt.t) Lwt.t ;
  inject_protocol:
    ?force:bool -> Store.protocol -> (Protocol_hash.t * unit tzresult Lwt.t) Lwt.t ;
  shutdown: unit -> unit Lwt.t ;
}

let request_operations net _net_id operations =
  (* TODO improve the lookup strategy.
          For now simply broadcast the request to all our neighbours. *)
  Tezos_p2p.broadcast net (Get_operations operations)

let request_blocks net _net_id blocks =
  (* TODO improve the lookup strategy.
          For now simply broadcast the request to all our neighbours. *)
  Tezos_p2p.broadcast net (Get_blocks blocks)

let request_protocols net protocols =
  (* TODO improve the lookup strategy.
          For now simply broadcast the request to all our neighbours. *)
  Tezos_p2p.broadcast net (Get_protocols protocols)

let init_p2p net_params =
  match net_params with
  | None ->
      lwt_log_notice "P2P layer is disabled" >>= fun () ->
      Lwt.return Tezos_p2p.faked_network
  | Some (config, limits) ->
      lwt_log_notice "bootstraping network..." >>= fun () ->
      Tezos_p2p.bootstrap config limits

let create
    ~genesis ~store_root ~context_root ?test_protocol ?patch_context net_params =
  lwt_debug "-> Node.create" >>= fun () ->
  init_p2p net_params >>= fun p2p ->
  lwt_log_info "reading state..." >>= fun () ->
  let request_operations = request_operations p2p in
  let request_blocks = request_blocks p2p in
  let request_protocols = request_protocols p2p in
  State.read
    ~request_operations ~request_blocks ~request_protocols
    ~store_root ~context_root ~ttl:(48 * 3600) (* 2 days *)
    ?patch_context () >>= fun state ->
  let validator = Validator.create_worker p2p state in
  let discoverer = Discoverer.create_worker p2p state in
  begin
    match State.Net.get state (Net genesis.Store.block) with
    | Ok net -> return net
    | Error _ -> State.Net.create state ?test_protocol genesis
  end >>=? fun global_net ->
  Validator.activate validator global_net >>= fun global_validator ->
  let cleanup () =
    Lwt.join [ Validator.shutdown validator ;
               Discoverer.shutdown discoverer ] >>= fun () ->
    State.store state
  in

  lwt_log_info "starting worker..." >>= fun () ->
  let worker =
    let handle_msg peer msg =
      process state validator msg >>= fun msgs ->
      List.iter
        (fun msg -> ignore @@ Tezos_p2p.try_send p2p peer msg)
        msgs;
      Lwt.return_unit
    in
    let rec worker_loop () =
      Tezos_p2p.recv p2p >>= fun (peer, msg) ->
      handle_msg peer msg >>= fun () ->
      worker_loop () in
    Lwt.catch
      worker_loop
      (function
        | Lwt_stream.Empty -> cleanup ()
        | exn ->
            lwt_log_error "unexpected exception in worker\n%s"
              (Printexc.to_string exn) >>= fun () ->
            Tezos_p2p.shutdown p2p >>= fun () ->
            cleanup ())
  in
  let shutdown () =
    lwt_log_info "stopping worker..." >>= fun () ->
    Tezos_p2p.shutdown p2p >>= fun () ->
    worker >>= fun () ->
    lwt_log_info "stopped"
  in
  lwt_debug "<- Node.create" >>= fun () ->
  return {
    state ;
    validator ;
    global_net ;
    global_validator ;
    inject_block = inject_block state validator ;
    inject_operation = inject_operation validator ;
    inject_protocol = inject_protocol state ;
    shutdown ;
  }

let shutdown node = node.shutdown ()

module RPC = struct

  type block = Node_rpc_services.Blocks.block
  type block_info = Node_rpc_services.Blocks.block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: Node_rpc_services.Blocks.net ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Node_rpc_services.Blocks.net * Time.t) option ;
 }

  let convert (block: State.Valid_block.t)  = {
    hash = block.hash ;
    predecessor = block.pred ;
    fitness = block.fitness ;
    timestamp = block.timestamp ;
    protocol = Some block.protocol_hash ;
    operations = Some block.operations ;
    net = block.net_id ;
    test_protocol = Some block.test_protocol_hash ;
    test_network = block.test_network ;
  }

  let convert_block hash (block: State.Block.shell_header)  = {
    net = block.net_id ;
    hash = hash ;
    predecessor = block.predecessor ;
    fitness = block.fitness ;
    timestamp = block.timestamp ;
    protocol = None ;
    operations = Some block.operations ;
    test_protocol = None ;
    test_network = None ;
  }

  let inject_block node = node.inject_block
  let inject_operation node = node.inject_operation
  let inject_protocol node = node.inject_protocol

  let raw_block_info node hash =
    State.Valid_block.read_exn node.state hash >|= convert

  let prevalidation_hash =
    Block_hash.of_b48check
      "eeeeeeeeeeeeeeefcF2dFpTjGjPAxRM3TqDrKkJf7DdkNHpX3DmaD"

  let get_net node = function
    | `Head _ | `Prevalidation -> node.global_validator, node.global_net
    | `Test_head _ | `Test_prevalidation ->
        match Validator.test_validator node.global_validator with
        | None -> raise Not_found
        | Some v -> v

  let get_pred node n (v: State.Valid_block.t) =
    if n <= 0 then Lwt.return v else
      let rec loop n h =
        if n <= 0 then Lwt.return h else
          State.Block.read_pred node.state h >>= function
          | None -> raise Not_found
          | Some pred -> loop (n-1) pred in
      loop n v.hash >>= fun h ->
      State.Valid_block.read node.state h >>= function
      | None | Some (Error _) -> Lwt.fail Not_found (* error in the DB *)
      | Some (Ok b) -> Lwt.return b

  let block_info node (block: block) =
    match block with
    | `Genesis -> State.Net.Blockchain.genesis node.global_net >|= convert
    | ( `Head n | `Test_head n ) as block ->
        let _, net = get_net node block in
        State.Net.Blockchain.head net >>= get_pred node n >|= convert
    | `Hash h -> State.Valid_block.read_exn node.state h >|= convert
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator, net = get_net node block in
        let pv = Validator.prevalidator validator in
        State.Net.Blockchain.head net >>= fun head ->
        let ctxt = Prevalidator.context pv in
        let (module Proto) = Prevalidator.protocol pv in
        Proto.fitness ctxt >|= fun fitness ->
        { (convert head) with
          hash = prevalidation_hash ;
          fitness ;
          timestamp = Prevalidator.timestamp pv
        }

  let get_context node block =
    match block with
    | `Genesis ->
        State.Net.Blockchain.genesis node.global_net >>= fun { context } ->
        Lwt.return (Some context)
    | ( `Head n | `Test_head n ) as block->
        let _, net = get_net node block in
        State.Net.Blockchain.head net >>= get_pred node n >>= fun { context } ->
        Lwt.return (Some context)
    | `Hash hash-> begin
        State.Valid_block.read node.state hash >|= function
        | None | Some (Error _) -> None
        | Some (Ok { context }) -> Some context
      end
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        Lwt.return (Some (Prevalidator.context pv))

  let operations node block =
    match block with
    | `Genesis ->
        State.Net.Blockchain.genesis node.global_net >>= fun { operations } ->
        Lwt.return operations
    | ( `Head n | `Test_head n ) as block ->
        let _, net = get_net node block in
        State.Net.Blockchain.head net >>= get_pred node n >>= fun { operations } ->
        Lwt.return operations
    | (`Prevalidation | `Test_prevalidation) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        let { Updater.applied }, _ = Prevalidator.operations pv in
        Lwt.return applied
    | `Hash hash->
        State.Block.read node.state hash >|= function
        | None -> []
        | Some { Time.data = { shell = { operations }}} -> operations

  let operation_content node hash =
    State.Operation.read node.state hash

  let pending_operations node block =
    match block with
    | ( `Head 0 | `Prevalidation
      | `Test_head 0 | `Test_prevalidation ) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        Lwt.return (Prevalidator.operations pv)
    | ( `Head n | `Test_head n ) as block ->
        let _validator, net = get_net node block in
        State.Net.Blockchain.head net >>= get_pred node n >>= fun b ->
        State.Net.Mempool.for_block net b >|= fun ops ->
        Updater.empty_result, ops
    | `Genesis ->
        let net = node.global_net in
        State.Net.Blockchain.genesis net >>= fun b ->
        State.Net.Mempool.for_block net b >|= fun ops ->
        Updater.empty_result, ops
    | `Hash h ->
        begin
          let nets = State.Net.active node.state in
          Lwt_list.filter_map_p
            (fun net ->
               State.Net.Blockchain.head net >|= fun head ->
               if Block_hash.equal h head.hash then Some (net, head) else None)
            nets >>= function
          | [] -> Lwt.return_none
          | [net] -> Lwt.return (Some net)
          | nets ->
              Lwt_list.filter_p
                (fun (net, (head: State.Valid_block.t)) ->
                   State.Net.Blockchain.genesis net >|= fun genesis ->
                   not (Block_hash.equal genesis.hash head.hash))
                nets >>= function
              | [net] -> Lwt.return (Some net)
              | _ -> Lwt.fail Not_found
        end >>= function
        | Some (net, _head) ->
            Validator.get_exn
              node.validator (State.Net.id net) >>= fun net_validator ->
            let pv = Validator.prevalidator net_validator in
            Lwt.return (Prevalidator.operations pv)
        | None ->
            State.Valid_block.read_exn node.state h >>= fun b ->
            if not (State.Net.is_active node.state b.net_id) then
              raise Not_found ;
            match State.Net.get node.state b.net_id with
            | Error _ -> raise Not_found
            | Ok net ->
                State.Net.Mempool.for_block net b >|= fun ops ->
                Updater.empty_result, ops

  let protocols { state } = State.Protocol.keys state

  let protocol_content node hash =
    State.Protocol.read node.state hash

  let preapply node block ~timestamp ~sort ops =
    begin
      match block with
      | `Genesis ->
          let net = node.global_net in
          State.Net.Blockchain.genesis net >>= return
      | ( `Head 0 | `Prevalidation
        | `Test_head 0 | `Test_prevalidation ) as block ->
          let _validator, net = get_net node block in
          State.Net.Blockchain.head net >>= return
      | `Head n | `Test_head n as block -> begin
          let _validator, net = get_net node block in
          State.Net.Blockchain.head net >>= get_pred node n >>= return
        end
      | `Hash hash -> begin
          State.Valid_block.read node.state hash >>= function
          | None -> Lwt.return (error_exn Not_found)
          | Some data -> Lwt.return data
        end
    end >>=? fun { hash ; context ; protocol } ->
    begin
      match protocol with
      | None -> failwith "Unknown protocol version"
      | Some protocol -> return protocol
    end >>=? function (module Proto) as protocol ->
    Prevalidator.preapply
      node.state context protocol hash timestamp sort ops >>=? fun (ctxt, r) ->
    Proto.fitness ctxt >>= fun fitness ->
    return (fitness, r)

  let complete node ?block str =
    match block with
    | None ->
        Base48.complete str
    | Some block ->
        get_context node block >>= function
        | None -> Lwt.fail Not_found
        | Some ctxt ->
            Context.get_protocol ctxt >>= fun protocol_hash ->
            let (module Proto) = Updater.get_exn protocol_hash in
            Base48.complete str >>= fun l1 ->
            Proto.complete_b48prefix ctxt str >>= fun l2 ->
            Lwt.return (l1 @ l2)

  let context_dir node block =
    get_context node block >>= function
    | None -> Lwt.return None
    | Some ctxt ->
        Context.get_protocol ctxt >>= fun protocol_hash ->
        let (module Proto) = Updater.get_exn protocol_hash in
        let dir =  RPC.map (fun () -> ctxt) Proto.rpc_services in
        Lwt.return (Some (RPC.map (fun _ -> ()) dir))

  let heads node =
    State.Valid_block.known_heads node.state >|= Block_hash_map.map convert

  let predecessors state ignored len head =
    try
      let rec loop acc len hash =
        State.Valid_block.read_exn state hash >>= fun block ->
        let bi = convert block in
        if Block_hash.equal bi.predecessor hash then
          Lwt.return (List.rev (bi :: acc))
        else begin
          if len = 0
             || Block_hash_set.mem hash ignored then
          Lwt.return (List.rev acc)
        else
          loop (bi :: acc) (len-1) bi.predecessor
        end in
      loop [] len head
    with Not_found -> Lwt.return_nil

  let list node len heads =
    Lwt_list.fold_left_s
      (fun (ignored, acc) head ->
         predecessors node.state ignored len head >|= fun predecessors ->
         let ignored =
           List.fold_right
             (fun x s -> Block_hash_set.add x.hash s)
             predecessors ignored in
         ignored, predecessors :: acc
      )
      (Block_hash_set.empty, [])
      heads >|= fun (_, blocks) ->
    List.rev blocks

  let block_watcher node =
    let stream, shutdown = State.Block.create_watcher node.state in
    Lwt_stream.map
      (fun (hash, block) -> convert_block hash block.Store.shell)
      stream,
    shutdown

  let valid_block_watcher node =
    State.Valid_block.create_watcher node.state >|= fun (stream, shutdown) ->
    Lwt_stream.map
      (fun block -> convert block)
      stream,
    shutdown

  let operation_watcher node =
    State.Operation.create_watcher node.state

  let protocol_watcher node =
    State.Protocol.create_watcher node.state

  let validate node net_id block =
    Validator.get node.validator net_id >>=? fun net_v ->
    Validator.fetch_block net_v block >>=? fun _ ->
    return ()

end
