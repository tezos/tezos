(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Logging.Node.Worker

let inject_operation validator ?chain_id bytes =
  let t =
    match Data_encoding.Binary.of_bytes Operation.encoding bytes with
    | None -> failwith "Can't parse the operation"
    | Some op ->
        Validator.inject_operation validator ?chain_id op
  in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_protocol state ?force:_ proto =
  let proto_bytes =
    Data_encoding.Binary.to_bytes Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    Updater.compile hash proto >>= function
    | false ->
        failwith
          "Compilation failed (%a)"
          Protocol_hash.pp_short hash
    | true ->
        State.Protocol.store state proto >>= function
        | None ->
            failwith
              "Previously registred protocol (%a)"
              Protocol_hash.pp_short hash
        | Some _ -> return ()
  in
  Lwt.return (hash, validation)

let inject_block validator ?force ?chain_id bytes operations =
  Validator.validate_block
    validator ?force ?chain_id bytes operations >>=? fun (hash, block) ->
  return (hash, (block >>=? fun _ -> return ()))

type t = {
  state: State.t ;
  distributed_db: Distributed_db.t ;
  validator: Validator.t ;
  mainchain_validator: Chain_validator.t ;
  inject_block:
    ?force:bool ->
    ?chain_id:Chain_id.t ->
    MBytes.t -> Operation.t list list ->
    (Block_hash.t * unit tzresult Lwt.t) tzresult Lwt.t ;
  inject_operation:
    ?chain_id:Chain_id.t -> MBytes.t ->
    (Operation_hash.t * unit tzresult Lwt.t) Lwt.t ;
  inject_protocol:
    ?force:bool -> Protocol.t ->
    (Protocol_hash.t * unit tzresult Lwt.t) Lwt.t ;
  p2p: Distributed_db.p2p ; (* For P2P RPCs *)
  shutdown: unit -> unit Lwt.t ;
}

let init_p2p p2p_params =
  match p2p_params with
  | None ->
      lwt_log_notice "P2P layer is disabled" >>= fun () ->
      Error_monad.return (P2p.faked_network Distributed_db_metadata.cfg)
  | Some (config, limits) ->
      lwt_log_notice "bootstraping chain..." >>= fun () ->
      P2p.create
        ~config ~limits
        Distributed_db_metadata.cfg
        Distributed_db_message.cfg >>=? fun p2p ->
      Lwt.async (fun () -> P2p.maintain p2p) ;
      Error_monad.return p2p

type config = {
  genesis: State.Chain.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_chain_max_tll: int option ;
}

and peer_validator_limits = Peer_validator.limits = {
  new_head_request_timeout: float ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  protocol_timeout: float ;
  worker_limits: Worker_types.limits
}

and prevalidator_limits = Prevalidator.limits = {
  max_refused_operations: int ;
  operation_timeout: float ;
  worker_limits : Worker_types.limits ;
}

and block_validator_limits = Block_validator.limits = {
  protocol_timeout: float ;
  worker_limits : Worker_types.limits ;
}

and chain_validator_limits = Chain_validator.limits = {
  bootstrap_threshold: int ;
  worker_limits : Worker_types.limits ;
}

let may_create_chain state genesis =
  State.Chain.get state (Chain_id.of_block_hash genesis.State.Chain.block) >>= function
  | Ok chain -> Lwt.return chain
  | Error _ ->
      State.Chain.create state genesis

let create { genesis ; store_root ; context_root ;
             patch_context ; p2p = p2p_params ;
             test_chain_max_tll = max_child_ttl }
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits =
  init_p2p p2p_params >>=? fun p2p ->
  State.read
    ~store_root ~context_root ?patch_context () >>=? fun state ->
  let distributed_db = Distributed_db.create state p2p in
  Validator.create state distributed_db
    peer_validator_limits
    block_validator_limits
    prevalidator_limits
    chain_validator_limits >>= fun validator ->
  may_create_chain state genesis >>= fun mainchain_state ->
  Validator.activate validator
    ?max_child_ttl mainchain_state >>= fun mainchain_validator ->
  let shutdown () =
    P2p.shutdown p2p >>= fun () ->
    Validator.shutdown validator >>= fun () ->
    State.close state >>= fun () ->
    Lwt.return_unit
  in
  return {
    state ;
    distributed_db ;
    validator ;
    mainchain_validator ;
    inject_block = inject_block validator ;
    inject_operation = inject_operation validator ;
    inject_protocol = inject_protocol state ;
    p2p ;
    shutdown ;
  }

let shutdown node = node.shutdown ()

module RPC = struct

  type block = Block_services.block
  type block_info = Block_services.block_info = {
    hash: Block_hash.t ;
    chain_id: Chain_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ; (* uint8 *)
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
    context: Context_hash.t ;
    protocol_data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_chain: Test_chain_status.t ;
  }

  let convert (block: State.Block.t) =
    let hash = State.Block.hash block in
    let header = State.Block.header block in
    State.Block.all_operations block >>= fun operations ->
    let operations =
      List.map (List.map (fun op -> (Operation.hash op, op))) operations in
    State.Block.context block >>= fun context ->
    Context.get_protocol context >>= fun protocol ->
    Context.get_test_chain context >>= fun test_chain ->
    Lwt.return {
      hash ;
      chain_id = State.Block.chain_id block ;
      level = header.shell.level ;
      proto_level = header.shell.proto_level ;
      predecessor = header.shell.predecessor ;
      timestamp = header.shell.timestamp ;
      validation_passes = header.shell.validation_passes ;
      operations_hash = header.shell.operations_hash ;
      fitness = header.shell.fitness ;
      context = header.shell.context ;
      protocol_data = header.protocol_data ;
      operations = Some operations ;
      protocol ;
      test_chain ;
    }

  let inject_block node = node.inject_block
  let inject_operation node = node.inject_operation
  let inject_protocol node = node.inject_protocol

  let raw_block_info node hash =
    State.read_block node.state hash >>= function
    | Some block ->
        convert block
    | None ->
        Lwt.fail Not_found

  let prevalidation_hash =
    Block_hash.of_b58check_exn
      "BLockPrevaLidationPrevaLidationPrevaLidationPrZ4mr6"

  let get_validator node = function
    | `Genesis | `Head _ | `Prevalidation -> node.mainchain_validator
    | `Test_head _ | `Test_prevalidation ->
        match Chain_validator.child node.mainchain_validator with
        | None -> raise Not_found
        | Some v -> v

  let get_validator_per_hash node hash =
    State.read_block_exn node.state hash >>= fun block ->
    let chain_id = State.Block.chain_id block in
    if Chain_id.equal (Chain_validator.chain_id node.mainchain_validator) chain_id then
      Lwt.return (Some node.mainchain_validator)
    else
      match Chain_validator.child node.mainchain_validator with
      | Some test_validator ->
          if Chain_id.equal (Chain_validator.chain_id test_validator) chain_id then
            Lwt.return_some test_validator
          else
            Lwt.return_none
      | _ -> Lwt.return_none

  let read_valid_block node h =
    State.read_block node.state h

  let read_valid_block_exn node h =
    State.read_block_exn node.state h

  let rec predecessor chain_db n v =
    if n <= 0 then
      Lwt.return v
    else
      State.Block.predecessor v >>= function
      | None -> Lwt.return v
      | Some v -> predecessor chain_db (n-1) v

  let block_info node (block: block) =
    match block with
    | `Genesis ->
        let chain_state = Chain_validator.chain_state node.mainchain_validator in
        Chain.genesis chain_state >>= convert
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let chain_db = Chain_validator.chain_db validator in
        let chain_state = Chain_validator.chain_state validator in
        Chain.head chain_state >>= fun head ->
        predecessor chain_db n head >>= convert
    | `Hash h ->
        read_valid_block_exn node h >>= convert
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator = get_validator node block in
        let pv = Chain_validator.prevalidator validator in
        let chain_state = Chain_validator.chain_state validator in
        Chain.head chain_state >>= fun head ->
        let head_header = State.Block.header head in
        let head_hash = State.Block.hash head in
        let head_chain_id = State.Block.chain_id head in
        State.Block.context head >>= fun head_context ->
        Context.get_protocol head_context >>= fun head_protocol ->
        Prevalidator.context pv >>= function
        | Error _ -> Lwt.fail Not_found
        | Ok { context ; fitness } ->
            Context.get_protocol context >>= fun protocol ->
            Context.get_test_chain context >>= fun test_chain ->
            let proto_level =
              if Protocol_hash.equal protocol head_protocol then
                head_header.shell.proto_level
              else
                ((head_header.shell.proto_level + 1) mod 256) in
            let operations =
              let pv_result, _ = Prevalidator.operations pv in
              [ pv_result.applied ] in
            Lwt.return
              { hash = prevalidation_hash ;
                level = Int32.succ head_header.shell.level ;
                proto_level ;
                predecessor = head_hash ;
                fitness ;
                timestamp = Prevalidator.timestamp pv ;
                protocol ;
                validation_passes = List.length operations ;
                operations_hash =
                  Operation_list_list_hash.compute
                    (List.map
                       (fun ops -> Operation_list_hash.compute (List.map fst ops))
                       operations) ;
                operations = Some operations ;
                context = Context_hash.zero ;
                protocol_data = MBytes.of_string "" ;
                chain_id = head_chain_id ;
                test_chain ;
              }

  let rpc_context block : Tezos_protocol_environment_shell.rpc_context Lwt.t =
    let block_hash = State.Block.hash block in
    let block_header = State.Block.header block in
    State.Block.context block >|= fun context ->
    { Tezos_protocol_environment_shell.block_hash ;
      block_header ;
      operation_hashes = (fun () -> State.Block.all_operation_hashes block) ;
      operations = (fun () -> State.Block.all_operations block) ;
      context ;
    }

  let get_rpc_context node block =
    match block with
    | `Genesis ->
        let chain_state = Chain_validator.chain_state node.mainchain_validator in
        Chain.genesis chain_state >>= fun block ->
        rpc_context block >>= fun ctxt ->
        Lwt.return (Some ctxt)
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let chain_state = Chain_validator.chain_state validator in
        let chain_db = Chain_validator.chain_db validator in
        Chain.head chain_state >>= fun head ->
        predecessor chain_db n head >>= fun block ->
        rpc_context block >>= fun ctxt ->
        Lwt.return (Some ctxt)
    | `Hash hash-> begin
        read_valid_block node hash >>= function
        | None ->
            Lwt.return_none
        | Some block ->
            rpc_context block >>= fun ctxt ->
            Lwt.return (Some ctxt)
      end
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator = get_validator node block in
        let pv = Chain_validator.prevalidator validator in
        let chain_state = Chain_validator.chain_state validator in
        Chain.head chain_state >>= fun head ->
        let head_header = State.Block.header head in
        let head_hash = State.Block.hash head in
        State.Block.context head >>= fun head_context ->
        Context.get_protocol head_context >>= fun head_protocol ->
        Prevalidator.context pv >>= function
        | Error _ -> Lwt.fail Not_found
        | Ok { context ; fitness } ->
            Context.get_protocol context >>= fun protocol ->
            let proto_level =
              if Protocol_hash.equal protocol head_protocol then
                head_header.shell.proto_level
              else
                ((head_header.shell.proto_level + 1) mod 256) in
            let operation_hashes, operations =
              let pv_result, _ = Prevalidator.operations pv in
              [ List.map fst pv_result.applied ],
              [ List.map snd pv_result.applied ] in
            let operations_hash =
              Operation_list_list_hash.compute
                (List.map Operation_list_hash.compute operation_hashes) in
            Lwt.return (Some {
                Tezos_protocol_environment_shell.
                block_hash = prevalidation_hash ;
                block_header = {
                  shell = {
                    level = Int32.succ head_header.shell.level ;
                    proto_level ;
                    predecessor = head_hash ;
                    timestamp = Prevalidator.timestamp pv ;
                    validation_passes = List.length operation_hashes ;
                    operations_hash ;
                    fitness ;
                    context = Context_hash.zero ;
                  } ;
                  protocol_data = MBytes.create 0 ;
                } ;
                operation_hashes = (fun () -> Lwt.return operation_hashes) ;
                operations = (fun () -> Lwt.return operations) ;
                context ;
              })

  let operation_hashes node block =
    match block with
    | `Genesis -> Lwt.return []
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let chain_state = Chain_validator.chain_state validator in
        let chain_db = Chain_validator.chain_db validator in
        Chain.head chain_state >>= fun head ->
        predecessor chain_db n head >>= fun block ->
        State.Block.all_operation_hashes block
    | (`Prevalidation | `Test_prevalidation) as block ->
        let validator = get_validator node block in
        let pv = Chain_validator.prevalidator validator in
        let { Preapply_result.applied }, _ = Prevalidator.operations pv in
        Lwt.return [List.map fst applied]
    | `Hash hash ->
        read_valid_block node hash >>= function
        | None -> Lwt.return_nil
        | Some block ->
            State.Block.all_operation_hashes block

  let operations node block =
    match block with
    | `Genesis -> Lwt.return []
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let chain_state = Chain_validator.chain_state validator in
        let chain_db = Chain_validator.chain_db validator in
        Chain.head chain_state >>= fun head ->
        predecessor chain_db n head >>= fun block ->
        State.Block.all_operations block
    | (`Prevalidation | `Test_prevalidation) as block ->
        let validator = get_validator node block in
        let pv = Chain_validator.prevalidator validator in
        let { Preapply_result.applied }, _ = Prevalidator.operations pv in
        Lwt.return [List.map snd applied]
    | `Hash hash ->
        read_valid_block node hash >>= function
        | None -> Lwt.return_nil
        | Some block ->
            State.Block.all_operations block

  let pending_operations node (block: block) =
    match block with
    | ( `Head 0 | `Prevalidation
      | `Test_head 0 | `Test_prevalidation ) as block ->
        let validator = get_validator node block in
        let pv = Chain_validator.prevalidator validator in
        Lwt.return (Prevalidator.operations pv)
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let prevalidator = Chain_validator.prevalidator validator in
        let chain_state = Chain_validator.chain_state validator in
        let chain_db = Chain_validator.chain_db validator in
        Chain.head chain_state >>= fun head ->
        predecessor chain_db n head >>= fun b ->
        Prevalidator.pending ~block:b prevalidator >|= fun ops ->
        Preapply_result.empty, ops
    | `Genesis ->
        let chain_state = Chain_validator.chain_state node.mainchain_validator in
        let prevalidator =
          Chain_validator.prevalidator node.mainchain_validator in
        Chain.genesis chain_state >>= fun b ->
        Prevalidator.pending ~block:b prevalidator >|= fun ops ->
        Preapply_result.empty, ops
    | `Hash h -> begin
        get_validator_per_hash node h >>= function
        | None ->
            Lwt.return (Preapply_result.empty, Operation_hash.Map.empty)
        | Some validator ->
            let chain_state = Chain_validator.chain_state validator in
            let prevalidator = Chain_validator.prevalidator validator in
            State.Block.read_exn chain_state h >>= fun block ->
            Prevalidator.pending ~block prevalidator >|= fun ops ->
            Preapply_result.empty, ops
      end

  let protocols { state } =
    State.Protocol.list state >>= fun set ->
    Lwt.return (Protocol_hash.Set.elements set)

  let protocol_content node hash =
    State.Protocol.read node.state hash

  let preapply
      node block
      ~timestamp ~protocol_data ~sort_operations:sort ops =
    begin
      match block with
      | `Genesis ->
          let chain_state = Chain_validator.chain_state node.mainchain_validator in
          Chain.genesis chain_state >>= return
      | ( `Head 0 | `Prevalidation
        | `Test_head 0 | `Test_prevalidation ) as block ->
          let validator = get_validator node block in
          let chain_state = Chain_validator.chain_state validator in
          Chain.head chain_state >>= return
      | `Head n | `Test_head n as block -> begin
          let validator = get_validator node block in
          let chain_state = Chain_validator.chain_state validator in
          let chain_db = Chain_validator.chain_db validator in
          Chain.head chain_state >>= fun head ->
          predecessor chain_db n head >>= return
        end
      | `Hash hash ->
          read_valid_block node hash >>= function
          | None -> Lwt.return (error_exn Not_found)
          | Some data -> return data
    end >>=? fun predecessor ->
    Prevalidation.start_prevalidation
      ~protocol_data ~predecessor ~timestamp () >>=? fun validation_state ->
    let ops = List.map (List.map (fun x -> Operation.hash x, x)) ops in
    Lwt_list.fold_left_s
      (fun (validation_state, rs) ops ->
         Prevalidation.prevalidate
           validation_state ~sort ops >>= fun (validation_state, r) ->
         Lwt.return (validation_state, rs @ [r]))
      (validation_state, []) ops >>= fun (validation_state, rs) ->
    let operations_hash =
      Operation_list_list_hash.compute
        (List.map
           (fun r ->
              Operation_list_hash.compute
                (List.map fst r.Preapply_result.applied))
           rs) in
    Prevalidation.end_prevalidation
      validation_state >>=? fun { fitness ; context ; message } ->
    let pred_shell_header = State.Block.shell_header predecessor in
    State.Block.protocol_hash predecessor >>= fun pred_protocol ->
    Context.get_protocol context >>= fun protocol ->
    let proto_level =
      if Protocol_hash.equal protocol pred_protocol then
        pred_shell_header.proto_level
      else
        ((pred_shell_header.proto_level + 1) mod 256) in
    Context.commit ?message ~time:timestamp context >>= fun context ->
    let shell_header : Block_header.shell_header = {
      level = Int32.succ pred_shell_header.level ;
      proto_level ;
      predecessor = State.Block.hash predecessor ;
      timestamp ;
      validation_passes = List.length rs ;
      operations_hash ;
      fitness ;
      context ;
    } in
    return (shell_header, rs)

  let complete node ?block str =
    match block with
    | None ->
        Base58.complete str
    | Some block ->
        get_rpc_context node block >>= function
        | None -> Lwt.fail Not_found
        | Some { context = ctxt } ->
            Context.get_protocol ctxt >>= fun protocol_hash ->
            let (module Proto) = Registred_protocol.get_exn protocol_hash in
            Base58.complete str >>= fun l1 ->
            Proto.complete_b58prefix ctxt str >>= fun l2 ->
            Lwt.return (l1 @ l2)

  let context_dir node block =
    get_rpc_context node block >>= function
    | None -> Lwt.return None
    | Some rpc_context ->
        Context.get_protocol rpc_context.context >>= fun protocol_hash ->
        let (module Proto) = Registred_protocol.get_exn protocol_hash in
        let dir = RPC_directory.map (fun () -> Lwt.return rpc_context) Proto.rpc_services in
        Lwt.return (Some (RPC_directory.map (fun _ -> ()) dir))

  let context_raw_get node block ~path ~depth =
    let open Block_services in
    (* negative depth could be handled by a more informative error *)
    if depth < 0 then Lwt.return_none else
      get_rpc_context node block >>= function
      | None -> Lwt.return_none
      | Some rpc_context ->
          let rec loop path depth = (* non tail-recursive *)
            if depth = 0 then Lwt.return Cut else
              (* try to read as file *)
              Context.get rpc_context.context path >>= function
              | Some v -> Lwt.return (Key v)
              | None -> (* try to read as directory *)
                  Context.fold rpc_context.context path ~init:[]
                    ~f:(fun k acc ->
                        match k with
                        | `Key k | `Dir k ->
                            loop k (depth-1) >>= fun v ->
                            let k = List.nth k ((List.length k)-1) in
                            Lwt.return ((k,v)::acc)) >>= fun l ->
                  Lwt.return (Dir (List.rev l))
          in
          Context.mem rpc_context.context path >>= fun mem ->
          Context.dir_mem rpc_context.context path >>= fun dir_mem ->
          if mem || dir_mem then
            loop path depth >>= Lwt.return_some
          else Lwt.return_none

  let heads node =
    let chain_state = Chain_validator.chain_state node.mainchain_validator in
    Chain.known_heads chain_state >>= fun heads ->
    begin
      match Chain_validator.child node.mainchain_validator with
      | None -> Lwt.return_nil
      | Some test_validator ->
          let chain_state = Chain_validator.chain_state test_validator in
          Chain.known_heads chain_state
    end >>= fun test_heads ->
    Lwt_list.fold_left_s
      (fun map block ->
         convert block >|= fun bi ->
         Block_hash.Map.add
           (State.Block.hash block) bi map)
      Block_hash.Map.empty (test_heads @ heads)

  let predecessors node len head =
    let rec loop acc len block =
      if len = 0 then
        Lwt.return (List.rev acc)
      else
        State.Block.predecessor block >>= function
        | None -> Lwt.return (List.rev acc)
        | Some block ->
            loop (State.Block.hash block :: acc) (len-1) block
    in
    try
      State.read_block_exn node.state head >>= fun block ->
      loop [] len block
    with Not_found -> Lwt.return_nil

  let predecessors_bi ignored len head =
    try
      let rec loop acc len block =
        convert block >>= fun bi ->
        State.Block.predecessor block >>= function
        | None ->
            Lwt.return (List.rev (bi :: acc))
        | Some pred ->
            if len = 0 ||
               Block_hash.Set.mem (State.Block.hash block) ignored then
              Lwt.return (List.rev acc)
            else
              loop (bi :: acc) (len-1) pred
      in
      loop [] len head
    with Not_found -> Lwt.return_nil

  let list node len heads =
    Lwt_list.fold_left_s
      (fun (ignored, acc) head ->
         State.read_block_exn node.state head >>= fun block ->
         predecessors_bi ignored len block >>= fun predecessors ->
         let ignored =
           List.fold_right
             (fun x s -> Block_hash.Set.add x.hash s)
             predecessors ignored in
         Lwt.return (ignored, predecessors :: acc)
      )
      (Block_hash.Set.empty, [])
      heads >>= fun (_, blocks) ->
    Lwt.return (List.rev blocks)

  let list_invalid node =
    State.Block.list_invalid (Chain_validator.chain_state node.mainchain_validator)

  let unmark_invalid node block =
    State.Block.unmark_invalid (Chain_validator.chain_state node.mainchain_validator) block

  let block_header_watcher node =
    Distributed_db.watch_block_header node.distributed_db

  let block_watcher node =
    let stream, shutdown = Validator.watcher node.validator in
    Lwt_stream.map_s (fun block -> convert block) stream,
    shutdown

  let operation_watcher node =
    Distributed_db.watch_operation node.distributed_db

  let protocol_watcher node =
    Distributed_db.Protocol.watch node.distributed_db

  let bootstrapped node =
    let block_stream, stopper =
      Chain_validator.new_head_watcher node.mainchain_validator in
    let first_run = ref true in
    let next () =
      if !first_run then begin
        first_run := false ;
        let chain_state = Chain_validator.chain_state node.mainchain_validator in
        Chain.head chain_state >>= fun head ->
        let head_hash = State.Block.hash head in
        let head_header = State.Block.header head in
        Lwt.return (Some (head_hash, head_header.shell.timestamp))
      end else begin
        Lwt.pick [
          ( Lwt_stream.get block_stream >|=
            Option.map ~f:(fun b ->
                (State.Block.hash b, (State.Block.header b).shell.timestamp)) ) ;
          (Chain_validator.bootstrapped node.mainchain_validator >|= fun () -> None) ;
        ]
      end in
    let shutdown () = Lwt_watcher.shutdown stopper in
    RPC_answer.{ next ; shutdown }

  let build_p2p_rpc_directory (t : t) =
    P2p.build_rpc_directory t.p2p

end
