(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Logging.Node.Worker

let inject_operation validator ?force bytes =
  let t =
    match Data_encoding.Binary.of_bytes Store.Operation.encoding bytes with
    | None -> failwith "Can't parse the operation"
    | Some operation ->
        Validator.get
          validator operation.shell.net_id >>=? fun net_validator ->
        let pv = Validator.prevalidator net_validator in
        Prevalidator.inject_operation pv ?force operation in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_protocol state ?force:_ proto =
  let proto_bytes =
    Data_encoding.Binary.to_bytes Store.Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    Updater.compile hash proto >>= function
    | false ->
        failwith
          "Compilation failed (%a)"
          Protocol_hash.pp_short hash
    | true ->
        State.Protocol.store state hash proto >>= function
        | false ->
            failwith
              "Previously registred protocol (%a)"
              Protocol_hash.pp_short hash
        | true -> return ()
  in
  Lwt.return (hash, validation)

let inject_block validator ?force bytes operations =
  Validator.inject_block
    validator ?force
    bytes operations >>=? fun (hash, block) ->
  return (hash, (block >>=? fun _ -> return ()))

type t = {
  state: State.t ;
  distributed_db: Distributed_db.t ;
  validator: Validator.worker ;
  mainnet_db: Distributed_db.net ;
  mainnet_net: State.Net.t ;
  mainnet_validator: Validator.t ;
  inject_block:
    ?force:bool ->
    MBytes.t -> Operation_hash.t list list ->
    (Block_hash.t * unit tzresult Lwt.t) tzresult Lwt.t ;
  inject_operation:
    ?force:bool -> MBytes.t ->
    (Operation_hash.t * unit tzresult Lwt.t) Lwt.t ;
  inject_protocol:
    ?force:bool -> Store.Protocol.t ->
    (Protocol_hash.t * unit tzresult Lwt.t) Lwt.t ;
  p2p: Distributed_db.p2p ; (* For P2P RPCs *)
  shutdown: unit -> unit Lwt.t ;
}

let init_p2p net_params =
  match net_params with
  | None ->
      lwt_log_notice "P2P layer is disabled" >>= fun () ->
      Lwt.return (P2p.faked_network Distributed_db_metadata.cfg)
  | Some (config, limits) ->
      lwt_log_notice "bootstraping network..." >>= fun () ->
      P2p.create
        ~config ~limits
        Distributed_db_metadata.cfg
        Distributed_db_message.cfg >>= fun p2p ->
      Lwt.async (fun () -> P2p.maintain p2p) ;
      Lwt.return p2p

type config = {
  genesis: State.Net.genesis ;
  store_root: string ;
  context_root: string ;
  test_protocol: Protocol_hash.t option ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
}

let may_create_net state ?test_protocol genesis =
  State.Net.get state (Net_id.of_block_hash genesis.State.Net.block) >>= function
  | Ok net -> Lwt.return net
  | Error _ ->
      State.Net.create state
        ?test_protocol
        ~forked_network_ttl:(48 * 3600) (* 2 days *)
        genesis


let create { genesis ; store_root ; context_root ;
             test_protocol ; patch_context ; p2p = net_params } =
  init_p2p net_params >>= fun p2p ->
  State.read
    ~store_root ~context_root ?patch_context () >>=? fun state ->
  let distributed_db = Distributed_db.create state p2p in
  let validator = Validator.create_worker state distributed_db in
  may_create_net state ?test_protocol genesis >>= fun mainnet_net ->
  Validator.activate validator mainnet_net >>= fun mainnet_validator ->
  let mainnet_db = Validator.net_db mainnet_validator in
  let shutdown () =
    P2p.shutdown p2p >>= fun () ->
    Validator.shutdown validator >>= fun () ->
    Lwt.return_unit
  in
  return {
    state ;
    distributed_db ;
    validator ;
    mainnet_db ;
    mainnet_net ;
    mainnet_validator ;
    inject_block = inject_block validator ;
    inject_operation = inject_operation validator ;
    inject_protocol = inject_protocol state ;
    p2p ;
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
    operations_hash: Operation_list_list_hash.t ;
    operations: Operation_hash.t list list option ;
    data: MBytes.t option ;
    net: Net_id.t ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Net_id.t * Time.t) option ;
 }

  let convert (block: State.Valid_block.t)  = {
    hash = block.hash ;
    predecessor = block.pred ;
    fitness = block.fitness ;
    timestamp = block.timestamp ;
    protocol = Some block.protocol_hash ;
    operations_hash = block.operations_hash ;
    operations = Some block.operations ;
    data = Some block.proto_header ;
    net = block.net_id ;
    test_protocol = Some block.test_protocol_hash ;
    test_network = block.test_network ;
  }

  let convert_block hash ({ shell ; proto }: State.Block_header.t)  = {
    net = shell.net_id ;
    hash = hash ;
    predecessor = shell.predecessor ;
    fitness = shell.fitness ;
    timestamp = shell.timestamp ;
    protocol = None ;
    operations_hash = shell.operations ;
    operations = None ;
    data = Some proto ;
    test_protocol = None ;
    test_network = None ;
  }

  let inject_block node = node.inject_block
  let inject_operation node = node.inject_operation
  let inject_protocol node = node.inject_protocol

  let raw_block_info node hash =
    Distributed_db.read_block node.distributed_db hash >>= function
    | Some (net_db, _block) ->
        let net = Distributed_db.state net_db in
        State.Valid_block.read_exn net hash >>= fun block ->
        Lwt.return (convert block)
    | None ->
        Lwt.fail Not_found

  let prevalidation_hash =
    Block_hash.of_b58check_exn
      "BLockPrevaLidationPrevaLidationPrevaLidationPrZ4mr6"

  let get_net node = function
    | `Genesis | `Head _ | `Prevalidation ->
        node.mainnet_validator, node.mainnet_db
    | `Test_head _ | `Test_prevalidation ->
        match Validator.test_validator node.mainnet_validator with
        | None -> raise Not_found
        | Some v -> v

  let get_validator node = function
    | `Genesis | `Head _ | `Prevalidation -> node.mainnet_validator
    | `Test_head _ | `Test_prevalidation ->
        match Validator.test_validator node.mainnet_validator with
        | None -> raise Not_found
        | Some (v, _) -> v

  let get_validator_per_hash node hash =
    Distributed_db.read_block_exn
      node.distributed_db hash >>= fun (_net_db, block) ->
    if Net_id.equal
        (State.Net.id node.mainnet_net)
        block.shell.net_id then
      Lwt.return (Some (node.mainnet_validator, node.mainnet_db))
    else
      match Validator.test_validator node.mainnet_validator with
      | Some (test_validator, net_db)
        when Net_id.equal
            (State.Net.id (Validator.net_state test_validator))
            block.shell.net_id ->
          Lwt.return (Some (node.mainnet_validator, net_db))
      | _ -> Lwt.return_none

  let read_valid_block node h =
    Distributed_db.read_block node.distributed_db h >>= function
    | None -> Lwt.return_none
    | Some (_net_db, block) ->
        State.Net.get node.state block.shell.net_id >>= function
        | Error _ -> Lwt.return_none
        | Ok net ->
            State.Valid_block.read_exn net h >>= fun block ->
            Lwt.return (Some block)

  let read_valid_block_exn node h =
    Distributed_db.read_block_exn
      node.distributed_db h >>= fun (net_db, _block) ->
    let net = Distributed_db.state net_db in
    State.Valid_block.read_exn net h >>= fun block ->
    Lwt.return block

  let get_pred net_db n (v: State.Valid_block.t) =
    let rec loop net_db n h =
      if n <= 0 then
        Lwt.return h
      else
        Distributed_db.Block_header.read net_db h >>= function
        | None -> Lwt.fail Not_found
        | Some { shell = { predecessor } } ->
            loop net_db (n-1) predecessor in
    if n <= 0 then
      Lwt.return v
    else
      loop net_db n v.hash >>= fun hash ->
      let net_state = Distributed_db.state net_db in
      State.Valid_block.read_exn net_state hash

  let block_info node (block: block) =
    match block with
    | `Genesis ->
        State.Valid_block.Current.genesis node.mainnet_net >|= convert
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let net_db = Validator.net_db validator in
        let net_state = Validator.net_state validator in
        State.Valid_block.Current.head net_state >>= fun head ->
        get_pred net_db n head >|= convert
    | `Hash h ->
        read_valid_block_exn node h >|= convert
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator = get_validator node block in
        let pv = Validator.prevalidator validator in
        let net_state = Validator.net_state validator in
        State.Valid_block.Current.head net_state >>= fun head ->
        let ctxt = Prevalidator.context pv in
        Context.get_fitness ctxt >|= fun fitness ->
        { (convert head) with
          hash = prevalidation_hash ;
          fitness ;
          timestamp = Prevalidator.timestamp pv
        }

  let get_context node block =
    match block with
    | `Genesis ->
        State.Valid_block.Current.genesis node.mainnet_net >>= fun block ->
        Lwt.return (Some block.context)
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let net_state = Validator.net_state validator in
        let net_db = Validator.net_db validator in
        State.Valid_block.Current.head net_state >>= fun head ->
        get_pred net_db n head >>= fun { context } ->
        Lwt.return (Some context)
    | `Hash hash-> begin
        read_valid_block node hash >|= function
        | None -> None
        | Some { context } -> Some context
      end
    | ( `Prevalidation | `Test_prevalidation ) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        Lwt.return (Some (Prevalidator.context pv))

  let operations node block =
    match block with
    | `Genesis ->
        State.Valid_block.Current.genesis node.mainnet_net >>= fun { operations } ->
        Lwt.return operations
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let net_state = Validator.net_state validator in
        let net_db = Validator.net_db validator in
        State.Valid_block.Current.head net_state >>= fun head ->
        get_pred net_db n head >>= fun { operations } ->
        Lwt.return operations
    | (`Prevalidation | `Test_prevalidation) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        let { Updater.applied }, _ = Prevalidator.operations pv in
        Lwt.return [applied]
    | `Hash hash->
        read_valid_block node hash >|= function
        | None -> []
        | Some { operations } -> operations

  let operation_content node hash =
    Distributed_db.read_operation node.distributed_db hash >>= fun op ->
    Lwt.return (map_option ~f:snd op)

  let pending_operations node (block: block) =
    match block with
    | ( `Head 0 | `Prevalidation
      | `Test_head 0 | `Test_prevalidation ) as block ->
        let validator, _net = get_net node block in
        let pv = Validator.prevalidator validator in
        Lwt.return (Prevalidator.operations pv)
    | ( `Head n | `Test_head n ) as block ->
        let validator = get_validator node block in
        let prevalidator = Validator.prevalidator validator in
        let net_state = Validator.net_state validator in
        let net_db = Validator.net_db validator in
        State.Valid_block.Current.head net_state >>= fun head ->
        get_pred net_db n head >>= fun b ->
        Prevalidator.pending ~block:b prevalidator >|= fun ops ->
        Updater.empty_result, ops
    | `Genesis ->
        let net = node.mainnet_net in
        State.Valid_block.Current.genesis net >>= fun b ->
        let validator = get_validator node `Genesis in
        let prevalidator = Validator.prevalidator validator in
        Prevalidator.pending ~block:b prevalidator >|= fun ops ->
        Updater.empty_result, ops
    | `Hash h -> begin
        get_validator_per_hash node h >>= function
        | None ->
            Lwt.return (Updater.empty_result, Operation_hash.Set.empty)
        | Some (validator, net_db) ->
            let net_state = Distributed_db.state net_db in
            let prevalidator = Validator.prevalidator validator in
            State.Valid_block.read_exn net_state h >>= fun block ->
            Prevalidator.pending ~block prevalidator >|= fun ops ->
            Updater.empty_result, ops
      end

  let protocols { state } =
    State.Protocol.list state >>= fun set ->
    Lwt.return (Protocol_hash.Set.elements set)

  let protocol_content node hash =
    State.Protocol.read node.state hash

  let preapply node block ~timestamp ~sort ops =
    begin
      match block with
      | `Genesis ->
          let net = node.mainnet_net in
          State.Valid_block.Current.genesis net >>= return
      | ( `Head 0 | `Prevalidation
        | `Test_head 0 | `Test_prevalidation ) as block ->
          let validator = get_validator node block in
          let net_state = Validator.net_state validator in
          State.Valid_block.Current.head net_state >>= return
      | `Head n | `Test_head n as block -> begin
          let validator = get_validator node block in
          let net_state = Validator.net_state validator in
          let net_db = Validator.net_db validator in
          State.Valid_block.Current.head net_state >>= fun head ->
          get_pred net_db n head >>= return
        end
      | `Hash hash ->
          read_valid_block node hash >>= function
          | None -> Lwt.return (error_exn Not_found)
          | Some data -> return data
    end >>=? fun { hash ; context ; protocol } ->
    begin
      match protocol with
      | None -> failwith "Unknown protocol version"
      | Some protocol -> return protocol
    end >>=? fun ((module Proto) as protocol) ->
    let net_db = Validator.net_db node.mainnet_validator in
    Prevalidator.preapply
      net_db context protocol hash timestamp sort ops >>=? fun (ctxt, r) ->
    Context.get_fitness ctxt >>= fun fitness ->
    return (fitness, r)

  let complete node ?block str =
    match block with
    | None ->
        Base58.complete str
    | Some block ->
        get_context node block >>= function
        | None -> Lwt.fail Not_found
        | Some ctxt ->
            Context.get_protocol ctxt >>= fun protocol_hash ->
            let (module Proto) = Updater.get_exn protocol_hash in
            Base58.complete str >>= fun l1 ->
            Proto.complete_b58prefix ctxt str >>= fun l2 ->
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
    State.Valid_block.known_heads node.mainnet_net >>= fun heads ->
    begin
      match Validator.test_validator node.mainnet_validator with
      | None -> Lwt.return_nil
      | Some (_, net_db) ->
          State.Valid_block.known_heads (Distributed_db.state net_db)
    end >>= fun test_heads ->
    let map =
      List.fold_left
        (fun map block ->
           Block_hash.Map.add
             block.State.Valid_block.hash (convert block) map)
        Block_hash.Map.empty (test_heads @ heads) in
    Lwt.return map

  let predecessors node len head =
    let rec loop net_db acc len hash (block: State.Block_header.t) =
      if Block_hash.equal block.shell.predecessor hash then
        Lwt.return (List.rev acc)
      else begin
        if len = 0 then
          Lwt.return (List.rev acc)
        else
          let hash = block.shell.predecessor in
          Distributed_db.Block_header.read_exn net_db hash >>= fun block ->
          loop net_db (hash :: acc) (len-1) hash block
      end in
    try
      Distributed_db.read_block_exn
        node.distributed_db head >>= fun (net_db, block) ->
      loop net_db [] len head block
    with Not_found -> Lwt.return_nil

  let predecessors_bi state ignored len head =
    try
      let rec loop acc len hash =
        State.Valid_block.read_exn state hash >>= fun block ->
        let bi = convert block in
        if Block_hash.equal bi.predecessor hash then
          Lwt.return (List.rev (bi :: acc))
        else begin
          if len = 0
             || Block_hash.Set.mem hash ignored then
          Lwt.return (List.rev acc)
        else
          loop (bi :: acc) (len-1) bi.predecessor
        end in
      loop [] len head
    with Not_found -> Lwt.return_nil

  let list node len heads =
    Lwt_list.fold_left_s
      (fun (ignored, acc) head ->
         Distributed_db.read_block_exn
           node.distributed_db head >>= fun (net_db, _block) ->
         let net_state = Distributed_db.state net_db in
         predecessors_bi net_state ignored len head >|= fun predecessors ->
         let ignored =
           List.fold_right
             (fun x s -> Block_hash.Set.add x.hash s)
             predecessors ignored in
         ignored, predecessors :: acc
      )
      (Block_hash.Set.empty, [])
      heads >>= fun (_, blocks) ->
    Lwt.return (List.rev blocks)

  let block_watcher node =
    let stream, shutdown = Distributed_db.watch_block node.distributed_db in
    Lwt_stream.map
      (fun (hash, block) -> convert_block hash block)
      stream,
    shutdown

  let valid_block_watcher node =
    let stream, shutdown = Validator.global_watcher node.validator in
    Lwt_stream.map (fun block -> convert block) stream,
    shutdown

  let operation_watcher node =
    Distributed_db.watch_operation node.distributed_db

  let protocol_watcher node =
    Distributed_db.watch_protocol node.distributed_db

  let validate node net_id block =
    Validator.get node.validator net_id >>=? fun net_v ->
    Validator.fetch_block net_v block >>=? fun _ ->
    return ()

  let bootstrapped node =
    let block_stream, stopper =
      Validator.new_head_watcher node.mainnet_validator in
    let first_run = ref true in
    let rec next () =
      if !first_run then begin
        first_run := false ;
        State.Valid_block.Current.head node.mainnet_net >>= fun head ->
        Lwt.return (Some (head.hash, head.timestamp))
      end else begin
        Lwt.pick [
          ( Lwt_stream.get block_stream >|=
            map_option ~f:(fun b -> (b.State.Valid_block.hash, b.timestamp)) ) ;
          (Validator.bootstrapped node.mainnet_validator >|= fun () -> None) ;
        ]
      end in
    let shutdown () = Watcher.shutdown stopper in
    RPC.Answer.{ next ; shutdown }

  module Network = struct

    let stat (node : t) =
      P2p.RPC.stat node.p2p

    let watch (node : t) =
      P2p.RPC.watch node.p2p

    let connect (node : t) =
      P2p.RPC.connect node.p2p

    module Connection = struct

      let info (node : t) =
        P2p.RPC.Connection.info node.p2p

      let kick (node : t) =
        P2p.RPC.Connection.kick node.p2p

      let list (node : t) =
        P2p.RPC.Connection.list node.p2p

      let count (node : t) =
        P2p.RPC.Connection.count node.p2p

    end

    module Point = struct

      let info (node : t) =
        P2p.RPC.Point.info node.p2p

      let list (node : t) restrict =
        P2p.RPC.Point.list ~restrict node.p2p

      let events (node : t) =
        P2p.RPC.Point.events node.p2p

      let watch (node : t) =
        P2p.RPC.Point.watch node.p2p

    end

    module Peer_id = struct

      let info (node : t) =
        P2p.RPC.Peer_id.info node.p2p

      let list (node : t) restrict =
        P2p.RPC.Peer_id.list ~restrict node.p2p

      let events (node : t) =
        P2p.RPC.Peer_id.events node.p2p

      let watch (node : t) =
        P2p.RPC.Peer_id.watch node.p2p

    end

  end

end
