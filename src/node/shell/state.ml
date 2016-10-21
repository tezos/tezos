(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.State

type error +=
  | Invalid_fitness of Fitness.fitness * Fitness.fitness
  | Unknown_protocol of Protocol_hash.t
  | Inactive_network of Store.net_id
  | Unknown_network of Store.net_id
  | Cannot_parse

let () =
  Error_monad.register_error_kind
    `Temporary
    ~id:"state.invalid_fitness"
    ~title:"Invalid fitness"
    ~description:"The computed fitness differs from the fitness found \
                 \ in the block header."
    ~pp:(fun ppf (expected, found) ->
        Format.fprintf ppf
          "@[<v 2>Invalid fitness@ \
           \ expected %a@ \
          \ found %a"
          Fitness.pp expected
          Fitness.pp found)
    Data_encoding.(obj2
                     (req "expected" Fitness.encoding)
                     (req "found" Fitness.encoding))
    (function Invalid_fitness (e, f) -> Some (e, f) | _ -> None)
    (fun (e, f) -> Invalid_fitness (e, f)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"state.unknown_network"
    ~title:"Unknown network"
    ~description:"TODO"
    ~pp:(fun ppf (Store.Net id) ->
        Format.fprintf ppf "Unknown network %a" Block_hash.pp_short id)
    Data_encoding.(obj1 (req "net" Updater.net_id_encoding))
    (function Unknown_network x -> Some x | _ -> None)
    (fun x -> Unknown_network x) ;

module Watcher = struct

  type 'a t = {
    id: int ;
    push: ('a option -> unit) ;
  }

  let notify watchers info =
    List.iter (fun w -> w.push (Some info)) watchers

  let create_stream watchers =
    let cpt = ref 0 in
    fun () ->
      let id = incr cpt ; !cpt in
      let stream, push = Lwt_stream.create () in
      watchers := { id ; push } :: !watchers ;
      let unregister () =
        push None ;
        watchers := List.filter (fun w -> w.id <> id) !watchers in
      stream, unregister

end

(** *)

type net_id = Store.net_id = Net of Block_hash.t

type t = {
  mutable active_net: net list ;
  nets: net Block_hash_table.t ;
  store: Store.store ;
  block_db: Db_proxy.Block.t ;
  block_watchers: (Block_hash.t * Store.block) Watcher.t list ref ;
  operation_db: Db_proxy.Operation.t ;
  operation_watchers:
    (Operation_hash.t * Store.operation) Watcher.t list ref ;
  protocol_db: Db_proxy.Protocol.t ;
  protocol_watchers:
    (Protocol_hash.t * Store.protocol) Watcher.t list ref ;
  valid_block_state: valid_block_state Persist.shared_ref ;
}

and state = t

and net = {
  state: state ;
  net_store: Store.net_store ;
  blockchain_state: blockchain_state Persist.shared_ref ;
}

and valid_block_state = {
  global_store: Store.generic_store Persist.shared_ref ;
  ttl: Int64.t ;
  index: Context.index ;
  block_db: Db_proxy.Block.t ;
  watchers: valid_block Watcher.t list ref ;
}

and blockchain_state = {
  genesis_block: valid_block ;
  current_head: valid_block ;
  current_protocol: (module Updater.REGISTRED_PROTOCOL) ;
  mempool: Operation_hash_set.t ;
  blockchain_store: Store.blockchain_store Persist.shared_ref ;
}

and valid_block = {
  net_id: net_id ;
  hash: Block_hash.t ;
  pred: Block_hash.t ;
  timestamp: Time.t ;
  fitness: Protocol.fitness ;
  operations: Operation_hash.t list ;
  discovery_time: Time.t ;
  protocol_hash: Protocol_hash.t ;
  protocol: (module Updater.REGISTRED_PROTOCOL) option ;
  test_protocol_hash: Protocol_hash.t ;
  test_protocol: (module Updater.REGISTRED_PROTOCOL) option ;
  test_network: (net_id * Time.t) option ;
  context: Context.t ;
  successors: Block_hash_set.t ;
  invalid_successors: Block_hash_set.t ;
}

module KnownHeads_key = struct
  include Block_hash
  let prefix = ["state"; "known_heads"]
  let length = path_len
end
module KnownHeads =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (KnownHeads_key) (Block_hash_set)

module KnownNets_key = struct
  include Block_hash
  let prefix = ["state"; "known_nets"]
  let length = path_len
end
module KnownNets =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (KnownNets_key) (Block_hash_set)

module InvalidOperations_key = struct
  include Operation_hash
  let prefix = ["state"; "invalid_operations"]
  let length = path_len
end
module InvalidOperations =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (InvalidOperations_key) (Operation_hash_set)

module InvalidProtocols_key = struct
  include Protocol_hash
  let prefix = ["state"; "invalid_protocols"]
  let length = path_len
end
module InvalidProtocols =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (InvalidProtocols_key) (Protocol_hash_set)

module InvalidBlocks_key = struct
  include Block_hash
  let prefix = ["state"; "invalid_blocks"]
  let length = path_len
end
module InvalidBlocks =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (InvalidBlocks_key) (Block_hash_set)

module PostponedBlocks_key = struct
  include Block_hash
  let prefix = ["state"; "postponed_blocks"]
  let length = path_len
end
module PostponedBlocks =
  Persist.MakeBufferedPersistentSet
    (Store.Faked_functional_store) (PostponedBlocks_key) (Block_hash_set)

let net_is_active { active_net } net_id =
  let same_id (Net id) { net_store = { net_genesis = { block } } } =
    Block_hash.equal id block in
  List.exists (same_id net_id) active_net

module Operation = struct
  type key = Store.Operation.key
  type shell_header = Store.shell_operation = {
    net_id: net_id ;
  }
  type t = Store.operation = {
    shell: shell_header ;
    proto: MBytes.t ;
  }
  type operation = t
  exception Invalid of key * error list
  let of_bytes = Store.Operation.of_bytes
  let to_bytes = Store.Operation.to_bytes
  let known t k = Db_proxy.Operation.known t.operation_db k
  let read t k = Db_proxy.Operation.read t.operation_db k
  let read_exn t k =
    Db_proxy.Operation.read t.operation_db k >>= function
    | None -> Lwt.fail Not_found
    | Some { data = Error e } -> Lwt.fail (Invalid (k, e))
    | Some { data = Ok data ; time }  -> Lwt.return { Time.data ; time }
  let hash = Store.Operation.hash
  let raw_read t k =
    Persist.use t.store.Store.operation
      (fun store -> Store.Operation.raw_get store k)
  let prefetch t net_id ks =
    List.iter (Db_proxy.Operation.prefetch t.operation_db net_id) ks
  let fetch t net_id k = Db_proxy.Operation.fetch t.operation_db net_id k
  let store t bytes =
    match of_bytes bytes with
    | None -> fail Cannot_parse
    | Some op ->
        if not (net_is_active t op.shell.net_id) then
          fail (Inactive_network op.shell.net_id)
        else
          let h = hash op in
          Db_proxy.Operation.store t.operation_db h (Time.make_timed (Ok op))
          >>= function
          | true ->
              Watcher.notify !(t.operation_watchers) (h, op) ;
              return (Some (h, op))
          | false ->
              return None
  let mark_invalid t k e =
    Db_proxy.Operation.update t.operation_db k (Time.make_timed (Error e))
    >>= function
    | true ->
        Persist.update t.store.global_store (fun store ->
            InvalidOperations.set store k >>= fun store ->
            Lwt.return (Some store)) >>= fun _ ->
        Lwt.return true
    | false -> Lwt.return false

  let invalid state =
    Persist.use state.store.global_store InvalidOperations.read

  let create_watcher t = Watcher.create_stream t.operation_watchers ()

end

module Protocol = struct
  type key = Store.Protocol.key

  type component = Store.component = {
    name: string;
    interface: string option;
    implementation: string
  }

  type t = Store.protocol

  type protocol = t
  exception Invalid of key * error list
  let of_bytes = Store.Protocol.of_bytes
  let to_bytes = Store.Protocol.to_bytes
  let known t k = Db_proxy.Protocol.known t.protocol_db k
  let read t k = Db_proxy.Protocol.read t.protocol_db k
  let read_exn t k =
    Db_proxy.Protocol.read t.protocol_db k >>= function
    | None -> Lwt.fail Not_found
    | Some { data = Error e } -> Lwt.fail (Invalid (k, e))
    | Some { data = Ok data ; time }  -> Lwt.return { Time.data ; time }
  let hash = Store.Protocol.hash
  let raw_read t k =
    Persist.use t.store.Store.protocol
      (fun store -> Store.Protocol.raw_get store k)
  let prefetch t net_id ks =
    List.iter (Db_proxy.Protocol.prefetch t.protocol_db net_id) ks
  let fetch t net_id k = Db_proxy.Protocol.fetch t.protocol_db net_id k
  let store t bytes =
    match of_bytes bytes with
    | None -> fail Cannot_parse
    | Some proto ->
        let h = hash proto in
        Db_proxy.Protocol.store t.protocol_db h (Time.make_timed (Ok proto))
        >>= function
        | true ->
            Watcher.notify !(t.protocol_watchers) (h, proto) ;
            return (Some (h, proto))
        | false ->
            return None
  let mark_invalid t k e =
    Db_proxy.Protocol.update t.protocol_db k (Time.make_timed (Error e))
    >>= function
    | true ->
        Persist.update t.store.global_store (fun store ->
            InvalidProtocols.set store k >>= fun store ->
            Lwt.return (Some store)) >>= fun _ ->
        Lwt.return true
    | false -> Lwt.return false

  let invalid state =
    Persist.use state.store.global_store InvalidProtocols.read

  let create_watcher t = Watcher.create_stream t.protocol_watchers ()

  let keys { protocol_db } = Db_proxy.Protocol.keys protocol_db

end

let iter_predecessors
    (type t)
    (compare: t -> t -> int)
    (predecessor: state -> t -> t option Lwt.t)
    (date: t -> Time.t)
    (fitness: t -> Fitness.fitness)
    state ?max ?min_fitness ?min_date heads ~f =
  let module Local = struct exception Exit end in
  let pop, push =
    (* Poor-man priority queue *)
    let queue : t list ref = ref [] in
    let pop () =
      match !queue with
      | [] -> None
      | b :: bs -> queue := bs ; Some b in
    let push b =
      let rec loop = function
        | [] -> [b]
        | b' :: bs' as bs ->
            let cmp = compare b b' in
            if cmp = 0 then
              bs
            else if cmp < 0 then
              b' :: loop bs'
            else
              b :: bs in
      queue := loop !queue in
    pop, push in
  let check_count =
    match max with
    | None -> (fun () -> ())
    | Some max ->
        let cpt = ref 0 in
        fun () ->
          if !cpt >= max then raise Local.Exit ;
          incr cpt in
  let check_fitness =
    match min_fitness with
    | None -> (fun _ -> true)
    | Some min_fitness ->
        (fun b -> Fitness.compare min_fitness (fitness b) <= 0) in
  let check_date =
    match min_date with
    | None -> (fun _ -> true)
    | Some min_date ->  (fun b -> Time.compare min_date (date b) <= 0) in
  let rec loop () =
      match pop () with
      | None -> return ()
      | Some b ->
          check_count () ;
          f b >>= fun () ->
          predecessor state b >>= function
          | None -> loop ()
          | Some p ->
              if check_fitness p && check_date p then push p ;
              loop () in
    List.iter push heads ;
    try loop () with Local.Exit -> return ()

module Block = struct

  type shell_header = Store.shell_block = {
    net_id: net_id ;
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    fitness: MBytes.t list ;
    operations: Operation_hash.t list ;
  }
  type t = Store.block = {
    shell: shell_header ;
    proto: MBytes.t ;
  }
  type block = t
  let of_bytes = Store.Block.of_bytes
  let to_bytes = Store.Block.to_bytes

  let known t k = Db_proxy.Block.known t.block_db k
  let db_read db k =
    Db_proxy.Block.read db k >>= function
    | None -> Lwt.return_none
    | Some (_, lazy block) -> block
  let read t k = db_read t.block_db k
  let read_exn t k =
    read t k >>= function
    | None -> Lwt.fail Not_found
    | Some { data = data ; time }  -> Lwt.return { Time.data ; time }
  let hash = Store.Block.hash
  let raw_read t k =
    Persist.use t.store.Store.block
      (fun store -> Store.Block.raw_get store k)
  let read_pred t k =
    Db_proxy.Block.read t.block_db k >>= function
    | None -> Lwt.return_none
    | Some (pred, _) -> Lwt.return (Some pred)
  let read_pred_exn t k =
    read_pred t k >>= function
    | None -> Lwt.fail Not_found
    | Some pred -> Lwt.return pred
  let prefetch t net_id ks =
    List.iter (Db_proxy.Block.prefetch t.block_db net_id) ks
  let fetch t net_id k =
    Db_proxy.Block.fetch t.block_db net_id k >>= fun (_, lazy block) ->
    block >>= function
    | None -> assert false
    | Some block -> Lwt.return block
  let db_store db k (v: Store.block) =
    Db_proxy.Block.store db k
      (v.shell.predecessor, lazy (Lwt.return (Some (Time.make_timed v))))
  let store t bytes =
    match of_bytes bytes with
    | None -> fail Cannot_parse
    | Some b ->
        if not (net_is_active t b.shell.net_id) then
          fail (Inactive_network b.shell.net_id)
        else
          let h = hash b in
          db_store t.block_db h b >>= function
          | true ->
              Persist.update t.store.global_store (fun store ->
                  PostponedBlocks.set store h >>= fun store ->
                  Lwt.return (Some store)) >>= fun _ ->
              Watcher.notify !(t.block_watchers) (h, b) ;
              return (Some (h, b))
          | false -> return None
  let create_watcher t = Watcher.create_stream t.block_watchers ()

  let check_block state h =
    known state h >>= function
    | true -> return ()
    | false -> failwith "Unknown block"

  let path state h1 h2 =
    trace_exn (Failure "State.path") begin
      check_block state h1 >>=? fun () ->
      check_block state h2 >>=? fun () ->
      let rec loop acc h =
        if Block_hash.equal h h1 then
          return acc
        else
          read_pred state h >>= function
          | None -> failwith "not an ancestor"
          | Some pred ->
              loop (h :: acc) pred in
      loop [] h2
    end

  let common_ancestor state h1 h2 =
    trace_exn (Failure "State.common_ancestor") begin
      check_block state h1 >>=? fun () ->
      check_block state h2 >>=? fun () ->
      let queue = Queue.create () in
      let rec visit seen =
        let h = Queue.pop queue in
        if Block_hash_set.mem h seen then
          return h
        else
          let seen = Block_hash_set.add h seen in
          read_pred state h >>= function
          | None -> failwith ".."
          | Some pred ->
              if not (Block_hash.equal pred h) then
                Queue.push pred queue;
              visit seen
      in
      Queue.push h1 queue;
      Queue.push h2 queue;
      Lwt.catch
        (fun () -> visit Block_hash_set.empty)
        (function exn -> Lwt.return (error_exn exn))
    end

  let rec block_locator_loop state acc sz step cpt h =
    if sz = 0 then Lwt.return (List.rev acc) else
    read_pred state h >>= function
    | None -> Lwt.return (List.rev (h :: acc))
    | Some pred ->
        if cpt = 0 then
          block_locator_loop state
            (h :: acc) (sz - 1) (step * 2) (step * 20 - 1) pred
        else if cpt mod step = 0 then
          block_locator_loop state (h :: acc) (sz - 1) step (cpt - 1) pred
        else
          block_locator_loop state acc sz step (cpt - 1) pred

  let block_locator state sz h =
    trace_exn (Failure "State.block_locator") begin
      check_block state h >>=? fun () ->
      block_locator_loop state [] sz 1 9 h >>= fun locator ->
      return locator
    end

  let iter_predecessors =
    let compare b1 b2 =
      match Fitness.compare b1.shell.fitness b2.shell.fitness with
      | 0 -> begin
          match Time.compare b1.shell.timestamp b2.shell.timestamp with
          | 0 -> Block_hash.compare (hash b1) (hash b2)
          | res -> res
        end
      | res -> res in
    let predecessor state b =
      read state b.shell.predecessor >|= function
      | None -> None
      | Some { data } ->
          if Block_hash.equal data.shell.predecessor b.shell.predecessor
             && Block_hash.equal (hash b) b.shell.predecessor
          then
            None
          else
            Some data in
    iter_predecessors compare predecessor
      (fun b -> b.shell.timestamp) (fun b -> b.shell.fitness)

end

module Valid_block = struct

  type t = valid_block = {
    net_id: net_id ;
    hash: Block_hash.t ;
    pred: Block_hash.t ;
    timestamp: Time.t ;
    fitness: Fitness.fitness ;
    operations: Operation_hash.t list ;
    discovery_time: Time.t ;
    protocol_hash: Protocol_hash.t ;
    protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    test_protocol_hash: Protocol_hash.t ;
    test_protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    test_network: (net_id * Time.t) option ;
    context: Context.t ;
    successors: Block_hash_set.t ;
    invalid_successors: Block_hash_set.t ;
  }
  type valid_block = t

  let use state f = Persist.use state.valid_block_state f
  let update state f = Persist.update state.valid_block_state f
  let update_with_res state f = Persist.update_with_res state.valid_block_state f

  let raw_read' { Time.data = { Store.shell = block } ;
                  time = discovery_time } successors invalid_successors index hash =
    Context.checkout index hash >>= function
    | (None | Some (Error _)) as e -> Lwt.return e
    | Some (Ok context) ->
        Context.get_protocol context >>= fun protocol_hash ->
        Context.get_test_protocol context >>= fun test_protocol_hash ->
        Context.get_test_network context >>= fun test_network ->
        Context.get_test_network_expiration
          context >>= fun test_network_expiration ->
        let test_network =
          match test_network, test_network_expiration with
          | None, _ | _, None -> None
          | Some net_id, Some time -> Some (net_id, time) in
        let protocol = Updater.get protocol_hash in
        let test_protocol = Updater.get test_protocol_hash in
        let valid_block = {
          net_id = block.net_id ;
          hash ;
          pred = block.predecessor ;
          timestamp = block.timestamp ;
          discovery_time ;
          operations = block.operations ;
          fitness = block.fitness ;
          protocol_hash ;
          protocol ;
          test_protocol_hash ;
          test_protocol ;
          test_network ;
          context ;
          successors ;
          invalid_successors ;
        } in
        Lwt.return (Some (Ok valid_block))

  let raw_read store block_db index hash =
    Block.db_read block_db hash >>= function
    | None ->
        (* TODO handle internal error... *)
        Lwt.return_none
    | Some block ->
        Persist.use store (fun store ->
            Store.Block_valid_succs.get store hash >|= function
            | None -> Block_hash_set.empty
            | Some set -> set) >>= fun valid_successors ->
        Persist.use store (fun store ->
            Store.Block_invalid_succs.get store hash >|= function
            | None -> Block_hash_set.empty
            | Some set -> set) >>= fun invalid_successors ->
        raw_read' block valid_successors invalid_successors index hash

  let create ?patch_context ~context_root store block_db ttl =
    Context.init ?patch_context ~root:context_root >>= fun index ->
    let ttl = Int64.of_int ttl in
    Lwt.return
      (Persist.share { global_store = store ;
                       block_db ; index ;
                       ttl ; watchers = ref []  })

  let locked_valid vstate h =
    Context.checkout vstate.index h >>= function
    | None | Some (Error _) -> Lwt.return_false
    | Some (Ok _) -> Lwt.return true

  let locked_known vstate h = Context.exists vstate.index h

  exception Invalid of Block_hash.t * error list

  let locked_read (vstate: valid_block_state) hash =
    raw_read vstate.global_store vstate.block_db vstate.index hash

  let locked_read_exn vstate hash =
    locked_read vstate hash >>= function
    | None -> Lwt.fail Not_found
    | Some (Error e) -> Lwt.fail (Invalid (hash, e))
    | Some (Ok data) -> Lwt.return data

  let locked_store vstate hash context =
    Context.exists vstate.index hash >>= function
    | true -> Lwt.return (Error []) (* TODO fail ?? *)
    | false ->
        Block.db_read vstate.block_db hash >>= function
        | None -> assert false
        | Some { data = block } ->
            Context.get_protocol context >>= fun protocol_hash ->
            match Updater.get protocol_hash with
            | None ->
                lwt_log_error
                  "State.Validated_block: unknown protocol (%a)"
                  Protocol_hash.pp_short protocol_hash >>= fun () ->
                Lwt.return (Error [Unknown_protocol protocol_hash])
            | Some (module Proto) ->
                Proto.fitness context >>= fun fitness ->
                if Fitness.compare fitness block.Store.shell.fitness <> 0
                then begin
                  let err = Invalid_fitness (block.Store.shell.fitness, fitness) in
                  Context.commit_invalid
                    vstate.index block hash [err] >>= fun () ->
                  Lwt.return (Error [err])
                end else begin
                  Context.read_and_reset_fork_test_network
                    context >>= fun (fork, context) ->
                  begin
                    if fork then begin
                      let eol = Time.(add block.shell.timestamp vstate.ttl) in
                      Context.set_test_network
                        context (Net hash) >>= fun context ->
                      Context.set_test_network_expiration context
                        eol >>= fun context ->
                      lwt_log_notice "Fork test network for %a (eol: %a)"
                        Block_hash.pp_short hash Time.pp_hum eol >>= fun () ->
                      Lwt.return context
                    end else begin
                      Context.get_test_network_expiration context >>= function
                      | Some eol when Time.(eol <= now ()) ->
                          lwt_log_notice
                            "Stop test network for %a (eol: %a, now: %a)"
                            Block_hash.pp_short hash
                            Time.pp_hum eol Time.pp_hum (Time.now ())
                          >>= fun () ->
                          Context.del_test_network context >>= fun context ->
                          Context.del_test_network_expiration context
                      | None | Some _ -> Lwt.return context
                    end
                  end >>= fun context ->
                  Context.commit vstate.index block hash context >>= fun () ->
                  locked_read_exn vstate hash >>= fun valid_block ->
                  Persist.update vstate.global_store (fun store ->
                      KnownHeads.del store block.shell.predecessor >>= fun store ->
                      KnownHeads.set store hash >>= fun store ->
                      PostponedBlocks.del store hash >>= fun store ->
                      begin
                        Store.Block_valid_succs.get
                          store block.shell.predecessor >|= function
                        | None -> Block_hash_set.singleton hash
                        | Some set -> Block_hash_set.add hash set
                      end >>= fun successors ->
                      Store.Block_valid_succs.set
                        store block.shell.predecessor successors >>= fun () ->
                      Lwt.return (Some store)) >>= fun _ ->
                  Watcher.notify !(vstate.watchers) valid_block ;
                  Lwt.return (Ok valid_block)
                end

  let create_genesis_block state (genesis: Store.genesis) test_protocol =
    use state (fun vstate ->
      locked_read vstate genesis.block >>= function
      | Some res ->
          (* TODO check coherency: test_protocol. *)
          Lwt.return res
      | None ->
          let test_protocol = Utils.unopt genesis.protocol test_protocol in
          Context.create_genesis_context
            vstate.index genesis test_protocol >>= fun _context ->
          Block.db_store vstate.block_db genesis.block {
            shell = {
              net_id = Net genesis.block ;
              predecessor = genesis.block ;
              timestamp = genesis.time ;
              fitness = [] ;
              operations = [] ;
            } ;
            proto = MBytes.create 0 ;
          } >>= fun _ ->
          locked_read vstate genesis.block >>= function
          | None -> failwith ""
          | Some (Error _ as err) -> Lwt.return err
          | Some (Ok valid_block) ->
              Persist.update vstate.global_store (fun store ->
                  KnownHeads.set store valid_block.hash >>= fun store ->
                  Lwt.return (Some store)) >>= fun _ ->
              return valid_block)

  let locked_store_invalid vstate hash exns =
    Context.exists vstate.index hash >>= function
    | true -> Lwt.return false (* TODO fail ?? *)
    | false ->
        Block.db_read vstate.block_db hash >>= function
        | None -> assert false
        | Some { data = block } ->
            Context.commit_invalid vstate.index block hash exns >>= fun () ->
            Persist.update vstate.global_store (fun store ->
                InvalidBlocks.set store hash >>= fun store ->
                begin
                  Store.Block_invalid_succs.get
                    store block.shell.predecessor >|= function
                  | None -> Block_hash_set.singleton hash
                  | Some set -> Block_hash_set.add hash set
                end >>= fun successors ->
                Store.Block_invalid_succs.set
                  store block.shell.predecessor successors >>= fun () ->
                Lwt.return (Some store)) >>= fun _ ->
            Lwt.return true

  let get_store { valid_block_state } = valid_block_state

  let valid state h =
    use state (fun vstate -> locked_valid vstate h)
  let known state h =
    use state (fun vstate -> locked_known vstate h)
  let read state hash =
    use state (fun vstate -> locked_read vstate hash)
  let read_exn state hash =
    use state (fun vstate -> locked_read_exn vstate hash)
  let store state hash context =
    use state
      (fun vstate -> locked_store vstate hash context) >>= fun block ->
    Lwt.return block
  let store_invalid state hash exns =
    use state (fun vstate -> locked_store_invalid vstate hash exns)

  let known_heads state =
    use state (fun vstate ->
        Persist.use vstate.global_store KnownHeads.read >>= fun heads ->
        let elements = Block_hash_set.elements heads in
        Lwt_list.fold_left_s
          (fun set hash ->
             Block.db_read vstate.block_db hash >>= function
           | None -> Lwt.return set
           | Some block ->
               Persist.use vstate.global_store (fun store ->
                   begin
                     Store.Block_invalid_succs.get
                       store block.data.shell.predecessor >|= function
                     | None -> Block_hash_set.singleton hash
                     | Some set -> set
                   end) >>= fun invalid_successors ->
               raw_read' block Block_hash_set.empty
                 invalid_successors vstate.index hash >>= function
               | Some (Ok bl) -> Lwt.return (Block_hash_map.add hash bl set)
               | None | Some (Error _) ->
                   lwt_log_error
                     "Error while reading \"known_heads\". Ignoring %a."
                     Block_hash.pp_short hash >>= fun () ->
                   Lwt.return set)
          Block_hash_map.empty
          elements)

  let postponed state =
    use state (fun vstate ->
        Persist.use vstate.global_store PostponedBlocks.read)

  let invalid state =
    use state (fun vstate ->
        Persist.use vstate.global_store InvalidBlocks.read)

  let path state b1 b2 =
    let rec loop acc b =
      if Block_hash.equal b.hash b1.hash then
        Lwt.return (Some acc)
      else
        read state b.pred >>= function
        | None -> Lwt.return None
        | Some (Error _) -> assert false
        | Some (Ok pred) -> loop (b :: acc) pred in
    loop [] b2

  let common_ancestor state b1 b2 =
    let queue = Queue.create () in
    let rec visit seen =
      let b = Queue.pop queue in
      if Block_hash_set.mem b.hash seen then
        Lwt.return b
      else
        let seen = Block_hash_set.add b.hash seen in
        read state b.pred >>= function
        | None -> visit seen
        | Some (Error _) -> assert false
        | Some (Ok pred) ->
            if not (Block_hash.equal pred.hash b.hash) then
              Queue.push pred queue;
            visit seen
    in
    Queue.push b1 queue;
    Queue.push b2 queue;
    visit Block_hash_set.empty

  let block_locator state sz b =
    Block.block_locator_loop state [] sz 1 9 b.hash

  let new_blocks state cur_block new_block =
    common_ancestor state cur_block new_block >>= fun ancestor ->
    path state ancestor new_block >>= function
    | None -> assert false
    | Some path -> Lwt.return (ancestor, path)

  let create_watcher state =
    use state (fun vstate ->
        Lwt.return (Watcher.create_stream vstate.watchers ()))

  module Store = struct
    type t = valid_block_state
    type key = Block_hash.t
    type value = Context.t tzresult
    let mem vstate h = locked_known vstate h
    let del _ _ = assert false (* unused *)
    let get vstate hash =
      locked_read vstate hash >>= function
      | None -> Lwt.return None
      | Some (Ok { context }) -> Lwt.return (Some (Ok context))
      | Some (Error exns) -> Lwt.return (Some (Error exns))
    let set vstate hash = function
      | Ok context -> begin
          locked_store vstate hash context >>= fun _ ->
          Lwt.return vstate
        end
      | Error exns ->
          locked_store_invalid vstate hash exns >>= fun _changed ->
          Lwt.return vstate

    let keys _ = Store.undefined_key_fn
  end

  let iter_predecessors =
    let compare b1 b2 =
      match Fitness.compare b1.fitness b2.fitness with
      | 0 -> begin
          match Time.compare b1.timestamp b2.timestamp with
          | 0 -> Block_hash.compare b1.hash b2.hash
          | res -> res
        end
      | res -> res in
    let predecessor state b =
      if Block_hash.equal b.hash b.pred then
        Lwt.return None
      else
        read state b.pred >|= function
        | None | Some (Error _) -> None
        | Some (Ok b) -> Some b in
    iter_predecessors compare predecessor
      (fun b -> b.timestamp) (fun b -> b.fitness)

end

module Blockchain = struct

  let use state f = Persist.use state.blockchain_state f
  let update state f = Persist.update state.blockchain_state f

  let read_state, store_state =
    let current_block_key = ["current_block"] in
    let module Mempool_key = struct
      include Operation_hash
      let prefix = ["mempool"]
      let length = path_len
    end in
    let module Mempool =
      Persist.MakeBufferedPersistentSet
        (Store.Faked_functional_store) (Mempool_key) (Operation_hash_set) in
    let read genesis gstore sstore (vstate: valid_block_state) =
      begin
        Valid_block.locked_read vstate genesis.Store.block >>= function
        | None | Some (Error _) -> fatal_error ""
        | Some (Ok genesis_block) ->
            match genesis_block.test_network with
            | None -> Lwt.return genesis_block
            | Some _ ->
                let context = genesis_block.context in
                Context.del_test_network context >>= fun context ->
                Context.set_protocol
                  context genesis_block.test_protocol_hash >>= fun context ->
                Lwt.return
                  { genesis_block with
                    net_id = Net genesis_block.hash ;
                    context ;
                    protocol = genesis_block.test_protocol ;
                    protocol_hash = genesis_block.test_protocol_hash ;
                    test_network = None ;
                  }
      end >>= fun genesis_block ->
      begin
        Persist.use gstore (fun store ->
            Store.get store current_block_key) >>= function
        | None -> Lwt.return genesis.Store.block
        | Some current_block -> Lwt.return (Block_hash.of_bytes current_block)
      end >>= fun current_head_hash ->
      begin
        if Block_hash.equal current_head_hash genesis_block.hash then
          Lwt.return genesis_block
        else
          Valid_block.locked_read vstate current_head_hash >>= function
          | None -> fatal_error "Internal error while loading the current block."
          | Some (Error exn) ->
              fatal_error
                "@[<v 2>Internal error while loading the current block:@ %a@]"
                (fun ppf -> Error_monad.pp_print_error ppf) exn
          | Some (Ok current_head) ->
              Lwt.return current_head
      end >>= fun current_head ->
      Persist.use gstore Mempool.read >>= fun mempool ->
      let current_protocol =
        match current_head.protocol with
        | None -> fatal_error "Protocol version for the current head is unknown"
        | Some protocol -> protocol in
      Lwt.return
        (Persist.share { current_head ; current_protocol ; genesis_block ;
                         mempool ; blockchain_store = sstore })
    in
    let store net { current_head ; mempool } =
      Persist.update net.net_store.net_store (fun store ->
          Store.set store current_block_key
            (Block_hash.to_bytes current_head.hash) >>= fun () ->
          Mempool.write store mempool >>= fun store ->
          Lwt.return (Some store)) >>= fun _ ->
      Lwt.return_unit
    in
  (read, store)

  let locked_head bstate = Lwt.return bstate.current_head

  let locked_protocol bstate = Lwt.return bstate.current_protocol

  let locked_mem (bstate : blockchain_state) store h =
    let genesis = bstate.genesis_block.hash in
    if Block_hash.equal genesis h then
      Lwt.return true
    else
      Store.Blockchain.mem store h

  let genesis net =
    use net (fun vstate -> Lwt.return vstate.genesis_block)

  let head net = use net locked_head
  let protocol net = use net locked_protocol
  let mem net h =
    use net (fun bstate ->
        Persist.use bstate.blockchain_store (fun store ->
            locked_mem bstate store h))

  let find_new net hist sz =
    let rec path net_id store sz acc h =
      if sz <= 0 then return (List.rev acc)
      else
        Store.Blockchain_succ.get store h >>= function
        | None -> return (List.rev acc)
        | Some s -> path net_id store (sz-1) (s :: acc) s
    in
    let rec common_ancestor (bstate: blockchain_state) store hist =
      match hist with
      | [] ->
          Lwt.return bstate.genesis_block.hash
      | h :: hist ->
          locked_mem bstate store h >>= function
          | false -> common_ancestor bstate store hist
          | true -> Lwt.return h in
    use net (fun bstate ->
        Persist.use bstate.blockchain_store
          (fun store ->
             common_ancestor bstate store hist >>= fun ancestor ->
             let net_id = Net bstate.genesis_block.hash in
             if Block_hash.equal ancestor bstate.genesis_block.hash then
               Store.Blockchain_test_succ.get store ancestor >>= function
               | None ->
                   if Block_hash.equal ancestor bstate.current_head.hash then
                     return []
                   else
                     return [ancestor]
               | Some s -> path net_id store (sz-1) [ancestor] s
             else
               path net_id store sz [] ancestor
          ))

  let pop_block state bstate =
    lwt_debug "pop_block %a"
      Block_hash.pp_short bstate.current_head.hash >>= fun () ->
    Valid_block.read_exn state bstate.current_head.pred >>= fun pred_block ->
    Persist.use bstate.blockchain_store (fun sstore ->
        Store.Blockchain.del sstore bstate.current_head.hash >>= fun () ->
        if Block_hash.equal pred_block.hash bstate.genesis_block.hash then
          Store.Blockchain_test_succ.del sstore pred_block.hash
        else
          Store.Blockchain_succ.del sstore pred_block.hash) >>= fun () ->
    let mempool =
      List.fold_left
        (fun mempool h -> Operation_hash_set.add h mempool)
        bstate.mempool bstate.current_head.operations in
    Lwt.return { bstate with current_head = pred_block ; mempool }

  let rec pop_blocks state bstate ancestor =
    if not (Block_hash.equal bstate.current_head.hash ancestor) then begin
      pop_block state bstate >>= fun bstate ->
      pop_blocks state bstate ancestor
    end else
      Lwt.return bstate

  let push_block time (bstate: blockchain_state) (block: valid_block) =
    lwt_debug "push_block %a" Block_hash.pp_short block.hash >>= fun () ->
    Persist.use bstate.blockchain_store (fun sstore ->
        Store.Blockchain.set sstore block.hash time >>= fun () ->
        if Block_hash.equal block.pred bstate.genesis_block.hash then
          Store.Blockchain_test_succ.set sstore block.pred block.hash
        else
          Store.Blockchain_succ.set sstore block.pred block.hash) >>= fun () ->
    let mempool =
      List.fold_left
        (fun mempool h -> Operation_hash_set.remove h mempool)
        bstate.mempool block.operations in
    Lwt.return { bstate with current_head = block ; mempool }

  let locked_set_head net bstate block =
    let Net net_id = block.net_id in
    if not (Block_hash.equal net_id net.net_store.net_genesis.block) then
      invalid_arg "State.Blockchain.set_head" ;
    lwt_debug "set_head %a" Block_hash.pp_short block.hash >>= fun () ->
    let current_protocol =
      match block.protocol with
      | None ->
          fatal_error "Protocol version for the new head is unknown"
      | Some protocol -> protocol in
    Valid_block.new_blocks
      net.state bstate.current_head block >>= fun (ancestor, path) ->
    pop_blocks net.state bstate ancestor.hash >>= fun bstate ->
    let time = Time.now () in
    Lwt_list.fold_left_s
      (push_block time) bstate path >>= fun bstate ->
    let bstate = { bstate with current_protocol } in
    store_state net bstate >>= fun () ->
    Lwt.return (Some bstate)

  let set_head net block =
    update net (fun bstate -> locked_set_head net bstate block) >>= fun _ ->
    Lwt.return_unit

  let test_and_set_head net ~old block =
    update net (fun bstate ->
        if not (Block_hash.equal bstate.current_head.hash old.hash) then
          Lwt.return None
        else
          locked_set_head net bstate block)

end

module Mempool = struct

  let use = Blockchain.use
  let update = Blockchain.update

  let get net =
    use net (fun bstate -> Lwt.return bstate.mempool)

  let add net h =
    update net (fun bstate ->
        if Operation_hash_set.mem h bstate.mempool then
          Lwt.return_none
        else begin
          let bstate =
            { bstate with
              mempool = Operation_hash_set.add h bstate.mempool } in
          Lwt.return (Some bstate)
        end)

  let remove net h =
    update net (fun bstate ->
        if Operation_hash_set.mem h bstate.mempool then begin
          let bstate =
            { bstate with
              mempool = Operation_hash_set.remove h bstate.mempool } in
          Lwt.return (Some bstate)
        end else
          Lwt.return_none)

  let for_block net block =
    let rec pop acc ancestor block =
      if Block_hash.equal ancestor.hash block.hash then
        Lwt.return acc
      else begin
        let acc =
          let add acc x = Operation_hash_set.add x acc in
          List.fold_left add acc block.operations in
        Valid_block.read_exn net.state block.pred >>= fun pred ->
        pop acc ancestor pred
      end in
    use net (fun bstate ->
        Valid_block.new_blocks
          net.state bstate.current_head block >>= fun (ancestor, path) ->
        pop bstate.mempool ancestor bstate.current_head >|= fun ops ->
        List.fold_left
          (fun ops (b: valid_block) ->
             let del acc x = Operation_hash_set.remove x acc in
             List.fold_left del ops b.operations)
          ops
          path)

end

module Net = struct

  type t = net
  type net = t

  module Blockchain = Blockchain
  module Mempool = Mempool

  let raw_create state (net_store : Store.net_store) =
    Persist.use state.valid_block_state (fun vstate ->
        Blockchain.read_state
          net_store.net_genesis
          net_store.net_store
          state.store.blockchain vstate)
    >|= fun blockchain_state ->
    { state ; net_store ; blockchain_state }

  let read_state, store_state =
    let read state store =
      Persist.use store.Store.global_store KnownNets.read >>= fun nets ->
      let elements = Block_hash_set.elements nets in
      Lwt_list.iter_p
        (fun hash ->
           store.net_read (Net hash) >>= function
           | Error err ->
               lwt_log_error "@[<v 2>Error while loading net:@ %a@]"
                 Error_monad.pp_print_error err
           | Ok net_store ->
               raw_create state net_store >>= fun net ->
               Block_hash_table.add state.nets hash net ;
               Lwt.return ()
        )
        elements
    in
    let store { store = { global_store }; nets } =
      Persist.update global_store
        (fun store ->
           let nets =
             Block_hash_table.fold
               (fun h _ s -> Block_hash_set.add h s)
               nets Block_hash_set.empty in
           KnownNets.write store nets >>= fun store ->
           Lwt.return (Some store)) >>= fun _ ->
      Lwt.return_unit in
    (read, store)

  let state { state } = state
  let active { active_net } = active_net
  let get { nets } (Net b) =
    try ok (Block_hash_table.find nets b)
    with Not_found -> error (Unknown_network (Net b))
  let all { nets } =
    Block_hash_table.fold (fun _ net acc -> net :: acc) nets []
  let id { net_store = { net_genesis = { block } } } = Net block
  let expiration { net_store = { net_expiration } } = net_expiration
  let same_id (Net id') net =
    let Net id = id net in
    Block_hash.equal id id'
  let is_active { active_net } net_id =
    List.exists (same_id net_id) active_net
  let activate net =
    let s = net.state in
    let net_id = id net in
    if not (List.exists (same_id net_id) s.active_net) then
    s.active_net <- net :: s.active_net
  let deactivate net =
    let s = net.state in
    let net_id = id net in
    s.active_net <-
      List.filter (fun net -> not (same_id net_id net)) s.active_net

  let create state ?expiration ?test_protocol net_genesis =
    Valid_block.create_genesis_block
      state net_genesis test_protocol >>=? fun _ ->
    state.store.net_init ?expiration net_genesis >>= fun net_store ->
    raw_create state net_store >>= fun net ->
    store_state state >>= fun () ->
    Block_hash_table.add state.nets net_genesis.block net ;
    return net

  let cleanup_blocks_and_operations net =
    let Net net_id = id net in
    let same_id (Net id) = Block_hash.equal net_id id in
    let cleanup_operation h =
      ignore @@
      Persist.use net.state.store.operation (fun store ->
          Store.Operation.del store h) in
    let rec cleanup_block h =
      Block.read net.state h >>= function
      | Some b when same_id b.data.shell.net_id ->
          Persist.use net.state.store.block (fun store ->
              Store.Block.del store h) >>= fun () ->
          List.iter cleanup_operation b.data.shell.operations ;
          cleanup_block b.data.shell.predecessor ;
      | None | Some _ -> Lwt.return_unit in
    Mempool.get net >>= fun ops ->
    Operation_hash_set.iter cleanup_operation ops ;
    Valid_block.postponed net.state >>= fun postponed ->
    Block_hash_set.iter (fun h -> ignore (cleanup_block h)) postponed ;
    Valid_block.known_heads net.state >>= fun known_heads ->
    Block_hash_map.iter
      (fun _ v ->
         if same_id v.net_id then
           ignore @@ begin
             Persist.use net.state.store.block (fun store ->
                 Store.Block.del store v.hash) >>= fun () ->
             cleanup_block v.pred
           end)
      known_heads ;
    Lwt.return_unit

  let destroy net =
    lwt_debug "destroy %a" Store.pp_net_id (id net) >>= fun () ->
    let Net net_genesis as net_id = id net in
    Block_hash_table.remove net.state.nets net_genesis ;
    net.state.active_net <-
      List.filter (fun net -> id net <> net_id) net.state.active_net ;
    store_state net.state >>= fun () ->
    net.state.store.net_destroy net.net_store >>= fun () ->
    Lwt.async (fun () -> cleanup_blocks_and_operations net);
    Lwt.return_unit

end


let () =
  let open Data_encoding in
  register_error_kind `Permanent
    ~id:"refusedOperation"
    ~title: "Refused operation"
    ~description:
      "An operation that will never be accepted (by any protocol version)."
    ~pp:(fun ppf hash ->
        Format.fprintf ppf "Refused operation %a"
          Operation_hash.pp_short hash)
    (obj1 (req "operation_hash" Operation_hash.encoding))
    (function Exn (Operation.Invalid (hash, _)) -> Some hash | _ -> None)
    (fun hash -> Exn (Operation.Invalid (hash, [(* TODO *)])))

let () =
  let open Data_encoding in
  register_error_kind `Permanent
    ~id: "invalidBlock"
    ~title: "Invalid block"
    ~description:
      "The economical protocol refused to validate the block."
    ~pp:(fun ppf block_hash ->
        Format.fprintf ppf "Cannot validate the block %a"
          Block_hash.pp_short block_hash)
    (obj1 (req "block_hash" Block_hash.encoding))
    (function Exn (Valid_block.Invalid (block_hash, _)) -> Some block_hash
            | _ -> None)
    (fun block_hash -> Exn (Valid_block.Invalid (block_hash, [(* TODO *)])))

(** Whole protocol state : read and store. *)

let read
    ~request_operations ~request_blocks ~request_protocols
    ~store_root ~context_root ~ttl ?patch_context () =
  Store.init store_root >>= fun store ->
  lwt_log_info "Initialising the distributed database..." >>= fun () ->
  let operation_db =
    Db_proxy.Operation.create { request_operations } store.operation in
  let protocol_db =
    Db_proxy.Protocol.create { request_protocols } store.protocol in
  let block_db =
    Db_proxy.Block.create { request_blocks } store.block in
  Valid_block.create
    ?patch_context ~context_root
    store.global_store block_db ttl >>= fun valid_block_state ->
  let rec state = {
    store ;
    active_net = [] ;
    nets = Block_hash_table.create 7 ;
    operation_db ;
    operation_watchers = ref [] ;
    protocol_db ;
    protocol_watchers = ref [] ;
    block_db ; block_watchers = ref [] ;
    valid_block_state ;
  }
  in
  Net.read_state state store >>= fun _nets ->
  Lwt.return state

let store state =
  let nets =
    Block_hash_table.fold (fun _ net acc -> net :: acc) state.nets [] in
  Net.store_state state >>= fun () ->
  Lwt_list.iter_s
    (fun net ->
       Blockchain.use net
         (fun bstate -> Blockchain.store_state net bstate))
    nets

let shutdown state =
  Lwt.join [ Db_proxy.Operation.shutdown state.operation_db ;
             Db_proxy.Block.shutdown state.block_db ;
           ] >>= fun () ->
  store state
