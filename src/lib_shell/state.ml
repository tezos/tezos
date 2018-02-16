(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.State

type error +=
  | Unknown_chain of Chain_id.t

type error += Bad_data_dir

type error += Block_not_invalid of Block_hash.t

let () =
  let open Error_monad in
  register_error_kind
    `Temporary
    ~id:"state.unknown_chain"
    ~title:"Unknown chain"
    ~description:"TODO"
    ~pp:(fun ppf id ->
        Format.fprintf ppf "Unknown chain %a" Chain_id.pp id)
    Data_encoding.(obj1 (req "chain" Chain_id.encoding))
    (function Unknown_chain x -> Some x | _ -> None)
    (fun x -> Unknown_chain x) ;
  register_error_kind
    `Permanent
    ~id:"badDataDir"
    ~title:"Bad data directory"
    ~description:"The data directory could not be read. \
                  This could be because it was generated with an \
                  old version of the tezos-node program. \
                  Deleting and regenerating this directory \
                  may fix the problem."
    Data_encoding.empty
    (function Bad_data_dir -> Some () | _ -> None)
    (fun () -> Bad_data_dir) ;
  register_error_kind
    `Permanent
    ~id:"blockNotInvalid"
    ~title:"Block not invalid"
    ~description:"The invalid block to be unmarked was not actually invalid."
    ~pp:(fun ppf block ->
        Format.fprintf ppf "Block %a was expected to be invalid, but was not actually invalid."
          Block_hash.pp block)
    Data_encoding.(obj1 (req "block" Block_hash.encoding))
    (function Block_not_invalid block -> Some block | _ -> None)
    (fun block -> Block_not_invalid block) ;

  (** *)

module Shared = struct
  type 'a t = {
    data: 'a ;
    lock: Lwt_mutex.t ;
  }
  let create data = { data ; lock = Lwt_mutex.create () }
  let use { data ; lock } f =
    Lwt_mutex.with_lock lock (fun () -> f data)
end

type global_state = {
  global_data: global_data Shared.t ;
  protocol_store: Store.Protocol.store Shared.t ;
}

and global_data = {
  chains: chain_state Chain_id.Table.t ;
  global_store: Store.t ;
  context_index: Context.index ;
}

and chain_state = {
  global_state: global_state ;
  chain_id: Chain_id.t ;
  genesis: genesis ;
  faked_genesis_hash: Block_hash.t ;
  expiration: Time.t option ;
  allow_forked_chain: bool ;
  block_store: Store.Block.store Shared.t ;
  context_index: Context.index Shared.t ;
  block_watcher: block Lwt_watcher.input ;
  chain_data: chain_data_state Shared.t ;
}

and genesis = {
  time: Time.t ;
  block: Block_hash.t ;
  protocol: Protocol_hash.t ;
}

and chain_data_state = {
  mutable data: chain_data ;
  chain_data_store: Store.Chain_data.store ;
}

and chain_data = {
  current_head: block ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  locator: Block_locator.t Lwt.t lazy_t ;
}

and block = {
  chain_state: chain_state ;
  hash: Block_hash.t ;
  contents: Store.Block.contents ;
}

let read_chain_data { chain_data } f =
  Shared.use chain_data begin fun state ->
    f state.chain_data_store state.data
  end

let update_chain_data { chain_id ; context_index ; chain_data } f =
  Shared.use chain_data begin fun state ->
    f state.chain_data_store state.data >>= fun (data, res) ->
    Lwt_utils.may data
      ~f:begin fun data ->
        state.data <- data ;
        Shared.use context_index begin fun context_index ->
          Context.set_head context_index chain_id
            data.current_head.contents.context
        end >>= fun () ->
        Lwt.return_unit
      end >>= fun () ->
    Lwt.return res
  end

(** The number of predecessors stored per block.
    This value chosen to compute efficiently block locators that
    can cover a chain of 2 months, at 1 block/min, which is ~86K
    blocks at the cost in space of ~72MB.
    |locator| = log2(|chain|/10) -1
*)
let stored_predecessors_size = 12

(**
   Takes a block and populates its predecessors store, under the
   assumption that all its predecessors have their store already
   populated. The precedecessors are distributed along the chain, up
   to the genesis, at a distance from [b] that grows exponentially.
   The store tabulates a function [p] from distances to block_ids such
   that if [p(b,d)=b'] then [b'] is at distance 2^d from [b].
   Example of how previous predecessors are used:
   p(n,0) = n-1
   p(n,1) = n-2  = p(n-1,0)
   p(n,2) = n-4  = p(n-2,1)
   p(n,3) = n-8  = p(n-4,2)
   p(n,4) = n-16 = p(n-8,3)
*)
let store_predecessors (store: Store.Block.store) (b: Block_hash.t) : unit Lwt.t =
  let rec loop pred dist =
    if dist = stored_predecessors_size
    then Lwt.return_unit
    else
      Store.Block.Predecessors.read_opt (store, pred) (dist-1) >>= function
      | None -> Lwt.return_unit (* we reached genesis *)
      | Some p ->
          Store.Block.Predecessors.store (store, b) dist p >>= fun () ->
          loop p (dist+1)
  in
  (* the first predecessor is fetched from the header *)
  Store.Block.Contents.read_exn (store, b) >>= fun contents ->
  let pred = contents.header.shell.predecessor in
  if Block_hash.equal b pred then
    Lwt.return_unit  (* genesis *)
  else
    Store.Block.Predecessors.store (store,b) 0 pred >>= fun () ->
    loop pred 1

(**
   [predecessor s b d] returns the hash of the node at distance [d] from [b].
   Returns [None] if [d] is greater than the distance of [b] from genesis or
   if [b] is genesis.
   Works in O(log|chain|) if the chain is shorter than 2^[stored_predecessors_size]
   and in O(|chain|) after that.
   @raise Invalid_argument "State.predecessors: negative distance"
*)
let predecessor_n (store: Store.Block.store) (b: Block_hash.t) (distance: int)
  : Block_hash.t option Lwt.t =
  (* helper functions *)
  (* computes power of 2 w/o floats *)
  let power_of_2 n =
    if n < 0 then invalid_arg "negative argument" else
      let rec loop cnt res =
        if cnt<1 then res
        else loop (cnt-1) (res*2)
      in
      loop n 1
  in
  (* computes the closest power of two smaller than a given
     a number and the rest w/o floats *)
  let closest_power_two_and_rest n =
    if n < 0 then invalid_arg "negative argument" else
      let rec loop cnt n rest =
        if n<=1
        then (cnt,rest)
        else loop (cnt+1) (n/2) (rest + (power_of_2 cnt) * (n mod 2))
      in
      loop 0 n 0
  in

  (* actual predecessor function *)
  if distance <= 0 then
    invalid_arg ("State.predecessor: distance <= 0"^(string_of_int distance))
  else
    let rec loop b distance =
      if distance = 1
      then Store.Block.Predecessors.read_opt (store, b) 0
      else
        let (power,rest) = closest_power_two_and_rest distance in
        let (power,rest) =
          if power < stored_predecessors_size then (power,rest)
          else
            let power = stored_predecessors_size-1 in
            let rest = distance - (power_of_2 power) in
            (power,rest)
        in
        Store.Block.Predecessors.read_opt (store, b) power >>= function
        | None -> Lwt.return_none (* reached genesis *)
        | Some pred ->
            if rest = 0
            then Lwt.return_some pred (* landed on the requested predecessor *)
            else loop pred rest       (* need to jump further back *)
    in
    loop b distance

let compute_locator_from_hash (chain : chain_state) ?(size = 200) head_hash =
  Shared.use chain.block_store begin fun block_store ->
    Store.Block.Contents.read_exn (block_store, head_hash) >>= fun { header } ->
    Block_locator.compute ~predecessor:(predecessor_n block_store)
      ~genesis:chain.genesis.block head_hash header size
  end

let compute_locator chain ?size head =
  compute_locator_from_hash chain ?size head.hash

type t = global_state

module Locked_block = struct

  let store_genesis store genesis context =
    let shell : Block_header.shell_header = {
      level = 0l ;
      proto_level = 0 ;
      predecessor = genesis.block ; (* genesis' predecessor is genesis *)
      timestamp = genesis.time ;
      fitness = [] ;
      validation_passes = 0 ;
      operations_hash = Operation_list_list_hash.empty ;
      context ;
    } in
    let header : Block_header.t = { shell ; proto = MBytes.create 0 } in
    Store.Block.Contents.store (store, genesis.block)
      { Store.Block.header ; message = Some "Genesis" ;
        max_operations_ttl = 0 ; context ;
        max_operation_data_length = 0;
      } >>= fun () ->
    Lwt.return header

end

module Chain = struct

  type nonrec genesis = genesis = {
    time: Time.t ;
    block: Block_hash.t ;
    protocol: Protocol_hash.t ;
  }
  let genesis_encoding =
    let open Data_encoding in
    conv
      (fun { time ; block ; protocol } -> (time, block, protocol))
      (fun (time, block, protocol) -> { time ; block ; protocol })
      (obj3
         (req "timestamp" Time.encoding)
         (req "block" Block_hash.encoding)
         (req "protocol" Protocol_hash.encoding))

  type t = chain_state
  type chain_state = t

  let allocate
      ~genesis ~faked_genesis_hash ~expiration ~allow_forked_chain
      ~current_head
      global_state context_index chain_data_store block_store =
    Store.Block.Contents.read_exn
      (block_store, current_head) >>= fun current_block ->
    let rec chain_data = {
      data = {
        current_head = {
          chain_state ;
          hash = current_head ;
          contents = current_block ;
        } ;
        current_mempool = Mempool.empty ;
        live_blocks = Block_hash.Set.singleton genesis.block ;
        live_operations = Operation_hash.Set.empty ;
        locator = lazy (compute_locator_from_hash chain_state current_head) ;
      } ;
      chain_data_store ;
    }
    and chain_state = {
      global_state ;
      chain_id = Chain_id.of_block_hash genesis.block ;
      chain_data = { Shared.data = chain_data ; lock = Lwt_mutex.create () } ;
      genesis ; faked_genesis_hash ;
      expiration ;
      allow_forked_chain ;
      block_store = Shared.create block_store ;
      context_index = Shared.create context_index ;
      block_watcher = Lwt_watcher.create_input () ;
    } in
    Lwt.return chain_state

  let locked_create
      global_state data ?expiration ?(allow_forked_chain = false)
      chain_id genesis commit =
    let chain_store = Store.Chain.get data.global_store chain_id in
    let block_store = Store.Block.get chain_store
    and chain_data_store = Store.Chain_data.get chain_store in
    Store.Chain.Genesis_hash.store chain_store genesis.block >>= fun () ->
    Store.Chain.Genesis_time.store chain_store genesis.time >>= fun () ->
    Store.Chain.Genesis_protocol.store chain_store genesis.protocol >>= fun () ->
    Store.Chain_data.Current_head.store chain_data_store genesis.block >>= fun () ->
    Store.Chain_data.Known_heads.store chain_data_store genesis.block >>= fun () ->
    begin
      match expiration with
      | None -> Lwt.return_unit
      | Some time -> Store.Chain.Expiration.store chain_store time
    end >>= fun () ->
    begin
      if allow_forked_chain then
        Store.Chain.Allow_forked_chain.store data.global_store chain_id
      else
        Lwt.return_unit
    end >>= fun () ->
    Locked_block.store_genesis
      block_store genesis commit >>= fun genesis_header ->
    allocate
      ~genesis
      ~faked_genesis_hash:(Block_header.hash genesis_header)
      ~current_head:genesis.block
      ~expiration
      ~allow_forked_chain
      global_state
      data.context_index
      chain_data_store
      block_store

  let create state ?allow_forked_chain genesis  =
    let chain_id = Chain_id.of_block_hash genesis.block in
    Shared.use state.global_data begin fun data ->
      if Chain_id.Table.mem data.chains chain_id then
        Pervasives.failwith "State.Chain.create"
      else
        Context.commit_genesis
          data.context_index
          ~chain_id
          ~time:genesis.time
          ~protocol:genesis.protocol >>= fun commit ->
        locked_create
          state data ?allow_forked_chain chain_id genesis commit >>= fun chain ->
        Chain_id.Table.add data.chains chain_id chain ;
        Lwt.return chain
    end

  let locked_read global_state data id =
    let chain_store = Store.Chain.get data.global_store id in
    let block_store = Store.Block.get chain_store
    and chain_data_store = Store.Chain_data.get chain_store in
    Store.Chain.Genesis_hash.read chain_store >>=? fun genesis_hash ->
    Store.Chain.Genesis_time.read chain_store >>=? fun time ->
    Store.Chain.Genesis_protocol.read chain_store >>=? fun protocol ->
    Store.Chain.Expiration.read_opt chain_store >>= fun expiration ->
    Store.Chain.Allow_forked_chain.known
      data.global_store id >>= fun allow_forked_chain ->
    Store.Block.Contents.read (block_store, genesis_hash) >>=? fun genesis_header ->
    let genesis = { time ; protocol ; block = genesis_hash } in
    Store.Chain_data.Current_head.read chain_data_store >>=? fun current_head ->
    try
      allocate
        ~genesis
        ~faked_genesis_hash:(Block_header.hash genesis_header.header)
        ~current_head
        ~expiration
        ~allow_forked_chain
        global_state
        data.context_index
        chain_data_store
        block_store >>= return
    with Not_found ->
      fail Bad_data_dir

  let locked_read_all global_state data =
    Store.Chain.list data.global_store >>= fun ids ->
    iter_p
      (fun id ->
         locked_read global_state data id >>=? fun chain ->
         Chain_id.Table.add data.chains id chain ;
         return ())
      ids

  let read_all state =
    Shared.use state.global_data begin fun data ->
      locked_read_all state data
    end

  let get state id =
    Shared.use state.global_data begin fun data ->
      try return (Chain_id.Table.find data.chains id)
      with Not_found -> fail (Unknown_chain id)
    end

  let all state =
    Shared.use state.global_data begin fun { chains } ->
      Lwt.return @@
      Chain_id.Table.fold (fun _ chain acc -> chain :: acc) chains []
    end

  let id { chain_id } = chain_id
  let genesis { genesis } = genesis
  let faked_genesis_hash { faked_genesis_hash } = faked_genesis_hash
  let expiration { expiration } = expiration
  let allow_forked_chain { allow_forked_chain } = allow_forked_chain
  let global_state { global_state } = global_state

  let destroy state chain =
    lwt_debug "destroy %a" Chain_id.pp (id chain) >>= fun () ->
    Shared.use state.global_data begin fun { global_store ; chains } ->
      Chain_id.Table.remove chains (id chain) ;
      Store.Chain.destroy global_store (id chain) >>= fun () ->
      Lwt.return_unit
    end

end

module Block = struct

  type t = block = {
    chain_state: Chain.t ;
    hash: Block_hash.t ;
    contents: Store.Block.contents ;
  }
  type block = t

  let compare b1 b2 = Block_hash.compare b1.hash b2.hash
  let equal b1 b2 = Block_hash.equal b1.hash b2.hash

  let hash { hash } = hash
  let header { contents = { header } } = header
  let chain_state { chain_state } = chain_state
  let chain_id { chain_state = { chain_id } } = chain_id
  let shell_header { contents = { header = { shell } } } = shell
  let timestamp b = (shell_header b).timestamp
  let fitness b = (shell_header b).fitness
  let level b = (shell_header b).level
  let proto_level b = (shell_header b).proto_level
  let validation_passes b = (shell_header b).validation_passes
  let message { contents = { message } } = message
  let max_operations_ttl { contents = { max_operations_ttl } } =
    max_operations_ttl
  let max_operation_data_length { contents = { max_operation_data_length } } =
    max_operation_data_length

  let is_genesis b = Block_hash.equal b.hash b.chain_state.genesis.block

  let known_valid chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Contents.known (store, hash)
    end
  let known_invalid chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Invalid_block.known store hash
    end
  let read_invalid chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Invalid_block.read_opt store hash
    end
  let list_invalid chain_state =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Invalid_block.fold store ~init:[]
        ~f:(fun hash { level ; errors } acc ->
            Lwt.return ((hash, level, errors) :: acc))
    end
  let unmark_invalid chain_state block =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Invalid_block.known store block >>= fun mem ->
      if mem
      then Store.Block.Invalid_block.remove store block >>= return
      else fail (Block_not_invalid block)
    end

  let known chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Contents.known (store, hash) >>= fun known ->
      if known then
        Lwt.return_true
      else
        Store.Block.Invalid_block.known store hash
    end

  let read chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Contents.read (store, hash) >>=? fun contents ->
      return { chain_state ; hash ; contents }
    end
  let read_opt chain_state hash =
    read chain_state hash >>= function
    | Error _ -> Lwt.return None
    | Ok v -> Lwt.return (Some v)
  let read_exn chain_state hash =
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Contents.read_exn (store, hash) >>= fun contents ->
      Lwt.return { chain_state ; hash ; contents }
    end

  (* Quick accessor to be optimized ?? *)
  let read_predecessor chain_state hash =
    read chain_state hash >>=? fun { contents = { header } } ->
    return header.shell.predecessor
  let read_predecessor_opt chain_state hash =
    read_predecessor chain_state hash >>= function
    | Error _ -> Lwt.return None
    | Ok v -> Lwt.return (Some v)
  let read_predecessor_exn chain_state hash =
    read_exn chain_state hash >>= fun { contents = { header } } ->
    Lwt.return header.shell.predecessor

  let predecessor { chain_state ; contents = { header } ; hash } =
    if Block_hash.equal hash header.shell.predecessor then
      Lwt.return_none           (* we are at genesis *)
    else
      read_exn chain_state header.shell.predecessor >>= fun block ->
      Lwt.return (Some block)

  let predecessor_n (chain: Chain.t) (b: Block_hash.t) (distance: int) : Block_hash.t option Lwt.t =
    Shared.use chain.block_store (fun store ->
        predecessor_n store b distance)


  type error += Inconsistent_hash of Context_hash.t * Context_hash.t

  let () =
    Error_monad.register_error_kind
      `Permanent
      ~id:"inconsistentContextHash"
      ~title:"Inconsistent commit hash"
      ~description:
        "When commiting the context of a block, the announced context \
         hash was not the one computed at commit time."
      ~pp: (fun ppf (got, exp) ->
          Format.fprintf ppf
            "@[<v 2>Inconsistant hash:@ got: %a@ expected: %a"
            Context_hash.pp got
            Context_hash.pp exp)
      Data_encoding.(obj2
                       (req "wrong_context_hash" Context_hash.encoding)
                       (req "expected_context_hash" Context_hash.encoding))
      (function Inconsistent_hash (got, exp) -> Some (got, exp) | _ -> None)
      (fun (got, exp) -> Inconsistent_hash (got, exp))

  let store
      ?(dont_enforce_context_hash = false)
      chain_state block_header operations
      { Updater.context ; message ; max_operations_ttl ;
        max_operation_data_length } =
    let bytes = Block_header.to_bytes block_header in
    let hash = Block_header.hash_raw bytes in
    (* let's the validator check the consistency... of fitness, level, ... *)
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Invalid_block.known store hash >>= fun known_invalid ->
      fail_when known_invalid (failure "Known invalid") >>=? fun () ->
      Store.Block.Contents.known (store, hash) >>= fun known ->
      if known then
        return None
      else begin
        Context.commit
          ~time:block_header.shell.timestamp ?message context >>= fun commit ->
        fail_unless
          (dont_enforce_context_hash
           || Context_hash.equal block_header.shell.context commit)
          (Inconsistent_hash (commit, block_header.shell.context)) >>=? fun () ->
        let contents = {
          Store.Block.header =
            if dont_enforce_context_hash then
              { block_header
                with shell = { block_header.shell with context = commit } }
            else
              block_header ;
          message ;
          max_operations_ttl ;
          max_operation_data_length ;
          context = commit ;
        } in
        Store.Block.Contents.store (store, hash) contents >>= fun () ->
        let hashes = List.map (List.map Operation.hash) operations in
        let list_hashes = List.map Operation_list_hash.compute hashes in
        Lwt_list.iteri_p
          (fun i hashes ->
             let path = Operation_list_list_hash.compute_path list_hashes i in
             Store.Block.Operation_hashes.store
               (store, hash) i hashes >>= fun () ->
             Store.Block.Operation_path.store (store, hash) i path)
          hashes >>= fun () ->
        Lwt_list.iteri_p
          (fun i ops -> Store.Block.Operations.store (store, hash) i ops)
          operations >>= fun () ->
        (* Store predecessors *)
        store_predecessors store hash >>= fun () ->
        (* Update the chain state. *)
        Shared.use chain_state.chain_data begin fun chain_data ->
          let store = chain_data.chain_data_store in
          let predecessor = block_header.shell.predecessor in
          Store.Chain_data.Known_heads.remove store predecessor >>= fun () ->
          Store.Chain_data.Known_heads.store store hash
        end >>= fun () ->
        let block = { chain_state ; hash ; contents } in
        Lwt_watcher.notify chain_state.block_watcher block ;
        return (Some block)
      end
    end

  let store_invalid chain_state block_header errors =
    let bytes = Block_header.to_bytes block_header in
    let hash = Block_header.hash_raw bytes in
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Contents.known (store, hash) >>= fun known_valid ->
      fail_when known_valid (failure "Known valid") >>=? fun () ->
      Store.Block.Invalid_block.known store hash >>= fun known_invalid ->
      if known_invalid then
        return false
      else
        Store.Block.Invalid_block.store store hash
          { level = block_header.shell.level ; errors } >>= fun () ->
        return true
    end

  let watcher chain_state =
    Lwt_watcher.create_stream chain_state.block_watcher

  let operation_hashes { chain_state ; hash ; contents } i =
    if i < 0 || contents.header.shell.validation_passes <= i then
      invalid_arg "State.Block.operations" ;
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Operation_hashes.read_exn (store, hash) i >>= fun hashes ->
      Store.Block.Operation_path.read_exn (store, hash) i >>= fun path ->
      Lwt.return (hashes, path)
    end

  let all_operation_hashes { chain_state ; hash ; contents } =
    Shared.use chain_state.block_store begin fun store ->
      Lwt_list.map_p
        (Store.Block.Operation_hashes.read_exn (store, hash))
        (0 -- (contents.header.shell.validation_passes - 1))
    end

  let operations { chain_state ; hash ; contents } i =
    if i < 0 || contents.header.shell.validation_passes <= i then
      invalid_arg "State.Block.operations" ;
    Shared.use chain_state.block_store begin fun store ->
      Store.Block.Operation_path.read_exn (store, hash) i >>= fun path ->
      Store.Block.Operations.read_exn (store, hash) i >>= fun ops ->
      Lwt.return (ops, path)
    end

  let all_operations { chain_state ; hash ; contents } =
    Shared.use chain_state.block_store begin fun store ->
      Lwt_list.map_p
        (fun i -> Store.Block.Operations.read_exn (store, hash) i)
        (0 -- (contents.header.shell.validation_passes - 1))
    end

  let context { chain_state ; hash } =
    Shared.use chain_state.block_store begin fun block_store ->
      Store.Block.Contents.read_exn (block_store, hash)
    end >>= fun { context = commit } ->
    Shared.use chain_state.context_index begin fun context_index ->
      Context.checkout_exn context_index commit
    end

  let protocol_hash block =
    context block >>= fun context ->
    Context.get_protocol context

  let test_chain block =
    context block >>= fun context ->
    Context.get_test_chain context

end

let read_block { global_data } hash =
  Shared.use global_data begin fun { chains } ->
    Chain_id.Table.fold
      (fun _chain_id chain_state acc ->
         acc >>= function
         | Some _ -> acc
         | None ->
             Block.read_opt chain_state hash >>= function
             | None -> acc
             | Some block -> Lwt.return (Some block))
      chains
      Lwt.return_none
  end

let read_block_exn t hash =
  read_block t hash >>= function
  | None -> Lwt.fail Not_found
  | Some b -> Lwt.return b

let fork_testchain block protocol expiration =
  Shared.use block.chain_state.global_state.global_data begin fun data ->
    Block.context block >>= fun context ->
    Context.set_test_chain context Not_running >>= fun context ->
    Context.set_protocol context protocol >>= fun context ->
    Context.commit_test_chain_genesis
      data.context_index block.hash block.contents.header.shell.timestamp
      context >>=? fun (chain_id, genesis, commit) ->
    let genesis = {
      block = genesis ;
      time = Time.add block.contents.header.shell.timestamp 1L ;
      protocol ;
    } in
    Chain.locked_create block.chain_state.global_state data
      chain_id ~expiration genesis commit >>= fun chain ->
    return chain
  end

module Protocol = struct

  include Protocol

  let known global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.known store hash
    end

  let read global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.read store hash
    end
  let read_opt global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.read_opt store hash
    end
  let read_exn global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.read_exn store hash
    end

  let read_raw global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.RawContents.read (store, hash)
    end
  let read_raw_opt global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.RawContents.read_opt (store, hash)
    end
  let read_raw_exn global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.RawContents.read_exn (store, hash)
    end

  let store global_state p =
    let bytes = Protocol.to_bytes p in
    let hash = Protocol.hash_raw bytes in
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.known store hash >>= fun known ->
      if known then
        Lwt.return None
      else
        Store.Protocol.RawContents.store (store, hash) bytes >>= fun () ->
        Lwt.return (Some hash)
    end

  let remove global_state hash =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.known store hash >>= fun known ->
      if known then
        Lwt.return_false
      else
        Store.Protocol.Contents.remove store hash >>= fun () ->
        Lwt.return_true
    end

  let list global_state =
    Shared.use global_state.protocol_store begin fun store ->
      Store.Protocol.Contents.fold_keys store
        ~init:Protocol_hash.Set.empty
        ~f:(fun x acc -> Lwt.return (Protocol_hash.Set.add x acc))
    end

end

module Current_mempool = struct

  let set chain_state ~head mempool =
    update_chain_data chain_state begin fun _chain_data_store data ->
      if Block_hash.equal head (Block.hash data.current_head) then
        Lwt.return (Some { data with current_mempool = mempool },
                    ())
      else
        Lwt.return (None, ())
    end

  let get chain_state =
    read_chain_data chain_state begin fun _chain_data_store data ->
      Lwt.return (Block.header data.current_head, data.current_mempool)
    end

end

let read
    ?patch_context
    ~store_root
    ~context_root
    () =
  Store.init store_root >>=? fun global_store ->
  Context.init ?patch_context ~root:context_root >>= fun context_index ->
  let global_data = {
    chains = Chain_id.Table.create 17 ;
    global_store ;
    context_index ;
  } in
  let state = {
    global_data = Shared.create global_data ;
    protocol_store = Shared.create @@ Store.Protocol.get global_store ;
  } in
  Chain.read_all state >>=? fun () ->
  return state

let close { global_data } =
  Shared.use global_data begin fun { global_store } ->
    Store.close global_store ;
    Lwt.return_unit
  end
