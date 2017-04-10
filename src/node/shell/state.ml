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
  | Invalid_fitness of { block: Block_hash.t ;
                         expected: Fitness.fitness ;
                         found: Fitness.fitness }
  | Invalid_operations of { block: Block_hash.t ;
                            expected: Operation_list_list_hash.t ;
                            found: Operation_hash.t list list }
  | Unknown_network of Net_id.t
  | Unknown_operation of Operation_hash.t
  | Unknown_block of Block_hash.t
  | Unknown_context of Block_hash.t
  | Unknown_protocol of Protocol_hash.t
  | Cannot_parse

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"state.invalid_fitness"
    ~title:"Invalid fitness"
    ~description:"The computed fitness differs from the fitness found \
                 \ in the block header."
    ~pp:(fun ppf (block, expected, found) ->
        Format.fprintf ppf
          "@[<v 2>Invalid fitness for block %a@ \
           \ expected %a@ \
          \ found %a"
          Block_hash.pp_short block
          Fitness.pp expected
          Fitness.pp found)
    Data_encoding.(obj3
                     (req "block" Block_hash.encoding)
                     (req "expected" Fitness.encoding)
                     (req "found" Fitness.encoding))
    (function Invalid_fitness { block ; expected ; found } ->
       Some (block, expected, found) | _ -> None)
    (fun (block, expected, found) ->
       Invalid_fitness { block ; expected ; found }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"state.unknown_network"
    ~title:"Unknown network"
    ~description:"TODO"
    ~pp:(fun ppf id ->
        Format.fprintf ppf "Unknown network %a" Net_id.pp id)
    Data_encoding.(obj1 (req "net" Net_id.encoding))
    (function Unknown_network x -> Some x | _ -> None)
    (fun x -> Unknown_network x) ;

(** *)

module Shared : sig
  type 'a t
  val create: 'a -> 'a t
  val use: 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
end = struct
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
  nets: net Net_id.Table.t ;
  global_store: Store.t ;
  init_index: Net_id.t -> Context.index Lwt.t ;
}

and net = {
  id: Net_id.t ;
  state: net_state Shared.t ;
  genesis: genesis ;
  expiration: Time.t option ;
  forked_network_ttl: Int64.t option ;
  operation_store: Store.Operation.store Shared.t ;
  block_header_store: Store.Block_header.store Shared.t ;
  valid_block_watcher: valid_block Watcher.input ;
}

and genesis = {
  time: Time.t ;
  block: Block_hash.t ;
  protocol: Protocol_hash.t ;
}

and net_state = {
  mutable current_head: valid_block ;
  chain_store: Store.Chain.store ;
  context_index: Context.index ;
}

and valid_block = {
  net_id: Net_id.t ;
  hash: Block_hash.t ;
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  fitness: Protocol.fitness ;
  operations_hash: Operation_list_list_hash.t ;
  operations: Operation_hash.t list list ;
  discovery_time: Time.t ;
  protocol_hash: Protocol_hash.t ;
  protocol: (module Updater.REGISTRED_PROTOCOL) option ;
  test_protocol_hash: Protocol_hash.t ;
  test_protocol: (module Updater.REGISTRED_PROTOCOL) option ;
  test_network: (Net_id.t * Time.t) option ;
  context: Context.t ;
  successors: Block_hash.Set.t ;
  invalid_successors: Block_hash.Set.t ;
  proto_header: MBytes.t ;
}

let build_valid_block
    hash header operations
    context discovery_time successors invalid_successors =
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
    net_id = header.Store.Block_header.shell.net_id ;
    hash ;
    predecessor = header.shell.predecessor ;
    timestamp = header.shell.timestamp ;
    discovery_time ;
    operations_hash = header.shell.operations ;
    operations ;
    fitness = header.shell.fitness ;
    protocol_hash ;
    protocol ;
    test_protocol_hash ;
    test_protocol ;
    test_network ;
    context ;
    successors ;
    invalid_successors ;
    proto_header = header.Store.Block_header.proto ;
  } in
  Lwt.return valid_block

type t = global_state

module type DATA_STORE = sig

  type store
  type key
  type value

  val known: store -> key -> bool Lwt.t

  (** Read a value in the local database. *)
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
  val read_exn: store -> key -> value Lwt.t

  (** Read a value in the local database (without parsing). *)
  val read_raw: store -> key -> MBytes.t tzresult Lwt.t
  val read_raw_opt: store -> key -> MBytes.t option Lwt.t
  val read_raw_exn: store -> key -> MBytes.t Lwt.t

  (** Read data discovery time (the time when `store` was called). *)
  val read_discovery_time: store -> key -> Time.t tzresult Lwt.t
  val read_discovery_time_opt: store -> key -> Time.t option Lwt.t
  val read_discovery_time_exn: store -> key -> Time.t Lwt.t

  val store: store -> key -> value -> bool Lwt.t
  val store_raw: store -> key -> MBytes.t -> value option tzresult Lwt.t
  val remove: store -> key -> bool Lwt.t

end

module type INTERNAL_DATA_STORE = sig

  include DATA_STORE

  val read_full: store -> key -> value tzresult Time.timed_data option Lwt.t

  val mark_valid: store -> key -> bool Lwt.t
  val mark_invalid: store -> key -> error list -> bool Lwt.t
  val unmark: store -> key -> bool Lwt.t

  val pending: store -> key -> bool Lwt.t
  val valid: store -> key -> bool Lwt.t
  val invalid: store -> key -> error list option Lwt.t

  type key_set
  val list_invalid: store -> key_set Lwt.t
  val list_pending: store -> key_set Lwt.t

  val list: store -> key_set Lwt.t

end

let wrap_not_found f s k =
  f s k >>= function
  | None -> Lwt.fail Not_found
  | Some v -> Lwt.return v

module Make_data_store
    (S : Store.DATA_STORE)
    (U : sig
       type store
       val use: store -> (S.store -> 'a Lwt.t) -> 'a Lwt.t
       val unknown: S.key -> 'a tzresult Lwt.t
     end)
    (Set : Set.S with type elt = S.key and type t = S.key_set) : sig
  include INTERNAL_DATA_STORE with type store = U.store
                               and type key = S.key
                               and type key_set := Set.t
                               and type value = S.value
  module Locked : INTERNAL_DATA_STORE with type store = S.store
                                       and type key = S.key
                                       and type key_set := Set.t
                                       and type value = S.value
end = struct

  type store = U.store
  type value = S.value
  type key = S.key
  type key_set = Set.t

  let of_bytes = Data_encoding.Binary.of_bytes S.encoding
  let to_bytes = Data_encoding.Binary.to_bytes S.encoding

  (* FIXME Document and check with a clear mind the invariant in the
           storage... *)

  module Locked = struct
    type store = S.store
    type value = S.value
    type key = S.key
    type key_set = Set.t
    let known s k = S.Discovery_time.known s k
    let read s k = S.Contents.read (s, k)
    let read_opt s k = S.Contents.read_opt (s, k)
    let read_exn s k = S.Contents.read_exn (s, k)
    let read_raw s k = S.RawContents.read (s, k)
    let read_raw_opt s k = S.RawContents.read_opt (s, k)
    let read_raw_exn s k = S.RawContents.read_exn (s, k)
    let read_discovery_time s k = S.Discovery_time.read s k
    let read_discovery_time_opt s k = S.Discovery_time.read_opt s k
    let read_discovery_time_exn s k = S.Discovery_time.read_exn s k
    let read_full s k =
      S.Discovery_time.read_opt s k >>= function
      | None -> Lwt.return_none
      | Some time ->
          S.Errors.read_opt s k >>= function
          | Some exns -> Lwt.return (Some { Time.data = Error exns ; time })
          | None ->
              S.Contents.read_opt (s, k) >>= function
              | None -> Lwt.return_none
              | Some v -> Lwt.return (Some { Time.data = Ok v ; time })
    let store s k v =
      S.Discovery_time.known s k >>= function
      | true -> Lwt.return_false
      | false ->
          let time = Time.now () in
          S.Contents.store (s, k) v >>= fun () ->
          S.Discovery_time.store s k time >>= fun () ->
          S.Pending.store s k >>= fun () ->
          Lwt.return_true
    let store_raw s k b =
      S.Discovery_time.known s k >>= function
      | true -> return None
      | false ->
          match Data_encoding.Binary.of_bytes S.encoding b with
          | None ->
              S.Errors.store s k [Cannot_parse] >>= fun () ->
              fail Cannot_parse
          | Some v ->
              let time = Time.now () in
              S.RawContents.store (s, k) b >>= fun () ->
              S.Discovery_time.store s k time >>= fun () ->
              return (Some v)
    let remove s k =
      S.Discovery_time.known s k >>= function
      | false -> Lwt.return_false
      | true ->
          S.Discovery_time.remove s k >>= fun () ->
          S.Contents.remove (s, k) >>= fun () ->
          S.Validation_time.remove (s, k) >>= fun () ->
          S.Errors.remove s k >>= fun () ->
          S.Pending.remove s k >>= fun () ->
          Lwt.return_true
    let pending s k = S.Pending.known s k
    let valid s k =
      S.Validation_time.known (s, k) >>= fun validated ->
      S.Errors.known s k >>= fun invalid ->
      Lwt.return (validated && not invalid)
    let invalid s k =
      S.Validation_time.known (s, k) >>= fun validated ->
      if validated then
        S.Errors.read_opt s k
      else
        Lwt.return None
    let mark_valid s k =
      S.Pending.known s k >>= fun pending ->
      if not pending then
        Lwt.return_false
      else
        S.Pending.remove s k >>= fun () ->
        S.Validation_time.store (s, k) (Time.now ()) >>= fun () ->
        Lwt.return_true
    let mark_invalid s k e =
      S.Discovery_time.known s k >>= fun pending ->
      if not pending then
        let now = Time.now () in
        S.Discovery_time.store s k now >>= fun () ->
        S.Validation_time.store (s, k) now >>= fun () ->
        S.Errors.store s k e >>= fun () ->
        Lwt.return_true
      else
        S.Errors.known s k >>= fun invalid ->
        if invalid then
          Lwt.return_false
        else
          S.Pending.remove s k >>= fun () ->
          S.Validation_time.store (s, k) (Time.now ()) >>= fun () ->
          S.Errors.store s k e >>= fun () ->
          Lwt.return_true
    let list_invalid s =
      S.Errors.fold_keys s ~init:Set.empty
        ~f:(fun k acc -> Lwt.return (Set.add k acc))
    let unmark s k =
      S.Pending.known s k >>= fun pending ->
      if not pending then
        S.Validation_time.remove (s, k) >>= fun () ->
        S.Errors.remove s k >>= fun () ->
        S.Pending.store s k >>= fun () ->
        Lwt.return_true
      else
        Lwt.return_false
    let list_pending = S.Pending.read_all
    let list s =
      S.Discovery_time.fold_keys s ~init:Set.empty
        ~f:(fun k acc -> Lwt.return (Set.add k acc))
  end

  let atomic1 f s = U.use s f
  let atomic2 f s k = U.use s (fun s -> f s k)
  let atomic3 f s k v = U.use s (fun s -> f s k v)

  let known = atomic2 Locked.known
  let read = atomic2 Locked.read
  let read_opt = atomic2 Locked.read_opt
  let read_exn = atomic2 Locked.read_exn
  let read_raw  = atomic2 Locked.read_raw
  let read_raw_opt = atomic2 Locked.read_raw_opt
  let read_raw_exn = atomic2 Locked.read_raw_exn
  let read_full = atomic2 Locked.read_full
  let read_discovery_time = atomic2 Locked.read_discovery_time
  let read_discovery_time_opt = atomic2 Locked.read_discovery_time_opt
  let read_discovery_time_exn = atomic2 Locked.read_discovery_time_exn
  let store = atomic3 Locked.store
  let store_raw = atomic3 Locked.store_raw
  let remove = atomic2 Locked.remove
  let mark_valid = atomic2 Locked.mark_valid
  let mark_invalid = atomic3 Locked.mark_invalid
  let unmark = atomic2 Locked.unmark
  let pending = atomic2 Locked.pending
  let valid = atomic2 Locked.valid
  let invalid = atomic2 Locked.invalid
  let list_invalid = atomic1 Locked.list_invalid
  let list_pending = atomic1 Locked.list_pending
  let list = atomic1 Locked.list

end

module Raw_operation =
  Make_data_store
    (Store.Operation)
    (struct
      type store = Store.Operation.store Shared.t
      let use s = Shared.use s
      let unknown k = fail (Unknown_operation k)
    end)
    (Operation_hash.Set)

module Raw_operation_list = struct

  module Locked = struct

    let known store (hash, ofs) =
      Store.Block_header.Operation_list.known (store, hash) ofs
    let read store (hash, ofs) =
      Store.Block_header.Operation_list.read
        (store, hash) ofs >>=? fun ops ->
      Store.Block_header.Operation_list_path.read
        (store, hash) ofs >>=? fun path ->
      return (ops, path)
    let read_opt store (hash, ofs) =
      Store.Block_header.Operation_list.read_opt
        (store, hash) ofs >>= function
      | None -> Lwt.return_none
      | Some ops ->
          Store.Block_header.Operation_list_path.read_exn
            (store, hash) ofs >>= fun path ->
          Lwt.return (Some (ops, path))
    let read_exn store (hash, ofs) =
      read_opt store (hash, ofs) >>= function
      | None -> Lwt.fail Not_found
      | Some (ops, path) -> Lwt.return (ops, path)
    let store store (hash, ofs) (ops, path) =
      Store.Block_header.Operation_list.known
        (store, hash) ofs >>= function
      | false ->
          Store.Block_header.Operation_list.store
            (store, hash) ofs ops >>= fun () ->
          Store.Block_header.Operation_list_path.store
            (store, hash) ofs path >>= fun () ->
          Lwt.return_true
      | true ->
          Lwt.return_false

    let remove store (hash, ofs) =
      Store.Block_header.Operation_list.known
        (store, hash) ofs >>= function
      | false ->
          Lwt.return_false
      | true ->
          Store.Block_header.Operation_list.remove
            (store, hash) ofs >>= fun () ->
          Store.Block_header.Operation_list_path.remove
            (store, hash) ofs >>= fun () ->
          Lwt.return_true

    let read_count store hash =
      Store.Block_header.Operation_list_count.read (store, hash)

    let read_count_opt store hash =
      read_count store hash >>= function
      | Ok cpt -> Lwt.return (Some cpt)
      | Error _ -> Lwt.return_none

    let read_count_exn store hash =
      read_count store hash >>= function
      | Ok cpt -> Lwt.return cpt
      | Error _ -> Lwt.fail Not_found

    let store_count store hash count =
      Store.Block_header.Operation_list_count.store (store, hash) count

    let read_all store hash =
      Store.Block_header.Operation_list_count.read (store, hash)
      >>=? fun operation_list_count ->
      let rec read acc i =
        if i <= 0 then return acc
        else
          Store.Block_header.Operation_list.read
            (store, hash) (i-1) >>=? fun ops ->
          read (ops :: acc) (i-1) in
      read [] operation_list_count

    let read_all_exn store hash =
      read_all store hash >>= function
      | Error _ -> Lwt.fail Not_found
      | Ok ops -> Lwt.return ops

    let store_all store hash op_hashes operations =
      Store.Block_header.Operation_list_count.store (store, hash)
        (List.length operations) >>= fun () ->
      Lwt_list.iteri_p
        (fun i ops ->
           Store.Block_header.Operation_list.store
             (store, hash) i ops >>= fun () ->
           Store.Block_header.Operation_list_path.store
             (store, hash) i
             (Operation_list_list_hash.compute_path op_hashes i)
           >>= fun () ->
           Lwt.return_unit)
        operations >>= fun () ->
      Lwt.return_unit

  end

  let atomic1 f s = Shared.use s f
  let atomic2 f s k = Shared.use s (fun s -> f s k)
  let atomic3 f s k v = Shared.use s (fun s -> f s k v)
  let atomic4 f s k v1 v2 = Shared.use s (fun s -> f s k v1 v2)

  let known = atomic2 Locked.known
  let read = atomic2 Locked.read
  let read_opt = atomic2 Locked.read_opt
  let read_exn = atomic2 Locked.read_exn
  let store = atomic3 Locked.store
  let remove = atomic2 Locked.remove

  let store_all = atomic4 Locked.store_all
  let read_all = atomic2 Locked.read_all
  let read_all_exn = atomic2 Locked.read_all_exn

end

module Raw_block_header = struct

  include
    Make_data_store
      (Store.Block_header)
      (struct
        type store = Store.Block_header.store Shared.t
        let use s = Shared.use s
        let unknown k = fail (Unknown_block k)
      end)
      (Block_hash.Set)

  let read_pred store k =
    read_opt store k >>= function
    | None -> Lwt.return_none
    | Some { shell = { predecessor } } ->
        if Block_hash.equal predecessor k then
          Lwt.return_none
        else
          Lwt.return (Some predecessor)
  let read_pred_exn = wrap_not_found read_pred

  let store_genesis store genesis =
    let shell : Store.Block_header.shell_header = {
      net_id = Net_id.of_block_hash genesis.block;
      predecessor = genesis.block ;
      timestamp = genesis.time ;
      fitness = [] ;
      operations = Operation_list_list_hash.empty ;
    } in
    let header =
      { Store.Block_header.shell ; proto = MBytes.create 0 } in
    let bytes =
      Data_encoding.Binary.to_bytes Store.Block_header.encoding header in
    Locked.store_raw store genesis.block bytes >>= fun _created ->
    Raw_operation_list.Locked.store_all store genesis.block [] [] >>= fun () ->
    Lwt.return header

  let store_testnet_genesis store genesis =
    let shell : Store.Block_header.shell_header = {
      net_id = Net_id.of_block_hash genesis.block;
      predecessor = genesis.block ;
      timestamp = genesis.time ;
      fitness = [] ;
      operations = Operation_list_list_hash.empty ;
    } in
    let bytes =
      Data_encoding.Binary.to_bytes Store.Block_header.encoding {
        shell ;
        proto = MBytes.create 0 ;
      } in
    Locked.store_raw store genesis.block bytes >>= fun _created ->
    Raw_operation_list.Locked.store_all store genesis.block [] [] >>= fun () ->
    Lwt.return shell

end

module Raw_helpers = struct

  let path store h1 h2 =
    let rec loop acc h =
      if Block_hash.equal h h1 then
        Lwt.return (Some acc)
      else
        Raw_block_header.read_opt store h >>= function
        | Some { shell = header }
          when not (Block_hash.equal header.predecessor h) ->
            loop ((h, header) :: acc) header.predecessor
        | Some _ | None -> Lwt.return_none in
    loop [] h2

  let rec common_ancestor store hash1 header1 hash2 header2 =
    if Block_hash.equal hash1 hash2 then
      Lwt.return (Some (hash1, header1))
    else if
      Time.compare
        header1.Store.Block_header.timestamp
        header2.Store.Block_header.timestamp <= 0
    then begin
      if Block_hash.equal header2.predecessor hash2 then
        Lwt.return_none
      else
        let hash2 = header2.predecessor in
        Raw_block_header.read_opt store hash2 >>= function
        | Some { shell = header2 } ->
            common_ancestor store hash1 header1 hash2 header2
        | None -> Lwt.return_none
    end else begin
      if Block_hash.equal header1.predecessor hash1 then
        Lwt.return_none
      else
        let hash1 = header1.predecessor in
        Raw_block_header.read_opt store hash1 >>= function
        | Some { shell = header1 } ->
            common_ancestor store hash1 header1 hash2 header2
        | None -> Lwt.return_none
    end

  let block_locator store sz h =
    let rec loop acc sz step cpt h =
      if sz = 0 then Lwt.return (List.rev acc) else
        Raw_block_header.read_pred store h >>= function
        | None -> Lwt.return (List.rev (h :: acc))
        | Some pred ->
            if cpt = 0 then
              loop (h :: acc) (sz - 1) (step * 2) (step * 20 - 1) pred
            else if cpt mod step = 0 then
              loop (h :: acc) (sz - 1) step (cpt - 1) pred
            else
              loop acc sz step (cpt - 1) pred in
    loop [] sz 1 9 h

  let iter_predecessors
      (type state)
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

end

module Block_header = struct

  type shell_header = Store.Block_header.shell_header = {
    net_id: Net_id.t ;
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    operations: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

  type t = Store.Block_header.t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  type block_header = t

  include
    Make_data_store
      (Store.Block_header)
      (struct
        type store = net
        let use s = Shared.use s.block_header_store
        let unknown k = fail (Unknown_block k)
      end)
      (Block_hash.Set)

  let read_pred_opt store k =
    read_opt store k >>= function
    | Some { shell = { predecessor } }
      when not (Block_hash.equal predecessor k) ->
        Lwt.return (Some predecessor)
    | Some _ | None -> Lwt.return_none
  let read_pred_exn = wrap_not_found read_pred_opt

  let read_operations s k =
    Raw_operation_list.read_all s.block_header_store k

  let mark_invalid net hash errors =
    mark_invalid net hash errors >>= fun marked ->
    if not marked then
      Lwt.return_false
    else begin
      Raw_block_header.read_opt net.block_header_store hash >>= function
      | Some { shell = { predecessor } } ->
          Shared.use net.state begin fun state ->
            Store.Chain.Valid_successors.remove
              (state.chain_store, predecessor) hash >>= fun () ->
            Store.Chain.Invalid_successors.store
              (state.chain_store, predecessor) hash
          end >>= fun () ->
          Lwt.return_true
      | None ->
          Lwt.return_true
    end

  module Helpers = struct

    let check_block state h =
      known state h >>= function
      | true -> return ()
      | false -> failwith "Unknown block %a" Block_hash.pp_short h

    let path state h1 h2 =
      trace_exn (Failure "State.path") begin
        check_block state h1 >>=? fun () ->
        check_block state h2 >>=? fun () ->
        Raw_helpers.path state.block_header_store h1 h2 >>= function
        | None -> failwith "not an ancestor"
        | Some x -> return x
      end

    let common_ancestor state hash1 hash2 =
      trace_exn (Failure "State.common_ancestor") begin
        read_opt state hash1 >>= function
        | None -> failwith "Unknown_block %a" Block_hash.pp_short hash1
        | Some { shell = header1 } ->
            read_opt state hash2 >>= function
            | None -> failwith "Unknown_block %a" Block_hash.pp_short hash1
            | Some { shell = header2 } ->
                Raw_helpers.common_ancestor state.block_header_store
                  hash1 header1 hash2 header2 >>= function
                | None -> failwith "No common ancestor found"
                | Some (hash, header) -> return (hash, header)
      end

    let block_locator state sz h =
      trace_exn (Failure "State.block_locator") begin
        check_block state h >>=? fun () ->
        Raw_helpers.block_locator
          state.block_header_store sz h >>= fun locator ->
        return locator
      end

    let iter_predecessors =
      let compare b1 b2 =
        match Fitness.compare b1.shell.fitness b2.shell.fitness with
        | 0 -> begin
            match Time.compare b1.shell.timestamp b2.shell.timestamp with
            | 0 ->
                Block_hash.compare
                  (Store.Block_header.hash b1) (Store.Block_header.hash b2)
            | res -> res
          end
        | res -> res in
      let predecessor net b =
        if Block_hash.equal net.genesis.block b.shell.predecessor then
          Lwt.return_none
        else
          Raw_block_header.read_opt
            net.block_header_store b.shell.predecessor in
      Raw_helpers.iter_predecessors compare predecessor
        (fun b -> b.shell.timestamp) (fun b -> b.shell.fitness)

  end

end

module Operation_list = struct

  type store = net
  type key = Block_hash.t * int
  type value = Operation_hash.t list * Operation_list_list_hash.path

  module Locked = Raw_operation_list.Locked

  let atomic1 f s =
    Shared.use s.block_header_store f
  let atomic2 f s k =
    Shared.use s.block_header_store (fun s -> f s k)
  let atomic3 f s k v =
    Shared.use s.block_header_store (fun s -> f s k v)
  let atomic4 f s k v1 v2 =
    Shared.use s.block_header_store (fun s -> f s k v1 v2)

  let known = atomic2 Locked.known
  let read = atomic2 Locked.read
  let read_opt = atomic2 Locked.read_opt
  let read_exn = atomic2 Locked.read_exn
  let store = atomic3 Locked.store
  let remove = atomic2 Locked.remove

  let store_all s k v =
    Shared.use s.block_header_store begin fun s ->
      let h = List.map Operation_list_hash.compute v in
      Locked.store_all s k h v
    end
  let read_all = atomic2 Locked.read_all
  let read_all_exn = atomic2 Locked.read_all_exn

  let read_count = atomic2 Locked.read_count
  let read_count_opt = atomic2 Locked.read_count_opt
  let read_count_exn = atomic2 Locked.read_count_exn
  let store_count = atomic3 Locked.store_count

end

module Raw_net = struct

  let build
    ~genesis
    ~genesis_block
    ~expiration
    ~forked_network_ttl
    context_index
    chain_store
    block_header_store
    operation_store =
  let net_state = {
    current_head = genesis_block ;
    chain_store ;
    context_index ;
  } in
  let net = {
    id = Net_id.of_block_hash genesis.block ;
    state = Shared.create net_state ;
    genesis ;
    expiration ;
    operation_store = Shared.create operation_store ;
    forked_network_ttl  ;
    block_header_store = Shared.create block_header_store ;
    valid_block_watcher = Watcher.create_input ();
  } in
  net

  let locked_create
      data
      ?initial_context ?forked_network_ttl
      ?test_protocol ?expiration genesis =
    let net_id = Net_id.of_block_hash genesis.block in
    let net_store = Store.Net.get data.global_store net_id in
    let operation_store = Store.Operation.get net_store
    and block_header_store = Store.Block_header.get net_store
    and chain_store = Store.Chain.get net_store in
    Store.Net.Genesis_hash.store net_store genesis.block >>= fun () ->
    Store.Net.Genesis_time.store net_store genesis.time >>= fun () ->
    Store.Net.Genesis_protocol.store net_store genesis.protocol >>= fun () ->
    let test_protocol = Utils.unopt ~default:genesis.protocol test_protocol in
    Store.Net.Genesis_test_protocol.store net_store test_protocol >>= fun () ->
    Store.Chain.Current_head.store chain_store genesis.block >>= fun () ->
    Store.Chain.Known_heads.store chain_store genesis.block >>= fun () ->
    data.init_index net_id >>= fun context_index ->
    begin
      match expiration with
      | None -> Lwt.return_unit
      | Some time -> Store.Net.Expiration.store net_store time
    end >>= fun () ->
    Raw_block_header.store_genesis
      block_header_store genesis >>= fun header ->
    begin
      match initial_context with
      | None ->
          Context.commit_genesis
            context_index
            ~id:genesis.block
            ~time:genesis.time
            ~protocol:genesis.protocol
            ~test_protocol
      | Some context ->
          Lwt.return context
    end >>= fun context ->
    build_valid_block
      genesis.block header [] context genesis.time
      Block_hash.Set.empty Block_hash.Set.empty >>= fun genesis_block ->
    Lwt.return @@
    build
      ~genesis
      ~genesis_block
      ~expiration
      ~forked_network_ttl
      context_index
      chain_store
      block_header_store
      operation_store

end


module Valid_block = struct

  type t = valid_block = {
    net_id: Net_id.t ;
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    fitness: Fitness.fitness ;
    operations_hash: Operation_list_list_hash.t ;
    operations: Operation_hash.t list list ;
    discovery_time: Time.t ;
    protocol_hash: Protocol_hash.t ;
    protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    test_protocol_hash: Protocol_hash.t ;
    test_protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    test_network: (Net_id.t * Time.t) option ;
    context: Context.t ;
    successors: Block_hash.Set.t ;
    invalid_successors: Block_hash.Set.t ;
    proto_header: MBytes.t ;
  }
  type valid_block = t

  module Locked = struct

    let known { context_index } hash =
      Context.exists context_index hash

    let raw_read block operations time chain_store context_index hash =
      Context.checkout context_index hash >>= function
      | None ->
          fail (Unknown_context hash)
      | Some context ->
          Store.Chain.Valid_successors.read_all (chain_store, hash)
          >>= fun successors ->
          Store.Chain.Invalid_successors.read_all (chain_store, hash)
          >>= fun invalid_successors ->
          build_valid_block hash block operations
            context time successors invalid_successors >>= fun block ->
          return block

    let raw_read_exn block operations time chain_store context_index hash =
      raw_read block operations time chain_store context_index hash >>= function
      | Error _ -> Lwt.fail Not_found
      | Ok data -> Lwt.return data

    let read net net_state hash =
      Block_header.read_full net hash >>= function
      | None | Some { Time.data = Error _ } ->
          fail (Unknown_block hash)
      | Some { Time.data = Ok block ; time } ->
          Block_header.read_operations net hash >>=? fun operations ->
          raw_read block operations
            time net_state.chain_store net_state.context_index hash

    let read_opt net net_state hash =
      read net net_state hash >>= function
      | Error _ -> Lwt.return_none
      | Ok data -> Lwt.return (Some data)

    let read_exn net net_state hash =
      read net net_state hash >>= function
      | Error _ -> Lwt.fail Not_found
      | Ok data -> Lwt.return data

    let store
        block_header_store
        (net_state: net_state)
        valid_block_watcher
        hash { Updater.context ; fitness ; message } ttl =
      (* Read the block header. *)
      Raw_block_header.Locked.read
        block_header_store hash >>=? fun block ->
      Raw_block_header.Locked.read_discovery_time
        block_header_store hash >>=? fun discovery_time ->
      (* Check fitness coherency. *)
      fail_unless
        (Fitness.equal fitness block.Store.Block_header.shell.fitness)
        (Invalid_fitness
           { block = hash ;
             expected = block.Store.Block_header.shell.fitness ;
             found = fitness ;
           }) >>=? fun () ->
      begin (* Patch context about the associated test network. *)
        Context.read_and_reset_fork_test_network
          context >>= fun (fork, context) ->
        if fork then
          match ttl with
          | None ->
              (* Ignore fork on forked networks. *)
              Context.del_test_network context >>= fun context ->
              Context.del_test_network_expiration context
          | Some ttl ->
              let eol = Time.(add block.shell.timestamp ttl) in
              Context.set_test_network
                context (Net_id.of_block_hash hash) >>= fun context ->
              Context.set_test_network_expiration
                context eol >>= fun context ->
              Lwt.return context
        else
          Context.get_test_network_expiration context >>= function
          | Some eol when Time.(eol <= now ()) ->
              Context.del_test_network context >>= fun context ->
              Context.del_test_network_expiration context
          | None | Some _ ->
              Lwt.return context
      end >>= fun context ->
      Raw_block_header.Locked.mark_valid
        block_header_store hash >>= fun _marked ->
      (* TODO fail if the block was previsouly stored ... ??? *)
      Operation_list.Locked.read_all
        block_header_store hash >>=? fun operations ->
      (* Let's commit the context. *)
      let message =
        match message with
        | Some message -> message
        | None ->
            Format.asprintf "%a: %a"
              Block_hash.pp_short hash
              Fitness.pp fitness in
      Context.commit
        hash ~time:block.shell.timestamp ~message context >>= fun () ->
      (* Update the chain state. *)
      let store = net_state.chain_store in
      let predecessor = block.shell.predecessor in
      Store.Chain.Known_heads.remove store predecessor >>= fun () ->
      Store.Chain.Known_heads.store store hash >>= fun () ->
      Store.Chain.Valid_successors.store
        (store, predecessor) hash >>= fun () ->
      (* Build the `valid_block` value. *)
      raw_read_exn
        block operations discovery_time
        net_state.chain_store net_state.context_index hash >>= fun valid_block ->
      Watcher.notify valid_block_watcher valid_block ;
      Lwt.return (Ok valid_block)

  end

  let atomic1 f net = Shared.use net.state f
  let atomic2 f net k = Shared.use net.state (fun s -> f s k)
  let atomic3 f net k v = Shared.use net.state (fun s -> f s k v)

  let known = atomic2 Locked.known
  let read net hash =
    Shared.use net.state begin fun state ->
      Locked.read net state hash
    end
  let read_opt net hash =
    read net hash >>= function
    | Error _ -> Lwt.return_none
    | Ok b -> Lwt.return (Some b)
  let read_exn net hash =
    read net hash >>= function
    | Error _ -> Lwt.fail Not_found
    | Ok b -> Lwt.return b

  let store net hash context =
    Shared.use net.state begin fun net_state ->
      Shared.use net.block_header_store begin fun block_header_store ->
        Context.exists net_state.context_index hash >>= function
        | true -> return None (* Previously stored context. *)
        | false ->
            Raw_block_header.Locked.invalid
              block_header_store hash >>= function
            | Some _ -> return None (* Previously invalidated block. *)
            | None ->
                Locked.store
                  block_header_store net_state net.valid_block_watcher
                  hash context net.forked_network_ttl >>=? fun valid_block ->
                return (Some valid_block)
      end
    end

  let watcher net =
    Watcher.create_stream net.valid_block_watcher

  let fork_testnet state net block expiration =
    assert (Net_id.equal block.net_id (Net_id.of_block_hash net.genesis.block)) ;
    let hash = Block_hash.hash_bytes [Block_hash.to_bytes block.hash] in
    let genesis : genesis = {
      block = hash ;
      time = Time.add block.timestamp 1L ;
      protocol = block.test_protocol_hash ;
    } in
    Shared.use state.global_data begin fun data ->
      if Net_id.Table.mem data.nets (Net_id.of_block_hash hash) then
        assert false (* This would mean a block is validated twice... *)
      else
        Context.init_test_network block.context
          ~time:genesis.time
          ~genesis:genesis.block >>=? fun initial_context ->
        Raw_net.locked_create data
          ~initial_context
          ~expiration
          genesis >>= fun net ->
        return net
    end

  module Helpers = struct

    let path net b1 b2 =
      let net_id = Net_id.of_block_hash net.genesis.block in
      if not ( Net_id.equal b1.net_id net_id
               && Net_id.equal b2.net_id net_id ) then
        invalid_arg "State.path" ;
      Raw_helpers.path net.block_header_store b1.hash b2.hash >>= function
      | None -> Lwt.return_none
      | Some blocks ->
          Lwt_list.map_p
            (fun (hash, _header) -> read_exn net hash) blocks >>= fun path ->
          Lwt.return (Some path)

    let common_ancestor net b1 b2 =
      let net_id = Net_id.of_block_hash net.genesis.block in
      if not ( Net_id.equal b1.net_id net_id
               && Net_id.equal b2.net_id net_id ) then
        invalid_arg "State.path" ;
      Raw_block_header.read_exn (* The blocks are known valid. *)
        net.block_header_store b1.hash >>= fun { shell = header1 } ->
      Raw_block_header.read_exn (* The blocks are known valid. *)
        net.block_header_store b2.hash >>= fun { shell = header2 } ->
      Raw_helpers.common_ancestor net.block_header_store
        b1.hash header1 b2.hash header2 >>= function
      | None -> assert false (* The blocks are known valid. *)
      | Some (hash, _header) -> read_exn net hash

    let block_locator state sz b =
      Raw_helpers.block_locator state.block_header_store sz b.hash

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
        if Block_hash.equal b.hash b.predecessor then
          Lwt.return None
        else
          read_opt state b.predecessor in
      Raw_helpers.iter_predecessors compare predecessor
        (fun b -> b.timestamp) (fun b -> b.fitness)

  end

  let known_heads net =
    Shared.use net.state begin fun net_state ->
      Store.Chain.Known_heads.elements net_state.chain_store >>= fun hashes ->
      Lwt_list.map_p (Locked.read_exn net net_state) hashes
    end

  module Current = struct

    let genesis net = read_exn net net.genesis.block

    let head net =
      Shared.use net.state begin fun { current_head } ->
        Lwt.return current_head
      end

    let protocol net =
      Shared.use net.state begin fun { current_head } ->
        match current_head.protocol with
        | None -> assert false (* TODO PROPER ERROR *)
        | Some proto -> Lwt.return proto
      end

    let mem net hash =
      Shared.use net.state begin fun { chain_store } ->
        Store.Chain.In_chain_insertion_time.known (chain_store, hash)
      end

    let find_new net hist sz =
      let rec common_ancestor hist =
        match hist with
        | [] -> Lwt.return net.genesis.block
        | h :: hist ->
            mem net h >>= function
            | false -> common_ancestor hist
            | true -> Lwt.return h in
      let rec path sz acc h =
        if sz <= 0 then return (List.rev acc)
        else
          Shared.use net.state begin fun { chain_store } ->
            Store.Chain.Successor_in_chain.read_opt (chain_store, h)
          end >>= function
          | None -> return (List.rev acc)
          | Some s -> path (sz-1) (s :: acc) s in
      common_ancestor hist >>= fun ancestor ->
      path sz [] ancestor

    let new_blocks store old_block new_block =
      Raw_block_header.read_exn (* valid block *)
        store old_block.hash >>= fun { shell = old_header } ->
      Raw_block_header.read_exn (* valid block *)
        store new_block.hash >>= fun { shell = new_header } ->
      Raw_helpers.common_ancestor store
        old_block.hash old_header
        new_block.hash new_header >>= function
      | None -> assert false (* valid block *)
      | Some (ancestor, _header) ->
          Raw_helpers.path store ancestor new_block.hash >>= function
          | None -> assert false (* valid block *)
          | Some path -> Lwt.return (ancestor, path)

    let locked_set_head block_header_store operation_store state block =
      let rec pop_blocks ancestor hash =
        if Block_hash.equal hash ancestor then
          Lwt.return_unit
        else
          lwt_debug "pop_block %a" Block_hash.pp_short hash >>= fun () ->
          Raw_block_header.read_exn
            block_header_store hash >>= fun { shell } ->
          Raw_operation_list.read_all_exn
            block_header_store hash >>= fun operations ->
          let operations = List.concat operations in
          Lwt_list.iter_p
            (fun h ->
               Raw_operation.Locked.unmark operation_store h >>= fun _ ->
               Lwt.return_unit)
            operations >>= fun () ->
          Store.Chain.In_chain_insertion_time.remove
            (state.chain_store, hash) >>= fun () ->
          Store.Chain.Successor_in_chain.remove
            (state.chain_store, shell.predecessor) >>= fun () ->
          pop_blocks ancestor shell.predecessor
      in
      let push_block time (hash, shell) =
        lwt_debug "push_block %a" Block_hash.pp_short hash >>= fun () ->
        Store.Chain.In_chain_insertion_time.store
          (state.chain_store, hash) time >>= fun () ->
        Store.Chain.Successor_in_chain.store
          (state.chain_store,
           shell.Store.Block_header.predecessor) hash >>= fun () ->
          Raw_operation_list.read_all_exn
            block_header_store hash >>= fun operations ->
        let operations = List.concat operations in
        Lwt_list.iter_p
          (fun h ->
             Raw_operation.Locked.mark_valid operation_store h >>= fun _ ->
            Lwt.return_unit)
          operations
      in
      let time = Time.now () in
      new_blocks
        block_header_store state.current_head block >>= fun (ancestor, path) ->
      pop_blocks ancestor state.current_head.hash >>= fun () ->
      Lwt_list.iter_p (push_block time) path >>= fun () ->
      state.current_head <- block ;
      Store.Chain.Current_head.store state.chain_store block.hash

    let set_head net block =
      Shared.use net.state begin fun state ->
        Shared.use net.operation_store begin fun operation_store ->
          locked_set_head net.block_header_store operation_store state block
        end
      end

    let test_and_set_head net ~old block =
      Shared.use net.state begin fun state ->
        if not (Block_hash.equal state.current_head.hash old.hash) then
          Lwt.return_false
        else
          Shared.use net.operation_store begin fun operation_store ->
            locked_set_head
              net.block_header_store operation_store state block >>= fun () ->
            Lwt.return_true
          end
      end

    let new_blocks net ~from_block ~to_block =
      new_blocks net.block_header_store from_block to_block

  end

end

module Net = struct

  type t = net
  type net = t

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

  let create state ?test_protocol ?forked_network_ttl genesis =
    let net_id = Net_id.of_block_hash genesis.block in
    let forked_network_ttl = map_option Int64.of_int forked_network_ttl in
    Shared.use state.global_data begin fun data ->
      if Net_id.Table.mem data.nets net_id then
        Pervasives.failwith "State.Net.create"
      else
        Raw_net.locked_create data
          ?test_protocol ?forked_network_ttl genesis >>= fun net ->
        Net_id.Table.add data.nets net_id net ;
        Lwt.return net
    end

  let locked_read data id =
    let net_store = Store.Net.get data.global_store id in
    let operation_store = Store.Operation.get net_store
    and block_header_store = Store.Block_header.get net_store
    and chain_store = Store.Chain.get net_store in
    Store.Net.Genesis_hash.read net_store >>=? fun genesis_hash ->
    Store.Net.Genesis_time.read net_store >>=? fun time ->
    Store.Net.Genesis_protocol.read net_store >>=? fun protocol ->
    Store.Net.Expiration.read_opt net_store >>= fun expiration ->
    Store.Net.Forked_network_ttl.read_opt net_store >>= fun forked_network_ttl ->
    let genesis = { time ; protocol ; block = genesis_hash } in
    Store.Chain.Current_head.read chain_store >>=? fun genesis_hash ->
    data.init_index id >>= fun context_index ->
    Block_header.Locked.read block_header_store
      genesis_hash >>=? fun genesis_shell_header ->
    Block_header.Locked.read_discovery_time block_header_store
      genesis_hash >>=? fun genesis_discovery_time ->
    Valid_block.Locked.raw_read
      genesis_shell_header [] genesis_discovery_time
      chain_store context_index genesis_hash >>=? fun genesis_block ->
    return @@
    Raw_net.build
      ~genesis
      ~genesis_block
      ~expiration
      ~forked_network_ttl
      context_index
      chain_store
      block_header_store
      operation_store

  let locked_read_all data =
    Store.Net.list data.global_store >>= fun ids ->
    iter_p
      (fun id ->
         locked_read data id >>=? fun net ->
         Net_id.Table.add data.nets id net ;
         return ())
      ids

  let read_all state =
    Shared.use state.global_data begin fun data ->
      locked_read_all data
    end

  let get state id =
    Shared.use state.global_data begin fun data ->
      try return (Net_id.Table.find data.nets id)
      with Not_found -> fail (Unknown_network id)
    end

  let all state =
    Shared.use state.global_data begin fun { nets } ->
      Lwt.return @@
      Net_id.Table.fold (fun _ net acc -> net :: acc) nets []
    end

  let id { id } = id
  let genesis { genesis } = genesis
  let expiration { expiration } = expiration
  let forked_network_ttl { forked_network_ttl } = forked_network_ttl

  let destroy state net =
    lwt_debug "destroy %a" Net_id.pp (id net) >>= fun () ->
    Shared.use state.global_data begin fun { global_store ; nets } ->
      Net_id.Table.remove nets (id net) ;
      Store.Net.destroy global_store (id net) >>= fun () ->
      Lwt.return_unit
    end

end


(*
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
      "The economic protocol refused to validate the block."
    ~pp:(fun ppf block_hash ->
        Format.fprintf ppf "Cannot validate the block %a"
          Block_hash.pp_short block_hash)
    (obj1 (req "block_hash" Block_hash.encoding))
    (function Exn (Valid_block.Invalid (block_hash, _)) -> Some block_hash
            | _ -> None)
    (fun block_hash -> Exn (Valid_block.Invalid (block_hash, [(* TODO *)])))

*)

module Operation = struct

  type shell_header = Store.Operation.shell_header = {
    net_id: Net_id.t ;
  }

  type t = Store.Operation.t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include Make_data_store
      (Store.Operation)
      (struct
        type store = net
        let use s = Shared.use s.operation_store
        let unknown k = fail (Unknown_operation k)
      end)
      (Operation_hash.Set)

  let in_chain = valid

end

module Protocol = struct

  type t = Store.Protocol.t

  include Make_data_store
      (Store.Protocol)
      (struct
        type store = global_state
        let use s = Shared.use s.protocol_store
        let unknown k = fail (Unknown_protocol k)
      end)
      (Protocol_hash.Set)

  (* TODO somehow export `mark_invalid`. *)

end

let read
  ?patch_context
  ~store_root
  ~context_root
  () =
  Store.init store_root >>=? fun store ->
  Context.init ?patch_context ~root:context_root >>= fun context_index ->
  let global_data = {
    nets = Net_id.Table.create 17 ;
    global_store = store ;
    init_index = (fun _ -> Lwt.return context_index) ;
  } in
  let state = {
    global_data = Shared.create global_data ;
    protocol_store = Shared.create @@ Store.Protocol.get store ;
  } in
  Net.read_all state >>=? fun () ->
  return state
