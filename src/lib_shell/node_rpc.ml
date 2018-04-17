(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.RPC

let filter_bi operations (bi: Block_services.block_info)  =
  let bi = if operations then bi else { bi with operations = None } in
  bi

let register_bi_dir node dir =
  let dir =
    let implementation b () include_ops =
      Node.RPC.block_info node b >>= fun bi ->
      return (filter_bi include_ops bi) in
    RPC_directory.register1 dir
      Block_services.S.info implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.hash in
    RPC_directory.register1 dir
      Block_services.S.hash
      implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.chain_id in
    RPC_directory.register1 dir
      Block_services.S.chain_id implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.level in
    RPC_directory.register1 dir
      Block_services.S.level implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.predecessor in
    RPC_directory.register1 dir
      Block_services.S.predecessor implementation in
  let dir =
    let implementation b () len =
      Node.RPC.block_info node b >>= fun bi ->
      Node.RPC.predecessors node len bi.hash >>= fun hashes ->
      return hashes in
    RPC_directory.register1 dir
      Block_services.S.predecessors implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.fitness in
    RPC_directory.register1 dir
      Block_services.S.fitness implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.timestamp in
    RPC_directory.register1 dir
      Block_services.S.timestamp implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.protocol in
    RPC_directory.register1 dir
      Block_services.S.protocol implementation in
  let dir =
    let implementation b () () =
      Node.RPC.block_info node b >>= fun bi ->
      return bi.test_chain in
    RPC_directory.register1 dir
      Block_services.S.test_chain implementation in
  let dir =
    let implementation b () { Block_services.S.contents } =
      Node.RPC.operation_hashes node b >>= fun hashes ->
      if contents then
        Node.RPC.operations node b >>= fun ops ->
        RPC_answer.return @@
        List.map2 (List.map2 (fun h op -> h, Some op)) hashes ops
      else
        RPC_answer.return @@
        List.map (List.map (fun h -> h, None)) hashes
    in
    RPC_directory.gen_register1 dir
      Block_services.S.operations implementation in

  let dir =
    let implementation
        b ()
        { Block_services.S.operations ; sort_operations ;
          timestamp ; protocol_data } =
      Node.RPC.preapply node b
        ~timestamp ~protocol_data ~sort_operations operations
      >>=? fun (shell_header, operations) ->
      return { Block_services.shell_header ; operations } in
    RPC_directory.register1 dir
      Block_services.S.preapply implementation in
  dir

let rec insert_future_block (bi: Block_services.block_info) = function
  | [] -> [bi]
  | ({timestamp} as head: Block_services.block_info) :: tail as all ->
      if Time.compare bi.timestamp timestamp < 0 then
        bi :: all
      else
        head :: insert_future_block bi tail

let create_delayed_stream
    ~filtering ~include_ops requested_heads bi_stream delay =
  let stream, push = Lwt_stream.create  () in
  let current_blocks =
    ref (List.fold_left
           (fun acc h -> Block_hash.Set.add h acc)
           Block_hash.Set.empty requested_heads) in
  let next_future_block, is_futur_block,
      insert_future_block, pop_future_block =
    let future_blocks = ref [] in (* FIXME *)
    let future_blocks_set = ref Block_hash.Set.empty in
    let next () =
      match !future_blocks with
      | [] -> None
      | bi :: _ -> Some bi
    and mem hash = Block_hash.Set.mem hash !future_blocks_set
    and insert bi =
      future_blocks := insert_future_block bi !future_blocks ;
      future_blocks_set :=
        Block_hash.Set.add bi.hash !future_blocks_set
    and pop time =
      match !future_blocks with
      | {timestamp} as bi :: rest when Time.(timestamp <= time) ->
          future_blocks := rest ;
          future_blocks_set :=
            Block_hash.Set.remove bi.hash !future_blocks_set ;
          Some bi
      | _ -> None in
    next, mem, insert, pop in
  let _block_watcher_worker =
    let never_ending = fst (Lwt.wait ()) in
    let rec worker_loop () =
      lwt_debug "WWW worker_loop" >>= fun () ->
      let time = Time.(add (now ()) (Int64.of_int ~-delay)) in
      let migration_delay =
        match next_future_block () with
        | None -> never_ending
        | Some bi ->
            let delay = Time.diff bi.timestamp time in
            if delay <= 0L then
              Lwt.return_unit
            else
              Lwt_unix.sleep (Int64.to_float delay) in
      Lwt.choose [(migration_delay >|= fun () -> `Migrate) ;
                  (Lwt_stream.get bi_stream >|= fun x -> `Block x) ]
      >>= function
      | `Block None ->
          lwt_debug "WWW worker_loop None" >>= fun () ->
          Lwt.return_unit
      | `Block (Some (bi : Block_services.block_info)) ->
          lwt_debug "WWW worker_loop Some" >>= fun () ->
          begin
            if not filtering
            || Block_hash.Set.mem bi.predecessor !current_blocks
            || is_futur_block bi.predecessor
            then begin
              let time = Time.(add (now ()) (Int64.of_int ~-delay)) in
              if Time.(time < bi.timestamp) then begin
                insert_future_block bi ;
                Lwt.return_unit
              end else begin
                current_blocks :=
                  Block_hash.Set.remove bi.predecessor !current_blocks
                  |> Block_hash.Set.add bi.hash ;
                push (Some [[filter_bi include_ops bi]]) ;
                Lwt.return_unit
              end
            end else begin
              Lwt.return_unit
            end
          end >>= fun () ->
          worker_loop ()
      | `Migrate ->
          lwt_debug "WWW worker_loop Migrate" >>= fun () ->
          let time = Time.(add (now ()) (Int64.of_int ~-delay)) in
          let rec migrate_future_blocks () =
            match pop_future_block time with
            | Some bi ->
                push (Some [[filter_bi include_ops bi]]) ;
                migrate_future_blocks ()
            | None -> Lwt.return_unit in
          migrate_future_blocks () >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker "block_watcher"
      ~run:worker_loop ~cancel:(fun () -> Lwt.return_unit) in
  stream

let list_blocks
    node ()
    { Block_services.S.include_ops ; length ; heads ; monitor ; delay ;
      min_date; min_heads} =
  let len = match length with None -> 1 | Some x -> x in
  let monitor = match monitor with None -> false | Some x -> x in
  let time =
    match delay with
    | None -> None
    | Some delay -> Some (Time.(add (now ()) (Int64.of_int ~-delay))) in
  begin
    match heads with
    | None ->
        Node.RPC.heads node >>= fun heads ->
        let heads = List.map snd (Block_hash.Map.bindings heads) in
        let heads =
          match min_date with
          | None -> heads
          | Some date ->
              let min_heads =
                match min_heads with
                | None -> 0
                | Some min_heads -> min_heads in
              snd @@
              List.fold_left (fun (min_heads, acc) (bi : Node.RPC.block_info) ->
                  min_heads - 1,
                  if Time.(>) bi.timestamp date || min_heads > 0 then bi :: acc
                  else acc)
                (min_heads, []) heads in
        begin
          match time with
          | None -> Lwt.return heads
          | Some time ->
              let rec current_predecessor (bi: Node.RPC.block_info)  =
                if Time.compare bi.timestamp time <= 0
                || bi.hash = bi.predecessor then
                  Lwt.return bi
                else
                  Node.RPC.raw_block_info node bi.predecessor >>=
                  current_predecessor in
              Lwt_list.map_p current_predecessor heads
        end >|= fun heads_info ->
        let sorted_infos =
          List.sort
            (fun
              (bi1: Block_services.block_info)
              (bi2: Block_services.block_info) ->
              ~- (Fitness.compare bi1.fitness bi2.fitness))
            heads_info in
        List.map
          (fun ({ hash } : Block_services.block_info) -> hash)
          sorted_infos
    | Some heads ->
        let known_block h =
          try ignore (Node.RPC.raw_block_info node h) ; true
          with Not_found -> false in
        Lwt.return (List.filter known_block heads)
  end >>= fun requested_heads ->
  Node.RPC.list node len requested_heads >>= fun requested_blocks ->
  if not monitor then
    let infos =
      List.map
        (List.map (filter_bi include_ops))
        requested_blocks in
    RPC_answer.return infos
  else begin
    let (bi_stream, stopper) = Node.RPC.block_watcher node in
    let stream =
      match delay with
      | None ->
          Lwt_stream.map (fun bi -> [[filter_bi include_ops bi]]) bi_stream
      | Some delay ->
          let filtering = heads <> None in
          create_delayed_stream
            ~filtering ~include_ops requested_heads bi_stream delay in
    let shutdown () = Lwt_watcher.shutdown stopper in
    let first_request = ref true in
    let next () =
      if not !first_request then begin
        Lwt_stream.get stream
      end else begin
        first_request := false ;
        let infos =
          List.map (List.map (filter_bi include_ops)) requested_blocks in
        Lwt.return (Some infos)
      end in
    RPC_answer.return_stream { next ; shutdown }
  end

let list_invalid node () () =
  Node.RPC.list_invalid node >>= return

let unmark_invalid node block () () =
  Node.RPC.unmark_invalid node block

let list_protocols node () { Protocol_services.S.monitor ; contents } =
  let monitor = match monitor with None -> false | Some x -> x in
  let include_contents = match contents with None -> false | Some x -> x in
  Node.RPC.protocols node >>= fun protocols ->
  Lwt_list.map_p
    (fun hash ->
       if include_contents then
         Node.RPC.protocol_content node hash >>= function
         | Error _  -> Lwt.return (hash, None)
         | Ok bytes -> Lwt.return (hash, Some bytes)
       else
         Lwt.return (hash, None))
    protocols >>= fun protocols ->
  if not monitor then
    RPC_answer.return protocols
  else
    let stream, stopper = Node.RPC.protocol_watcher node in
    let shutdown () = Lwt_watcher.shutdown stopper in
    let first_request = ref true in
    let next () =
      if not !first_request then
        Lwt_stream.get stream >>= function
        | None -> Lwt.return_none
        | Some (h, op) when include_contents -> Lwt.return (Some [h, Some op])
        | Some (h, _) -> Lwt.return (Some [h, None])
      else begin
        first_request := false ;
        Lwt.return (Some protocols)
      end in
    RPC_answer.return_stream { next ; shutdown }

let get_protocols node hash () () =
  Node.RPC.protocol_content node hash

let build_rpc_directory node =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.gen_register0 dir Block_services.S.list
      (list_blocks node) in
  let dir =
    RPC_directory.register0 dir Block_services.S.list_invalid
      (list_invalid node) in
  let dir =
    RPC_directory.register1 dir Block_services.S.unmark_invalid
      (unmark_invalid node) in
  let dir = register_bi_dir node dir in
  let dir =
    let implementation block =
      Lwt.catch (fun () ->
          Node.RPC.context_dir node block >>= function
          | None -> Lwt.fail Not_found
          | Some context_dir -> Lwt.return context_dir)
        (fun _ -> Lwt.return RPC_directory.empty) in
    RPC_directory.register_dynamic_directory1
      ~descr:
        "All the RPCs which are specific to the protocol version."
      dir (Block_services.S.proto_path ()) implementation in
  let dir =
    RPC_directory.gen_register0 dir Protocol_services.S.list
      (list_protocols node) in
  let dir =
    RPC_directory.register1 dir Protocol_services.S.contents
      (get_protocols node) in
  let dir =
    let implementation () header =
      let res =
        Data_encoding.Binary.to_bytes Block_header.encoding header in
      RPC_answer.return res in
    RPC_directory.gen_register0 dir Shell_services.S.forge_block_header
      implementation in
  let dir =
    let implementation ()
        { Shell_services.S.raw ; blocking ; force ; operations } =
      begin
        Node.RPC.inject_block
          node ~force
          raw operations >>=? fun (hash, wait) ->
        (if blocking then wait else return ()) >>=? fun () -> return hash
      end in
    RPC_directory.register0 dir Shell_services.S.inject_block implementation in
  let dir =
    let implementation () (contents, blocking, chain_id) =
      Node.RPC.inject_operation
        node ?chain_id contents >>= fun (hash, wait) ->
      begin
        (if blocking then wait else return ()) >>=? fun () -> return hash
      end in
    RPC_directory.register0 dir Shell_services.S.inject_operation implementation in
  let dir =
    let implementation () (proto, blocking, force) =
      Node.RPC.inject_protocol ?force node proto >>= fun (hash, wait) ->
      begin
        (if blocking then wait else return ()) >>=? fun () -> return hash
      end in
    RPC_directory.register0 dir Shell_services.S.inject_protocol implementation in
  let dir =
    let implementation () () =
      RPC_answer.return_stream (Node.RPC.bootstrapped node) in
    RPC_directory.gen_register0 dir Shell_services.S.bootstrapped implementation in
  let dir =
    let implementation () () =
      return Data_encoding.Json.(schema Error_monad.error_encoding) in
    RPC_directory.register0 dir RPC_service.error_service implementation in
  let dir =
    RPC_directory.register1 dir Shell_services.S.complete
      (fun s () () -> Node.RPC.complete node s >>= return) in
  let dir =
    RPC_directory.register2 dir Block_services.S.complete
      (fun block s () () -> Node.RPC.complete node ~block s >>= return) in
  let dir =
    RPC_directory.register2 dir Block_services.S.raw_context
      (fun block path q () ->
         Node.RPC.context_raw_get node block ~path ~depth:q#depth >>= function
         | None -> raise Not_found
         | Some v -> return v)
  in

  (* Workers : Prevalidators *)

  let dir  =
    RPC_directory.register0 dir Worker_services.Prevalidators.S.list
      (fun () () ->
         return
           (List.map
              (fun (id, w) -> (id, Prevalidator.status w))
              (Prevalidator.running_workers ()))) in
  let dir  =
    RPC_directory.register1 dir Worker_services.Prevalidators.S.state
      (fun chain_id () () ->
         let w = List.assoc chain_id (Prevalidator.running_workers ()) in
         return
           { Worker_types.status = Prevalidator.status w ;
             pending_requests = Prevalidator.pending_requests w ;
             backlog = Prevalidator.last_events w ;
             current_request = Prevalidator.current_request w }) in

  (* Workers : Block_validator *)

  let dir  =
    RPC_directory.register0 dir Worker_services.Block_validator.S.state
      (fun () () ->
         let w = Block_validator.running_worker () in
         return
           { Worker_types.status = Block_validator.status w ;
             pending_requests = Block_validator.pending_requests w ;
             backlog = Block_validator.last_events w ;
             current_request = Block_validator.current_request w }) in

  (* Workers : Peer validators *)

  let dir  =
    RPC_directory.register1 dir Worker_services.Peer_validators.S.list
      (fun chain_id () () ->
         return
           (List.filter_map
              (fun ((id, peer_id), w) ->
                 if Chain_id.equal id chain_id then
                   Some (peer_id, Peer_validator.status w)
                 else None)
              (Peer_validator.running_workers ()))) in
  let dir  =
    RPC_directory.register2 dir Worker_services.Peer_validators.S.state
      (fun chain_id peer_id () () ->
         let w = List.assoc (chain_id, peer_id) (Peer_validator.running_workers ()) in
         return
           { Worker_types.status = Peer_validator.status w ;
             pending_requests = [] ;
             backlog = Peer_validator.last_events w ;
             current_request = Peer_validator.current_request w }) in

  (* Workers : Net validators *)
  let dir  =
    RPC_directory.register0 dir Worker_services.Chain_validators.S.list
      (fun () () ->
         return
           (List.map
              (fun (id, w) -> (id, Chain_validator.status w))
              (Chain_validator.running_workers ()))) in
  let dir  =
    RPC_directory.register1 dir Worker_services.Chain_validators.S.state
      (fun chain_id () () ->
         let w = List.assoc chain_id (Chain_validator.running_workers ()) in
         return
           { Worker_types.status = Chain_validator.status w ;
             pending_requests = Chain_validator.pending_requests w ;
             backlog = Chain_validator.last_events w ;
             current_request = Chain_validator.current_request w }) in

  (* Network  *)
  let dir = RPC_directory.merge dir (Node.RPC.build_p2p_rpc_directory node) in

  (* Mempool *)
  let dir =
    let implementation () () () =
      Node.RPC.pending_operations node  >>= fun res ->
      return res in
    RPC_directory.register dir
      Mempool_services.S.pending_operations
      implementation in

  let dir =
    RPC_directory.register_describe_directory_service
      dir RPC_service.description_service in


  dir

