(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Validator

type worker = {
  activate: ?parent:t -> State.Net.t -> t Lwt.t ;
  get: State.Net_id.t -> t tzresult Lwt.t ;
  get_exn: State.Net_id.t -> t Lwt.t ;
  deactivate: t -> unit Lwt.t ;
  inject_block:
    ?force:bool ->
    MBytes.t -> Operation_hash.t list list ->
    (Block_hash.t * State.Valid_block.t tzresult Lwt.t) tzresult Lwt.t ;
  notify_block: Block_hash.t -> Store.Block_header.t -> unit Lwt.t ;
  shutdown: unit -> unit Lwt.t ;
  valid_block_input: State.Valid_block.t Watcher.input ;
  db: Distributed_db.t ;
}

and t = {
  net: State.Net.t ;
  worker: worker ;
  parent: t option ;
  mutable child: t option ;
  prevalidator: Prevalidator.t ;
  net_db: Distributed_db.net ;
  notify_block: Block_hash.t -> Store.Block_header.t -> unit Lwt.t ;
  fetch_block: Block_hash.t -> State.Valid_block.t tzresult Lwt.t ;
  create_child: State.Valid_block.t -> unit tzresult Lwt.t ;
  test_validator: unit -> (t * Distributed_db.net) option ;
  shutdown: unit -> unit Lwt.t ;
  valid_block_input: State.Valid_block.t Watcher.input ;
  new_head_input: State.Valid_block.t Watcher.input ;
  bootstrapped: unit Lwt.t ;
}

let net_state { net } = net
let net_db { net_db } = net_db

let activate w net = w.activate net
let deactivate t = t.worker.deactivate t
let get w = w.get
let get_exn w = w.get_exn
let notify_block w = w.notify_block
let inject_block w = w.inject_block
let shutdown w = w.shutdown ()
let test_validator w = w.test_validator ()

let fetch_block v = v.fetch_block
let prevalidator v = v.prevalidator
let bootstrapped v = v.bootstrapped

(** Current block computation *)

let may_change_test_network v (block: State.Valid_block.t) =
  let change =
    match block.test_network, v.child with
    | None, None -> false
    | Some _, None
    | None, Some _ -> true
    | Some (net_id, _), Some { net } ->
        let net_id' = State.Net.id net in
        not (State.Net_id.equal net_id net_id') in
  if change then begin
    v.create_child block >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
        lwt_log_error "@[<v 2>Error while switch test network:@ %a@]"
          Error_monad.pp_print_error err
  end else
    Lwt.return_unit

let fetch_protocol v hash =
  lwt_log_notice "Fetching protocol %a"
    Protocol_hash.pp_short hash >>= fun () ->
  Distributed_db.Protocol.fetch
    v.worker.db hash >>= fun protocol ->
  Updater.compile hash protocol >>= fun valid ->
  if valid then begin
    lwt_log_notice "Successfully compiled protocol %a"
      Protocol_hash.pp_short hash >>= fun () ->
    Distributed_db.Protocol.commit
      v.worker.db hash >>= fun () ->
    return true
  end else begin
    lwt_log_error "Failed to compile protocol %a"
      Protocol_hash.pp_short hash >>= fun () ->
    failwith "Cannot compile the protocol %a" Protocol_hash.pp_short hash
  end

let fetch_protocols v (block: State.Valid_block.t) =
  let proto_updated =
    match block.protocol with
    | Some _ -> return false
    | None -> fetch_protocol v block.protocol_hash
  and test_proto_updated =
    match block.test_protocol with
    | Some _ -> return false
    | None -> fetch_protocol v block.test_protocol_hash in
  proto_updated >>=? fun proto_updated ->
  test_proto_updated >>=? fun test_proto_updated ->
  if test_proto_updated || proto_updated then
    State.Valid_block.read_exn v.net block.hash >>= return
  else
    return block

let rec may_set_head v (block: State.Valid_block.t) =
  State.Valid_block.Current.head v.net >>= fun head ->
  if Fitness.compare head.fitness block.fitness >= 0 then
    Lwt.return_unit
  else begin
    State.Valid_block.Current.test_and_set_head v.net
      ~old:head block >>= function
    | false -> may_set_head v block
    | true ->
        Distributed_db.broadcast_head v.net_db block.hash [] ;
        Prevalidator.flush v.prevalidator block ;
        may_change_test_network v block >>= fun () ->
        Watcher.notify v.new_head_input block ;
        lwt_log_notice "update current head %a %a %a(%t)"
          Block_hash.pp_short block.hash
          Fitness.pp block.fitness
          Time.pp_hum block.timestamp
          (fun ppf ->
             if Block_hash.equal head.hash block.pred then
               Format.fprintf ppf "same branch"
             else
               Format.fprintf ppf "changing branch") >>= fun () ->
        Lwt.return_unit
  end

(** Block validation *)

type error +=
   | Invalid_operation of Operation_hash.t
   | Non_increasing_timestamp
   | Non_increasing_fitness

let apply_block net db
    (pred: State.Valid_block.t) hash (block: State.Block_header.t) =
  let id = State.Net.id net in
  lwt_log_notice "validate block %a (after %a), net %a"
    Block_hash.pp_short hash
    Block_hash.pp_short block.shell.predecessor
    State.Net_id.pp id
  >>= fun () ->
  lwt_log_info "validation of %a: looking for dependencies..."
    Block_hash.pp_short hash >>= fun () ->
  Distributed_db.Operation_list.fetch
    db (hash, 0) block.shell.operations >>= fun operation_hashes ->
  Lwt_list.map_p
    (fun op -> Distributed_db.Operation.fetch db op)
    operation_hashes >>= fun operations ->
  lwt_debug "validation of %a: found operations"
    Block_hash.pp_short hash >>= fun () ->
  begin (* Are we validating a block in an expired test network ? *)
    match State.Net.expiration net with
    | Some eol when Time.(eol <= block.shell.timestamp) ->
        failwith "This test network expired..."
    | None | Some _ -> return ()
  end >>=? fun () ->
  begin
    if Time.(pred.timestamp >= block.shell.timestamp) then
      fail Non_increasing_timestamp
    else
      return ()
  end >>=? fun () ->
  begin
    if Fitness.compare pred.fitness block.shell.fitness >= 0 then
      fail Non_increasing_fitness
    else
      return ()
  end >>=? fun () ->
  begin
    match pred.protocol with
    | None -> fail (State.Unknown_protocol pred.protocol_hash)
    | Some p ->
       Context.set_timestamp pred.context block.shell.timestamp >>= fun c ->
       return (p, c)
  end >>=? fun ((module Proto), patched_context) ->
  lwt_debug "validation of %a: Proto %a"
    Block_hash.pp_short hash
    Protocol_hash.pp_short Proto.hash >>= fun () ->
  lwt_debug "validation of %a: parsing header..."
    Block_hash.pp_short hash >>= fun () ->
  Lwt.return (Proto.parse_block block pred.timestamp) >>=? fun parsed_header ->
  lwt_debug "validation of %a: parsing operations..."
    Block_hash.pp_short hash >>= fun () ->
  map2_s
    (fun op_hash raw ->
       Lwt.return (Proto.parse_operation op_hash raw)
       |> trace (Invalid_operation op_hash))
    operation_hashes
    operations >>=? fun parsed_operations ->
  lwt_debug "validation of %a: applying block..."
    Block_hash.pp_short hash >>= fun () ->
  Proto.apply
    patched_context parsed_header parsed_operations >>=? fun new_context ->
  lwt_log_info "validation of %a: success"
    Block_hash.pp_short hash >>= fun () ->
  return new_context

(** *)

module Context_db = struct

    type key = Block_hash.t
    type value = State.Valid_block.t

    type data =
      { validator: t ;
        state: [ `Inited of Store.Block_header.t tzresult
               | `Initing of Store.Block_header.t tzresult Lwt.t
               | `Running of State.Valid_block.t tzresult Lwt.t ] ;
        wakener: State.Valid_block.t tzresult Lwt.u }

    type context =
      { tbl : data Block_hash.Table.t ;
        canceler : Lwt_utils.Canceler.t ;
        worker_trigger: unit -> unit;
        worker_waiter: unit -> unit Lwt.t ;
        worker: unit Lwt.t ;
        net_db : Distributed_db.net ;
        net_state : State.Net.t }

    let pending_requests { tbl } =
      Block_hash.Table.fold
        (fun h data acc ->
           match data.state with
           | `Initing _ -> acc
           | `Running _ -> acc
           | `Inited d -> (h, d, data) :: acc)
        tbl []

    let pending { tbl } hash = Block_hash.Table.mem tbl hash

    let request validator { tbl ; worker_trigger ; net_db } hash =
      assert (not (Block_hash.Table.mem tbl hash));
      let waiter, wakener = Lwt.wait () in
      let data =
        Distributed_db.Block_header.fetch net_db hash >>= return in
      match Lwt.state data with
      | Lwt.Return data ->
          let state = `Inited data in
          Block_hash.Table.add tbl hash { validator ; state ; wakener } ;
          worker_trigger () ;
          waiter
      | _ ->
          let state = `Initing data in
          Block_hash.Table.add tbl hash { validator ; state ; wakener } ;
          Lwt.async
            (fun () ->
               data >>= fun data ->
               let state = `Inited data in
               Block_hash.Table.replace tbl hash { validator ; state ; wakener } ;
               worker_trigger () ;
               Lwt.return_unit) ;
          waiter

    let prefetch validator ({ net_state ; tbl } as session) hash =
      Lwt.ignore_result
        (State.Valid_block.known net_state hash >>= fun exists ->
         if not exists && not (Block_hash.Table.mem tbl hash) then
           request validator session hash >>= fun _ -> Lwt.return_unit
         else
           Lwt.return_unit)

    let known { net_state } hash =
      State.Valid_block.known net_state hash

    let read { net_state } hash =
      State.Valid_block.read net_state hash

    let fetch ({ net_state ; tbl } as session) validator hash =
      try Lwt.waiter_of_wakener (Block_hash.Table.find tbl hash).wakener
      with Not_found ->
        State.Valid_block.read_opt net_state hash >>= function
        | Some op ->
            Lwt.return (Ok op)
        | None ->
            try Lwt.waiter_of_wakener (Block_hash.Table.find tbl hash).wakener
            with Not_found -> request validator session hash

    let store { net_state ; net_db ; tbl } hash data =
      begin
        match data with
        | Ok data ->
            Distributed_db.Block_header.commit net_db hash >>= fun () ->
            Distributed_db.Operation_list.commit_all
              net_db hash 1 >>= fun () ->
            begin
              State.Valid_block.store net_state hash data >>=? function
              | None ->
                  State.Valid_block.read net_state hash >>=? fun block ->
                  Lwt_list.iter_p
                    (Lwt_list.iter_p (fun hash ->
                         Distributed_db.Operation.commit net_db hash))
                    block.operations >>= fun () ->
                  return (Ok block, false)
              | Some block ->
                  Lwt_list.iter_p
                    (Lwt_list.iter_p (fun hash ->
                         Distributed_db.Operation.commit net_db hash))
                    block.operations >>= fun () ->
                  return (Ok block, true)
            end
        | Error err ->
            State.Block_header.mark_invalid
              net_state hash err >>= fun changed ->
            return (Error err, changed)
      end >>= function
      | Ok (block, changed) ->
          let wakener = (Block_hash.Table.find tbl hash).wakener in
          Block_hash.Table.remove tbl hash;
          Lwt.wakeup wakener block ;
          Lwt.return changed
      | Error _ as err ->
          let wakener = (Block_hash.Table.find tbl hash).wakener in
          Block_hash.Table.remove tbl hash;
          Lwt.wakeup wakener err ;
          Lwt.return false

  let process (v:t) ~get_context ~set_context hash block =
    let state = Distributed_db.state v.net_db in
    get_context v block.State.Block_header.shell.predecessor >>= function
    | Error _ as error ->
        set_context v hash (Error [(* TODO *)]) >>= fun () ->
        Lwt.return error
    | Ok _context ->
        lwt_debug "process %a" Block_hash.pp_short hash >>= fun () ->
        begin
          State.Valid_block.Current.genesis state >>= fun genesis ->
          if Block_hash.equal genesis.hash block.shell.predecessor then
            Lwt.return genesis
          else
            State.Valid_block.read_exn state block.shell.predecessor
        end >>= fun pred ->
        apply_block state v.net_db pred hash block >>= function
        | Error ([State.Unknown_protocol _] as err) as error ->
            lwt_log_error
              "@[<v 2>Ignoring block %a@ %a@]"
              Block_hash.pp_short hash
              Error_monad.pp_print_error err >>= fun () ->
            Lwt.return error
        | Error exns as error ->
            set_context v hash error >>= fun () ->
            lwt_warn "Failed to validate block %a."
              Block_hash.pp_short hash >>= fun () ->
            lwt_debug "%a" Error_monad.pp_print_error exns >>= fun () ->
            Lwt.return error
        | Ok new_context ->
            (* The sanity check `set_context` detects differences
               between the computed fitness and the fitness announced
               in the block header. Then `Valid_block.read` will
               return an error. *)
            set_context v hash (Ok new_context) >>= fun () ->
            State.Valid_block.read state hash >>= function
            | Error err as error ->
                lwt_log_error
                  "@[<v 2>Ignoring block %a@ %a@]"
                  Block_hash.pp_short hash
                  Error_monad.pp_print_error err >>= fun () ->
                Lwt.return error
            | Ok block ->
                lwt_debug
                  "validation of %a: reevaluate current block"
                  Block_hash.pp_short hash >>= fun () ->
                Watcher.notify v.worker.valid_block_input block ;
                Watcher.notify v.valid_block_input block ;
                fetch_protocols v block >>=? fun block ->
                may_set_head v block >>= fun () ->
                return block

  let request session ~get_context ~set_context pendings =
      let time = Time.now () in
      let min_block b pb =
        match pb with
        | None -> Some b
        | Some pb
          when b.Store.Block_header.shell.timestamp
               < pb.Store.Block_header.shell.timestamp ->
            Some b
        | Some _ as pb -> pb in
      let next =
        List.fold_left
          (fun acc (hash, block, (data : data)) ->
             match block with
             | Error _ ->
                 acc
             | Ok block ->
                 if Time.(block.Store.Block_header.shell.timestamp > time) then
                   min_block block acc
                 else begin
                   Block_hash.Table.replace session.tbl hash { data with state = `Running begin
                       Lwt_main.yield () >>= fun () ->
                       process data.validator ~get_context ~set_context hash block >>= fun res ->
                       Block_hash.Table.remove session.tbl hash ;
                       Lwt.return res
                     end } ;
                   acc
                 end)
          None
          pendings in
      match next with
      | None -> 0.
      | Some b -> Int64.to_float (Time.diff b.Store.Block_header.shell.timestamp time)

  let create net_db =
    let net_state = Distributed_db.state net_db in
    let tbl = Block_hash.Table.create 50 in
    let canceler = Lwt_utils.Canceler.create () in
    let worker_trigger, worker_waiter = Lwt_utils.trigger () in
    let session =
      { tbl ; net_db ; net_state ; worker = Lwt.return () ;
        canceler ; worker_trigger ; worker_waiter } in
    let worker =
      let rec worker_loop () =
        Lwt_utils.protect ~canceler begin fun () ->
          worker_waiter () >>= return
        end >>= function
        | Error [Lwt_utils.Canceled] -> Lwt.return_unit
        | Error err ->
            lwt_log_error
              "@[Unexpected error in validation:@ %a@]"
              pp_print_error err >>= fun () ->
            worker_loop ()
        | Ok () ->
            begin
              match pending_requests session with
              | [] -> ()
              | requests ->
                  let set_context _validator hash context =
                    store session hash context >>= fun _ ->
                    Lwt.return_unit in
                  let timeout =
                    request session
                      ~get_context:(fetch session)
                      ~set_context requests in
                  if timeout > 0. then
                    Lwt.ignore_result
                      (Lwt_unix.sleep timeout >|= worker_trigger);
            end ;
            worker_loop ()
      in
      Lwt_utils.worker "validation"
        ~run:worker_loop
        ~cancel:(fun () -> Lwt_utils.Canceler.cancel canceler) in
    { session with worker }

  let shutdown { canceler ; worker } =
    Lwt_utils.Canceler.cancel canceler >>= fun () -> worker

end


let rec create_validator ?parent worker state db net =

  let queue = Lwt_pipe.create () in
  let current_ops = ref (fun () -> []) in

  let callback : Distributed_db.callback = {
    notify_branch = begin fun gid locator ->
      Lwt.async (fun () -> Lwt_pipe.push queue (`Branch (gid, locator)))
    end ;
    current_branch = begin fun size ->
      State.Valid_block.Current.head net >>= fun head ->
      State.Valid_block.Helpers.block_locator net size head
    end ;
    notify_head =  begin fun gid block ops ->
      Lwt.async (fun () -> Lwt_pipe.push queue (`Head (gid, block, ops))) ;
    end ;
    current_head = begin fun size ->
      State.Valid_block.Current.head net >>= fun head ->
      Lwt.return (head.hash, Utils.list_sub (!current_ops ()) size)
    end ;
    disconnection = (fun _gid -> ()) ;
  } in

  let net_id = State.Net.id net in
  let net_db = Distributed_db.activate ~callback db net in
  let session = Context_db.create net_db in

  Prevalidator.create net_db >>= fun prevalidator ->
  current_ops :=
    (fun () ->
       let res, _ = Prevalidator.operations prevalidator in
       res.applied);
  let new_blocks = ref Lwt.return_unit in

  let shutdown () =
    lwt_log_notice "shutdown %a" State.Net_id.pp net_id >>= fun () ->
    Distributed_db.deactivate net_db >>= fun () ->
    Lwt_pipe.close queue ;
    Lwt.join [
      Context_db.shutdown session ;
      !new_blocks ;
      Prevalidator.shutdown prevalidator ;
    ]
  in

  let valid_block_input = Watcher.create_input () in
  let new_head_input = Watcher.create_input () in

  let bootstrapped =
    (* TODO improve by taking current peers count and current
       locators into account... *)
    let stream, stopper =
      Watcher.create_stream valid_block_input in
    let rec wait () =
      Lwt.pick [ ( Lwt_stream.get stream ) ;
                 ( Lwt_unix.sleep 30. >|= fun () -> None) ] >>= function
      | Some block
        when Time.(block.State.Valid_block.timestamp < add (Time.now ()) (-60L)) ->
          wait ()
      | _ ->
          State.Valid_block.Current.head net >>= fun head ->
          State.Valid_block.Current.genesis net >>= fun genesis ->
          if Block_hash.equal head.hash genesis.hash then
            wait ()
          else
            Lwt.return_unit in
    let t =
      wait () >>= fun () ->
      Watcher.shutdown stopper ;
      Lwt.return_unit in
    Lwt.no_cancel t
  in

  let rec v = {
    net ;
    worker ;
    parent ;
    child = None ;
    prevalidator ;
    net_db ;
    shutdown ;
    notify_block ;
    fetch_block ;
    create_child ;
    test_validator ;
    bootstrapped ;
    new_head_input ;
    valid_block_input ;
  }

  and notify_block hash block =
    lwt_debug "-> Validator.notify_block %a"
      Block_hash.pp_short hash >>= fun () ->
    State.Valid_block.Current.head net >>= fun head ->
    if Fitness.compare head.fitness block.shell.fitness <= 0 then
      Context_db.prefetch v session hash ;
    Lwt.return_unit

  and fetch_block hash =
    Context_db.fetch session v hash

  and create_child block =
    begin
      match v.child with
      | None -> Lwt.return_unit
      | Some child ->
          v.child <- None ;
          deactivate child
    end >>= fun () ->
    match block.test_network with
    | None -> return ()
    | Some (net_id, expiration) ->
        begin
          State.Net.get state net_id >>= function
          | Ok net_store -> return net_store
          | Error _ ->
              State.Valid_block.fork_testnet
                state net block expiration >>=? fun net_store ->
              State.Valid_block.Current.head net_store >>= fun block ->
              Watcher.notify v.worker.valid_block_input block ;
              return net_store
        end >>=? fun net_store ->
        worker.activate ~parent:v net_store >>= fun child ->
        v.child <- Some child ;
        return ()

  and test_validator () =
    match v.child with
    | None -> None
    | Some child -> Some (child, child.net_db)

  in

  new_blocks := begin
    let rec loop () =
      Lwt_pipe.pop queue >>= function
      | `Branch (_gid, locator) ->
          List.iter (Context_db.prefetch v session) locator ;
          loop ()
      | `Head (gid, head, ops) ->
          Context_db.prefetch v session head ;
          Prevalidator.notify_operations prevalidator gid ops ;
          loop ()
    in
    Lwt.catch loop
      (function Lwt_pipe.Closed -> Lwt.return_unit
              | exn -> Lwt.fail exn)
  end ;

  Lwt.return v

type error += Unknown_network of State.Net_id.t

let create_worker state db =

  let validators : t Lwt.t State.Net_id.Table.t =
    Store.Net_id.Table.create 7 in

  let valid_block_input = Watcher.create_input () in

  let get_exn net = State.Net_id.Table.find validators net in
  let get net =
    try get_exn net >>= fun v -> return v
    with Not_found -> fail (State.Unknown_network net) in
  let remove net = State.Net_id.Table.remove validators net in

  let deactivate { net } =
    let id = State.Net.id net in
    get id >>= function
    | Error _ -> Lwt.return_unit
    | Ok v ->
        lwt_log_notice "deactivate network %a" State.Net_id.pp id >>= fun () ->
        remove id ;
        v.shutdown ()
  in

  let notify_block hash (block : Store.Block_header.t) =
    match get_exn block.shell.net_id with
    | exception Not_found -> Lwt.return_unit
    | net ->
        net >>= fun net ->
        net.notify_block hash block in

  let cancelation, cancel, _on_cancel = Lwt_utils.canceler () in

  let maintenance_worker =
    let next_net_maintenance = ref (Time.now ()) in
    let net_maintenance () =
      lwt_log_info "net maintenance" >>= fun () ->
      let time = Time.now () in
      Store.Net_id.Table.fold
        (fun _ v acc ->
           v >>= fun v ->
           acc >>= fun () ->
           match State.Net.expiration v.net with
           | Some eol when Time.(eol <= time) -> deactivate v
           | Some _ | None -> Lwt.return_unit)
        validators Lwt.return_unit >>= fun () ->
      State.Net.all state >>= fun all_net ->
      Lwt_list.iter_p
        (fun net ->
           match State.Net.expiration net with
           | Some eol when Time.(eol <= time) ->
               lwt_log_notice "destroy network %a"
                 State.Net_id.pp (State.Net.id net) >>= fun () ->
               State.Net.destroy state net
           | Some _ | None -> Lwt.return_unit)
        all_net >>= fun () ->
      next_net_maintenance := Time.add (Time.now ()) (Int64.of_int 55) ;
      Lwt.return_unit in
    let next_head_maintenance = ref (Time.now ()) in
    let head_maintenance () =
      lwt_log_info "head maintenance" >>= fun () ->
      (* TODO *)
      next_head_maintenance := Time.add (Time.now ()) (Int64.of_int 55) ;
      Lwt.return_unit in
    let rec worker_loop () =
      let timeout =
        let next = min !next_head_maintenance !next_net_maintenance in
        let delay = Time.(diff next (now ())) in
        if delay <= 0L then
          Lwt.return_unit
        else
          Lwt_unix.sleep (Int64.to_float delay) in
      Lwt.pick [(timeout >|= fun () -> `Process);
                (cancelation () >|= fun () -> `Cancel)] >>= function
      | `Cancel -> Lwt.return_unit
      | `Process ->
          begin
            if !next_net_maintenance < Time.now () then
              net_maintenance ()
            else
              Lwt.return ()
          end >>= fun () ->
          begin
            if !next_head_maintenance < Time.now () then
              head_maintenance ()
            else
              Lwt.return ()
          end >>= fun () ->
          worker_loop ()
    in
    Lwt_utils.worker "validator_maintenance" ~run:worker_loop ~cancel in

  let shutdown () =
    cancel () >>= fun () ->
    let validators =
      Store.Net_id.Table.fold
        (fun _ (v: t Lwt.t) acc -> (v >>= fun v -> v.shutdown ()) :: acc)
        validators [] in
    Lwt.join (maintenance_worker :: validators) in

  let inject_block ?(force = false) bytes operations =
    Distributed_db.inject_block db bytes operations >>=? fun (hash, block) ->
    get block.shell.net_id >>=? fun net ->
(*
    Lwt_list.filter_map_s
      (fun bytes ->
         let hash = Operation_hash.hash_bytes [bytes] in
         match Data_encoding.
         Distributed_db.Operation.inject net.net_db hash bytes >>= function
         | false -> Lwt.return_none
         | true ->
             if List.exists
                 (List.exists (Operation_hash.equal hash))
                 operations then
               Lwt.return (Some hash)
             else
               Lwt.return_none)
      injected_operations >>= fun injected_operations ->
*)
    let validation =
      State.Valid_block.Current.head net.net >>= fun head ->
      if force
      || Fitness.compare head.fitness block.shell.fitness <= 0 then
        fetch_block net hash
      else
        failwith "Fitness is below the current one" in
    return (hash, validation) in

  let rec activate ?parent net =
    lwt_log_notice "activate network %a"
      State.Net_id.pp (State.Net.id net) >>= fun () ->
    State.Valid_block.Current.genesis net >>= fun genesis ->
    let net_id = State.Net_id.Id genesis.hash in
    get net_id >>= function
    | Error _ ->
        let v = create_validator ?parent worker state db net in
        Store.Net_id.Table.add validators net_id v ;
        v
    | Ok v -> Lwt.return v

  and worker = {
    get ; get_exn ;
    activate ; deactivate ;
    notify_block ;
    inject_block ;
    shutdown ;
    valid_block_input ;
    db ;
  }

  in

  worker

let new_head_watcher ({ new_head_input } : t) =
  Watcher.create_stream new_head_input

let watcher ({ valid_block_input } : t) =
  Watcher.create_stream valid_block_input

let global_watcher ({ valid_block_input } : worker) =
  Watcher.create_stream valid_block_input
