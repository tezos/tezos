(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module P2p = Netparams

open Logging.Node.Validator

type worker = {
  p2p: P2p.net ;
  activate: ?parent:t -> State.Net.t -> t Lwt.t ;
  get: State.net_id -> t tzresult Lwt.t ;
  get_exn: State.net_id -> t Lwt.t ;
  deactivate: t -> unit Lwt.t ;
  notify_block: Block_hash.t -> Store.block -> unit Lwt.t ;
  shutdown: unit -> unit Lwt.t ;
}

and t = {
  net: State.Net.t ;
  worker: worker ;
  parent: t option ;
  mutable child: t option ;
  prevalidator: Prevalidator.t ;
  notify_block: Block_hash.t -> Store.block -> unit Lwt.t ;
  fetch_block: Block_hash.t -> State.Valid_block.t tzresult Lwt.t ;
  create_child: State.Valid_block.t -> unit tzresult Lwt.t ;
  test_validator: unit -> (t * State.Net.t) option ;
  shutdown: unit -> unit Lwt.t ;
}

let activate w net = w.activate net
let deactivate t = t.worker.deactivate t
let get w = w.get
let get_exn w = w.get_exn
let notify_block w = w.notify_block
let shutdown w = w.shutdown ()
let test_validator w = w.test_validator ()

let fetch_block v = v.fetch_block
let prevalidator v = v.prevalidator

let broadcast w m = P2p.broadcast w.p2p m

(** Current block computation *)

let may_change_test_network v (block: State.Valid_block.t) =
  let change =
    match block.test_network, v.child with
    | None, None -> false
    | Some _, None
    | None, Some _ -> true
    | Some (Net net_id, _), Some { net } ->
        let Store.Net net_id' = State.Net.id net in
        not (Block_hash.equal net_id net_id') in
  if change then begin
    v.create_child block >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
        lwt_log_error "@[<v 2>Error while switch test network:@ %a@]"
          Error_monad.pp_print_error err
  end else
    Lwt.return_unit

let rec may_set_head v (block: State.Valid_block.t) =
  State.Net.Blockchain.head v.net >>= fun head ->
  if Fitness.compare head.fitness block.fitness >= 0 then
    Lwt.return_unit
  else
    State.Net.Blockchain.test_and_set_head v.net ~old:head block >>= function
    | false -> may_set_head v block
    | true ->
        broadcast v.worker Messages.(Block_inventory (State.Net.id v.net, [])) ;
        Prevalidator.flush v.prevalidator ;
        may_change_test_network v block >>= fun () ->
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


(** Block validation *)

type error += Invalid_operation of Operation_hash.t

let apply_block net (pred: State.Valid_block.t) hash (block: State.Block.t) =
  let state = State.Net.state net in
  let State.Net id = State.Net.id net in
  lwt_log_notice "validate block %a (after %a), net %a"
    Block_hash.pp_short hash
    Block_hash.pp_short block.shell.predecessor
    Block_hash.pp_short id
  >>= fun () ->
  lwt_log_info "validation of %a: looking for dependencies..."
    Block_hash.pp_short hash >>= fun () ->
  map_p
    (fun op ->
       State.Operation.fetch state (State.Net.id net) op >>= function
       | { data = Error _ as e} -> Lwt.return e
       | { data = Ok data } -> Lwt.return (Ok data))
    block.shell.operations >>=? fun operations ->
  lwt_debug "validation of %a: found operations"
    Block_hash.pp_short hash >>= fun () ->
  begin (* Are we validating a block in an expired test network ? *)
    match State.Net.expiration net with
    | Some eol when Time.(eol <= block.shell.timestamp) ->
        failwith "This test network expired..."
    | None | Some _ -> return ()
  end >>=? fun () ->
  begin
    match pred.protocol with
    | None -> fail (State.Unknown_protocol pred.protocol_hash)
    | Some p -> return (p, pred.context)
  end >>=? fun ((module Proto), patched_context) ->
  lwt_debug "validation of %a: Proto %a"
    Block_hash.pp_short hash
    Protocol_hash.pp_short Proto.hash >>= fun () ->
  lwt_debug "validation of %a: parsing header..."
    Block_hash.pp_short hash >>= fun () ->
  Lwt.return (Proto.parse_block block) >>=? fun parsed_header ->
  lwt_debug "validation of %a: parsing operations..."
    Block_hash.pp_short hash >>= fun () ->
  map2_s
    (fun op_hash raw ->
       Lwt.return (Proto.parse_operation op_hash raw)
       |> trace (Invalid_operation op_hash))
    block.Store.shell.operations operations >>=? fun parsed_operations ->
  lwt_debug "validation of %a: applying block..."
    Block_hash.pp_short hash >>= fun () ->
  Proto.apply
    patched_context parsed_header parsed_operations >>=? fun new_context ->
  lwt_log_info "validation of %a: success"
    Block_hash.pp_short hash >>= fun () ->
  return new_context

(** *)

module Validation_scheduler = struct
  let name = "validator"
  type state = State.Net.t * Block_hash_set.t ref
  type rdata = t
  type data = Store.block Time.timed_data
  let init_request (net, _) hash =
    State.Block.fetch (State.Net.state net) (State.Net.id net) hash

  let process
      net v ~get:get_context ~set:set_context hash block =
    match block with
    | { Time.data = block } ->
        get_context block.Store.shell.predecessor >>= function
        | Error _ ->
            set_context hash (Error [(* TODO *)])
        | Ok _context ->
            lwt_debug "process %a" Block_hash.pp_short hash >>= fun () ->
            begin
              State.Net.Blockchain.genesis net >>= fun genesis ->
              if Block_hash.equal genesis.hash block.shell.predecessor then
                Lwt.return genesis
              else
                State.Valid_block.read_exn
                  (State.Net.state net) block.shell.predecessor
            end >>= fun pred ->
            apply_block net pred hash block >>= function
            | Error ([State.Unknown_protocol _] as err) ->
                lwt_log_error
                  "@[<v 2>Ignoring block %a@ %a@]"
                  Block_hash.pp_short hash
                  Error_monad.pp_print_error err
            | Error exns as error ->
                set_context hash error >>= fun () ->
                lwt_warn "Failed to validate block %a."
                  Block_hash.pp_short hash >>= fun () ->
                lwt_debug "%a" Error_monad.pp_print_error exns
            | Ok new_context ->
                (* The sanity check `set_context` detects differences
                   between the computed fitness and the fitness announced
                   in the block header. When distinct `Valid_block.read`
                   will return an error. *)
                set_context hash (Ok new_context) >>= fun () ->
                State.Valid_block.read
                  (State.Net.state net) hash >>= function
                | None ->
                    lwt_log_error
                      "Unexpected error while saving context for block %a."
                      Block_hash.pp_short hash
                | Some (Error err) ->
                    lwt_log_error
                      "@[<v 2>Ignoring block %a@ %a@]"
                      Block_hash.pp_short hash
                      Error_monad.pp_print_error err
                | Some (Ok block) ->
                    lwt_debug
                      "validation of %a: reevaluate current block"
                      Block_hash.pp_short hash >>= fun () ->
                    may_set_head v block

    let request (net, running) ~get ~set pendings =
      let time = Time.now () in
      let min_block b pb =
        match pb with
        | None -> Some b
        | Some pb when b.Store.shell.timestamp < pb.Store.shell.timestamp -> Some b
        | Some _ as pb -> pb in
      let next =
        List.fold_left
          (fun acc (hash, block, v) ->
             match block with
             | { Time.data = block }
               when Time.(block.Store.shell.timestamp > time) ->
                 min_block block acc
             | { Time.data = _ } as block ->
                 if not (Block_hash_set.mem hash !running) then begin
                   running := Block_hash_set.add hash !running ;
                   Lwt.async (fun () ->
                       process net v
                         ~get:(get v) ~set:set hash block >>= fun () ->
                       running := Block_hash_set.remove hash !running ;
                       Lwt.return_unit
                     )
                 end ;
                 acc)
          None
          pendings in
      match next with
      | None -> 0.
      | Some b -> Int64.to_float (Time.diff b.Store.shell.timestamp time)

end

module Context_db =
  Persist.MakeImperativeProxy
    (State.Valid_block.Store)(Block_hash_table)(Validation_scheduler)

let rec create_validator ?parent worker net =

  Prevalidator.create worker.p2p net >>= fun prevalidator ->
  let state = State.Net.state net in
  let proxy =
    Context_db.create
      (net, ref Block_hash_set.empty)
      (State.Valid_block.get_store state) in
  State.Net.activate net ;

  let shutdown () =
    lwt_log_notice "shutdown %a"
      Store.pp_net_id (State.Net.id net) >>= fun () ->
    State.Net.deactivate net ;
    Lwt.join [
      Context_db.shutdown proxy ;
      Prevalidator.shutdown prevalidator ;
    ]
  in

  let rec v = {
    net ;
    worker ;
    parent ;
    child = None ;
    prevalidator ;
    shutdown ;
    notify_block ;
    fetch_block ;
    create_child ;
    test_validator ;
  }

  and notify_block hash block =
    lwt_debug "-> Validator.notify_block %a"
      Block_hash.pp_short hash >>= fun () ->
    State.Net.Blockchain.head net >>= fun head ->
    if Fitness.compare head.fitness block.shell.fitness <= 0 then
      Context_db.prefetch proxy v hash;
    Lwt.return_unit

  and fetch_block hash =
    Context_db.fetch proxy v hash >>=? fun _context ->
    State.Valid_block.read_exn (State.Net.state net) hash >>= fun block ->
    return block

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
    | Some (Net block as net_id, expiration) ->
        begin
          match State.Net.get state net_id with
          | Ok net_store -> return net_store
          | Error _ ->
              State.Valid_block.read_exn state block >>= fun block ->
              let genesis = {
                Store.block = block.hash ;
                time = block.timestamp ;
                protocol = block.test_protocol_hash ;
              } in
              State.Net.create state ~expiration genesis
        end >>=? fun net_store ->
        worker.activate ~parent:v net_store >>= fun child ->
        v.child <- Some child ;
        return ()

  and test_validator () =
    match v.child with
    | None -> None
    | Some child -> Some (child, child.net)

  in

  Lwt.return v

type error += Unknown_network of State.net_id

let create_worker p2p state =

  let validators : t Lwt.t Block_hash_table.t = Block_hash_table.create 7 in

  let get_exn (State.Net net) = Block_hash_table.find validators net in
  let get net =
    try get_exn net >>= fun v -> return v
    with Not_found -> fail (State.Unknown_network net) in
  let remove (State.Net net) = Block_hash_table.remove validators net in

  let deactivate { net } =
    let id = State.Net.id net in
    get id >>= function
    | Error _ -> Lwt.return_unit
    | Ok v ->
        lwt_log_notice "deactivate network %a" Store.pp_net_id id >>= fun () ->
        remove id ;
        v.shutdown ()
  in

  let notify_block hash (block : Store.block) =
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
      Block_hash_table.fold
        (fun _ v acc ->
           v >>= fun v ->
           acc >>= fun () ->
           match State.Net.expiration v.net with
           | Some eol when Time.(eol <= time) -> deactivate v
           | Some _ | None -> Lwt.return_unit)
        validators Lwt.return_unit >>= fun () ->
      Lwt_list.iter_p
        (fun net ->
           match State.Net.expiration net with
           | Some eol when Time.(eol <= time) ->
               lwt_log_notice "destroy network %a"
                 Store.pp_net_id (State.Net.id net) >>= fun () ->
               State.Net.destroy net
           | Some _ | None -> Lwt.return_unit)
        (State.Net.all state) >>= fun () ->
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
      Block_hash_table.fold
        (fun _ (v: t Lwt.t) acc -> (v >>= fun v -> v.shutdown ()) :: acc)
        validators [] in
    Lwt.join (maintenance_worker :: validators) in

  let rec activate ?parent net =
    lwt_log_notice "activate network %a"
      Store.pp_net_id (State.Net.id net) >>= fun () ->
    State.Net.Blockchain.genesis net >>= fun genesis ->
    get (Net genesis.hash) >>= function
    | Error _ ->
        let v = create_validator ?parent worker net in
        Block_hash_table.add validators genesis.hash v ;
        v
    | Ok v -> Lwt.return v

  and worker = {
    p2p ;
    get ; get_exn ;
    activate ; deactivate ;
    notify_block ;
    shutdown ;
  }

  in

  worker

