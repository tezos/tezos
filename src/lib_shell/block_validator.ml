(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Block_validator_worker_state
open Block_validator_errors
let invalid_block block error = Invalid_block { block ; error }

type limits = {
  protocol_timeout: float ;
  worker_limits : Worker_types.limits ;
}

module Name = struct
  type t = unit
  let encoding = Data_encoding.empty
  let base = [ "validator.block" ]
  let pp _ () = ()
end

module Types = struct
  include Worker_state
  type state = {
    protocol_validator: Protocol_validator.t ;
    limits : limits ;
  }
  type parameters = limits * Distributed_db.t
  let view _state _parameters = ()
end

module Request = struct
  include Request
  type 'a t =
    | Request_validation : {
        chain_db: Distributed_db.chain_db ;
        notify_new_block: State.Block.t -> unit ;
        canceler: Lwt_canceler.t option ;
        peer: P2p_peer.Id.t option ;
        hash: Block_hash.t ;
        header: Block_header.t ;
        operations: Operation.t list list ;
      } -> State.Block.t option tzresult t
  let view
    : type a. a t -> view
    = fun (Request_validation { chain_db ; peer ; hash }) ->
      let chain_id = chain_db |> Distributed_db.chain_state |> State.Chain.id in
      { chain_id ; block = hash ; peer = peer }
end

module Worker = Worker.Make (Name) (Event) (Request) (Types)

type t = Worker.infinite Worker.queue Worker.t
type error += Closed = Worker.Closed

let debug w =
  Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

let check_header
    (pred: State.Block.t) validation_passes hash (header: Block_header.t) =
  let pred_header = State.Block.header pred in
  fail_unless
    (Int32.succ pred_header.shell.level = header.shell.level)
    (invalid_block hash @@
     Invalid_level { expected = Int32.succ pred_header.shell.level ;
                     found = header.shell.level }) >>=? fun () ->
  fail_unless
    Time.(pred_header.shell.timestamp < header.shell.timestamp)
    (invalid_block hash Non_increasing_timestamp) >>=? fun () ->
  fail_unless
    Fitness.(pred_header.shell.fitness < header.shell.fitness)
    (invalid_block hash Non_increasing_fitness) >>=? fun () ->
  fail_unless
    (header.shell.validation_passes = validation_passes)
    (invalid_block hash
       (Unexpected_number_of_validation_passes header.shell.validation_passes)
    ) >>=? fun () ->
  return ()

let assert_no_duplicate_operations block live_operations operation_hashes =
  fold_left_s (fold_left_s (fun live_operations oph ->
      fail_when (Operation_hash.Set.mem oph live_operations)
        (invalid_block block @@ Replayed_operation oph) >>=? fun () ->
      return (Operation_hash.Set.add oph live_operations)))
    live_operations operation_hashes >>=? fun _ ->
  return ()

let assert_operation_liveness block live_blocks operations =
  iter_s (iter_s (fun op ->
      fail_unless
        (Block_hash.Set.mem op.Operation.shell.branch live_blocks)
        (invalid_block block @@
         Outdated_operation { operation = Operation.hash op ;
                              originating_block = op.shell.branch })))
    operations

let check_liveness chain_state pred hash operations_hashes operations =
  begin
    Chain.data chain_state >>= fun chain_data ->
    if State.Block.equal chain_data.current_head pred then
      Lwt.return (chain_data.live_blocks, chain_data.live_operations)
    else
      Chain_traversal.live_blocks
        pred (State.Block.max_operations_ttl pred)
  end >>= fun (live_blocks, live_operations) ->
  assert_no_duplicate_operations
    hash live_operations operations_hashes >>=? fun () ->
  assert_operation_liveness hash live_blocks operations >>=? fun () ->
  return ()

let apply_block
    chain_state
    pred (module Proto : Registered_protocol.T)
    hash (header: Block_header.t)
    operations =
  let pred_header = State.Block.header pred
  and pred_hash = State.Block.hash pred in
  check_header pred (List.length Proto.validation_passes) hash header >>=? fun () ->
  iteri2_p
    (fun i ops quota ->
       fail_unless
         (Option.unopt_map ~default:true
            ~f:(fun max -> List.length ops <= max) quota.Tezos_protocol_environment_shell.max_op)
         (let max = Option.unopt ~default:~-1 quota.max_op in
          invalid_block hash @@
          Too_many_operations
            { pass = i + 1 ; found = List.length ops ; max }) >>=? fun () ->
       let max_size = State.Block.max_operation_data_length pred in
       iter_p (fun op ->
           let size = Data_encoding.Binary.length Operation.encoding op in
           fail_unless
             (size <= max_size)
             (invalid_block hash @@
              Oversized_operation
                { operation = Operation.hash op ;
                  size ; max = max_size })) ops >>=? fun () ->
       return ())
    operations Proto.validation_passes >>=? fun () ->
  let operation_hashes = List.map (List.map Operation.hash) operations in
  check_liveness chain_state pred hash operation_hashes operations >>=? fun () ->
  begin
    match
      Data_encoding.Binary.of_bytes
        Proto.block_header_data_encoding
        header.protocol_data with
    | None ->
        fail (invalid_block hash Cannot_parse_block_header)
    | Some protocol_data ->
        return ({ shell = header.shell ; protocol_data } : Proto.block_header)
  end >>=? fun header ->
  mapi2_s (fun pass -> map2_s begin fun op_hash op ->
      match
        Data_encoding.Binary.of_bytes
          Proto.operation_data_encoding
          op.Operation.proto with
      | None ->
          fail (invalid_block hash (Cannot_parse_operation op_hash))
      | Some protocol_data ->
          let op = { Proto.shell = op.shell ; protocol_data } in
          let allowed_pass = Proto.acceptable_passes op in
          fail_unless (List.mem pass allowed_pass)
            (invalid_block hash
               (Unallowed_pass { operation = op_hash ;
                                 pass ; allowed_pass } )) >>=? fun () ->
          return op
    end)
    operation_hashes
    operations >>=? fun parsed_operations ->
  State.Block.context pred >>= fun pred_context ->
  Context.reset_test_chain
    pred_context pred_hash header.shell.timestamp >>= fun context ->
  (* TODO wrap 'proto_error' into 'block_error' *)
  Proto.begin_application
    ~predecessor_context:context
    ~predecessor_timestamp:pred_header.shell.timestamp
    ~predecessor_fitness:pred_header.shell.fitness
    header >>=? fun state ->
  fold_left_s
    (fun (state, acc) ops ->
       fold_left_s
         (fun (state, acc) op ->
            Proto.apply_operation state op >>=? fun (state, op_metadata) ->
            return (state, op_metadata :: acc))
         (state, []) ops >>=? fun (state, ops_metadata) ->
       return (state, List.rev ops_metadata :: acc))
    (state, []) parsed_operations >>=? fun (state, ops_metadata) ->
  let ops_metadata = List.rev ops_metadata in
  Proto.finalize_block state >>=? fun (validation_result, block_data) ->
  Context.get_protocol validation_result.context >>= fun new_protocol ->
  let expected_proto_level =
    if Protocol_hash.equal new_protocol Proto.hash then
      pred_header.shell.proto_level
    else
      (pred_header.shell.proto_level + 1) mod 256 in
  fail_when (header.shell.proto_level <> expected_proto_level)
    (invalid_block hash @@  Invalid_proto_level {
        found = header.shell.proto_level ;
        expected = expected_proto_level ;
      }) >>=? fun () ->
  fail_when
    Fitness.(validation_result.fitness <> header.shell.fitness)
    (invalid_block hash @@ Invalid_fitness {
        expected = header.shell.fitness ;
        found = validation_result.fitness ;
      }) >>=? fun () ->
  begin
    if Protocol_hash.equal new_protocol Proto.hash then
      return validation_result
    else
      match Registered_protocol.get new_protocol with
      | None ->
          fail (Unavailable_protocol { block = hash ;
                                       protocol = new_protocol })
      | Some (module NewProto) ->
          NewProto.init validation_result.context header.shell
  end >>=? fun validation_result ->
  let max_operations_ttl =
    max 0
      (min
         ((State.Block.max_operations_ttl pred)+1)
         validation_result.max_operations_ttl) in
  let validation_result =
    { validation_result with max_operations_ttl } in
  let block_data =
    Data_encoding.Binary.to_bytes_exn Proto.block_header_metadata_encoding block_data in
  let ops_metadata =
    List.map
      (List.map
         (Data_encoding.Binary.to_bytes_exn
            Proto.operation_receipt_encoding))
      ops_metadata in
  return (validation_result, block_data, ops_metadata)

let check_chain_liveness chain_db hash (header: Block_header.t) =
  let chain_state = Distributed_db.chain_state chain_db in
  match State.Chain.expiration chain_state with
  | Some eol when Time.(eol <= header.shell.timestamp) ->
      fail @@ invalid_block hash @@
      Expired_chain { chain_id = State.Chain.id chain_state ;
                      expiration = eol ;
                      timestamp = header.shell.timestamp }
  | None | Some _ -> return ()

let get_proto pred hash =
  State.Block.context pred >>= fun pred_context ->
  Context.get_protocol pred_context >>= fun pred_protocol_hash ->
  match Registered_protocol.get pred_protocol_hash with
  | None ->
      fail (Unavailable_protocol { block = hash ;
                                   protocol = pred_protocol_hash })
  | Some p -> return p

let on_request
  : type r. t -> r Request.t -> r tzresult Lwt.t
  = fun w
    (Request.Request_validation
       { chain_db ; notify_new_block ; canceler ;
         peer ; hash ; header ; operations }) ->
    let bv = Worker.state w in
    let chain_state = Distributed_db.chain_state chain_db in
    State.Block.read_opt chain_state hash >>= function
    | Some block ->
        debug w "previously validated block %a (after pipe)"
          Block_hash.pp_short hash ;
        Protocol_validator.prefetch_and_compile_protocols
          bv.protocol_validator
          ?peer ~timeout:bv.limits.protocol_timeout
          block ;
        return (Ok None)
    | None ->
        State.Block.read_invalid chain_state hash >>= function
        | Some { errors } ->
            return (Error errors)
        | None ->
            begin
              debug w "validating block %a" Block_hash.pp_short hash ;
              State.Block.read
                chain_state header.shell.predecessor >>=? fun pred ->
              get_proto pred hash >>=? fun proto ->
              (* TODO also protect with [Worker.canceler w]. *)
              protect ?canceler begin fun () ->
                apply_block
                  (Distributed_db.chain_state chain_db)
                  pred proto hash
                  header operations >>=? fun (result, header_data, operations_data) ->
                Distributed_db.commit_block
                  chain_db hash
                  header header_data operations operations_data
                  result >>=? function
                | None -> assert false (* should not happen *)
                | Some block -> return block
              end
            end >>= function
            | Ok block ->
                Protocol_validator.prefetch_and_compile_protocols
                  bv.protocol_validator
                  ?peer ~timeout:bv.limits.protocol_timeout
                  block ;
                notify_new_block block ;
                return (Ok (Some block))
            (* TODO catch other temporary error (e.g. system errors)
               and do not 'commit' them on disk... *)
            | Error [Canceled | Unavailable_protocol _] as err ->
                return err
            | Error errors ->
                Worker.protect w begin fun () ->
                  Distributed_db.commit_invalid_block
                    chain_db hash header errors
                end >>=? fun commited ->
                assert commited ;
                return (Error errors)

let on_launch _ _ (limits, db) =
  let protocol_validator = Protocol_validator.create db in
  Lwt.return { Types.protocol_validator ; limits }

let on_error w r st errs =
  Worker.record_event w (Validation_failure (r, st, errs)) ;
  Lwt.return (Error errs)

let on_completion
  : type a. t -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
  = fun w (Request.Request_validation _ as r) v st ->
    match v with
    | Ok (Some _) ->
        Worker.record_event w
          (Event.Validation_success (Request.view r, st)) ;
        Lwt.return ()
    | Ok None ->
        Lwt.return ()
    | Error errs ->
        Worker.record_event w
          (Event.Validation_failure (Request.view r, st, errs)) ;
        Lwt.return ()

let table = Worker.create_table Queue

let create limits db =
  let module Handlers = struct
    type self = t
    let on_launch = on_launch
    let on_request = on_request
    let on_close _ = Lwt.return ()
    let on_error = on_error
    let on_completion = on_completion
    let on_no_request _ = return ()
  end in
  Worker.launch
    table
    limits.worker_limits
    ()
    (limits, db)
    (module Handlers)

let shutdown = Worker.shutdown

let validate w
    ?canceler ?peer ?(notify_new_block = fun _ -> ())
    chain_db hash (header : Block_header.t) operations =
  let bv = Worker.state w in
  let chain_state = Distributed_db.chain_state chain_db in
  State.Block.read_opt chain_state hash >>= function
  | Some block ->
      debug w "previously validated block %a (before pipe)"
        Block_hash.pp_short hash ;
      Protocol_validator.prefetch_and_compile_protocols
        bv.protocol_validator
        ?peer ~timeout:bv.limits.protocol_timeout
        block ;
      return None
  | None ->
      map_p (map_p (fun op ->
          let op_hash = Operation.hash op in
          return op_hash))
        operations >>=? fun hashes ->
      let computed_hash =
        Operation_list_list_hash.compute
          (List.map Operation_list_hash.compute hashes) in
      fail_when
        (Operation_list_list_hash.compare
           computed_hash header.shell.operations_hash <> 0)
        (Inconsistent_operations_hash {
            block = hash ;
            expected = header.shell.operations_hash ;
            found = computed_hash ;
          }) >>=? fun () ->
      check_chain_liveness chain_db hash header >>=? fun () ->
      Worker.push_request_and_wait w
        (Request_validation
           { chain_db ; notify_new_block ; canceler ;
             peer ; hash ; header ; operations }) >>=? fun result ->
      Lwt.return result

let fetch_and_compile_protocol w =
  let bv = Worker.state w in
  Protocol_validator.fetch_and_compile_protocol bv.protocol_validator

let status = Worker.status

let running_worker () =
  match Worker.list table with
  | (_, single) :: _ -> single
  | [] -> raise Not_found

let pending_requests t = Worker.pending_requests t

let current_request t = Worker.current_request t

let last_events = Worker.last_events
