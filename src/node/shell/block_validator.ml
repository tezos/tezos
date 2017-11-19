(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "node.validator.block" end)
module Canceler = Lwt_utils.Canceler

type 'a request =
  | Request_validation: {
      net_db: Distributed_db.net_db ;
      notify_new_block: State.Block.t -> unit ;
      canceler: Canceler.t option ;
      peer: P2p.Peer_id.t option ;
      hash: Block_hash.t ;
      header: Block_header.t ;
      operations: Operation.t list list ;
    } -> State.Block.t tzresult request

type message = Message: 'a request * 'a Lwt.u option -> message

type t = {
  protocol_validator: Protocol_validator.t ;
  protocol_timeout: float ;
  mutable worker: unit Lwt.t ;
  messages: message Lwt_pipe.t ;
  canceler: Canceler.t ;
}

(** Block validation *)

type block_error =
  | Cannot_parse_operation of Operation_hash.t
  | Invalid_fitness of { expected: Fitness.t ; found: Fitness.t }
  | Non_increasing_timestamp
  | Non_increasing_fitness
  | Invalid_level of { expected: Int32.t ; found: Int32.t }
  | Invalid_proto_level of { expected: int ; found: int }
  | Replayed_operation of Operation_hash.t
  | Outdated_operation of
      { operation: Operation_hash.t;
        originating_block: Block_hash.t }
  | Expired_network of
      { net_id: Net_id.t ;
        expiration: Time.t ;
        timestamp: Time.t ;
      }
  | Unexpected_number_of_validation_passes of int (* uint8 *)
  | Too_many_operations of { pass: int; found: int; max: int }
  | Oversized_operation of { operation: Operation_hash.t;
                             size: int; max: int }

let block_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (obj2
           (req "error" (constant "cannot_parse_operation"))
           (req "operation" Operation_hash.encoding))
        (function Cannot_parse_operation operation -> Some ((), operation)
                | _ -> None)
        (fun ((), operation) -> Cannot_parse_operation operation) ;
      case
        (obj3
           (req "error" (constant "invalid_fitness"))
           (req "expected" Fitness.encoding)
           (req "found" Fitness.encoding))
        (function
          | Invalid_fitness { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) -> Invalid_fitness { expected ; found }) ;
      case
        (obj1
           (req "error" (constant "non_increasing_timestamp")))
        (function Non_increasing_timestamp -> Some ()
                | _ -> None)
        (fun () -> Non_increasing_timestamp) ;
      case
        (obj1
           (req "error" (constant "non_increasing_fitness")))
        (function Non_increasing_fitness -> Some ()
                | _ -> None)
        (fun () -> Non_increasing_fitness) ;
      case
        (obj3
           (req "error" (constant "invalid_level"))
           (req "expected" int32)
           (req "found" int32))
        (function
          | Invalid_level { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) -> Invalid_level { expected ; found }) ;
      case
        (obj3
           (req "error" (constant "invalid_proto_level"))
           (req "expected" uint8)
           (req "found" uint8))
        (function
          | Invalid_proto_level { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) ->
           Invalid_proto_level { expected ; found }) ;
      case
        (obj2
           (req "error" (constant "replayed_operation"))
           (req "operation" Operation_hash.encoding))
        (function Replayed_operation operation -> Some ((), operation)
                | _ -> None)
        (fun ((), operation) -> Replayed_operation operation) ;
      case
        (obj3
           (req "error" (constant "outdated_operation"))
           (req "operation" Operation_hash.encoding)
           (req "originating_block" Block_hash.encoding))
        (function
          | Outdated_operation { operation ; originating_block } ->
              Some ((), operation, originating_block)
          | _ -> None)
        (fun ((), operation, originating_block) ->
           Outdated_operation { operation ; originating_block }) ;
      case
        (obj2
           (req "error" (constant "unexpected_number_of_passes"))
           (req "found" uint8))
        (function
          | Unexpected_number_of_validation_passes n -> Some ((), n)
          | _ -> None)
        (fun ((), n) -> Unexpected_number_of_validation_passes n) ;
      case
        (obj4
           (req "error" (constant "too_many_operations"))
           (req "validation_pass" uint8)
           (req "found" uint16)
           (req "max" uint16))
        (function
          | Too_many_operations { pass ; found ; max } ->
              Some ((), pass, found, max)
          | _ -> None)
        (fun ((), pass, found, max) ->
           Too_many_operations { pass ; found ; max }) ;
      case
        (obj4
           (req "error" (constant "oversized_operation"))
           (req "operation" Operation_hash.encoding)
           (req "found" int31)
           (req "max" int31))
        (function
          | Oversized_operation { operation ; size ; max } ->
              Some ((), operation, size, max)
          | _ -> None)
        (fun ((), operation, size, max) ->
           Oversized_operation { operation ; size ; max }) ;
    ]

let pp_block_error ppf = function
  | Cannot_parse_operation oph ->
      Format.fprintf ppf
        "Failed to parse the operation %a."
        Operation_hash.pp_short oph
  | Invalid_fitness { expected ; found } ->
      Format.fprintf ppf
        "@[<v 2>Invalid fitness:@ \
        \ expected %a@ \
        \ found %a@]"
        Fitness.pp expected
        Fitness.pp found
  | Non_increasing_timestamp ->
      Format.fprintf ppf "Non increasing timestamp"
  | Non_increasing_fitness ->
      Format.fprintf ppf "Non increasing fitness"
  | Invalid_level { expected ; found } ->
      Format.fprintf ppf
        "Invalid level:@ \
        \ expected %ld@ \
        \ found %ld"
        expected
        found
  | Invalid_proto_level { expected ; found } ->
      Format.fprintf ppf
        "Invalid protocol level:@ \
        \ expected %d@ \
        \ found %d"
        expected
        found
  | Replayed_operation oph ->
      Format.fprintf ppf
        "The operation %a was previously included in the chain."
        Operation_hash.pp_short oph
  | Outdated_operation { operation ; originating_block } ->
      Format.fprintf ppf
        "The operation %a is outdated (originated in block: %a)"
        Operation_hash.pp_short operation
        Block_hash.pp_short originating_block
  | Expired_network { net_id ; expiration ; timestamp } ->
      Format.fprintf ppf
        "The block timestamp (%a) is later than \
         its network expiration date: %a (net: %a)."
        Time.pp_hum timestamp
        Time.pp_hum expiration
        Net_id.pp_short net_id
  | Unexpected_number_of_validation_passes n ->
      Format.fprintf ppf
        "Invalid number of validation passes (found: %d)"
        n
  | Too_many_operations { pass ; found ; max } ->
      Format.fprintf ppf
        "Too many operations in validation pass %d (found: %d, max: %d)"
        pass found max
  | Oversized_operation { operation ; size ; max } ->
      Format.fprintf ppf
        "Oversized operation %a (size: %d, max: %d)"
        Operation_hash.pp_short operation size max

type error +=
  | Invalid_block of
      { block: Block_hash.t ; error: block_error }
  | Unavailable_protocol of
      { block: Block_hash.t ; protocol: Protocol_hash.t }
  | Inconsistent_operations_hash of
      { block: Block_hash.t ;
        expected: Operation_list_list_hash.t ;
        found: Operation_list_list_hash.t }

let invalid_block block error = Invalid_block { block ; error }

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"validator.invalid_block"
    ~title:"Invalid block"
    ~description:"Invalid block."
    ~pp:begin fun ppf (block, error) ->
      Format.fprintf ppf
        "@[<v 2>Invalid block %a@ %a@]"
        Block_hash.pp_short block pp_block_error error
    end
    Data_encoding.(merge_objs
                     (obj1 (req "invalid_block" Block_hash.encoding))
                     block_error_encoding)
    (function Invalid_block { block ; error } ->
       Some (block, error) | _ -> None)
    (fun (block, error) ->
       Invalid_block { block ; error }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"validator.unavailable_protocol"
    ~title:"Missing protocol"
    ~description:"The protocol required for validating a block is missing."
    ~pp:begin fun ppf (block, protocol) ->
      Format.fprintf ppf
        "Missing protocol (%a) when validating the block %a."
        Protocol_hash.pp_short protocol
        Block_hash.pp_short block
    end
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (req "missing_protocol" Protocol_hash.encoding))
    (function
      | Unavailable_protocol { block ; protocol } ->
          Some (block, protocol)
      | _ -> None)
    (fun (block, protocol) -> Unavailable_protocol { block ; protocol }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"validator.inconsistent_operations_hash"
    ~title:"Invalid merkle tree"
    ~description:"The provided list of operations is inconsistent with \
                  the block header."
    ~pp:begin fun ppf (block, expected, found) ->
      Format.fprintf ppf
        "@[<v 2>The provided list of operations for block %a \
        \ is inconsistent with the block header@ \
        \ expected: %a@ \
        \ found: %a@]"
        Block_hash.pp_short block
        Operation_list_list_hash.pp_short expected
        Operation_list_list_hash.pp_short found
    end
    Data_encoding.(
      obj3
        (req "block" Block_hash.encoding)
        (req "expected" Operation_list_list_hash.encoding)
        (req "found" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operations_hash { block ; expected ; found } ->
          Some (block, expected, found)
      | _ -> None)
    (fun (block, expected, found) ->
       Inconsistent_operations_hash { block ; expected ; found })

let check_header
    (pred: State.Block.t) hash (header: Block_header.t) =
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
    (header.shell.validation_passes =
     List.length (State.Block.max_number_of_operations pred))
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

let check_liveness net_state pred hash operations_hashes operations =
  begin
    Chain.data net_state >>= fun chain_data ->
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
    net_state
    pred (module Proto : State.Registred_protocol.T)
    hash (header: Block_header.t)
    operations =
  let pred_header = State.Block.header pred
  and pred_hash = State.Block.hash pred in
  check_header pred hash header >>=? fun () ->
  iteri2_p
    (fun i ops max ->
       fail_unless
         (List.length ops <= max)
         (invalid_block hash @@
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
    operations (State.Block.max_number_of_operations pred) >>=? fun () ->
  let operation_hashes = List.map (List.map Operation.hash) operations in
  check_liveness net_state pred hash operation_hashes operations >>=? fun () ->
  map2_s (map2_s begin fun op_hash raw ->
      Lwt.return (Proto.parse_operation op_hash raw)
      |> trace (invalid_block hash (Cannot_parse_operation op_hash))
    end)
    operation_hashes
    operations >>=? fun parsed_operations ->
  State.Block.context pred >>= fun pred_context ->
  Context.reset_test_network
    pred_context pred_hash header.shell.timestamp >>= fun context ->
  (* TODO wrap 'proto_error' into 'block_error' *)
  Proto.begin_application
    ~predecessor_context:context
    ~predecessor_timestamp:pred_header.shell.timestamp
    ~predecessor_fitness:pred_header.shell.fitness
    header >>=? fun state ->
  fold_left_s (fold_left_s (fun state op ->
      Proto.apply_operation state op >>=? fun state ->
      return state))
    state parsed_operations >>=? fun state ->
  Proto.finalize_block state >>=? fun new_context ->
  Context.get_protocol new_context.context >>= fun new_protocol ->
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
    Fitness.(new_context.fitness <> header.shell.fitness)
    (invalid_block hash @@ Invalid_fitness {
        expected = header.shell.fitness ;
        found = new_context.fitness ;
      }) >>=? fun () ->
  let max_operations_ttl =
    max 0
      (min
         ((State.Block.max_operations_ttl pred)+1)
         new_context.max_operations_ttl) in
  let new_context =
    { new_context with max_operations_ttl } in
  return new_context

let check_net_liveness net_db hash (header: Block_header.t) =
  let net_state = Distributed_db.net_state net_db in
  match State.Net.expiration net_state with
  | Some eol when Time.(eol <= header.shell.timestamp) ->
      fail @@ invalid_block hash @@
      Expired_network { net_id = State.Net.id net_state ;
                        expiration = eol ;
                        timestamp = header.shell.timestamp }
  | None | Some _ -> return ()

let get_proto pred hash =
  State.Block.context pred >>= fun pred_context ->
  Context.get_protocol pred_context >>= fun pred_protocol_hash ->
  match State.Registred_protocol.get pred_protocol_hash with
  | None ->
      fail (Unavailable_protocol { block = hash ;
                                   protocol = pred_protocol_hash })
  | Some p -> return p

let rec worker_loop bv =
  begin
    Lwt_utils.protect ~canceler:bv.canceler begin fun () ->
      Lwt_pipe.pop bv.messages >>= return
    end >>=? fun (Message (request, wakener)) ->
    let may_wakeup =
      match wakener with
      | None -> (fun _ -> ())
      | Some wakener -> (fun v -> Lwt.wakeup_later wakener v)
    in
    match request with
    | Request_validation { net_db ; notify_new_block ; canceler ;
                           peer ; hash ; header ; operations } ->
        let net_state = Distributed_db.net_state net_db in
        State.Block.read_opt net_state hash >>= function
        | Some block ->
            lwt_debug "previously validated block %a (after pipe)"
              Block_hash.pp_short hash >>= fun () ->
            Protocol_validator.prefetch_and_compile_protocols
              bv.protocol_validator
              ?peer ~timeout:bv.protocol_timeout
              block ;
            may_wakeup (Ok block) ;
            return ()
        | None ->
            begin
              lwt_debug "validating block %a"
                Block_hash.pp_short hash >>= fun () ->
              State.Block.read
                net_state header.shell.predecessor >>=? fun pred ->
              get_proto pred hash >>=? fun proto ->
              (* TODO also protect with [bv.canceler]. *)
              Lwt_utils.protect ?canceler begin fun () ->
                apply_block
                  (Distributed_db.net_state net_db)
                  pred proto hash header operations
              end
            end >>= function
            | Ok result -> begin
                lwt_log_info "validated block %a"
                  Block_hash.pp_short hash >>= fun () ->
                Lwt_utils.protect ~canceler:bv.canceler begin fun () ->
                  Distributed_db.commit_block
                    net_db hash header operations result
                end >>=? function
                | None ->
                    assert false (* should not happen *)
                | Some block ->
                    Protocol_validator.prefetch_and_compile_protocols
                      bv.protocol_validator
                      ?peer ~timeout:bv.protocol_timeout
                      block ;
                    may_wakeup (Ok block) ;
                    notify_new_block block ;
                    return ()
              end
            (* TODO catch other temporary error (e.g. system errors)
               and do not 'commit' them on disk... *)
            | Error [Lwt_utils.Canceled | Unavailable_protocol _] as err ->
                may_wakeup err ;
                return ()
            | Error errors as err ->
                Lwt_utils.protect ~canceler:bv.canceler begin fun () ->
                  Distributed_db.commit_invalid_block
                    net_db hash header errors
                end >>=? fun commited ->
                assert commited ;
                may_wakeup err ;
                return ()
  end >>= function
  | Ok () ->
      worker_loop bv
  | Error [Exn (Unix.Unix_error _) as err] ->
      lwt_log_error "validation failed with %a"
        pp_print_error [err] >>= fun () ->
      worker_loop bv
  | Error [Lwt_utils.Canceled | Exn Lwt_pipe.Closed] ->
      lwt_log_notice "terminating" >>= fun () ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error "@[Unexpected error:@ %a@]"
        pp_print_error err >>= fun () ->
      Canceler.cancel bv.canceler >>= fun () ->
      Lwt.return_unit

let create ~protocol_timeout db =
  let protocol_validator = Protocol_validator.create db in
  let canceler = Canceler.create () in
  let messages = Lwt_pipe.create () in
  let bv = {
    protocol_validator ;
    protocol_timeout ;
    canceler ; messages ;
    worker = Lwt.return_unit } in
  Canceler.on_cancel bv.canceler begin fun () ->
    Lwt_pipe.close bv.messages ;
    Lwt.return_unit
  end ;
  bv.worker <-
    Lwt_utils.worker "block_validator"
      ~run:(fun () -> worker_loop bv)
      ~cancel:(fun () -> Canceler.cancel bv.canceler) ;
  bv

let shutdown { canceler ; worker } =
  Canceler.cancel canceler >>= fun () ->
  worker

let validate { messages ; protocol_validator ; protocol_timeout }
    ?canceler ?peer ?(notify_new_block = fun _ -> ())
    net_db hash (header : Block_header.t) operations =
  let net_state = Distributed_db.net_state net_db in
  State.Block.read_opt net_state hash >>= function
  | Some block ->
      lwt_debug "previously validated block %a (before pipe)"
        Block_hash.pp_short hash >>= fun () ->
      Protocol_validator.prefetch_and_compile_protocols
        protocol_validator
        ?peer ~timeout:protocol_timeout
        block ;
      return block
  | None ->
      let res, wakener = Lwt.task () in
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
      check_net_liveness net_db hash header >>=? fun () ->
      lwt_debug "pushing validation request for block %a"
        Block_hash.pp_short hash >>= fun () ->
      Lwt_pipe.push messages
        (Message (Request_validation
                    { net_db ; notify_new_block ; canceler ;
                      peer ; hash ; header ; operations },
                  Some wakener)) >>= fun () ->
      res

let fetch_and_compile_protocol bv =
  Protocol_validator.fetch_and_compile_protocol bv.protocol_validator
