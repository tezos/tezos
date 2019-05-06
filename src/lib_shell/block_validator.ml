(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Block_validator_worker_state
open Block_validator_errors

type limits = {
  protocol_timeout: Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}

type validator_kind = Block_validator_process.validator_kind =
  | Internal of Context.index

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
    validation_process: Block_validator_process.t ;
    limits : limits ;
    start_testchain : bool ;
  }
  type parameters = limits * bool * Distributed_db.t * Block_validator_process.validator_kind
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
    = fun (Request_validation { chain_db ; peer ; hash ; _ }) ->
      let chain_id = chain_db |> Distributed_db.chain_state |> State.Chain.id in
      { chain_id ; block = hash ; peer = peer }
end

module Worker = Worker.Make (Name) (Event) (Request) (Types)

type t = Worker.infinite Worker.queue Worker.t

let debug w =
  Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

let check_chain_liveness chain_db hash (header: Block_header.t) =
  let chain_state = Distributed_db.chain_state chain_db in
  match State.Chain.expiration chain_state with
  | Some eol when Time.Protocol.(eol <= header.shell.timestamp) ->
      fail @@ invalid_block hash @@
      Expired_chain { chain_id = State.Chain.id chain_state ;
                      expiration = eol ;
                      timestamp = header.shell.timestamp }
  | None | Some _ -> return_unit

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
        | Some { errors ; _ } ->
            return (Error errors)
        | None ->
            begin
              debug w "validating block %a" Block_hash.pp_short hash ;
              State.Block.read
                chain_state header.shell.predecessor >>=? fun pred ->
              (* TODO also protect with [Worker.canceler w]. *)
              protect ?canceler begin fun () ->
                begin Block_validator_process.apply_block
                    bv.validation_process
                    ~predecessor:pred
                    header operations >>= function
                  | Ok x -> return x
                  | Error [ Missing_test_protocol protocol ] ->
                      Protocol_validator.fetch_and_compile_protocol
                        bv.protocol_validator
                        ?peer ~timeout:bv.limits.protocol_timeout
                        protocol >>=? fun _ ->
                      Block_validator_process.apply_block
                        bv.validation_process
                        ~predecessor:pred
                        header operations
                  | Error _ as x -> Lwt.return x
                end >>=? fun { validation_result ; block_metadata ;
                               ops_metadata ; context_hash ; forking_testchain } ->
                let validation_store =
                  ({ context_hash ;
                     message = validation_result.message ;
                     max_operations_ttl =  validation_result.max_operations_ttl ;
                     last_allowed_fork_level = validation_result.last_allowed_fork_level} :
                     State.Block.validation_store) in
                Distributed_db.commit_block
                  chain_db hash
                  header block_metadata operations ops_metadata
                  validation_store
                  ~forking_testchain >>=? function
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
            | Error [Canceled | Unavailable_protocol _ | Missing_test_protocol _ | System_error _ ] as err ->
                (* FIXME: Canceled can escape. Canceled is not registered. BOOM! *)
                return err
            | Error errors ->
                Worker.protect w begin fun () ->
                  Distributed_db.commit_invalid_block
                    chain_db hash header errors
                end >>=? fun commited ->
                assert commited ;
                return (Error errors)

let on_launch _ _ (limits, start_testchain, db, validation_kind) =
  let protocol_validator = Protocol_validator.create db in
  Block_validator_process.init validation_kind >>= fun validation_process ->
  return { Types.protocol_validator ; validation_process ; limits ; start_testchain }

let on_error w r st errs =
  Worker.record_event w (Validation_failure (r, st, errs)) ;
  Lwt.return_error errs

let on_completion
  : type a. t -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
  = fun w (Request.Request_validation _ as r) v st ->
    match v with
    | Ok (Some _) ->
        Worker.record_event w
          (Event.Validation_success (Request.view r, st)) ;
        Lwt.return_unit
    | Ok None ->
        Lwt.return_unit
    | Error errs ->
        Worker.record_event w
          (Event.Validation_failure (Request.view r, st, errs)) ;
        Lwt.return_unit

let on_close w =
  let bv = Worker.state w in
  Block_validator_process.close bv.validation_process

let table = Worker.create_table Queue

let create limits db validation_process_kind ~start_testchain =
  let module Handlers = struct
    type self = t
    let on_launch = on_launch
    let on_request = on_request
    let on_close = on_close
    let on_error = on_error
    let on_completion = on_completion
    let on_no_request _ = return_unit
  end in
  Worker.launch
    table
    limits.worker_limits
    ()
    (limits, start_testchain, db, validation_process_kind)
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
      return_none
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
      Worker.Queue.push_request_and_wait w
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

let pending_requests t = Worker.Queue.pending_requests t

let current_request t = Worker.current_request t

let last_events = Worker.last_events
