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

open Prevalidator_worker_state

type limits = {
  max_refused_operations : int ;
  operation_timeout : Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}

type name_t = (Chain_id.t * Protocol_hash.t)

module type T = sig

  module Proto: Registered_protocol.T
  val name: name_t
  val parameters: limits *  Distributed_db.chain_db
  module Prevalidation: Prevalidation.T with module Proto = Proto
  type types_state = {
    chain_db : Distributed_db.chain_db ;
    limits : limits ;
    mutable predecessor : State.Block.t ;
    mutable timestamp : Time.System.t ;
    mutable live_blocks : Block_hash.Set.t ;
    mutable live_operations : Operation_hash.Set.t ;
    refused : Operation_hash.t Ring.t ;
    mutable refusals : (Operation.t * error list) Operation_hash.Map.t ;
    branch_refused : Operation_hash.t Ring.t ;
    mutable branch_refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_delayed : Operation_hash.t Ring.t ;
    mutable branch_delays : (Operation.t * error list) Operation_hash.Map.t;
    mutable fetching : Operation_hash.Set.t ;
    mutable pending : Operation.t Operation_hash.Map.t ;
    mutable mempool : Mempool.t ;
    mutable in_mempool : Operation_hash.Set.t ;
    mutable applied : (Operation_hash.t * Operation.t) list;
    mutable applied_count : int ;
    mutable validation_state : Prevalidation.t tzresult ;
    mutable operation_stream :
      ([ `Applied | `Refused | `Branch_refused | `Branch_delayed ] *
       Operation.shell_header *
       Proto.operation_data
      ) Lwt_watcher.input;
    mutable advertisement : [ `Pending of Mempool.t | `None ] ;
    mutable rpc_directory : types_state RPC_directory.t lazy_t ;
  }
  module Name: Worker.NAME with type t = name_t
  module Types: Worker.TYPES with type state = types_state
  module Worker: Worker.T
    with type Event.t = Event.t
     and type 'a Request.t = 'a Request.t
     and type Request.view = Request.view
     and type Types.state = types_state
  type worker = Worker.infinite Worker.queue Worker.t
  val list_pendings:
    ?maintain_chain_db:Distributed_db.chain_db ->
    from_block:State.Block.t ->
    to_block:State.Block.t ->
    Operation.t Operation_hash.Map.t ->
    (Operation.t Operation_hash.Map.t * Block_hash.Set.t * Operation_hash.Set.t) Lwt.t
  val validation_result: types_state -> error Preapply_result.t

  val fitness: unit -> Fitness.t Lwt.t
  val initialization_errors: unit tzresult Lwt.t
  val worker: worker Lazy.t

end

module type ARG = sig
  val limits: limits
  val chain_db: Distributed_db.chain_db
  val chain_id: Chain_id.t
end

type t = (module T)

module Make(Proto: Registered_protocol.T)(Arg: ARG): T = struct
  module Proto = Proto
  let name = (Arg.chain_id, Proto.hash)
  let parameters = (Arg.limits, Arg.chain_db)
  module Prevalidation = Prevalidation.Make(Proto)
  type types_state = {
    chain_db : Distributed_db.chain_db ;
    limits : limits ;
    mutable predecessor : State.Block.t ;
    mutable timestamp : Time.System.t ;
    mutable live_blocks : Block_hash.Set.t ; (* just a cache *)
    mutable live_operations : Operation_hash.Set.t ; (* just a cache *)
    refused : Operation_hash.t Ring.t ;
    mutable refusals : (Operation.t * error list) Operation_hash.Map.t ;
    branch_refused : Operation_hash.t Ring.t ;
    mutable branch_refusals : (Operation.t * error list) Operation_hash.Map.t;
    branch_delayed : Operation_hash.t Ring.t ;
    mutable branch_delays : (Operation.t * error list) Operation_hash.Map.t;
    mutable fetching : Operation_hash.Set.t ;
    mutable pending : Operation.t Operation_hash.Map.t ;
    mutable mempool : Mempool.t ;
    mutable in_mempool : Operation_hash.Set.t ;
    mutable applied : (Operation_hash.t * Operation.t) list;
    mutable applied_count : int ;
    mutable validation_state : Prevalidation.t tzresult ;
    mutable operation_stream :
      ([ `Applied | `Refused | `Branch_refused | `Branch_delayed ] *
       Operation.shell_header *
       Proto.operation_data
      ) Lwt_watcher.input;
    mutable advertisement : [ `Pending of Mempool.t | `None ] ;
    mutable rpc_directory : types_state RPC_directory.t lazy_t ;
  }

  module Name = struct
    type t = name_t
    let encoding =
      Data_encoding.tup2
        Chain_id.encoding
        Protocol_hash.encoding
    let chain_id_string =
      let _: string = Format.flush_str_formatter () in
      Chain_id.pp_short Format.str_formatter Arg.chain_id;
      Format.flush_str_formatter ()
    let proto_hash_string =
      let _: string = Format.flush_str_formatter () in
      Protocol_hash.pp_short Format.str_formatter Proto.hash;
      Format.flush_str_formatter ()
    let base = [ "prevalidator" ; chain_id_string ; proto_hash_string  ]
    let pp fmt (chain_id, proto_hash) =
      Chain_id.pp_short fmt chain_id;
      Format.pp_print_string fmt ".";
      Protocol_hash.pp_short fmt proto_hash
  end

  module Types = struct
    (* Invariants:
       - an operation is in only one of these sets (map domains):
         pv.refusals pv.pending pv.fetching pv.live_operations pv.in_mempool
       - pv.in_mempool is the domain of all fields of pv.prevalidation_result
       - pv.prevalidation_result.refused = Ø, refused ops are in pv.refused
       - the 'applied' operations in pv.validation_result are in reverse order. *)
    type state = types_state
    type parameters = limits * Distributed_db.chain_db

    include Worker_state

    let view (state : state) _ : view =
      let domain map =
        Operation_hash.Map.fold
          (fun elt _ acc -> Operation_hash.Set.add elt acc)
          map Operation_hash.Set.empty in
      { head = State.Block.hash state.predecessor ;
        timestamp = state.timestamp ;
        fetching = state.fetching ;
        pending = domain state.pending ;
        applied =
          List.rev
            (List.map (fun (h, _) -> h)
               state.applied) ;
        delayed =
          Operation_hash.Set.union
            (domain state.branch_delays)
            (domain state.branch_refusals) }

  end

  module Worker: Worker.T
    with type Name.t = Name.t
     and type Event.t = Event.t
     and type 'a Request.t = 'a Request.t
     and type Request.view = Request.view
     and type Types.state = Types.state
     and type Types.parameters = Types.parameters
    = Worker.Make (Name) (Prevalidator_worker_state.Event)
      (Prevalidator_worker_state.Request) (Types)

  (** Centralised operation stream for the RPCs *)
  let notify_operation { operation_stream ; _ } result  { Operation.shell ; proto } =
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding
        proto in
    Lwt_watcher.notify operation_stream (result, shell, protocol_data)

  open Types

  type worker = Worker.infinite Worker.queue Worker.t

  let debug w =
    Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

  let list_pendings ?maintain_chain_db  ~from_block ~to_block old_mempool =
    let rec pop_blocks ancestor block mempool =
      let hash = State.Block.hash block in
      if Block_hash.equal hash ancestor then
        Lwt.return mempool
      else
        State.Block.all_operations block >>= fun operations ->
        Lwt_list.fold_left_s
          (Lwt_list.fold_left_s (fun mempool op ->
               let h = Operation.hash op in
               Lwt_utils.may maintain_chain_db
                 ~f:begin fun chain_db ->
                   Distributed_db.inject_operation chain_db h op >>= fun _ ->
                   Lwt.return_unit
                 end >>= fun () ->
               Lwt.return (Operation_hash.Map.add h op mempool)))
          mempool operations >>= fun mempool ->
        State.Block.predecessor block >>= function
        | None -> assert false
        | Some predecessor -> pop_blocks ancestor predecessor mempool
    in
    let push_block mempool block =
      State.Block.all_operation_hashes block >|= fun operations ->
      Option.iter maintain_chain_db
        ~f:(fun chain_db ->
            List.iter
              (List.iter (Distributed_db.Operation.clear_or_cancel chain_db))
              operations) ;
      List.fold_left
        (List.fold_left (fun mempool h -> Operation_hash.Map.remove h mempool))
        mempool operations
    in
    Chain_traversal.new_blocks ~from_block ~to_block >>= fun (ancestor, path) ->
    pop_blocks
      (State.Block.hash ancestor)
      from_block old_mempool >>= fun mempool ->
    Lwt_list.fold_left_s push_block mempool path >>= fun new_mempool ->
    Chain_traversal.live_blocks
      to_block
      (State.Block.max_operations_ttl to_block)
    >>= fun (live_blocks, live_operations) ->
    let new_mempool, outdated =
      Operation_hash.Map.partition
        (fun _oph op ->
           Block_hash.Set.mem op.Operation.shell.branch live_blocks)
        new_mempool in
    Option.iter maintain_chain_db
      ~f:(fun chain_db ->
          Operation_hash.Map.iter
            (fun oph _op -> Distributed_db.Operation.clear_or_cancel chain_db oph)
            outdated) ;
    Lwt.return (new_mempool, live_blocks, live_operations)

  let already_handled pv oph =
    Operation_hash.Map.mem oph pv.refusals
    || Operation_hash.Map.mem oph pv.pending
    || Operation_hash.Set.mem oph pv.fetching
    || Operation_hash.Set.mem oph pv.live_operations
    || Operation_hash.Set.mem oph pv.in_mempool

  let validation_result (state : types_state) =
    { Preapply_result.applied = List.rev state.applied ;
      branch_delayed = state.branch_delays ;
      branch_refused = state.branch_refusals ;
      refused = Operation_hash.Map.empty }

  let advertise (w : worker) pv mempool =
    match pv.advertisement with
    | `Pending { Mempool.known_valid ; pending } ->
        pv.advertisement <-
          `Pending
            { known_valid = known_valid @ mempool.Mempool.known_valid ;
              pending = Operation_hash.Set.union pending mempool.pending }
    | `None ->
        pv.advertisement <- `Pending mempool ;
        Lwt.async (fun () ->
            Lwt_unix.sleep 0.01 >>= fun () ->
            Worker.Queue.push_request_now w Advertise ;
            Lwt.return_unit)

  let is_endorsement ( op : Prevalidation.operation ) =
    Proto.acceptable_passes {
      shell = op.raw.shell ;
      protocol_data = op.protocol_data } = [0]

  let is_endorsement_raw op =
    match Prevalidation.parse op with
    |Ok op -> is_endorsement op
    |Error _ -> false

  let handle_unprocessed w pv =
    begin match pv.validation_state with
      | Error err ->
          Operation_hash.Map.iter
            (fun h op ->
               Option.iter (Ring.add_and_return_erased pv.branch_delayed h)
                 ~f:(fun e ->
                     pv.branch_delays <- Operation_hash.Map.remove e pv.branch_delays ;
                     pv.in_mempool <- Operation_hash.Set.remove e pv.in_mempool) ;
               pv.in_mempool <-
                 Operation_hash.Set.add h pv.in_mempool ;
               pv.branch_delays <-
                 Operation_hash.Map.add h (op, err) pv.branch_delays)
            pv.pending ;
          pv.pending <-
            Operation_hash.Map.empty ;
          Lwt.return_unit
      | Ok state ->
          match Operation_hash.Map.cardinal pv.pending with
          | 0 -> Lwt.return_unit
          | n ->
              debug w "processing %d operations" n ;
              let operations = List.map snd (Operation_hash.Map.bindings pv.pending) in
              Lwt_list.fold_left_s (fun (acc_validation_state, acc_mempool) op ->
                  let refused hash raw errors =
                    notify_operation pv `Refused raw ;
                    let new_mempool = Mempool.{ acc_mempool with pending = Operation_hash.Set.add hash acc_mempool.pending } in
                    Option.iter (Ring.add_and_return_erased pv.refused hash)
                      ~f:(fun e -> pv.refusals <- Operation_hash.Map.remove e pv.refusals) ;
                    pv.refusals <- Operation_hash.Map.add hash (raw, errors) pv.refusals ;
                    Distributed_db.Operation.clear_or_cancel pv.chain_db hash ;
                    Lwt.return (acc_validation_state, new_mempool) in
                  match Prevalidation.parse op with
                  | Error errors ->
                      refused (Operation.hash op) op errors
                  | Ok op ->
                      Prevalidation.apply_operation state op >>= function
                      | Applied (new_acc_validation_state, _) ->
                          if pv.applied_count <= 2000
                          (* this test is a quick fix while we wait for the new mempool *)
                          || is_endorsement op then begin
                            notify_operation pv `Applied op.raw ;
                            let new_mempool = Mempool.{ acc_mempool with known_valid = op.hash :: acc_mempool.known_valid } in
                            pv.applied <- (op.hash, op.raw) :: pv.applied ;
                            pv.in_mempool <- Operation_hash.Set.add op.hash pv.in_mempool ;
                            Lwt.return (new_acc_validation_state, new_mempool)
                          end else
                            Lwt.return (acc_validation_state, acc_mempool)
                      | Branch_delayed errors ->
                          notify_operation pv `Branch_delayed op.raw ;
                          let new_mempool = Mempool.{ acc_mempool with pending = Operation_hash.Set.add op.hash acc_mempool.pending } in
                          Option.iter (Ring.add_and_return_erased pv.branch_delayed op.hash)
                            ~f:(fun e ->
                                pv.branch_delays <- Operation_hash.Map.remove e pv.branch_delays ;
                                pv.in_mempool <- Operation_hash.Set.remove e pv.in_mempool) ;
                          pv.in_mempool <- Operation_hash.Set.add op.hash pv.in_mempool ;
                          pv.branch_delays <- Operation_hash.Map.add op.hash (op.raw, errors) pv.branch_delays ;
                          Lwt.return (acc_validation_state, new_mempool)
                      | Branch_refused errors ->
                          notify_operation pv `Branch_refused op.raw ;
                          let new_mempool = Mempool.{ acc_mempool with pending = Operation_hash.Set.add op.hash acc_mempool.pending } in
                          Option.iter (Ring.add_and_return_erased pv.branch_refused op.hash)
                            ~f:(fun e ->
                                pv.branch_refusals <- Operation_hash.Map.remove e pv.branch_refusals ;
                                pv.in_mempool <- Operation_hash.Set.remove e pv.in_mempool) ;
                          pv.in_mempool <- Operation_hash.Set.add op.hash pv.in_mempool ;
                          pv.branch_refusals <- Operation_hash.Map.add op.hash (op.raw, errors) pv.branch_refusals ;
                          Lwt.return (acc_validation_state, new_mempool)
                      | Refused errors ->
                          refused op.hash op.raw errors
                      | Duplicate | Outdated -> Lwt.return (acc_validation_state, acc_mempool))
                (state, Mempool.empty)
                operations >>= fun (state, advertised_mempool) ->
              pv.validation_state <- Ok state ;
              pv.pending <- Operation_hash.Map.empty ;
              advertise w pv
                { advertised_mempool with known_valid = List.rev advertised_mempool.known_valid } ;
              Lwt.return_unit
    end >>= fun () ->
    pv.mempool <-
      { Mempool.known_valid =
          List.rev_map fst pv.applied ;
        pending =
          Operation_hash.Map.fold
            (fun k (op,_) s ->
               if is_endorsement_raw op then
                 Operation_hash.Set.add k s
               else s)
            pv.branch_delays @@
          Operation_hash.Map.fold
            (fun k (op,_) s ->
               if is_endorsement_raw op then
                 Operation_hash.Set.add k s
               else s)
            pv.branch_refusals @@
          Operation_hash.Set.empty
      } ;
    State.Current_mempool.set (Distributed_db.chain_state pv.chain_db)
      ~head:(State.Block.hash pv.predecessor) pv.mempool >>= fun () ->
    Lwt.return_unit

  let fetch_operation w pv ?peer oph =
    debug w
      "fetching operation %a"
      Operation_hash.pp_short oph ;
    Distributed_db.Operation.fetch
      ~timeout:pv.limits.operation_timeout
      pv.chain_db ?peer oph () >>= function
    | Ok op ->
        Worker.Queue.push_request_now w (Arrived (oph, op)) ;
        Lwt.return_unit
    | Error [ Distributed_db.Operation.Canceled _ ] ->
        debug w
          "operation %a included before being prevalidated"
          Operation_hash.pp_short oph ;
        Lwt.return_unit
    | Error _ -> (* should not happen *)
        Lwt.return_unit

  let rpc_directory = lazy (
    let dir : state RPC_directory.t ref = ref RPC_directory.empty in

    let module Proto_services = Block_services.Make(Proto)(Proto) in

    dir := RPC_directory.register !dir
        (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
        (fun pv () () ->
           let map_op op =
             let protocol_data =
               Data_encoding.Binary.of_bytes_exn
                 Proto.operation_data_encoding
                 op.Operation.proto in
             { Proto.shell = op.shell ; protocol_data } in
           let map_op_error (op, error) = (map_op op, error) in
           return {
             Proto_services.Mempool.applied =
               List.map
                 (fun (hash, op) -> (hash, map_op op))
                 (List.rev pv.applied) ;
             refused =
               Operation_hash.Map.map map_op_error pv.refusals ;
             branch_refused =
               Operation_hash.Map.map map_op_error pv.branch_refusals ;
             branch_delayed =
               Operation_hash.Map.map map_op_error pv.branch_delays ;
             unprocessed =
               Operation_hash.Map.map map_op pv.pending ;
           }) ;

    dir := RPC_directory.register !dir
        (Proto_services.S.Mempool.request_operations RPC_path.open_root)
        (fun pv () () ->
           Distributed_db.Request.current_head pv.chain_db () ;
           return_unit
        ) ;

    dir := RPC_directory.gen_register !dir
        (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
        begin fun { applied ; refusals = refused ; branch_refusals = branch_refused ; branch_delays = branch_delayed ; operation_stream ; _ } params () ->
          let op_stream, stopper = Lwt_watcher.create_stream operation_stream in
          (* Convert ops *)
          let map_op op =
            let protocol_data =
              Data_encoding.Binary.of_bytes_exn
                Proto.operation_data_encoding
                op.Operation.proto in
            Proto.{ shell = op.shell ; protocol_data } in
          let fold_op _k (op, _error) acc = map_op op :: acc in
          (* First call : retrieve the current set of op from the mempool *)
          let applied = if params#applied then List.map map_op (List.map snd applied) else [] in
          let refused = if params#refused then
              Operation_hash.Map.fold fold_op refused [] else [] in
          let branch_refused = if params#branch_refused then
              Operation_hash.Map.fold fold_op branch_refused [] else [] in
          let branch_delayed = if params#branch_delayed then
              Operation_hash.Map.fold fold_op branch_delayed [] else [] in
          let current_mempool = List.concat [ applied ; refused ; branch_refused ; branch_delayed ] in
          let current_mempool = ref (Some current_mempool) in
          let filter_result = function
            | `Applied -> params#applied
            | `Refused -> params#refused
            | `Branch_refused -> params#branch_refused
            | `Branch_delayed -> params#branch_delayed
          in
          let rec next () =
            match !current_mempool with
            | Some mempool -> begin
                current_mempool := None ;
                Lwt.return_some mempool
              end
            | None -> begin
                Lwt_stream.get op_stream >>= function
                | Some (kind, shell, protocol_data) when filter_result kind ->
                    (* NOTE: Should the protocol change, a new Prevalidation
                     * context would be created. Thus, we use the same Proto. *)
                    let bytes = Data_encoding.Binary.to_bytes_exn
                        Proto.operation_data_encoding
                        protocol_data in
                    let protocol_data = Data_encoding.Binary.of_bytes_exn
                        Proto.operation_data_encoding
                        bytes in
                    Lwt.return_some [ { Proto.shell ; protocol_data } ]
                | Some _ -> next ()
                | None -> Lwt.return_none
              end
          in
          let shutdown () = Lwt_watcher.shutdown stopper in
          RPC_answer.return_stream { next ; shutdown }
        end ;

    !dir
  )

  module Handlers = struct

    type self = worker

    let on_operation_arrived (pv : state) oph op =
      pv.fetching <- Operation_hash.Set.remove oph pv.fetching ;
      if not (Block_hash.Set.mem op.Operation.shell.branch pv.live_blocks) then begin
        Distributed_db.Operation.clear_or_cancel pv.chain_db oph
        (* TODO: put in a specific delayed map ? *)
      end else if not (already_handled pv oph) (* prevent double inclusion on flush *) then begin
        pv.pending <- Operation_hash.Map.add oph op pv.pending
      end

    let on_inject pv op =
      let oph = Operation.hash op in
      if already_handled pv oph then
        return_unit (* FIXME : is this an error ? *)
      else
        Lwt.return pv.validation_state >>=? fun validation_state ->
        Lwt.return (Prevalidation.parse op) >>=? fun parsed_op ->
        Prevalidation.apply_operation validation_state parsed_op >>= function
        | Applied (_, _result) ->
            Distributed_db.inject_operation pv.chain_db oph op >>= fun (_ : bool) ->
            pv.pending <- Operation_hash.Map.add parsed_op.hash op pv.pending ;
            return_unit
        | res ->
            failwith "Error while applying operation %a:@ %a"
              Operation_hash.pp oph
              Prevalidation.pp_result res

    let on_notify w pv peer mempool =
      let all_ophs =
        List.fold_left
          (fun s oph -> Operation_hash.Set.add oph s)
          mempool.Mempool.pending mempool.known_valid in
      let to_fetch =
        Operation_hash.Set.filter
          (fun oph -> not (already_handled pv oph))
          all_ophs in
      pv.fetching <-
        Operation_hash.Set.union
          to_fetch
          pv.fetching ;
      Operation_hash.Set.iter
        (fun oph -> Lwt.ignore_result (fetch_operation w pv ~peer oph))
        to_fetch

    let on_flush w pv predecessor =
      Lwt_watcher.shutdown_input pv.operation_stream;
      list_pendings
        ~maintain_chain_db:pv.chain_db
        ~from_block:pv.predecessor ~to_block:predecessor
        (Preapply_result.operations (validation_result pv))
      >>= fun (pending, new_live_blocks, new_live_operations) ->
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      Prevalidation.create ~predecessor ~timestamp () >>= fun validation_state ->
      debug w "%d operations were not washed by the flush"
        (Operation_hash.Map.cardinal pending) ;
      pv.predecessor <- predecessor ;
      pv.live_blocks <- new_live_blocks ;
      pv.live_operations <- new_live_operations ;
      pv.timestamp <- timestamp_system ;
      pv.mempool <- { known_valid = [] ; pending = Operation_hash.Set.empty };
      pv.pending <- pending ;
      pv.in_mempool <- Operation_hash.Set.empty ;
      Ring.clear pv.branch_delayed ;
      pv.branch_delays <- Operation_hash.Map.empty ;
      Ring.clear pv.branch_refused ;
      pv.branch_refusals <- Operation_hash.Map.empty ;
      pv.applied <- [] ;
      pv.applied_count <- 0 ;
      pv.validation_state <- validation_state ;
      pv.operation_stream <- Lwt_watcher.create_input () ;
      return_unit

    let on_advertise pv =
      match pv.advertisement with
      | `None -> () (* should not happen *)
      | `Pending mempool ->
          pv.advertisement <- `None ;
          Distributed_db.Advertise.current_head pv.chain_db ~mempool pv.predecessor

    let on_request
      : type r. worker -> r Request.t -> r tzresult Lwt.t
      = fun w request ->
        let pv = Worker.state w in
        begin match request with
          | Request.Flush hash ->
              on_advertise pv ;
              (* TODO: rebase the advertisement instead *)
              let chain_state = Distributed_db.chain_state pv.chain_db in
              State.Block.read chain_state hash >>=? fun block ->
              on_flush w pv block >>=? fun () ->
              return (() : r)
          | Request.Notify (peer, mempool) ->
              on_notify w pv peer mempool ;
              return_unit
          | Request.Inject op ->
              on_inject pv op
          | Request.Arrived (oph, op) ->
              on_operation_arrived pv oph op ;
              return_unit
          | Request.Advertise ->
              on_advertise pv ;
              return_unit
        end >>=? fun r ->
        handle_unprocessed w pv >>= fun () ->
        return r

    let on_close w =
      let pv = Worker.state w in
      Operation_hash.Set.iter
        (Distributed_db.Operation.clear_or_cancel pv.chain_db)
        pv.fetching ;
      Lwt.return_unit

    let on_launch w _ (limits, chain_db) =
      let chain_state = Distributed_db.chain_state chain_db in
      Chain.data chain_state >>= fun
        { current_head = predecessor ; current_mempool = mempool ;
          live_blocks ; live_operations ; _ } ->
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      Prevalidation.create ~predecessor ~timestamp () >>= fun validation_state ->
      let fetching =
        List.fold_left
          (fun s h -> Operation_hash.Set.add h s)
          Operation_hash.Set.empty mempool.known_valid in
      let pv =
        { limits ; chain_db ;
          predecessor ; timestamp = timestamp_system ;
          live_blocks ; live_operations ;
          mempool = { known_valid = [] ; pending = Operation_hash.Set.empty };
          refused = Ring.create limits.max_refused_operations ;
          refusals = Operation_hash.Map.empty ;
          fetching ;
          pending = Operation_hash.Map.empty ;
          in_mempool = Operation_hash.Set.empty ;
          applied = [] ;
          applied_count = 0 ;
          branch_refused = Ring.create limits.max_refused_operations ;
          branch_refusals = Operation_hash.Map.empty ;
          branch_delayed = Ring.create limits.max_refused_operations ;
          branch_delays = Operation_hash.Map.empty ;
          validation_state ;
          operation_stream = Lwt_watcher.create_input () ;
          advertisement = `None ;
          rpc_directory = rpc_directory ;
        } in
      List.iter
        (fun oph -> Lwt.ignore_result (fetch_operation w pv oph))
        mempool.known_valid ;
      return pv

    let on_error w r st errs =
      Worker.record_event w (Event.Request (r, st, Some errs)) ;
      match r with
      | Request.(View (Inject _)) -> return_unit
      | _ -> Lwt.return_error errs

    let on_completion w r _ st =
      Worker.record_event w (Event.Request (Request.view r, st, None)) ;
      Lwt.return_unit

    let on_no_request _ = return_unit

  end

  let table = Worker.create_table Queue

  (* NOTE: we register a single worker for each instantiation of this Make
   * functor (and thus a single worker for the single instantiaion of Worker).
   * Whislt this is somewhat abusing the intended purpose of worker, it is part
   * of a transition plan to a one-worker-per-peer architecture. *)
  let worker_promise =
    Worker.launch table Arg.limits.worker_limits
      name
      (Arg.limits, Arg.chain_db)
      (module Handlers)

  let initialization_errors =
    worker_promise >>=? fun _ -> return_unit

  let worker = lazy begin
    match Lwt.state worker_promise with
    | Lwt.Return (Ok worker) -> worker
    | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep -> assert false
  end

  let fitness () =
    let w = Lazy.force worker in
    let pv = Worker.state w in
    begin
      Lwt.return pv.validation_state >>=? fun state ->
      Prevalidation.status state >>=? fun status ->
      return status.block_result.fitness
    end >>= function
    | Ok fitness -> Lwt.return fitness
    | Error _ ->
        Lwt.return (State.Block.fitness pv.predecessor)

end

module ChainProto_registry =
  Registry.Make(struct
    type v = t
    type t = (Chain_id.t * Protocol_hash.t)
    let compare (c1, p1) (c2, p2) =
      let pc = Protocol_hash.compare p1 p2 in
      if pc = 0 then
        Chain_id.compare c1 c2
      else
        pc
  end)


let create limits (module Proto: Registered_protocol.T) chain_db =
  let chain_state = Distributed_db.chain_state chain_db in
  let chain_id = State.Chain.id chain_state in
  match ChainProto_registry.query (chain_id, Proto.hash) with
  | None ->
      let module Prevalidator =
        Make(Proto)(struct
          let limits = limits
          let chain_db = chain_db
          let chain_id = chain_id
        end) in
      (* Checking initialization errors before giving a reference to dnagerous
       * `worker` value to caller. *)
      Prevalidator.initialization_errors >>=? fun () ->
      ChainProto_registry.register Prevalidator.name (module Prevalidator: T);
      return (module Prevalidator: T)
  | Some p ->
      return p

let shutdown (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  ChainProto_registry.remove Prevalidator.name;
  Prevalidator.Worker.shutdown w

let flush (t:t) head =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait w (Request.Flush head)

let notify_operations (t:t) peer mempool =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request w (Request.Notify (peer, mempool))

let operations (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  ({ (Prevalidator.validation_result pv) with applied = List.rev pv.applied },
   pv.pending)

let pending ?block (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  let ops = Preapply_result.operations (Prevalidator.validation_result pv) in
  match block with
  | Some to_block ->
      Prevalidator.list_pendings
        ~from_block:pv.predecessor ~to_block ops >>= fun (pending, _, _) ->
      Lwt.return pending
  | None -> Lwt.return ops

let timestamp (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let pv = Prevalidator.Worker.state w in
  pv.timestamp

let fitness (t:t) =
  let module Prevalidator: T = (val t) in
  Prevalidator.fitness ()

let inject_operation (t:t) op =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait w (Inject op)

let status (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.status w

let running_workers () =
  ChainProto_registry.fold
    (fun (id, proto) t acc -> (id, proto, t) :: acc)
    []

let pending_requests (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests w

let current_request (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.current_request w

let last_events (t:t) =
  let module Prevalidator: T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.last_events w

let protocol_hash (t:t) =
  let module Prevalidator: T = (val t) in
  Prevalidator.Proto.hash

let parameters (t:t) =
  let module Prevalidator: T = (val t) in
  Prevalidator.parameters

let empty_rpc_directory : unit RPC_directory.t =
  RPC_directory.register
    RPC_directory.empty
    (Block_services.Empty.S.Mempool.pending_operations RPC_path.open_root)
    (fun _pv () () ->
       return {
         Block_services.Empty.Mempool.applied = [] ;
         refused = Operation_hash.Map.empty ;
         branch_refused = Operation_hash.Map.empty ;
         branch_delayed = Operation_hash.Map.empty ;
         unprocessed = Operation_hash.Map.empty ;
       })


let rpc_directory  : t option RPC_directory.t =
  RPC_directory.register_dynamic_directory
    RPC_directory.empty
    (Block_services.mempool_path RPC_path.open_root)
    (function
      | None ->
          Lwt.return (RPC_directory.map (fun _ -> Lwt.return_unit) empty_rpc_directory)
      | Some t ->
          let module Prevalidator: T = (val t: T) in
          Prevalidator.initialization_errors >>= function
          | Error _ ->
              Lwt.return (RPC_directory.map (fun _ -> Lwt.return_unit) empty_rpc_directory)
          | Ok () ->
              let w = Lazy.force Prevalidator.worker in
              let pv = Prevalidator.Worker.state w in
              let pv_rpc_dir = Lazy.force pv.rpc_directory in
              Lwt.return (RPC_directory.map (fun _ -> Lwt.return pv) pv_rpc_dir))
