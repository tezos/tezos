(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Validating batches of operations with some peer-based
 * compartimentatilsation. *)

type limits = {
  worker_limits : Worker_types.limits ;
}

module type T = sig
  module Proto: Registered_protocol.T
  module Mempool_worker: Mempool_worker.T with module Proto = Proto

  type t
  type input = Operation_hash.t list
  type result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Mempool_worker.result
  type output = result Operation_hash.Map.t

  val create: limits -> P2p_peer.Id.t -> t Lwt.t
  val shutdown: t -> unit Lwt.t

  val validate: Mempool_worker.t -> t -> input -> output tzresult Lwt.t

  (* For special use when a bunch of operations is not attributed to a specific
   * peer worker. E.g., for injecting operations, for recylcing known operations
   * from a previous protocol, etc. *)
  val bypass_peer_workers: Mempool_worker.t -> input -> output Lwt.t

  val rpc_directory : t RPC_directory.t

end

module Make (Mempool_worker: Mempool_worker.T) : T with module Proto = Mempool_worker.Proto = struct

  (* 0. Prelude: set up base modules and types *)
  (* See interface file for info if needed. *)

  module Proto = Mempool_worker.Proto
  module Mempool_worker = Mempool_worker

  type input = Operation_hash.t list
  type result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Mempool_worker.result
  type output = result Operation_hash.Map.t

  module Log = Tezos_stdlib.Logging.Make(struct
      let name = "node.mempool.peer_worker"
    end)


  (* 1. Core: the carefully scheduled work performed by the worker *)

  module Work : sig
    val work: Mempool_worker.t -> input -> output Lwt.t
  end = struct
    type t = {
      received: Operation_hash.t Queue.t;
      downloading: (Operation_hash.t * Operation.t tzresult Lwt.t) Queue.t;
      parsing: (Operation_hash.t * Mempool_worker.operation tzresult Lwt.t) Queue.t;
      applying: (Mempool_worker.operation * Mempool_worker.result tzresult Lwt.t) Queue.t;
      mutable results: result Operation_hash.Map.t
    }

    (* Primitives *)

    let is_empty t =
      Queue.is_empty t.received &&
      Queue.is_empty t.downloading &&
      Queue.is_empty t.parsing &&
      Queue.is_empty t.applying

    let has_resolved t = match Lwt.state t with
      | Lwt.Return _ | Lwt.Fail _ -> true
      | Lwt.Sleep -> false

    let head_is_resolved q =
      (not (Queue.is_empty q)) && has_resolved (snd (Queue.peek q))

    let select t =
      (* A `select`-like function to wait on any of the pipeline's buffers'
       * heads to resolve *)
      assert (not (Queue.is_empty t.downloading && Queue.is_empty t.applying));
      let first_task_or_never q =
        if Queue.is_empty q then
          Lwt_utils.never_ending ()
        else
          snd (Queue.peek q) >>= fun _ -> Lwt.return_unit
      in
      Lwt.choose (
        (first_task_or_never t.downloading) ::
        (first_task_or_never t.parsing) ::
        (first_task_or_never t.applying) ::
        []
      )

    let record_result pipeline op_hash result =
      pipeline.results <- Operation_hash.Map.add op_hash result pipeline.results

    let q_of_list l =
      let q = Queue.create () in
      List.iter (fun x -> Queue.add x q) l;
      q

    let create op_hashes =
      {
        received = q_of_list op_hashes;
        downloading = Queue.create ();
        parsing = Queue.create ();
        applying = Queue.create ();
        results = Operation_hash.Map.empty;
      }


    (* Exported interactions *)

    let step mempool_worker pipeline =
      (* Going through each buffer one by one. *)
      (* op_hash: Opertation_hash.t
       * op: Operation.t
       * mop: Mempool_worker.operation *)

      if head_is_resolved pipeline.applying then begin
        let (op, p) = Queue.pop pipeline.applying in
        p >>= function
        | Error errs ->
            record_result pipeline op.hash (Cannot_validate errs);
            Lwt.return_unit
        | Ok mempool_result ->
            record_result pipeline op.hash (Mempool_result mempool_result);
            Lwt.return_unit
      end

      else if head_is_resolved pipeline.parsing then begin
        let (op_hash, mop) = Queue.pop pipeline.parsing in
        mop >>= function
        | Error errs ->
            record_result pipeline op_hash (Cannot_parse errs);
            Lwt.return_unit
        | Ok mop ->
            let p = Mempool_worker.validate mempool_worker mop in
            Queue.push (mop, p) pipeline.applying;
            Lwt.return_unit
      end

      else if head_is_resolved pipeline.downloading then begin
        let (op_hash, p) = Queue.pop pipeline.downloading in
        p >>= function
        | Error errs ->
            record_result pipeline op_hash (Cannot_download errs);
            Lwt.return_unit
        | Ok op ->
            let p = Mempool_worker.parse mempool_worker op in
            Queue.push (op_hash, p) pipeline.parsing;
            Lwt.return_unit
      end

      else if (not (Queue.is_empty pipeline.received)) then begin
        let op_hash = Queue.pop pipeline.received in
        (* TODO[?] should we specify the current peer for fetching? *)
        let chain_db = Mempool_worker.chain_db mempool_worker in
        let p = Distributed_db.Operation.fetch chain_db op_hash () in
        Queue.push (op_hash, p) pipeline.downloading;
        Lwt.return_unit
      end

      else
        (* There are some pending operations, we need to wait on them *)
        select pipeline >>= fun () ->
        Lwt.return_unit

    let work mempool_worker input =
      let pipeline = create input in
      let rec loop () =
        if is_empty pipeline then begin
          Lwt.return pipeline.results
        end else
          step mempool_worker pipeline >>= fun () ->
          loop ()
      in
      let work = loop () in
      Lwt.on_cancel work
        (fun () ->
           let cancel_snd (_, p) = Lwt.cancel p in
           Queue.iter cancel_snd pipeline.downloading;
           Queue.iter cancel_snd pipeline.parsing;
           Queue.iter cancel_snd pipeline.applying);
      work

  end


  (* 2. Boilerplate: the set up for the worker architecture *)

  module Name = struct
    type t = P2p_peer.Id.t
    let encoding = P2p_peer.Id.encoding
    let base = [ "node.mempool.peer_worker" ]
    let pp = P2p_peer.Id.pp
  end

  module Request = struct
    type 'a t = Batch : (Mempool_worker.t * input) -> output t
    type view = input
    let view
      : type a. a t -> view
      = fun (Batch (_, os)) -> os
    let encoding =
      let open Data_encoding in
      list Operation_hash.encoding
    let pp = Format.pp_print_list Operation_hash.pp
  end

  module Event = struct
    type t =
      | Start of input
      | End_ok of (Request.view * Worker_types.request_status)
      | End_error of (Request.view * Worker_types.request_status * error list)

    let level req =
      match req with
      | Start _ -> Logging.Info
      | End_ok _ -> Logging.Info
      | End_error _ -> Logging.Error

    let encoding =
      let open Data_encoding in
      union
        [ case (Tag 0)
            ~title:"Start"
            (obj1 (req "input" (list Operation_hash.encoding)))
            (function Start input -> Some input | _ -> None)
            (fun input -> Start input) ;
          case (Tag 1)
            ~title:"End_ok"
            (obj2
               (req "request" Request.encoding)
               (req "status" Worker_types.request_status_encoding))
            (function End_ok (view, status) -> Some (view, status) | _ -> None)
            (fun (view, status) -> End_ok (view, status)) ;
          case (Tag 2)
            ~title:"End_error"
            (obj3
               (req "failed_request" Request.encoding)
               (req "status" Worker_types.request_status_encoding)
               (req "error" RPC_error.encoding))
            (function End_error (view, status, errs) -> Some (view, status, errs) | _ -> None)
            (fun (view, status, errs) -> End_error (view, status, errs)) ]

    let pp ppf = function
      | Start input ->
          Format.fprintf ppf
            "@[<v 0>Starting: %a@]"
            (Format.pp_print_list Operation_hash.pp)
            input
      | End_ok (view, _) ->
          Format.fprintf ppf
            "@[<v 0>Finished: %a@]"
            Request.pp view
      | End_error (view, _, errs) ->
          Format.fprintf ppf
            "@[<v 0>Errors: %a, Operations: %a@]"
            (Format.pp_print_list Error_monad.pp) errs
            Request.pp view
  end

  module Types = struct
    type parameters = unit
    type state = unit
    type view = unit
    let view () () = ()
    let encoding = Data_encoding.unit
    let pp _ _ = ()
  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)
  type t = Worker.infinite Worker.queue Worker.t
  let table = Worker.create_table Queue


  (* 3. Workers' work: setting workers' callbacks to perform core work *)

  module Handlers = struct

    type self = t

    let on_launch _ _ () = Lwt.return_unit

    let on_request : type a. self -> a Request.t -> a tzresult Lwt.t
      = fun t (Request.Batch (mempool_worker, os)) ->
        Worker.record_event t (Event.Start os) ;
        Work.work mempool_worker os >>= fun r ->
        return r

    let on_no_request _ = return_unit

    let on_close _ = Lwt.return_unit

    let on_error t view st errs =
      Worker.record_event t (Event.End_error (view, st, errs)) ;
      Lwt.return (Error errs)

    let on_completion
      : type a. self -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
      = fun t r _ st ->
        Worker.record_event t (Event.End_ok (Request.view r, st)) ;
        Lwt.return_unit

  end


  (* 4. Public interface: exporting a thin wrapper around workers and work. *)
  (* See interface file for documentation *)

  let create limits peer_id =
    Worker.launch table limits.worker_limits peer_id () (module Handlers)
  let shutdown w = Worker.shutdown w

  let validate mempool_worker t os =
    Worker.push_request_and_wait t (Request.Batch (mempool_worker, os))

  let bypass_peer_workers mempool_worker input =
    (* TODO: log, but not through the Worker's event system because no worker is available. *)
    Work.work mempool_worker input


  (* 5. Introspection *)

  let rpc_directory = Pervasives.failwith "TODO"

end
