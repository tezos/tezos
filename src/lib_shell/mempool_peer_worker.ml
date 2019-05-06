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

(** Validating batches of operations with some peer-based
 * compartimentatilsation. *)

type limits = {
  max_promises_per_request : int ;
  worker_limits : Worker_types.limits ;
}

module type T = sig
  module Mempool_worker: Mempool_worker.T

  type t
  type input = Operation_hash.t list

  val create: limits -> P2p_peer.Id.t -> Mempool_worker.t -> t tzresult Lwt.t
  val shutdown: t -> input Lwt.t

  val validate: t -> input -> unit tzresult Lwt.t

end


module type STATIC = sig
  val max_pending_requests : int
end

module Make (Static: STATIC) (Mempool_worker: Mempool_worker.T)
  : T with module Mempool_worker = Mempool_worker
= struct

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

  let pp_input ppf input =
    Format.fprintf ppf
      "@[<v 0>%a@]"
      (Format.pp_print_list Operation_hash.pp)
      input
  let result_encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Cannot download"
          (obj1 (req "download_errors" (list Error_monad.error_encoding)))
          (function Cannot_download errs -> Some errs | _ -> None)
          (fun errs -> Cannot_download errs) ;
        case (Tag 1)
          ~title:"Cannot parse"
          (obj1 (req "parse_errors" (list Error_monad.error_encoding)))
          (function Cannot_parse errs -> Some errs | _ -> None)
          (fun errs -> Cannot_parse errs) ;
        case (Tag 2)
          ~title:"Cannot validate"
          (obj1 (req "validation_errors" (list Error_monad.error_encoding)))
          (function Cannot_validate errs -> Some errs | _ -> None)
          (fun errs -> Cannot_validate errs) ;
        case (Tag 3)
          ~title:"Validation result"
          (obj1 (req "validation_result" Mempool_worker.result_encoding))
          (function Mempool_result result -> Some result | _ -> None)
          (fun result -> Mempool_result result) ]

  module Log =
    Internal_event.Legacy_logging.Make (struct
      let name = "node.mempool.peer_worker"
    end)


  (* 1. Core: the carefully scheduled work performed by the worker *)

  module Work : sig
    val process_batch: Mempool_worker.t -> int -> input -> output Lwt.t
  end = struct
    type t = {
      pool: unit Lwt_pool.t;
      received: Operation_hash.t Queue.t;
      downloading: (Operation_hash.t * Operation.t tzresult Lwt.t) Queue.t;
      applying: (Mempool_worker.operation * Mempool_worker.result tzresult Lwt.t) Queue.t;
      mutable results: result Operation_hash.Map.t
    }

    (* Primitives *)

    let is_empty t =
      Queue.is_empty t.received &&
      Queue.is_empty t.downloading &&
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
        (first_task_or_never t.applying) ::
        []
      )

    let record_result pipeline op_hash result =
      pipeline.results <- Operation_hash.Map.add op_hash result pipeline.results

    let q_of_list l =
      let q = Queue.create () in
      List.iter (fun x -> Queue.add x q) l;
      q

    let create pool_size op_hashes =
      {
        pool = Lwt_pool.create pool_size Lwt.return;
        received = q_of_list op_hashes;
        downloading = Queue.create ();
        applying = Queue.create ();
        results = Operation_hash.Map.empty;
      }

    let cancel pipeline =
      let cancel_snd (_, p) = Lwt.cancel p in
      Queue.iter cancel_snd pipeline.downloading;
      Queue.iter cancel_snd pipeline.applying


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

      else if head_is_resolved pipeline.downloading then begin
        let (op_hash, p) = Queue.pop pipeline.downloading in
        p >>= function
        | Error errs ->
            record_result pipeline op_hash (Cannot_download errs);
            Lwt.return_unit
        | Ok op ->
            match Mempool_worker.parse op with
            | Error errs ->
                record_result pipeline op_hash (Cannot_parse errs);
                Lwt.return_unit
            | Ok mop ->
                let p =
                  Lwt_pool.use pipeline.pool (fun () ->
                      Mempool_worker.validate mempool_worker mop) in
                Queue.push (mop, p) pipeline.applying;
                Lwt.return_unit
      end

      else if (not (Queue.is_empty pipeline.received)) then begin
        let op_hash = Queue.pop pipeline.received in
        (* TODO[?] should we specify the current peer for fetching? *)
        let chain_db = Mempool_worker.chain_db mempool_worker in
        let p =
          Lwt_pool.use pipeline.pool (fun () ->
              Distributed_db.Operation.fetch chain_db op_hash ()) in
        Queue.push (op_hash, p) pipeline.downloading;
        Lwt.return_unit
      end

      else
        (* There are some pending operations, we need to wait on them *)
        select pipeline >>= fun () ->
        Lwt.return_unit

    let process_batch mempool_worker pool_size input =
      let pipeline = create pool_size input in
      let rec loop () =
        if is_empty pipeline then
          Lwt.return pipeline.results
        else
          step mempool_worker pipeline >>= fun () ->
          loop ()
      in
      let work = loop () in
      Lwt.on_cancel work (fun () -> cancel pipeline);
      work

  end


  (* 2. Boilerplate: the set up for the worker architecture *)

  module Name = struct
    type t = P2p_peer.Id.t
    let encoding = P2p_peer.Id.encoding
    let base =
      let proto_hash =
        let _: string = Format.flush_str_formatter () in
        Format.fprintf Format.str_formatter "%a" Protocol_hash.pp Proto.hash;
        Format.flush_str_formatter () in
      [ "node"; "mempool"; "peer_worker"; proto_hash ]
    let pp = P2p_peer.Id.pp
  end

  module Request = struct
    type 'a t = Batch : input -> output t
    type view = input
    let view
      : type a. a t -> view
      = fun (Batch os) -> os
    let encoding =
      let open Data_encoding in
      list Operation_hash.encoding
    let pp ppf = function
      |[] -> Format.fprintf ppf "@[<v 2>Request:@, Empty List of Operations@]"
      |os ->
          Format.fprintf ppf
            "@[<v 2>Request:@,%a@]"
            (Format.pp_print_list Operation_hash.pp)
            os
  end

  module Event = struct
    type t =
      | Start of input
      | End_ok of (Request.view * Worker_types.request_status * output)
      | End_error of (Request.view * Worker_types.request_status * error list)

    let level req =
      let open Internal_event in
      match req with
      | Start _ -> Info
      | End_ok _ -> Info
      | End_error _ -> Error

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
            (obj3
               (req "request" Request.encoding)
               (req "status" Worker_types.request_status_encoding)
               (req "output" (Operation_hash.Map.encoding result_encoding)))
            (function End_ok (view, status, result) -> Some (view, status, result) | _ -> None)
            (fun (view, status, result) -> End_ok (view, status, result)) ;
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
            pp_input
            input
      | End_ok (view, _, _) ->
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
    type parameters = Mempool_worker.t * int
    type state = { mempool_worker: Mempool_worker.t ; pool_size: int }
    type view = unit
    let view _ _ = ()
    let encoding = Data_encoding.unit
    let pp _ _ = ()
  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)
  type t = Worker.bounded Worker.queue Worker.t
  let table =
    let open Worker in
    create_table (Bounded { size = Static.max_pending_requests })


  (* 3. Workers' work: setting workers' callbacks to perform core work *)

  module Handlers = struct

    type self = t

    let on_launch _ _ (mempool_worker, pool_size) =
      return Types.{ mempool_worker; pool_size }

    let on_request : type a. self -> a Request.t -> a tzresult Lwt.t
      = fun t (Request.Batch os) ->
        let st = Worker.state t in
        Worker.record_event t (Event.Start os) ;
        Work.process_batch st.mempool_worker st.pool_size os >>= fun r ->
        return r

    let on_no_request _ = return_unit

    let on_close _ = Lwt.return_unit

    let on_error t view st errs =
      Worker.record_event t (Event.End_error (view, st, errs)) ;
      Lwt.return_error errs

    let on_completion
      : type a. self -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
      = fun t req output st ->
        match req with
        | Request.Batch _ ->
            Worker.record_event t (Event.End_ok (Request.view req, st, output)) ;
            Lwt.return_unit

  end


  (* 4. Public interface: exporting a thin wrapper around workers and work. *)
  (* See interface file for documentation *)

  let validate t os =
    Worker.Queue.push_request_and_wait t (Request.Batch os)
    >>=? fun (_: output) -> return_unit

  let create limits peer_id mempool_worker =
    Worker.launch
      table
      limits.worker_limits
      peer_id
      (mempool_worker, limits.max_promises_per_request)
      (module Handlers)

  let shutdown w =
    let recycled = Operation_hash.Set.empty in
    let recycled =
      List.fold_left
        (fun recycled (_, input) ->
           List.fold_left
             (fun recycled op_h -> Operation_hash.Set.add op_h recycled)
             recycled
             input)
        recycled
        (Worker.Queue.pending_requests w)
    in
    let recycled =
      match Worker.current_request w with
      | Some (_, _, input) ->
          List.fold_left
            (fun recycled op_h -> Operation_hash.Set.add op_h recycled)
            recycled
            input
      | None -> recycled
    in
    let input = Operation_hash.Set.elements recycled in
    Worker.shutdown w >>= fun () ->
    Lwt.return input

end
