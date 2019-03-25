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

module Request = struct
  type 'a t =
    | Flush : Block_hash.t -> unit t
    | Notify : P2p_peer.Id.t * Mempool.t -> unit t
    | Inject : Operation.t -> unit t
    | Arrived : Operation_hash.t * Operation.t -> unit t
    | Advertise : unit t
  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Flush"
          (obj2
             (req "request" (constant "flush"))
             (req "block" Block_hash.encoding))
          (function View (Flush hash) -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> View (Flush hash)) ;
        case (Tag 1)
          ~title:"Notify"
          (obj3
             (req "request" (constant "notify"))
             (req "peer" P2p_peer.Id.encoding)
             (req "mempool" Mempool.encoding))
          (function View (Notify (peer, mempool)) -> Some ((), peer, mempool) | _ -> None)
          (fun ((), peer, mempool) -> View (Notify (peer, mempool))) ;
        case (Tag 2)
          ~title:"Inject"
          (obj2
             (req "request" (constant "inject"))
             (req "operation" Operation.encoding))
          (function View (Inject op) -> Some ((), op) | _ -> None)
          (fun ((), op) -> View (Inject op)) ;
        case (Tag 3)
          ~title:"Arrived"
          (obj3
             (req "request" (constant "arrived"))
             (req "operation_hash" Operation_hash.encoding)
             (req "operation" Operation.encoding))
          (function View (Arrived (oph, op)) -> Some ((), oph, op) | _ -> None)
          (fun ((), oph, op) -> View (Arrived (oph, op))) ;
        case (Tag 4)
          ~title:"Advertise"
          (obj1 (req "request" (constant "advertise")))
          (function View Advertise -> Some () | _ -> None)
          (fun () -> View Advertise) ]

  let pp ppf (View r) = match r with
    | Flush hash ->
        Format.fprintf ppf "switching to new head %a"
          Block_hash.pp hash
    | Notify (id, { Mempool.known_valid ; pending }) ->
        Format.fprintf ppf "@[<v 2>notified by %a of operations"
          P2p_peer.Id.pp id ;
        List.iter
          (fun oph ->
             Format.fprintf ppf "@,%a (applied)"
               Operation_hash.pp oph)
          known_valid ;
        List.iter
          (fun oph ->
             Format.fprintf ppf "@,%a (pending)"
               Operation_hash.pp oph)
          (Operation_hash.Set.elements pending) ;
        Format.fprintf ppf "@]"
    | Inject op ->
        Format.fprintf ppf "injecting operation %a"
          Operation_hash.pp (Operation.hash op)
    | Arrived (oph, _) ->
        Format.fprintf ppf "operation %a arrived"
          Operation_hash.pp oph
    | Advertise ->
        Format.fprintf ppf "advertising pending operations"
end

module Event = struct
  type t =
    | Request of (Request.view * Worker_types.request_status * error list option)
    | Debug of string

  let level req =
    let open Request in
    match req with
    | Debug _ -> Internal_event.Debug
    | Request (View (Flush _), _, _) -> Internal_event.Notice
    | Request (View (Notify _), _, _) -> Internal_event.Debug
    | Request (View (Inject _), _, _) -> Internal_event.Notice
    | Request (View (Arrived _), _, _) -> Internal_event.Debug
    | Request (View Advertise, _, _) -> Internal_event.Debug

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Debug"
          (obj1 (req "message" string))
          (function Debug msg -> Some msg | _ -> None)
          (fun msg -> Debug msg) ;
        case (Tag 1)
          ~title:"Request"
          (obj2
             (req "request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Request (req, t, None) -> Some (req, t) | _ -> None)
          (fun (req, t) -> Request (req, t, None)) ;
        case (Tag 2)
          ~title:"Failed request"
          (obj3
             (req "error" RPC_error.encoding)
             (req "failed_request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Request (req, t, Some errs) -> Some (errs, req, t) | _ -> None)
          (fun (errs, req, t) -> Request (req, t, Some errs)) ]

  let pp ppf = function
    | Debug msg -> Format.fprintf ppf "%s" msg
    | Request (view, { pushed ; treated ; completed }, None)  ->
        Format.fprintf ppf
          "@[<v 0>%a@,\
           Pushed: %a, Treated: %a, Completed: %a@]"
          Request.pp view
          Time.System.pp_hum pushed Time.System.pp_hum treated Time.System.pp_hum completed
    | Request (view, { pushed ; treated ; completed }, Some errors)  ->
        Format.fprintf ppf
          "@[<v 0>%a@,\
           Pushed: %a, Treated: %a, Failed: %a@,\
           %a@]"
          Request.pp view
          Time.System.pp_hum pushed Time.System.pp_hum treated Time.System.pp_hum completed
          (Format.pp_print_list Error_monad.pp) errors
end

module Worker_state = struct
  type view =
    { head : Block_hash.t ;
      timestamp : Time.System.t ;
      fetching : Operation_hash.Set.t ;
      pending : Operation_hash.Set.t ;
      applied : Operation_hash.t list ;
      delayed : Operation_hash.Set.t }

  let encoding =
    let open Data_encoding in
    conv
      (fun { head ; timestamp ; fetching ; pending ; applied ; delayed } ->
         (head, timestamp, fetching, pending, applied, delayed))
      (fun (head, timestamp, fetching, pending, applied, delayed) ->
         { head ; timestamp ; fetching ; pending ; applied ; delayed })
      (obj6
         (req "head" Block_hash.encoding)
         (req "timestamp" Time.System.encoding)
         (req "fetching" Operation_hash.Set.encoding)
         (req "pending" Operation_hash.Set.encoding)
         (req "applied" (list Operation_hash.encoding))
         (req "delayed" Operation_hash.Set.encoding))

  let pp ppf view =
    Format.fprintf ppf
      "@[<v 0>\
       Head: %a@,\
       Timestamp: %a@,
       @[<v 2>Fetching: %a@]@,
       @[<v 2>Pending: %a@]@,
       @[<v 2>Applied: %a@]@,
       @[<v 2>Delayed: %a@]@]"
      Block_hash.pp
      view.head
      Time.System.pp_hum
      view.timestamp
      (Format.pp_print_list Operation_hash.pp)
      (Operation_hash.Set.elements view.fetching)
      (Format.pp_print_list Operation_hash.pp)
      (Operation_hash.Set.elements  view.pending)
      (Format.pp_print_list Operation_hash.pp)
      view.applied
      (Format.pp_print_list Operation_hash.pp)
      (Operation_hash.Set.elements  view.delayed)
end
