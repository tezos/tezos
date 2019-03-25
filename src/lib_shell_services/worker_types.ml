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

type limits =
  { backlog_size : int ;
    backlog_level : Internal_event.level ; }

type worker_status =
  | Launching of Time.System.t
  | Running of Time.System.t
  | Closing of Time.System.t * Time.System.t
  | Closed of Time.System.t * Time.System.t * error list option

let worker_status_encoding error_encoding =
  let open Data_encoding in
  union
    [ case (Tag 0)
        ~title:"Launching"
        (obj2
           (req "phase" (constant "launching"))
           (req "since" Time.System.encoding))
        (function Launching t -> Some ((), t) | _ -> None)
        (fun ((), t) -> Launching t) ;
      case (Tag 1)
        ~title:"Running"
        (obj2
           (req "phase" (constant "running"))
           (req "since" Time.System.encoding))
        (function Running t -> Some ((), t) | _ -> None)
        (fun ((), t) -> Running t) ;
      case (Tag 2)
        ~title:"Closing"
        (obj3
           (req "phase" (constant "closing"))
           (req "birth" Time.System.encoding)
           (req "since" Time.System.encoding))
        (function Closing (t0, t) -> Some ((), t0, t) | _ -> None)
        (fun ((), t0, t) -> Closing (t0, t))  ;
      case (Tag 3)
        ~title:"Closed"
        (obj3
           (req "phase" (constant "closed"))
           (req "birth" Time.System.encoding)
           (req "since" Time.System.encoding))
        (function Closed (t0, t, None) -> Some ((), t0, t) | _ -> None)
        (fun ((), t0, t) -> Closed (t0, t, None)) ;
      case (Tag 4)
        ~title:"Crashed"
        (obj4
           (req "phase" (constant "crashed"))
           (req "birth" Time.System.encoding)
           (req "since" Time.System.encoding)
           (req "errors" error_encoding))
        (function Closed (t0, t, Some errs) -> Some ((), t0, t, errs) | _ -> None)
        (fun ((), t0, t, errs) -> Closed (t0, t, Some errs )) ]

type worker_information = {
  instances_number : int ;
  wstatus : worker_status ;
  queue_length : int ;
}

let worker_information_encoding error_encoding =
  Data_encoding.(
    conv
      (fun { instances_number ; wstatus ; queue_length } ->
         (instances_number, wstatus, queue_length))
      (fun (instances_number, wstatus, queue_length) ->
         { instances_number ; wstatus ; queue_length })
      (obj3
         (req "instances" int31)
         (req "status" (worker_status_encoding error_encoding))
         (req "queue_length" int31)
      )
  )

type request_status =
  { pushed : Time.System.t ;
    treated : Time.System.t ;
    completed : Time.System.t }

let request_status_encoding =
  let open Data_encoding in
  conv
    (fun { pushed ; treated ; completed } ->
       (pushed, treated, completed))
    (fun (pushed, treated, completed) ->
       { pushed ; treated ; completed })
    (obj3
       (req "pushed" Time.System.encoding)
       (req "treated" Time.System.encoding)
       (req "completed" Time.System.encoding))

type ('req, 'evt) full_status =
  { status : worker_status ;
    pending_requests : (Time.System.t * 'req) list ;
    backlog : (Internal_event.level * 'evt list) list ;
    current_request : (Time.System.t * Time.System.t * 'req) option }

let full_status_encoding req_encoding evt_encoding error_encoding =
  let open Data_encoding in
  let requests_encoding =
    list
      (obj2
         (req "pushed" Time.System.encoding)
         (req "request" (dynamic_size req_encoding))) in
  let events_encoding =
    list
      (obj2
         (req "level" Internal_event.Level.encoding)
         (req "events" (dynamic_size (list (dynamic_size evt_encoding))))) in
  let current_request_encoding =
    obj3
      (req "pushed" Time.System.encoding)
      (req "treated" Time.System.encoding)
      (req "request" req_encoding) in
  conv
    (fun { status  ; pending_requests ; backlog ; current_request } ->
       (status, pending_requests, backlog, current_request))
    (fun (status, pending_requests, backlog, current_request) ->
       { status  ; pending_requests ; backlog ; current_request })
    (obj4
       (req "status" (worker_status_encoding error_encoding))
       (req "pending_requests" requests_encoding)
       (req "backlog" events_encoding)
       (opt "current_request" current_request_encoding))
