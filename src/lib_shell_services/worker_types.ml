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

let level_encoding =
  let open Logging in
  let open Data_encoding in
  conv
    (function
      | Fatal -> "fatal"
      | Error -> "error"
      | Warning -> "warning"
      | Notice -> "notice"
      | Info -> "info"
      | Debug -> "debug")
    (function
      | "error" -> Error
      | "warn" -> Warning
      | "notice" -> Notice
      | "info" -> Info
      | "debug" -> Debug
      | "fatal" -> Fatal
      | _ -> invalid_arg "Logging.level")
    string

type limits =
  { backlog_size : int ;
    backlog_level : Logging.level ;
    zombie_lifetime : float ;
    zombie_memory : float }

type worker_status =
  | Launching of Time.t
  | Running of Time.t
  | Closing of Time.t * Time.t
  | Closed of Time.t * Time.t * error list option

let worker_status_encoding error_encoding =
  let open Data_encoding in
  union
    [ case (Tag 0)
        ~title:"Launching"
        (obj2
           (req "phase" (constant "launching"))
           (req "since" Time.encoding))
        (function Launching t -> Some ((), t) | _ -> None)
        (fun ((), t) -> Launching t) ;
      case (Tag 1)
        ~title:"Running"
        (obj2
           (req "phase" (constant "running"))
           (req "since" Time.encoding))
        (function Running t -> Some ((), t) | _ -> None)
        (fun ((), t) -> Running t) ;
      case (Tag 2)
        ~title:"Closing"
        (obj3
           (req "phase" (constant "closing"))
           (req "birth" Time.encoding)
           (req "since" Time.encoding))
        (function Closing (t0, t) -> Some ((), t0, t) | _ -> None)
        (fun ((), t0, t) -> Closing (t0, t))  ;
      case (Tag 3)
        ~title:"Closed"
        (obj3
           (req "phase" (constant "closed"))
           (req "birth" Time.encoding)
           (req "since" Time.encoding))
        (function Closed (t0, t, None) -> Some ((), t0, t) | _ -> None)
        (fun ((), t0, t) -> Closed (t0, t, None)) ;
      case (Tag 4)
        ~title:"Crashed"
        (obj4
           (req "phase" (constant "crashed"))
           (req "birth" Time.encoding)
           (req "since" Time.encoding)
           (req "errors" error_encoding))
        (function Closed (t0, t, Some errs) -> Some ((), t0, t, errs) | _ -> None)
        (fun ((), t0, t, errs) -> Closed (t0, t, Some errs )) ]

type request_status =
  { pushed : Time.t ;
    treated : Time.t ;
    completed : Time.t }

let request_status_encoding =
  let open Data_encoding in
  conv
    (fun { pushed ; treated ; completed } ->
       (pushed, treated, completed))
    (fun (pushed, treated, completed) ->
       { pushed ; treated ; completed })
    (obj3
       (req "pushed" Time.encoding)
       (req "treated" Time.encoding)
       (req "completed" Time.encoding))

type ('req, 'evt) full_status =
  { status : worker_status ;
    pending_requests : (Time.t * 'req) list ;
    backlog : (Logging.level * 'evt list) list ;
    current_request : (Time.t * Time.t * 'req) option }

let full_status_encoding req_encoding evt_encoding error_encoding =
  let open Data_encoding in
  let requests_encoding =
    list
      (obj2
         (req "pushed" Time.encoding)
         (req "request" (dynamic_size req_encoding))) in
  let events_encoding =
    list
      (obj2
         (req "level" level_encoding)
         (req "events" (dynamic_size (list (dynamic_size evt_encoding))))) in
  let current_request_encoding =
    obj3
      (req "pushed" Time.encoding)
      (req "treated" Time.encoding)
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
