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

(** Some memory and time limits. *)
type limits =
  { backlog_size : int
  (** Number of event stored in the backlog for each debug level. *) ;
    backlog_level : Logging.level
  (** Stores events at least as important as this value. *) ;
    zombie_lifetime : float
  (** How long dead workers are kept in the introspection table. *) ;
    zombie_memory : float
    (** How long zombie workers' logs are kept. *) }

(** The running status of an individual worker. *)
type worker_status =
  | Launching of Time.t
  | Running of Time.t
  | Closing of Time.t * Time.t
  | Closed of Time.t * Time.t * error list option

(** Worker status serializer for RPCs. *)
val worker_status_encoding : error list Data_encoding.t -> worker_status Data_encoding.t

(** The runnning status of an individual request. *)
type request_status =
  { pushed : Time.t ;
    treated : Time.t ;
    completed : Time.t }

(** Request status serializer for RPCs. *)
val request_status_encoding : request_status Data_encoding.t

(** The full status of an individual worker. *)
type ('req, 'evt) full_status =
  { status : worker_status ;
    pending_requests : (Time.t * 'req) list ;
    backlog : (Logging.level * 'evt list) list ;
    current_request : (Time.t * Time.t * 'req) option }

(** Full worker status serializer for RPCs. *)
val full_status_encoding :
  'req Data_encoding.t ->
  'evt Data_encoding.t ->
  error list Data_encoding.t ->
  ('req, 'evt) full_status Data_encoding.t
