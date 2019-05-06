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

open Lwt.Infix

type t =
  { mutable pending_tasks : unit Lwt.u list ;
    mutable pending_idle : (unit -> unit Lwt.t) list ;
    mutable running_tasks : int ;
    mutable running_idle : bool ;
    mutable prevent_tasks : bool }

let create () =
  { pending_tasks = [] ;
    pending_idle = [] ;
    running_tasks = 0 ;
    running_idle = false ;
    prevent_tasks = false }

let rec may_run_idle_tasks w =
  if w.running_tasks = 0 && not w.running_idle then
    match w.pending_idle with
    | [] -> ()
    | pending_idle ->
        w.running_idle <- true ;
        w.prevent_tasks <- false ;
        w.pending_idle <- [] ;
        Lwt.async (fun () ->
            let pending_idle = List.rev pending_idle in
            Lwt_list.iter_s (fun f -> f ()) pending_idle >>= fun () ->
            w.running_idle <- false ;
            let pending_tasks = List.rev w.pending_tasks in
            w.pending_tasks <- [] ;
            List.iter (fun u -> Lwt.wakeup u ()) pending_tasks ;
            may_run_idle_tasks w ;
            Lwt.return_unit)

let wrap_error f =
  Lwt.catch
    (fun () -> f () >>= fun r -> Lwt.return_ok r)
    (fun exn -> Lwt.return_error exn)

let unwrap_error = function
  | Ok r -> Lwt.return r
  | Error exn -> Lwt.fail exn

let wakeup_error u = function
  | Ok r -> Lwt.wakeup u r
  | Error exn -> Lwt.wakeup_exn u exn

let rec task w f =
  if w.running_idle || w.prevent_tasks then
    let t, u = Lwt.task () in
    w.pending_tasks <- u :: w.pending_tasks ;
    t >>= fun () -> task w f
  else begin
    w.running_tasks <- w.running_tasks + 1 ;
    wrap_error f >>= fun res ->
    w.running_tasks <- w.running_tasks - 1 ;
    may_run_idle_tasks w ;
    unwrap_error res
  end

let when_idle w f =
  let t, u = Lwt.task () in
  let canceled = ref false in
  Lwt.on_cancel t (fun () -> canceled := true) ;
  let f () =
    if !canceled then
      Lwt.return_unit
    else
      wrap_error f >>= fun res ->
      wakeup_error u res ;
      Lwt.return_unit in
  w.pending_idle <- f :: w.pending_idle ;
  may_run_idle_tasks w ;
  t

let force_idle w f =
  w.prevent_tasks <- true ;
  when_idle w f
