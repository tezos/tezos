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

module Inttbl = Hashtbl.Make(struct
    type t = int
    let equal (x: int) (y: int) = x = y
    let hash = Hashtbl.hash
  end)

type t = {
  id: int;
  alpha: int ;
  mutable total: int64 ;
  mutable current: int ;
  mutable average: int ;
}

let counters = Inttbl.create 51

let updated = Lwt_condition.create ()

let update_hook = ref []
let on_update f = update_hook := f :: !update_hook

let worker_loop () =
  let prev = ref @@ Mtime_clock.elapsed () in
  let rec inner sleep =
    sleep >>= fun () ->
    let sleep = Lwt_unix.sleep 1. in
    let now = Mtime_clock.elapsed () in
    let elapsed = int_of_float (Mtime.Span.(to_ms now -. to_ms !prev)) in
    prev := now;
    Inttbl.iter
      (fun _ c ->
         c.average <-
           (c.alpha * c.current) / elapsed + (1000 - c.alpha) * c.average / 1000;
         c.current <- 0)
      counters ;
    List.iter (fun f -> f ()) !update_hook ;
    Lwt_condition.broadcast updated () ;
    inner sleep
  in
  inner (Lwt_unix.sleep 1.)

let worker =
  lazy begin
    Lwt.async begin fun () ->
      Lwt_utils.worker "counter"
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:worker_loop
        ~cancel:(fun _ -> Lwt.return_unit)
    end
  end

let create =
  let cpt = ref 0 in
  fun ~init ~alpha ->
    Lazy.force worker ;
    let id = !cpt in
    incr cpt ;
    assert (0. < alpha && alpha <= 1.) ;
    let alpha = int_of_float (1000. *. alpha) in
    let c = { id ; alpha ; total = 0L ; current = 0 ; average = init } in
    Inttbl.add counters id c ;
    c

let add c x =
  c.total <- Int64.(add c.total (of_int x)) ;
  c.current <- c.current + x

let destroy c =
  Inttbl.remove counters c.id

type stat = {
  total: int64 ;
  average: int ;
}

let stat ({ total ; average ; _ } : t) : stat =
  { total ; average }
