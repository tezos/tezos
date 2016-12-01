(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

type 'a t =
  { queue : 'a Queue.t ;
    size : int ;
    mutable push_waiter : (unit Lwt.t * unit Lwt.u) option ;
    mutable pop_waiter : (unit Lwt.t * unit Lwt.u) option }

let create ~size =
  { queue = Queue.create () ;
    size ;
    push_waiter = None ;
    pop_waiter = None }

let notify_push q =
  match q.push_waiter with
  | None -> ()
  | Some (_, w) ->
      q.push_waiter <- None ;
      Lwt.wakeup_later w ()

let notify_pop q =
  match q.pop_waiter with
  | None -> ()
  | Some (_, w) ->
      q.pop_waiter <- None ;
      Lwt.wakeup_later w ()

let wait_push q =
  match q.push_waiter with
  | Some (t, _) -> t
  | None ->
      let waiter, wakener = Lwt.wait () in
      q.push_waiter <- Some (waiter, wakener) ;
      waiter

let wait_pop q =
  match q.pop_waiter with
  | Some (t, _) -> t
  | None ->
      let waiter, wakener = Lwt.wait () in
      q.pop_waiter <- Some (waiter, wakener) ;
      waiter

let rec push ({ queue ; size } as q) elt =
  if Queue.length queue < size then begin
    Queue.push elt queue ;
    notify_push q ;
    Lwt.return_unit
  end else
    wait_pop q >>= fun () ->
    push q elt

let rec push_now ({ queue; size } as q) elt =
  Queue.length queue < size && begin
    Queue.push elt queue ;
    notify_push q ;
    true
  end

let rec pop ({ queue } as q) =
  if not (Queue.is_empty queue) then
    let elt = Queue.pop queue in
    notify_pop q ;
    Lwt.return elt
  else
    wait_push q >>= fun () ->
    pop q

let rec peek ({ queue } as q) =
  if not (Queue.is_empty queue) then
    let elt = Queue.peek queue in
    Lwt.return elt
  else
    wait_push q >>= fun () ->
    peek q

let pop_now_exn ({ queue } as q) =
  let elt = Queue.pop queue in
  notify_pop q ;
  elt

let pop_now q =
  match pop_now_exn q with
  | exception Queue.Empty -> None
  | elt -> Some elt

let length { queue } = Queue.length queue
let is_empty { queue } = Queue.is_empty queue

let rec values_available q =
  if is_empty q then
    wait_push q >>= fun () ->
    values_available q
  else
    Lwt.return_unit
