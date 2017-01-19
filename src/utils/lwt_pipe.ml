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
    size : int option ;
    mutable closed : bool ;
    mutable push_waiter : (unit Lwt.t * unit Lwt.u) option ;
    mutable pop_waiter : (unit Lwt.t * unit Lwt.u) option ;
    empty: unit Lwt_condition.t ;
    full: unit Lwt_condition.t ;
    not_full : unit Lwt_condition.t ;
  }

let create ?size () =
  { queue = Queue.create () ;
    size ;
    closed = false ;
    push_waiter = None ;
    pop_waiter = None ;
    empty = Lwt_condition.create () ;
    full = Lwt_condition.create () ;
    not_full = Lwt_condition.create () ;
  }

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
  | Some (t, _) -> Lwt.protected t
  | None ->
      let waiter, wakener = Lwt.wait () in
      q.push_waiter <- Some (waiter, wakener) ;
      Lwt.protected waiter

let wait_pop q =
  match q.pop_waiter with
  | Some (t, _) -> Lwt.protected t
  | None ->
      let waiter, wakener = Lwt.wait () in
      q.pop_waiter <- Some (waiter, wakener) ;
      Lwt.protected waiter

let available_space { size } len =
  match size with
  | None -> true
  | Some size -> len < size

let length { queue } = Queue.length queue
let is_empty { queue } = Queue.is_empty queue
let is_full ({ queue } as q) = not (available_space q (Queue.length queue))

let rec empty q =
  if is_empty q
  then Lwt.return_unit
  else (Lwt_condition.wait q.empty >>= fun () -> empty q)
let rec full q =
  if is_full q
  then Lwt.return_unit
  else (Lwt_condition.wait q.full >>= fun () -> full q)
let rec not_full q =
  if not (is_empty q)
  then Lwt.return_unit
  else (Lwt_condition.wait q.not_full >>= fun () -> not_full q)

exception Closed

let rec push ({ closed ; queue ; full } as q) elt =
  let len = Queue.length queue in
  if closed then Lwt.fail Closed
  else if available_space q len then begin
    Queue.push elt queue ;
    notify_push q ;
    (if not (available_space q (len + 1)) then Lwt_condition.signal full ());
    Lwt.return_unit
  end else
    wait_pop q >>= fun () ->
    push q elt

let rec push_now ({ closed ; queue ; full } as q) elt =
  if closed then raise Closed ;
  let len = Queue.length queue in
  available_space q len && begin
    Queue.push elt queue ;
    notify_push q ;
    (if not (available_space q (len + 1)) then Lwt_condition.signal full ()) ;
    true
  end

exception Full

let push_now_exn q elt =
  if not (push_now q elt) then raise Full

let rec pop_all ({ closed ; queue ; empty ; not_full } as q) =
  let was_full = is_full q in
  if not (Queue.is_empty queue) then
    let queue_copy = Queue.copy queue in
    Queue.clear queue;
    notify_pop q ;
    (if was_full then Lwt_condition.signal not_full ());
    Lwt_condition.signal empty ();
    Lwt.return queue_copy
  else if closed then
    Lwt.fail Closed
  else
    wait_push q >>= fun () ->
    pop_all q

let rec pop ({ closed ; queue ; empty ; not_full } as q) =
  let was_full = is_full q in
  if not (Queue.is_empty queue) then
    let elt = Queue.pop queue in
    notify_pop q ;
    (if was_full then Lwt_condition.signal not_full ());
    (if Queue.length queue = 0 then Lwt_condition.signal empty ());
    Lwt.return elt
  else if closed then
    Lwt.fail Closed
  else
    wait_push q >>= fun () ->
    pop q

let rec peek ({ closed ; queue } as q) =
  if not (Queue.is_empty queue) then
    let elt = Queue.peek queue in
    Lwt.return elt
  else if closed then
    Lwt.fail Closed
  else
    wait_push q >>= fun () ->
    peek q

exception Empty

let pop_now_exn ({ closed ; queue ; empty ; not_full } as q) =
  let was_full = is_full q in
  if Queue.is_empty queue then
    (if closed then raise Closed else raise Empty) ;
  let elt = Queue.pop queue in
  (if was_full then Lwt_condition.signal not_full ());
  (if Queue.length queue = 0 then Lwt_condition.signal empty ());
  notify_pop q ;
  elt

let pop_all_now ({ closed ; queue ; empty ; not_full } as q) =
  let was_empty = is_empty q in
  let was_full = is_full q in
  if Queue.is_empty queue then
    (if closed then raise Closed else raise Empty) ;
  let queue_copy = Queue.copy queue in
  Queue.clear queue ;
  (if was_full then Lwt_condition.signal not_full ());
  (if not was_empty then Lwt_condition.signal empty ());
  notify_pop q ;
  queue_copy

let pop_now q =
  match pop_now_exn q with
  | exception Empty -> None
  | elt -> Some elt

let rec values_available q =
  if is_empty q then
    if q.closed then
      raise Closed
    else
      wait_push q >>= fun () ->
      values_available q
  else
    Lwt.return_unit

let close q =
  if not q.closed then begin
    q.closed <- true ;
    notify_push q ;
    notify_pop q ;
    Lwt_condition.broadcast_exn q.full Closed ;
  end

let rec iter q ~f =
  Lwt.catch begin fun () ->
    pop q >>= fun elt ->
    f elt >>= fun () ->
    iter q ~f
  end begin function
    | Closed -> Lwt.return_unit
    | exn -> Lwt.fail exn
  end

