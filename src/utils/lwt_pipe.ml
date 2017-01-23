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
  { queue : (int * 'a) Queue.t ;
    mutable current_size : int ;
    max_size : int ;
    compute_size : ('a -> int) ;
    mutable closed : bool ;
    mutable push_waiter : (unit Lwt.t * unit Lwt.u) option ;
    mutable pop_waiter : (unit Lwt.t * unit Lwt.u) option ;
    empty: unit Lwt_condition.t ;
  }

let create ?size () =
  let max_size, compute_size = match size with
    | None -> max_int, (fun _ -> 0)
    | Some (max_size, compute_size) ->
        max_size, (fun e -> 4 * (Sys.word_size / 8) + compute_size e) in
  { queue = Queue.create () ;
    current_size = 0 ;
    max_size ;
    compute_size ;
    closed = false ;
    push_waiter = None ;
    pop_waiter = None ;
    empty = Lwt_condition.create () ;
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

let length { queue } = Queue.length queue
let is_empty { queue } = Queue.is_empty queue

let rec empty q =
  if is_empty q
  then Lwt.return_unit
  else (Lwt_condition.wait q.empty >>= fun () -> empty q)

exception Closed

let rec push ({ closed ; queue ; current_size ;
                max_size ; compute_size} as q) elt =
  if closed then Lwt.fail Closed
  else
    let elt_size = compute_size elt in
  if current_size + elt_size < max_size then begin
    Queue.push (elt_size, elt) queue ;
    q.current_size <- current_size + elt_size ;
    notify_push q ;
    Lwt.return_unit
  end
  else
    wait_pop q >>= fun () ->
    push q elt

let rec push_now ({ closed ; queue ; compute_size ;
                    current_size ; max_size
                  } as q) elt =
  if closed then raise Closed ;
  let elt_size = compute_size elt in
  (current_size + elt_size < max_size)
  && begin
    Queue.push (elt_size, elt) queue ;
    q.current_size <- current_size + elt_size ;
    notify_push q ;
    true
  end

exception Full

let push_now_exn q elt =
  if not (push_now q elt) then raise Full

let rec pop ({ closed ; queue ; empty ; current_size } as q) =
  if not (Queue.is_empty queue) then
    let (elt_size, elt) = Queue.pop queue in
    notify_pop q ;
    q.current_size <- current_size - elt_size ;
    (if Queue.length queue = 0 then Lwt_condition.signal empty ());
    Lwt.return elt
  else if closed then
    Lwt.fail Closed
  else
    wait_push q >>= fun () ->
    pop q

let rec peek ({ closed ; queue } as q) =
  if not (Queue.is_empty queue) then
    let (_elt_size, elt) = Queue.peek queue in
    Lwt.return elt
  else if closed then
    Lwt.fail Closed
  else
    wait_push q >>= fun () ->
    peek q

exception Empty

let pop_now_exn ({ closed ; queue ; empty ; current_size } as q) =
  if Queue.is_empty queue then
    (if closed then raise Closed else raise Empty) ;
  let (elt_size, elt) = Queue.pop queue in
  (if Queue.length queue = 0 then Lwt_condition.signal empty ());
  q.current_size <- current_size - elt_size ;
  notify_pop q ;
  elt

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

