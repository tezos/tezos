(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module LC = Lwt_condition
open Logging.Core

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let never_ending = fst (Lwt.wait ())

(* A non exception-based cancelation mechanism. Builds a [cancelation]
   thread to bind / pick on, awoken when a cancelation is requested by
   [cancel ()]. [on_cancel cb] registers a callback to be called at
   cancelation. [cancel ()] finishes when all calbacks have completed
   (sequentially), instantly when called more than once. *)
let canceler ()
  : (unit -> unit Lwt.t) *
    (unit -> unit Lwt.t) *
    ((unit -> unit Lwt.t) -> unit) =
  let cancelation = LC.create () in
  let cancelation_complete = LC.create () in
  let cancel_hook = ref (fun () -> Lwt.return ()) in
  let canceling = ref false and canceled = ref false  in
  let cancel () =
    if !canceled then
      Lwt.return ()
    else if !canceling then
      LC.wait cancelation_complete
    else begin
      canceling := true ;
      LC.broadcast cancelation () ;
      !cancel_hook () >>= fun () ->
      canceled := true ;
      LC.broadcast cancelation_complete () ;
      Lwt.return ()
    end
  in
  let on_cancel cb =
    let hook = !cancel_hook in
    cancel_hook := (fun () -> hook () >>= cb) ;
  in
  let cancelation () =
    if !canceling then Lwt.return ()
    else LC.wait cancelation
  in
  cancelation, cancel, on_cancel

type trigger =
  | Absent
  | Present
  | Waiting of unit Lwt.u

let trigger () : (unit -> unit) * (unit -> unit Lwt.t) =
  let state = ref Absent in
  let trigger () =
    match !state with
    | Absent -> state := Present
    | Present -> ()
    | Waiting u ->
        state := Absent;
        Lwt.wakeup u ()
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, u = Lwt.wait () in
        state := Waiting u;
        waiter
    | Present ->
        state := Absent;
        Lwt.return_unit
    | Waiting u ->
        Lwt.waiter_of_wakener u
  in
  trigger, wait

type 'a queue =
  | Absent
  | Present of 'a list ref
  | Waiting of 'a list Lwt.u

let queue () : ('a -> unit) * (unit -> 'a list Lwt.t) =
  let state = ref Absent in
  let queue v =
    match !state with
    | Absent -> state := Present (ref [v])
    | Present r -> r := v :: !r
    | Waiting u ->
        state := Absent;
        Lwt.wakeup u [v]
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, u = Lwt.wait () in
        state := Waiting u;
        waiter
    | Present r ->
        state := Absent;
        Lwt.return (List.rev !r)
    | Waiting u ->
        Lwt.waiter_of_wakener u
  in
  queue, wait

(* A worker launcher, takes a cancel callback to call upon *)
let worker ?(safe=false) name ~run ~cancel =
  let stop = LC.create () in
  let fail e =
    log_error "%s worker failed with %s" name (Printexc.to_string e) ;
    cancel () >>= fun () ->
    if safe then Lwt.return_unit else Lwt.fail e
  in
  let waiter = LC.wait stop in
  log_info "%s worker started" name ;
  Lwt.async
    (fun () ->
       Lwt.catch run fail >>= fun () ->
       LC.signal stop ();
       Lwt.return ()) ;
  waiter >>= fun () ->
  log_info "%s worker ended" name ;
  Lwt.return ()


let rec chop k l =
  if k = 0 then l else begin
    match l with
    | _::t -> chop (k-1) t
    | _ -> assert false
  end
let stable_sort cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1::t1, h2::t2 ->
        cmp h1 h2 >>= function
        | x when x <= 0 -> rev_merge t1 l2 (h1::accu)
        | _             -> rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> Lwt.return (List.rev_append l2 accu)
    | l1, [] -> Lwt.return (List.rev_append l1 accu)
    | h1::t1, h2::t2 ->
        cmp h1 h2 >>= function
        | x when x > 0 -> rev_merge_rev t1 l2 (h1::accu)
        | _            -> rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ -> begin
        cmp x1 x2 >|= function
        | x when x <= 0 -> [x1; x2]
        | _             -> [x2; x1]
      end
    | 3, x1 :: x2 :: x3 :: _ -> begin
        cmp x1 x2 >>= function
        | x when x <= 0 -> begin
            cmp x2 x3 >>= function
            | x when x <= 0 -> Lwt.return [x1; x2; x3]
            | _ -> cmp x1 x3 >|= function
              | x when x <= 0 -> [x1; x3; x2]
              | _ -> [x3; x1; x2]
          end
        | _ -> begin
            cmp x1 x3 >>= function
            | x when x <= 0 -> Lwt.return [x2; x1; x3]
            | _ -> cmp x2 x3 >|= function
              | x when x <= 0 -> [x2; x3; x1]
              | _ -> [x3; x2; x1]
          end
      end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       rev_sort n1 l >>= fun s1 ->
       rev_sort n2 l2 >>= fun s2 ->
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ -> begin
        cmp x1 x2 >|= function
        | x when x > 0 -> [x1; x2]
        | _ -> [x2; x1]
      end
    | 3, x1 :: x2 :: x3 :: _ -> begin
        cmp x1 x2 >>= function
        | x when x > 0 -> begin
            cmp x2 x3 >>= function
            | x when x > 0 -> Lwt.return [x1; x2; x3]
            | _ ->
                cmp x1 x3 >|= function
                | x when x > 0 -> [x1; x3; x2]
                | _ -> [x3; x1; x2]
          end
        | _ -> begin
            cmp x1 x3 >>= function
            | x when x > 0 -> Lwt.return [x2; x1; x3]
            | _ ->
                cmp x2 x3 >|= function
                | x when x > 0 -> [x2; x3; x1]
                | _ -> [x3; x2; x1]
          end
      end
    | n, l ->
        let n1 = n asr 1 in
        let n2 = n - n1 in
        let l2 = chop n1 l in
        sort n1 l >>= fun s1 ->
        sort n2 l2 >>= fun s2 ->
        rev_merge s1 s2 []
  in
  let len = List.length l in
  if len < 2 then Lwt.return l else sort len l

let sort = stable_sort
