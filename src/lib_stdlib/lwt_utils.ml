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

module LC = Lwt_condition

open Lwt.Infix

let may ~f = function
  | None -> Lwt.return_unit
  | Some x -> f x

let never_ending () = fst (Lwt.wait ())

type trigger =
  | Absent
  | Present
  | Waiting of unit Lwt.t * unit Lwt.u

let trigger () : (unit -> unit) * (unit -> unit Lwt.t) =
  let state = ref Absent in
  let trigger () =
    match !state with
    | Absent -> state := Present
    | Present -> ()
    | Waiting (_waiter, wakener) ->
        state := Absent;
        Lwt.wakeup wakener ()
  in
  let wait () =
    match !state with
    | Absent ->
        let waiter, wakener = Lwt.wait () in
        state := Waiting (waiter, wakener) ;
        waiter
    | Present ->
        state := Absent;
        Lwt.return_unit
    | Waiting (waiter, _wakener)  ->
        waiter
  in
  trigger, wait

(* A worker launcher, takes a cancel callback to call upon *)
let worker name ~on_event ~run ~cancel =
  let stop = LC.create () in
  let fail e =
    on_event name
      (`Failed (Printf.sprintf "Exception: %s" (Printexc.to_string e)))
    >>= fun () ->
    cancel ()
  in
  let waiter = LC.wait stop in
  on_event name `Started >>= fun () ->
  Lwt.async
    (fun () ->
       Lwt.catch run fail >>= fun () ->
       LC.signal stop ();
       Lwt.return_unit) ;
  waiter >>= fun () ->
  on_event name `Ended >>= fun () ->
  Lwt.return_unit


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

let unless cond f =
  if cond then Lwt.return_unit else f ()


