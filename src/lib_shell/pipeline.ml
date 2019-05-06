(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type limiter = {
  mutable current : int ;
  max : int ;
}
let with_limiter limiter f v =
  if limiter.current <= limiter.max then begin
    limiter.current <- limiter.current + 1 ;
    let p = f v in
    Lwt.on_termination p (fun () -> limiter.current <- limiter.current - 1) ;
    Some p
  end else
    None
let limiter = function
  | None -> { current = 0 ; max = max_int ; }
  | Some max -> { current = 0 ; max ; }


type ('a, 'b) step =
  | Async_p of ('a -> 'b Lwt.t)
  | Async_s of ('a -> 'b Lwt.t)
  | Sync of ('a -> 'b)
let sync f = Sync f
let async_p f = Async_p f
let async_s f = Async_s f

let all_ok = Sync (fun v -> Ok v)
let map_in_err m = function
  | Sync f ->
      Sync (function
          | Ok _ as ok -> f ok
          | Error e -> f (Error (m e)))
  | Async_s f ->
      Async_s (function
          | Ok _ as ok -> f ok
          | Error e -> f (Error (m e)))
  | Async_p f ->
      Async_p (function
          | Ok _ as ok -> f ok
          | Error e -> f (Error (m e)))
let map_out_err m = function
  | Sync f ->
      Sync (fun x -> match f x with
          | Ok _ as ok -> ok
          | Error e -> Error (m e))
  | Async_s f ->
      Async_s (fun x -> f x >>= function
        | Ok _ as ok -> Lwt.return ok
        | Error e -> Lwt.return_error (m e))
  | Async_p f ->
      Async_p (fun x -> f x >>= function
        | Ok _ as ok -> Lwt.return ok
        | Error e -> Lwt.return_error (m e))
let with_err = function
  | Async_s f -> Async_s (function | Ok v -> f v | Error e -> Lwt.return_error e)
  | Async_p f -> Async_p (function | Ok v -> f v | Error e -> Lwt.return_error e)
  | Sync f -> Sync (function | Ok v -> f v | Error e -> Error e)

let recover f =
  Sync (function | Ok v -> v | Error e -> f e)

let with_key = function
  | Async_s f ->
      Async_s (fun (key, a) -> f a >>= fun b -> Lwt.return (key, b))
  | Async_p f ->
      Async_p (fun (key, a) -> f a >>= fun b -> Lwt.return (key, b))
  | Sync f -> Sync (fun (key, a) -> let b = f a in (key, b))
let init_key = Sync (fun x -> (x, x))

type ('i, 'o) pipe =
  | Nil : ('x, 'x) pipe
  | Cons : ('a, 'input) step * ('input, 'output) pipe -> ('a, 'output) pipe
let nil : ('x, 'x) pipe = Nil
let cons
  : ('a, 'b) step -> ('b, 'c) pipe -> ('a, 'c) pipe
  = fun step pipe -> Cons (step, pipe)


(* Instantiated values: values with buffers attached *)
type ('i, 'o) istep =
  | ISync of { f : ('i, exn) result -> ('o, exn) result }
  | IAsync_s of {
      qin : ('i, exn) result Queue.t ;
      f : 'i -> ('o, exn) result Lwt.t ;
      vout : ('o, exn) result Lwt.t option ref ;
    }
  | IAsync_p of {
      qin : ('i, exn) result Queue.t ;
      f : 'i -> ('o, exn) result Lwt.t ;
      qout : ('o, exn) result Lwt.t Queue.t ;
    }

type (_, _) ipipe =
  | ICons : { step : ('a, 'b) istep ; pipe : ('b, 'c) ipipe } -> ('a, 'c) ipipe
  | IEnd : { q : ('i, exn) result Queue.t } -> ('i, 'i) ipipe

let sync_wrap f =
  function
  | Ok v -> (try Ok (f v) with exc -> Error exc)
  | Error _ as e -> e
let wrap f =
  fun v ->
    Lwt.catch
      (fun () -> f v >>= Lwt.return_ok)
      Lwt.return_error

let rec instantiate_pipe
  : type i o. (i, o) pipe -> (i, o) ipipe
  = function
    | Nil -> IEnd { q = Queue.create () }
    | Cons (Sync step, pipe) ->
        let step = ISync { f = sync_wrap step } in
        let pipe = instantiate_pipe pipe in
        ICons { step ; pipe }
    | Cons (Async_s step, pipe) ->
        let step = IAsync_s { qin = Queue.create () ; f = wrap step ; vout = ref None } in
        let pipe = instantiate_pipe pipe in
        ICons { step ; pipe }
    | Cons (Async_p step, pipe) ->
        let step = IAsync_p { qin = Queue.create () ; f = wrap step ; qout = Queue.create () } in
        let pipe = instantiate_pipe pipe in
        ICons { step ; pipe }

let cancel_istep
  : ('i, 'o) istep -> unit
  = function
    | ISync _ -> ()
    | IAsync_s { vout = { contents = None } ; _ } -> ()
    | IAsync_s { vout = { contents = Some p } ; _ } -> Lwt.cancel p
    | IAsync_p { qout ; _ } -> Queue.iter Lwt.cancel qout

let rec cancel_ipipe
  : type i o . (i, o) ipipe -> unit
  = function
    | ICons { step ; pipe } ->
        cancel_istep step ;
        cancel_ipipe pipe
    | IEnd _ -> ()



let wait_for p = (p >>= fun _ -> Lwt.return_unit)

let rec progress_async_s limiter waiters qin f vout data_out =
  match !vout with
  | None -> begin
      if Queue.is_empty qin then
        (data_out, waiters)
      else
        match Queue.peek qin with
        | Error _ as e ->
            progress_async_s limiter waiters qin f vout (e :: data_out)
        | Ok v ->
            match with_limiter limiter f v with
            | Some p ->
                ignore (Queue.pop qin) ;
                vout := Some p ;
                (data_out, wait_for p :: waiters)
            | None ->
                (data_out, waiters)
    end
  | Some p -> begin
      match Lwt.state p with
      | Lwt.Sleep ->
          (data_out, wait_for p :: waiters)
      | Lwt.Return v ->
          vout := None ;
          progress_async_s limiter waiters qin f vout (v :: data_out)
      | Lwt.Fail exc ->
          vout := None ;
          progress_async_s limiter waiters qin f vout (Error exc :: data_out)
    end

let rec make_promises limiter qin f qout =
  if Queue.is_empty qin then
    ()
  else
    match Queue.peek qin with
    | Error _ as e ->
        ignore (Queue.pop qin) ;
        Queue.push (Lwt.return e) qout ;
        make_promises limiter qin f qout
    | Ok v ->
        match with_limiter limiter f v with
        | Some p ->
            ignore (Queue.pop qin) ;
            Queue.push p qout ;
            make_promises limiter qin f qout
        | None ->
            ()
let rec get_resolved_top waiters qout resolved =
  if Queue.is_empty qout then
    (List.rev resolved, waiters)
  else
    let p = Queue.peek qout in
    match Lwt.state p with
    | Lwt.Sleep -> (List.rev resolved, wait_for p :: waiters)
    | Lwt.Return v ->
        ignore (Queue.pop qout) ;
        get_resolved_top waiters qout (v :: resolved)
    | Lwt.Fail exc ->
        ignore (Queue.pop qout) ;
        get_resolved_top waiters qout (Error exc :: resolved)

let progress_async_p limiter waiters qin f qout =
  make_promises limiter qin f qout ;
  get_resolved_top waiters qout []


let rec progress
  : type i o . limiter -> (i, exn) result list -> unit Lwt.t list -> (i, o) ipipe -> unit Lwt.t option
  = fun limiter data waiters pipe ->
    match pipe with
    | ICons { step = ISync { f } ; pipe } ->
        progress limiter (List.map f data) waiters pipe
    | ICons { step = IAsync_s { qin ; f ; vout } ; pipe } ->
        List.iter (fun v -> Queue.push v qin) data ;
        let (data_out, waiters) = progress_async_s limiter waiters qin f vout [] in
        progress limiter data_out waiters pipe
    | ICons { step = IAsync_p { qin ; f ; qout } ; pipe } ->
        List.iter (fun v -> Queue.push v qin) data ;
        let (data_out, waiters) = progress_async_p limiter waiters qin f qout in
        progress limiter data_out waiters pipe
    | IEnd { q } ->
        List.iter (fun v -> Queue.push v q) data ;
        match waiters with
        | [] -> None
        | _ :: _ -> Some (Lwt.join waiters)


let rec get_result
  : type i o . (i, o) ipipe -> (o, exn) result list
  = function
    | ICons { step = ISync _ ; pipe } ->
        get_result pipe
    | ICons { step = IAsync_s { vout ; _ } ; pipe } ->
        assert (!vout = None) ;
        get_result pipe
    | ICons { step = IAsync_p { qout ; _ } ; pipe } ->
        assert (Queue.is_empty qout) ;
        get_result pipe
    | IEnd { q } ->
        let rec mk_list acc =
          if Queue.is_empty q then
            List.rev acc
          else
            mk_list (Queue.pop q :: acc)
        in
        mk_list []

let instantiate
  : ('input, 'output) pipe -> ('input, 'output) ipipe
  = fun pipe ->
    instantiate_pipe pipe

let run
  : limiter -> ('input, 'output) ipipe -> 'input list -> ('output, exn) result list Lwt.t
  = fun limiter ipipe input ->
    let rec loop pipe =
      match progress limiter [] [] pipe with
      | Some wait ->
          wait >>= fun () ->
          loop pipe
      | None ->
          Lwt.return (get_result ipipe)
    in
    match progress limiter (List.map (fun v -> Ok v) input) [] ipipe with
    | None ->
        Lwt.return (get_result ipipe)
    | Some wait ->
        wait >>= fun () ->
        loop ipipe

let rec separate vs errs excs = function
  | [] -> (List.rev vs, List.rev errs, List.rev excs)
  | (Ok (Ok v)) :: res -> separate (v :: vs) errs excs res
  | (Ok (Error err)) :: res -> separate vs (err :: errs) excs res
  | (Error exc) :: res -> separate vs errs (exc :: excs) res
let partition_by_error res = separate [] [] [] res

let index_by_key vs empty add =
  List.fold_left
    (fun (m, excs) -> function
       | Ok (k, v) -> (add k v m, excs)
       | Error exc -> (m, exc :: excs))
    (empty, [])
    vs

let run ?pool pipe input =
  let limiter = limiter pool in
  let ipipe = instantiate pipe in
  let p = run limiter ipipe input in
  Lwt.on_cancel p (fun () -> cancel_ipipe ipipe) ;
  p
