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

open Pipeline
open Lwt.Infix
let fail pp v exp =
  let open Format in
  let pp_print_result pv pe fmt = function
    | Ok v -> Format.fprintf fmt "Ok %a" pv v
    | Error e -> Format.fprintf fmt "Error %a" pe e
  in
  let pp_print_exc fmt exc =
    Format.fprintf fmt "%s" (Printexc.to_string exc) in
  kasprintf Pervasives.failwith "Got [%a], expected [%a]"
    (pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ';') (pp_print_result pp pp_print_exc)) v
    (pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ';') (pp_print_result pp pp_print_exc)) exp
let expect pp v vv =
  if v = vv then
    Lwt.return_unit
  else
    fail pp v vv

let wrap (s, f) =
  Alcotest_lwt.test_case s `Quick (fun _ () -> f ())


(* Some helper functions *)
let rec stall n =
  if n <= 0 then
    Lwt.return_unit
  else
    Lwt_main.yield () >>= fun () ->
    stall (n - 1)

let req () =
  stall (Random.int 5) >>= fun () ->
  Lwt.return (Random.int 128)


module Order = struct

  let tests =
    List.map wrap
      [
        ("s-two-values", fun () ->
            run
              (cons (async_s Lwt.return) nil)
              [1 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 5]
        ) ;
        ("p-two-values", fun () ->
            run
              (cons (async_p Lwt.return) nil)
              [1 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 5]
        ) ;
        ("s-many-values", fun () ->
            run
              (cons (async_s Lwt.return) nil)
              [1 ; 2 ; 1 ; 2 ; 5 ; 6 ; 7 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 2 ; Ok 1 ; Ok 2 ; Ok 5 ; Ok 6 ; Ok 7 ; Ok 5]
        ) ;
        ("p-many-values", fun () ->
            run
              (cons (async_p Lwt.return) nil)
              [1 ; 2 ; 1 ; 2 ; 5 ; 6 ; 7 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 2 ; Ok 1 ; Ok 2 ; Ok 5 ; Ok 6 ; Ok 7 ; Ok 5]
        ) ;
        ("s-stalls", fun () ->
            run
              (cons (async_s (fun i -> stall i >>= fun () -> Lwt.return i)) nil)
              [1 ; 2 ; 1 ; 2 ; 5 ; 6 ; 7 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 2 ; Ok 1 ; Ok 2 ; Ok 5 ; Ok 6 ; Ok 7 ; Ok 5]
        ) ;
        ("p-stalls", fun () ->
            run
              (cons (async_p (fun i -> stall i >>= fun () -> Lwt.return i)) nil)
              [1 ; 2 ; 1 ; 2 ; 5 ; 6 ; 7 ; 5]
            >>= expect Format.pp_print_int [Ok 1 ; Ok 2 ; Ok 1 ; Ok 2 ; Ok 5 ; Ok 6 ; Ok 7 ; Ok 5]
        ) ;
      ]

end

module Many_passes = struct

  let f1 (n, tok) =
    stall n >>= fun () ->
    Lwt.return (n * 256, tok)
  let f2 (n, tok) =
    Lwt.return (n, tok)
  let f3 (i, tok) =
    req () >>= fun j ->
    req () >>= fun k ->
    Lwt.return (i + j, i + k, tok)
  let f4 (i, j, tok) =
    Lwt.return (i + j + 2, tok)
  let f5 (i, j, tok) =
    if i > j then
      req () >>= fun (_: int) ->
      Lwt.return tok
    else
      Lwt.return tok

  let pipe =
    let (@) = cons in
    async_p f1 @ async_s f2 @
    async_s f3 @ async_p f4 @
    async_p f1 @ async_s f2 @
    async_s f3 @ async_p f4 @
    async_s f3 @ async_p f5 @ nil

  let tests =
    List.map wrap
      [
        ("many-passes", fun () ->
            run pipe
              [ (1, "a"); (2, "b"); (1, "c"); (3, "d"); (1, "e") ] >>= function
            | [ Ok "a"; Ok "b"; Ok "c"; Ok "d"; Ok "e"; ] -> Lwt.return_unit
            | _ -> Format.kasprintf Pervasives.failwith "non identical output"
        ) ;
      ]

end
