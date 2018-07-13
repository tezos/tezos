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

open Error_monad

include Resto_directory.Make(RPC_encoding)

let gen_register dir service handler =
  register dir service
    (fun p q i ->
       Lwt.catch
         (fun () -> handler p q i)
         (function
           | Not_found -> RPC_answer.not_found
           | exn -> RPC_answer.fail [Exn exn]))

let gen_register =
  (gen_register
   : _ -> _ -> (_ -> _ -> _ -> _ RPC_answer.t Lwt.t) -> _
   :> _ -> _ -> (_ -> _ -> _ -> [< _ RPC_answer.t ] Lwt.t) -> _)

let register dir service handler =
  gen_register dir service
    (fun p q i ->
       handler p q i >>= function
       | Ok o -> RPC_answer.return o
       | Error e -> RPC_answer.fail e)

let opt_register dir service handler =
  gen_register dir service
    (fun p q i ->
       handler p q i >>= function
       | Ok (Some o) -> RPC_answer.return o
       | Ok None -> RPC_answer.not_found
       | Error e -> RPC_answer.fail e)

let lwt_register dir service handler =
  gen_register dir service
    (fun p q i ->
       handler p q i >>= fun o ->
       RPC_answer.return o)

open Curry

let register0 root s f = register root s (curry Z f)
let register1 root s f = register root s (curry (S Z) f)
let register2 root s f = register root s (curry (S (S Z)) f)
let register3 root s f = register root s (curry (S (S (S Z))) f)
let register4 root s f = register root s (curry (S (S (S (S Z)))) f)
let register5 root s f = register root s (curry (S (S (S (S (S Z))))) f)

let opt_register0 root s f = opt_register root s (curry Z f)
let opt_register1 root s f = opt_register root s (curry (S Z) f)
let opt_register2 root s f = opt_register root s (curry (S (S Z)) f)
let opt_register3 root s f = opt_register root s (curry (S (S (S Z))) f)
let opt_register4 root s f = opt_register root s (curry (S (S (S (S Z)))) f)
let opt_register5 root s f = opt_register root s (curry (S (S (S (S (S Z))))) f)

let gen_register0 root s f = gen_register root s (curry Z f)
let gen_register1 root s f = gen_register root s (curry (S Z) f)
let gen_register2 root s f = gen_register root s (curry (S (S Z)) f)
let gen_register3 root s f = gen_register root s (curry (S (S (S Z))) f)
let gen_register4 root s f = gen_register root s (curry (S (S (S (S Z)))) f)
let gen_register5 root s f = gen_register root s (curry (S (S (S (S (S Z))))) f)

let lwt_register0 root s f = lwt_register root s (curry Z f)
let lwt_register1 root s f = lwt_register root s (curry (S Z) f)
let lwt_register2 root s f = lwt_register root s (curry (S (S Z)) f)
let lwt_register3 root s f = lwt_register root s (curry (S (S (S Z))) f)
let lwt_register4 root s f = lwt_register root s (curry (S (S (S (S Z)))) f)
let lwt_register5 root s f = lwt_register root s (curry (S (S (S (S (S Z))))) f)
