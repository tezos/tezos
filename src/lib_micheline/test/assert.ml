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

(* Mini compatibility layer to avoid circular dependency *)
module Compat = struct
  let failwith fmt = Format.kasprintf (fun s -> Lwt.return_error s) fmt
  let return_unit = Lwt.return_ok ()
  let (>>=) = Lwt.bind
  let (>>=?) v f =
    v >>= function
    | Error _ as err -> Lwt.return err
    | Ok v -> f v
  let rec iter2_p f l1 l2 =
    match l1, l2 with
    | [], [] -> return_unit
    | [], _ | _, [] -> invalid_arg "Error_monad.iter2_p"
    | x1 :: l1 , x2 :: l2 ->
        let tx = f x1 x2 and tl = iter2_p f l1 l2 in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match tx_res, tl_res with
        | Ok (), Ok () -> Lwt.return_ok ()
        | Error exn1, Error exn2 -> failwith "%s -- %s" exn1 exn2
        | Ok (), Error exn
        | Error exn, Ok () -> Lwt.return_error exn
end

open Compat

let fail loc printer given expected msg =
  failwith
    "@[<v 2> On %s : %s@ @[Given:\t%a@]@ @[Expected:\t%a@]@]"
    loc msg printer given printer expected

let default_printer fmt _ = Format.fprintf fmt ""

let equal ~loc ?(eq=(=)) ?(printer=default_printer) ?(msg="") given expected =
  if not (eq given expected) then
    fail loc printer given expected msg
  else
    return_unit

let not_equal ~loc ?(eq=(=)) ?(printer=default_printer) ?(msg="") given expected =
  if eq given expected then
    fail loc printer given expected msg
  else
    return_unit

let pp_tokens fmt tokens =
  let token_value_printer fmt token_value =
    Format.fprintf fmt "@[%s@]"
      (let open Micheline_parser in
       match token_value with
         String s -> Format.sprintf "String %S" s
       | Bytes s ->  Format.sprintf "Bytes %S" s
       | Int s -> Format.sprintf "Int %S" s
       | Ident s -> Format.sprintf "Ident %S" s
       | Annot s -> Format.sprintf "Annot %S" s
       | Comment s -> Format.sprintf "Comment %S" s
       | Eol_comment s -> Format.sprintf "Eol_comment %S" s
       | Semi -> Format.sprintf "Semi"
       | Open_paren -> Format.sprintf "Open_paren"
       | Close_paren -> Format.sprintf "Close_paren"
       | Open_brace -> Format.sprintf "Open_brace"
       | Close_brace -> Format.sprintf "Close_brace"
      ) in
  Format.fprintf fmt "%a"
    (Format.pp_print_list token_value_printer)
    tokens

let equal_tokens ~loc given expected =
  equal ~loc ~eq:(=) ~printer:pp_tokens ~msg:"Tokens are not equal" given expected

let not_equal_tokens ~loc given expected =
  not_equal ~loc ~eq:(=) ~printer:pp_tokens ~msg:"Tokens are equal" given expected
