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

type 'a lazyt = unit -> 'a
type 'a lazy_list_t = LCons of 'a * ('a lazy_list_t tzresult Lwt.t lazyt)
type 'a lazy_list = 'a lazy_list_t tzresult Lwt.t

let rec (-->) i j = (* [i; i+1; ...; j] *)
  if Compare.Int.(i > j)
  then []
  else i :: (succ i --> j)

let rec (--->) i j = (* [i; i+1; ...; j] *)
  if Compare.Int32.(i > j)
  then []
  else i :: (Int32.succ i ---> j)

let split delim ?(limit = max_int) path =
  let l = String.length path in
  let rec do_slashes acc limit i =
    if Compare.Int.(i >= l) then
      List.rev acc
    else if Compare.Char.(String.get path i = delim) then
      do_slashes acc limit (i + 1)
    else
      do_split acc limit i
  and do_split acc limit i =
    if Compare.Int.(limit <= 0) then
      if Compare.Int.(i = l) then
        List.rev acc
      else
        List.rev (String.sub path i (l - i) :: acc)
    else
      do_component acc (pred limit) i i
  and do_component acc limit i j =
    if Compare.Int.(j >= l) then
      if Compare.Int.(i = j) then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if Compare.Char.(String.get path j = delim) then
      do_slashes (String.sub path i (j - i) :: acc) limit j
    else
      do_component acc limit i (j + 1) in
  if Compare.Int.(limit > 0) then
    do_slashes [] limit 0
  else
    [ path ]

let pp_print_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
    (split ' ' description)

let take n l =
  let rec loop acc n = function
    | xs when Compare.Int.(n <= 0) -> Some (List.rev acc, xs)
    | [] -> None
    | x :: xs -> loop (x :: acc) (n-1) xs in
  loop [] n l

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if Compare.Int.(n >= x) && Compare.String.(String.sub s 0 x = prefix) then
    Some (String.sub s x (n - x))
  else
    None

let rec remove_elem_from_list nb = function
  | [] -> []
  | l when Compare.Int.(nb <= 0) -> l
  | _ :: tl -> remove_elem_from_list (nb - 1) tl
