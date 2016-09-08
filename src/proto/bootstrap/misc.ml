(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a lazyt = unit -> 'a
type 'a lazy_list_t = LCons of 'a * ('a lazy_list_t tzresult Lwt.t lazyt)
type 'a lazy_list = 'a lazy_list_t tzresult Lwt.t

let rec (-->) i j = (* [i; i+1; ...; j-1] *)
  if Compare.Int.(i >= j)
  then []
  else i :: (succ i --> j)

let rec (--->) i j = (* [i; i+1; ...; j-1] *)
  if Compare.Int32.(i >= j)
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
