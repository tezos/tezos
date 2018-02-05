(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let split_path path =
  let l = String.length path in
  let rec do_slashes acc i =
    if i >= l then
      List.rev acc
    else if String.get path i = '/' then
      do_slashes acc (i + 1)
    else
      do_component acc i i
  and do_component acc i j =
    if j >= l then
      if i = j then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if String.get path j = '/' then
      do_slashes (String.sub path i (j - i) :: acc) j
    else
      do_component acc i (j + 1) in
  do_slashes [] 0
