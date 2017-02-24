(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type fitness = MBytes.t list


(* Fitness comparison:
   - shortest lists are smaller ;
   - lexicographical order for lists of the same length. *)
let compare_bytes b1 b2 =
  let len1 = MBytes.length b1 in
  let len2 = MBytes.length b2 in
  let c = compare len1 len2 in
  if c <> 0
  then c
  else
    let rec compare_byte b1 b2 pos len =
      if pos = len
      then 0
      else
        let c = compare (MBytes.get_char b1 pos) (MBytes.get_char b2 pos) in
        if c <> 0
        then c
        else compare_byte b1 b2 (pos+1) len
    in
    compare_byte b1 b2 0 len1

let compare f1 f2 =
  let rec compare_rec f1 f2 = match f1, f2 with
    | [], [] -> 0
    | i1 :: f1, i2 :: f2 ->
        let i = compare_bytes i1 i2 in
        if i = 0 then compare_rec f1 f2 else i
    | _, _ -> assert false in
  let len = compare (List.length f1) (List.length f2) in
  if len = 0 then compare_rec f1 f2 else len

let equal f1 f2 = compare f1 f2 = 0

let rec pp fmt = function
  | [] -> ()
  | [f] -> Format.fprintf fmt "%s" (Hex_encode.hex_of_bytes f)
  | f1 :: f -> Format.fprintf fmt "%s::%a" (Hex_encode.hex_of_bytes f1) pp f

let to_string x = Format.asprintf "%a" pp x

let encoding =
  let open Data_encoding in
  describe ~title: "Tezos block fitness"
    (list bytes)
