(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  name : string ;
  major : int ;
  minor : int ;
}

let pp ppf { name ; major ; minor } =
  Format.fprintf ppf "%s.%d.%d" name major minor

let encoding =
  let open Data_encoding in
  conv
    (fun { name; major; minor } -> (name, major, minor))
    (fun (name, major, minor) -> { name; major; minor })
    (obj3
       (req "name" string)
       (req "major" uint16)
       (req "minor" uint16))

(* the common version for a pair of peers, if any, is the maximum one,
   in lexicographic order *)
let common la lb =
  let la = List.sort (fun l r -> compare r l) la in
  let lb = List.sort (fun l r -> compare r l) lb in
  let rec find = function
    | [], _ | _, [] -> None
    | ((a :: ta) as la), ((b :: tb) as lb) ->
        if a = b then Some a
        else if a < b then find (ta, lb)
        else find (la, tb)
  in find (la, lb)
