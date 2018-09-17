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

let common la lb =
  let la = List.sort (fun l r -> compare r l) la in
  let lb = List.sort (fun l r -> compare r l) lb in
  let rec find = function
    | [], _ | _, [] -> None
    | ((a :: ta) as la), ((b :: tb) as lb) ->
        if a = b then Some a
        else if a > b then find (ta, lb)
        else find (la, tb)
  in find (la, lb)

let best lv =
  if lv = [] then
    invalid_arg "P2p_version.best"
  else
    List.hd (List.sort (fun l r -> compare r l) lv)
