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

module Infix = struct

  let (<<) g f = fun a -> g (f a)

  let (--) i j =
    let rec loop acc j =
      if j < i then acc else loop (j :: acc) (pred j) in
    loop [] j

end

let nbsp = Re.(compile (str "\xC2\xA0"))
let display_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline
       (fun ppf line ->
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf w ->
               (* replace &nbsp; by real spaces... *)
               Format.fprintf ppf "%s@ "
                 (Re.replace ~all:true nbsp ~f:(fun _ -> " ") w))
            ppf
            (TzString.split ' ' line)))
    (TzString.split ~dup:false '\n' description)

let finalize f g = try let res = f () in g (); res with exn -> g (); raise exn
