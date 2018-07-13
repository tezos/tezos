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

let dump_file oc file =
  let ic = open_in file in
  let buflen = 8096 in
  let buf = Bytes.create buflen in
  let rec loop () =
    let len = input ic buf 0 buflen in
    if len <> 0 then begin
      Printf.fprintf oc "%s" (Bytes.to_string (if len = buflen then buf else Bytes.sub buf 0 len)) ;
      loop ()
    end
  in
  loop () ;
  close_in ic

let opened_modules = [
  "Pervasives" ;
  "Error_monad" ;
]

let include_mli oc file =
  let unit =
    String.capitalize_ascii
      (Filename.chop_extension (Filename.basename file)) in
  Printf.fprintf oc "module %s : sig\n" unit ;
  Printf.fprintf oc "# 1 %S\n" file ;
  dump_file oc file ;
  Printf.fprintf oc "end\n" ;
  if unit = "Result" then
    Printf.fprintf oc
      "type ('a, 'b) result = ('a, 'b) Result.result = \
      \ Ok of 'a | Error of 'b\n" ;
  if List.mem unit opened_modules then Printf.fprintf oc "open %s\n" unit

let () =
  Printf.fprintf stdout "module type T = sig\n" ;
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    include_mli stdout file ;
  done ;
  Printf.fprintf stdout "end\n%!"
