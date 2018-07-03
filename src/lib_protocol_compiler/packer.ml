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
      Printf.fprintf oc "%s"
        (if len = buflen then Bytes.unsafe_to_string buf else Bytes.sub_string buf 0 len) ;
      loop ()
    end
  in
  loop () ;
  close_in ic

let include_ml oc file =
  let unit =
    String.capitalize_ascii
      (Filename.chop_extension (Filename.basename file)) in
  (* FIXME insert .mli... *)
  Printf.fprintf oc "module %s " unit ;
  if Sys.file_exists (file ^ "i") then begin
    Printf.fprintf oc ": sig\n" ;
    Printf.fprintf oc "# 1 %S\n" (file ^ "i");
    dump_file oc (file ^ "i") ;
    Printf.fprintf oc "end " ;
  end ;
  Printf.fprintf oc "= struct\n" ;
  Printf.fprintf oc "# 1 %S\n" file ;
  dump_file oc file ;
  Printf.fprintf oc "end\n%!"

let opened_modules = [
  "Tezos_protocol_environment" ;
  "Pervasives" ;
  "Error_monad" ;
  "Logging" ;
]

let dump oc hash files =
  Printf.fprintf oc
    "module Make (Tezos_protocol_environment : Tezos_protocol_environment_sigs__V1.T) = struct\n" ;
  Printf.fprintf oc "[@@@ocaml.warning \"-33\"]\n" ;
  List.iter (Printf.fprintf oc "open %s\n") opened_modules ;
  Printf.fprintf oc "[@@@ocaml.warning \"+33\"]\n" ;
  Printf.fprintf oc "let hash = Protocol_hash.of_b58check_exn %S;;\n"
    (Protocol_hash.to_b58check hash) ;
  for i = 0 to Array.length files - 1 do
    include_ml oc files.(i) ;
  done ;
  Printf.fprintf oc "  include %s\n"
    (String.capitalize_ascii
       (Filename.basename
          (Filename.chop_extension files.(Array.length files - 1)))) ;
  Printf.fprintf oc "end\n%!"
