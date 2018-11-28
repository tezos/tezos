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

let protocols = [
  "Alpha", "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" ;
]

let main _node =
  (* Style : hack *)
  Format.printf "%a@." Rst.pp_raw_html Rst.style ;
  (* Script : hack *)
  Format.printf "%a@." Rst.pp_raw_html Rst.script ;
  (* Page title *)
  Format.printf "%a" Rst.pp_h1 "P2P message format" ;
  (* include/copy usage.rst from input  *)
  let rec loop () =
    let s = read_line () in
    Format.printf "%s@\n" s ;
    loop () in
  begin try loop () with End_of_file -> () end ;
  Format.printf "@\n" ;
  (* Data *)
  Format.printf "%a@\n@\n%a@\n@."
    Rst.pp_h2 "Block header (shell)"
    Data_encoding.Binary_schema.pp
    (Data_encoding.Binary.describe Block_header.encoding) ;
  Format.printf "%a@\n@\n%a@\n@."
    Rst.pp_h2 "Operation (shell)"
    Data_encoding.Binary_schema.pp
    (Data_encoding.Binary.describe Operation.encoding) ;
  List.iter
    (fun (_name, hash) ->
       let hash = Protocol_hash.of_b58check_exn hash in
       let (module Proto) = Registered_protocol.get_exn hash in
       Format.printf "%a@\n@\n%a@\n@."
         Rst.pp_h2 "Block_header (alpha-specific)"
         Data_encoding.Binary_schema.pp
         (Data_encoding.Binary.describe Proto.block_header_data_encoding) ;
       Format.printf "%a@\n@\n%a@\n@."
         Rst.pp_h2 "Operation (alpha-specific)"
         Data_encoding.Binary_schema.pp
         (Data_encoding.Binary.describe Proto.operation_data_encoding) ;
    )
    protocols ;
  return ()

let () =
  Lwt_main.run (Node_helpers.with_node main)
