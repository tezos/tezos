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

open Alpha_context

let custom_root =
  (RPC_path.(open_root / "context" / "constants") : RPC_context.t RPC_path.context)

module S = struct

  open Data_encoding

  let errors =
    RPC_service.get_service
      ~description: "Schema for all the RPC errors from this protocol version"
      ~query: RPC_query.empty
      ~output: json_schema
      RPC_path.(custom_root / "errors")

  let all =
    RPC_service.get_service
      ~description: "All constants"
      ~query: RPC_query.empty
      ~output: Alpha_context.Constants.encoding
      custom_root

end

let register () =
  let open Services_registration in
  register0_noctxt S.errors begin fun () () ->
    return (Data_encoding.Json.(schema error_encoding))
  end ;
  register0 S.all begin fun ctxt () () ->
    let open Constants in
    return { fixed = fixed ;
             parametric = parametric ctxt }
  end

let errors ctxt block =
  RPC_context.make_call0 S.errors ctxt block () ()
let all ctxt block =
  RPC_context.make_call0 S.all ctxt block () ()
