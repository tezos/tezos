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

open Data_encoding

module S = struct

  let protocols_arg = Protocol_hash.rpc_arg

  let contents =
    RPC_service.get_service
      ~query: RPC_query.empty
      ~output: Protocol.encoding
      RPC_path.(root / "protocols" /: protocols_arg)

  let list =
    RPC_service.get_service
      ~query: RPC_query.empty
      ~output: (list Protocol_hash.encoding)
      RPC_path.(root / "protocols")

  let fetch =
    RPC_service.get_service
      ~description: "Fetch a protocol from the network."
      ~query: RPC_query.empty
      ~output: unit
      RPC_path.(root / "fetch_protocol" /: protocols_arg)

end

open RPC_context
let contents ctxt h =
  make_call1 S.contents ctxt h () ()

let list ctxt =
  make_call S.list ctxt () () ()

let fetch ctxt h =
  make_call1 S.fetch ctxt h () ()
