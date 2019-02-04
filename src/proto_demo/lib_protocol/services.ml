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

module S = struct

  let path = RPC_path.open_root

  let echo_service =
    RPC_service.post_service
      ~description: "A dummy echo service"
      ~query: RPC_query.empty
      ~input: Data_encoding.(obj1 (req "msg" string))
      ~output: Data_encoding.(obj1 (req "msg" string))
      RPC_path.(path / "echo")

  let failing_service =
    RPC_service.post_service
      ~description: "A failing service"
      ~query: RPC_query.empty
      ~input: Data_encoding.(obj1 (req "arg" int31))
      ~output: Data_encoding.empty
      RPC_path.(path / "failing")

end

let echo ctxt block msg =
  RPC_context.make_call0 S.echo_service ctxt block () msg

let failing ctxt block n =
  RPC_context.make_call0 S.failing_service ctxt block () n

let rpc_services : Updater.rpc_context RPC_directory.t =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register
      dir
      S.failing_service
      (fun _ctxt () x -> Error.demo_error x)
  in
  let dir =
    RPC_directory.register
      dir
      S.echo_service
      (fun _ctxt () x -> return x)
  in
  dir
