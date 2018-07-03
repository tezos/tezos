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

let echo_service custom_root =
  RPC_service.post_service
    ~description: "An dummy echo service"
    ~query: RPC_query.empty
    ~input: Data_encoding.(obj1 (req "msg" string))
    ~output: Data_encoding.(obj1 (req "msg" string))
    RPC_path.(custom_root / "echo")

let failing_service custom_root =
  RPC_service.post_service
    ~description: "A failing service"
    ~query: RPC_query.empty
    ~input: Data_encoding.(obj1 (req "arg" int31))
    ~output: Data_encoding.empty
    RPC_path.(custom_root / "failing")

let rpc_services : Updater.rpc_context RPC_directory.t =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register
      dir
      (failing_service RPC_path.open_root)
      (fun _ctxt () x -> Error.demo_error x)
  in
  let dir =
    RPC_directory.register
      dir
      (echo_service RPC_path.open_root)
      (fun _ctxt () x -> return x)
  in
  dir
