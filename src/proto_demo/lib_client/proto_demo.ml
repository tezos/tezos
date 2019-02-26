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

module Name = struct let name = "demo" end
module Demo_environment = Tezos_protocol_environment_faked.MakeV1(Name)()
module Proto = Tezos_protocol_demo.Functor.Make(Demo_environment)
module Demo_block_services = Block_services.Make(Proto)(Proto)
include Proto

class type rpc_context = object
  inherit RPC_context.json
  inherit [Shell_services.chain * Shell_services.block] Demo_environment.RPC_context.simple
end

class type full = object
  inherit Client_context.full
  inherit [Shell_services.chain * Shell_services.block] Demo_environment.RPC_context.simple
end

class wrap_full (t : Client_context.full) : full = object
  inherit Client_context.proxy_context t
  inherit [Shell_services.chain, Shell_services.block] Demo_environment.proto_rpc_context
      (t :> RPC_context.t)
      Shell_services.Blocks.path
end