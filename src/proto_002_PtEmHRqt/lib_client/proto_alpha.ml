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

module Name = struct let name = "alpha" end
module T = Tezos_protocol_environment.Make(Tezos_storage.Context)
module Alpha_environment = T.MakeV1(Name)()

module Proto = Tezos_protocol_alpha.Functor.Make(Alpha_environment)
module Alpha_block_services = Block_services.Make(Proto)(Proto)

include Proto
module LiftedMain = Alpha_environment.Lift(Proto)

class type rpc_context = object
  inherit RPC_context.json
  inherit [Shell_services.chain * Shell_services.block] Alpha_environment.RPC_context.simple
end

class wrap_proto_context (t : RPC_context.json) : rpc_context = object
  method base : Uri.t = t#base
  method generic_json_call = t#generic_json_call
  method call_service : 'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    'p -> 'q -> 'i -> 'o tzresult Lwt.t= t#call_service
  method call_streamed_service : 'm 'p 'q 'i 'o.
    ([< Resto.meth ] as 'm, unit, 'p, 'q, 'i, 'o) RPC_service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i -> (unit -> unit) tzresult Lwt.t = t#call_streamed_service
  inherit [Shell_services.chain,
           Shell_services.block] Alpha_environment.proto_rpc_context
      (t :> RPC_context.t)
      Shell_services.Blocks.path
end

class type full = object
  inherit Client_context.full
  inherit [Shell_services.chain * Shell_services.block] Alpha_environment.RPC_context.simple
end

class wrap_full (t : Client_context.full) : full = object
  inherit Client_context.proxy_context t
  inherit [Shell_services.chain, Shell_services.block] Alpha_environment.proto_rpc_context
      (t :> RPC_context.t)
      Shell_services.Blocks.path
end

let register_error_kind
    category ~id ~title ~description ?pp
    encoding from_error to_error =
  let id = "client." ^ Name.name ^ "." ^ id in
  register_error_kind
    category ~id ~title ~description ?pp
    encoding from_error to_error

