(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Name = struct let name = "alpha" end
module Alpha_environment = Tezos_protocol_environment_faked.MakeV1(Name)()
module Proto = Tezos_protocol_alpha.Functor.Make(Alpha_environment)
module Block_services = struct
  include Block_services
  include Block_services.Make(Proto)(Proto)
end
include Proto

class type rpc_context = object
  inherit RPC_context.json
  inherit [Chain_services.chain * Block_services.block] Alpha_environment.RPC_context.simple
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
  inherit [Chain_services.chain,
           Block_services.block] Alpha_environment.proto_rpc_context
      (t :> RPC_context.t)
      Block_services.path
end

class type full = object
  inherit Client_context.full
  inherit [Chain_services.chain * Block_services.block] Alpha_environment.RPC_context.simple
end

class wrap_full (t : Client_context.full) : full = object
  inherit Client_context.proxy_context t
  inherit [Chain_services.chain, Block_services.block] Alpha_environment.proto_rpc_context
      (t :> RPC_context.t)
      Block_services.path
end
