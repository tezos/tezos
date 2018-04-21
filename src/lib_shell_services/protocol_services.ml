(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

end

open RPC_context
let contents ctxt h =
  make_call1 S.contents ctxt h () ()
let list ctxt =
  make_call S.list ctxt () () ()

