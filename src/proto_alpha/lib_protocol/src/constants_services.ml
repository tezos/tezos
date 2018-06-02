(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
