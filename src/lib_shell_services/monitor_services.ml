(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module S = struct

  open Data_encoding
  let path = RPC_path.(root / "monitor")

  let valid_blocks_query =
    let open RPC_query in
    query (fun protocols next_protocols chains -> object
            method protocols = protocols
            method next_protocols = next_protocols
            method chains = chains
          end)
    |+ multi_field "protocol"
      Protocol_hash.rpc_arg (fun t -> t#protocols)
    |+ multi_field "next_protocol"
      Protocol_hash.rpc_arg (fun t -> t#next_protocols)
    |+ multi_field "chain"
      Chain_services.chain_arg (fun t -> t#chains)
    |> seal

  let valid_blocks =
    RPC_service.get_service
      ~description:""
      ~query: valid_blocks_query
      ~output: (obj2
                  (req "chain_id" Chain_id.encoding)
                  (req "hash" Block_hash.encoding))
      RPC_path.(path / "valid_blocks")

  let heads_query =
    let open RPC_query in
    query (fun next_protocols -> object
            method next_protocols = next_protocols
          end)
    |+ multi_field "next_protocol"
      Protocol_hash.rpc_arg (fun t -> t#next_protocols)
    |> seal

  let heads =
    RPC_service.get_service
      ~description:""
      ~query: heads_query
      ~output: Block_hash.encoding
      RPC_path.(path / "heads" /: Chain_services.chain_arg)

end

open RPC_context

let valid_blocks
    ctxt ?(chains = [`Main]) ?(protocols = []) ?(next_protocols = []) () =
  make_streamed_call S.valid_blocks ctxt () (object
    method chains = chains
    method protocols = protocols
    method next_protocols = next_protocols
  end) ()

let heads ctxt ?(next_protocols = []) chain =
  make_streamed_call S.heads ctxt ((), chain) (object
    method next_protocols = next_protocols
  end) ()
