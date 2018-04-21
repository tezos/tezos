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

  let forge_block_header =
    RPC_service.post_service
      ~description: "Forge a block header"
      ~query: RPC_query.empty
      ~input: Block_header.encoding
      ~output: (obj1 (req "block" bytes))
      RPC_path.(root / "forge_block_header")

end

open RPC_context

let forge_block_header ctxt header =
  make_call S.forge_block_header ctxt () () header
