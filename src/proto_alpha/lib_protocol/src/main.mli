(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Protocol Signature Instance *)

type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t ;
      baker : Alpha_context.public_key_hash ;
    }
  | Partial_construction of {
      predecessor : Block_hash.t ;
    }
  | Full_construction of {
      predecessor : Block_hash.t ;
      protocol_data : Alpha_context.Block_header.contents ;
      baker : Alpha_context.public_key_hash ;
    }

type validation_state =
  { mode : validation_mode ;
    ctxt : Alpha_context.t ;
    op_count : int ;
    deposit : Alpha_context.Tez.t ;
  }

include Updater.PROTOCOL with type block_header_data = Alpha_context.Block_header.protocol_data
                          and type block_header_metadata = Alpha_context.Block_header.metadata
                          and type block_header = Alpha_context.Block_header.t
                          and type operation_data = Alpha_context.Operation.protocol_data
                          and type operation_metadata = Apply_operation_result.operation_result
                          and type operation = Alpha_context.operation
                          and type validation_state := validation_state
