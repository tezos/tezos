(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type incremental = {
  predecessor: Client_baking_blocks.block_info ;
  context : Context.t ;
  state: Main.validation_state ;
  rev_operations: Operation.packed list ;
  header: Tezos_base.Block_header.shell_header ;
}

val load_context : context_path:string -> Context.index Lwt.t

val begin_construction : timestamp:Time.t -> ?protocol_data: block_header_data -> Context.index -> Client_baking_blocks.block_info -> incremental tzresult Lwt.t

val add_operation : incremental -> Operation.packed -> incremental tzresult Lwt.t

val finalize_construction : incremental -> (T.validation_result * LiftedMain.block_header_metadata) tzresult Lwt.t 
