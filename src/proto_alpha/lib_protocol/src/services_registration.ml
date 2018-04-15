(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type rpc_context = {
  block_hash: Block_hash.t ;
  block_header: Block_header.shell_header ;
  context: Alpha_context.t ;
}

let rpc_init (rpc_context : Updater.rpc_context Lwt.t) =
  rpc_context >>= fun { block_hash ; block_header ; context } ->
  let level = block_header.level in
  let timestamp = block_header.timestamp in
  let fitness = block_header.fitness in
  Alpha_context.prepare ~level ~timestamp ~fitness context >>=? fun context ->
  return { block_hash ; block_header ; context }

let rpc_services = ref (RPC_directory.empty : Updater.rpc_context Lwt.t RPC_directory.t)

let register0_fullctxt s f =
  rpc_services :=
    RPC_directory.register !rpc_services s
      (fun ctxt q i ->
         rpc_init ctxt >>=? fun ctxt ->
         f ctxt q i)
let opt_register0_fullctxt s f =
  rpc_services :=
    RPC_directory.opt_register !rpc_services s
      (fun ctxt q i ->
         rpc_init ctxt >>=? fun ctxt ->
         f ctxt q i)
let register0 s f =
  register0_fullctxt s (fun { context ; _ } -> f context)
let register0_noctxt s f =
  rpc_services :=
    RPC_directory.register !rpc_services s
      (fun _ q i -> f q i)

let register1_fullctxt s f =
  rpc_services :=
    RPC_directory.register !rpc_services s
      (fun (ctxt, arg) q i ->
         rpc_init ctxt >>=? fun ctxt ->
         f ctxt arg q i)
let register1 s f = register1_fullctxt s (fun { context ; _ } x -> f context x)
let register1_noctxt s f =
  rpc_services :=
    RPC_directory.register !rpc_services s
      (fun (_, arg) q i -> f arg q i)

let register2_fullctxt s f =
  rpc_services :=
    RPC_directory.register !rpc_services s
      (fun ((ctxt, arg1), arg2) q i ->
         rpc_init ctxt >>=? fun ctxt ->
         f ctxt arg1 arg2 q i)
let register2 s f =
  register2_fullctxt s (fun { context ; _ } a1 a2 q i -> f context a1 a2 q i)

let get_rpc_services () = !rpc_services
