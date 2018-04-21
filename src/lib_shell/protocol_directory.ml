(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let build_rpc_directory state =

  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let gen_register0 s f =
    dir := RPC_directory.gen_register !dir s (fun () p q -> f p q) in
  let register1 s f =
    dir := RPC_directory.register !dir s (fun ((), a) p q -> f a p q) in

  gen_register0 Protocol_services.S.list begin fun () () ->
    State.Protocol.list state >>= fun set ->
    let protocols =
      Protocol_hash.Set.elements set @
      Registered_protocol.list_embedded () in
    RPC_answer.return protocols
  end ;

  register1 Protocol_services.S.contents begin fun hash () () ->
    match Registered_protocol.get_embedded_sources hash with
    | Some p -> return p
    | None -> State.Protocol.read state hash
  end ;

  !dir
