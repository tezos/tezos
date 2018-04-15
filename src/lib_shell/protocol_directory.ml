(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let build_rpc_directory state distributed_db =

  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let gen_register0 s f =
    dir := RPC_directory.gen_register !dir s (fun () p q -> f p q) in
  let register1 s f =
    dir := RPC_directory.register !dir s (fun ((), a) p q -> f a p q) in

  gen_register0 Protocol_services.S.list begin fun () p ->
    let { Protocol_services.S.monitor ; contents } = p in
    let monitor = match monitor with None -> false | Some x -> x in
    let include_contents = match contents with None -> false | Some x -> x in
    State.Protocol.list state >>= fun set ->
    let protocols = Protocol_hash.Set.elements set in
    Lwt_list.map_p
      (fun hash ->
         if include_contents then
           State.Protocol.read state hash >>= function
           | Error _  -> Lwt.return (hash, None)
           | Ok bytes -> Lwt.return (hash, Some bytes)
         else
           Lwt.return (hash, None))
      protocols >>= fun protocols ->
    if not monitor then
      RPC_answer.return protocols
    else
      let stream, stopper =
        Distributed_db.Protocol.watch distributed_db in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let first_request = ref true in
      let next () =
        if not !first_request then
          Lwt_stream.get stream >>= function
          | None -> Lwt.return_none
          | Some (h, op) when include_contents -> Lwt.return (Some [h, Some op])
          | Some (h, _) -> Lwt.return (Some [h, None])
        else begin
          first_request := false ;
          Lwt.return (Some protocols)
        end in
      RPC_answer.return_stream { next ; shutdown }
  end;

  register1 Protocol_services.S.contents begin fun hash () () ->
    State.Protocol.read state hash
  end ;

  !dir
