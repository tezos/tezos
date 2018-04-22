(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let wait_for_operation_inclusion
    (ctxt : #Client_context.full)
    ?(predecessors = 10)
    ?(confirmations = 1)
    operation_hash =
  let confirmed_blocks = Hashtbl.create confirmations in
  let process block =
    Block_services.hash ctxt block >>=? fun hash ->
    Block_services.predecessor ctxt block >>=? fun predecessor ->
    match Hashtbl.find_opt confirmed_blocks predecessor with
    | Some n ->
        ctxt#answer
          "Operation received %d confirmations as of block: %a"
          (n+1) Block_hash.pp hash >>= fun () ->
        if n+1 < confirmations then begin
          Hashtbl.add confirmed_blocks hash (n+1) ;
          return false
        end else
          return true
    | None ->
        Block_services.operations
          ctxt ~contents:false block >>=? fun operations ->
        let in_block =
          List.exists
            (List.exists
               (fun (oph, _) -> Operation_hash.equal operation_hash oph))
            operations in
        if not in_block then
          return false
        else begin
          ctxt#answer
            "Operation found in block: %a"
            Block_hash.pp hash >>= fun () ->
          if confirmations <= 0 then
            return true
          else begin
            Hashtbl.add confirmed_blocks hash 0 ;
            return false
          end
        end in
  Block_services.monitor
    ~include_ops:false
    ~length:predecessors ctxt >>=? fun (stream, stop) ->
  let exception WrapError of error list in
  let stream = Lwt_stream.map_list List.concat stream in
  Lwt.catch
    (fun () ->
       Lwt_stream.find_s
         (fun bi ->
            process (`Hash (bi.Block_services.hash, 0)) >>= function
            | Ok b -> Lwt.return b
            | Error err ->
                Lwt.fail (WrapError err)) stream >>= return)
    (function
      | WrapError e -> Lwt.return (Error e)
      | exn -> Lwt.fail exn) >>=? fun _ ->
  stop () ;
  return ()
