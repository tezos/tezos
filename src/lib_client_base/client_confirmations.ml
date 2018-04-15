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
    ~chain
    ?(predecessors = 10)
    ?(confirmations = 1)
    operation_hash =

  (* Table of known blocks:
     - None: if neither the block or its predecessors contains the operation
     - (Some n): if the `n-th` predecessors of the block contains the operation *)

  let blocks : int option Block_hash.Table.t =
    Block_hash.Table.create confirmations in

  (* Fetch _all_ the 'unknown' predecessors af a block. *)

  let fetch_predecessors block =
    let rec loop acc block =
      Block_services.Empty.Header.Shell.predecessor
        ctxt ~chain ~block:(`Hash (block, 0)) () >>=? fun predecessor ->
      if Block_hash.Table.mem blocks predecessor then
        return acc
      else
        loop (predecessor :: acc) predecessor
    in
    loop [block] block >>= function
    | Ok blocks -> Lwt.return blocks
    | Error err ->
        ctxt#warning
          "Error while fetching block (ignored): %a"
          pp_print_error err >>= fun () ->
        (* Will be retried when a new head arrives *)
        Lwt.return [] in

  (* Check whether a block as enough confirmations. This function
     assumes that the block predecessor has been processed already. *)

  let process block =
    Block_services.Empty.hash ctxt ~chain ~block () >>=? fun hash ->
    Block_services.Empty.Header.Shell.predecessor
      ctxt ~chain ~block () >>=? fun predecessor ->
    match Block_hash.Table.find blocks predecessor with
    | Some n ->
        ctxt#answer
          "Operation received %d confirmations as of block: %a"
          (n+1) Block_hash.pp hash >>= fun () ->
        if n+1 < confirmations then begin
          Block_hash.Table.add blocks hash (Some (n+1)) ;
          return false
        end else
          return true
    | None ->
        Block_services.Empty.Operation_hash.operation_hashes
          ctxt ~chain ~block () >>=? fun operations ->
        let in_block =
          List.exists
            (List.exists
               (Operation_hash.equal operation_hash))
            operations in
        if not in_block then begin
          Block_hash.Table.add blocks hash None ;
          return false
        end else begin
          ctxt#answer
            "Operation found in block: %a"
            Block_hash.pp hash >>= fun () ->
          if confirmations <= 0 then
            return true
          else begin
            Block_hash.Table.add blocks hash (Some 0) ;
            return false
          end
        end in

  Shell_services.Monitor.heads ctxt chain >>=? fun (stream, stop) ->
  Lwt_stream.get stream >>= function
  | None -> assert false
  | Some head ->
      let rec loop n =
        if n >= 0 then
          process (`Hash (head, n)) >>=? function
          | true ->
              stop () ;
              return ()
          | false ->
              loop (n-1)
        else
          let exception WrapError of error list in
          Lwt.catch
            (fun () ->
               let stream = Lwt_stream.map_list_s fetch_predecessors stream in
               Lwt_stream.find_s
                 (fun block ->
                    process (`Hash (block, 0)) >>= function
                    | Ok b -> Lwt.return b
                    | Error err ->
                        Lwt.fail (WrapError err)) stream >>= return)
            (function
              | WrapError e -> Lwt.return (Error e)
              | exn -> Lwt.fail exn) >>=? fun _ ->
          stop () ;
          return () in
      Block_services.Empty.hash
        ctxt ~block:(`Hash (head, predecessors+1)) () >>=? fun oldest ->
      Block_hash.Table.add blocks oldest None ;
      loop predecessors

