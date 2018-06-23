(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let wait_for_bootstrapped (ctxt : #Client_context.full) =
  let display = ref false in
  Lwt.async begin fun () ->
    Lwt_unix.sleep 0.3 >>= fun () ->
    if not !display then
      ctxt#answer "Waiting for the node to be bootstrapped before injection..." >>= fun () ->
      display := true ;
      Lwt.return ()
    else
      Lwt.return ()
  end ;
  Monitor_services.bootstrapped ctxt >>=? fun (stream, _stop) ->
  Lwt_stream.iter_s
    (fun (hash, time) ->
       if !display then
         ctxt#message "Current head: %a (timestamp: %a, validation: %a)"
           Block_hash.pp_short hash
           Time.pp_hum time
           Time.pp_hum (Time.now ())
       else Lwt.return ()) stream >>= fun () ->
  display := true ;
  ctxt#answer "Node is bootstrapped, ready for injecting operations." >>= fun () ->
  return ()

let wait_for_operation_inclusion
    (ctxt : #Client_context.full)
    ~chain
    ?(predecessors = 10)
    ?(confirmations = 1)
    operation_hash =

  (* Table of known blocks:
     - None: if neither the block or its predecessors contains the operation
     - (Some ((hash, i, j), n)):
          if the `hash` contains the operation in list `i` at position `j`
          and if `hash` denotes the `n-th` predecessors of the block. *)

  let blocks : ((Block_hash.t * int * int) * int) option Block_hash.Table.t =
    Block_hash.Table.create confirmations in

  (* Fetch _all_ the 'unknown' predecessors af a block. *)

  let fetch_predecessors (hash, header) =
    let rec loop acc (_hash, header) =
      let predecessor = header.Block_header.predecessor in
      if Block_hash.Table.mem blocks predecessor then
        return acc
      else
        Chain_services.Blocks.Header.shell_header
          ctxt ~chain ~block:(`Hash (predecessor, 0)) () >>=? fun shell ->
        let block = (predecessor, shell) in
        loop (block :: acc) block in
    loop [hash, header.Block_header.shell] (hash, header.shell) >>= function
    | Ok blocks -> Lwt.return blocks
    | Error err ->
        ctxt#warning
          "Error while fetching block (ignored): %a"
          pp_print_error err >>= fun () ->
        (* Will be retried when a new head arrives *)
        Lwt.return [] in

  (* Check whether a block as enough confirmations. This function
     assumes that the block predecessor has been processed already. *)

  let process hash header =
    let block = `Hash (hash, 0) in
    let predecessor = header.Tezos_base.Block_header.predecessor in
    match Block_hash.Table.find blocks predecessor with
    | Some (block_with_op, n) ->
        ctxt#answer
          "Operation received %d confirmations as of block: %a"
          (n+1) Block_hash.pp hash >>= fun () ->
        Block_hash.Table.add blocks hash (Some (block_with_op, n+1)) ;
        if n+1 < confirmations then begin
          return None
        end else
          return (Some block_with_op)
    | None ->
        Shell_services.Blocks.Operation_hashes.operation_hashes
          ctxt ~chain ~block () >>=? fun operations ->
        let in_block =
          let exception Found of int * int in
          try
            List.iteri
              (fun i ops ->
                 List.iteri (fun j op ->
                     if Operation_hash.equal operation_hash op then
                       raise (Found (i,j))) ops)
              operations ;
            None
          with Found (i,j) -> Some (i, j) in
        match in_block with
        | None ->
            Block_hash.Table.add blocks hash None ;
            return None
        | Some (i, j) -> begin
            ctxt#answer
              "Operation found in block: %a (pass: %d, offset: %d)"
              Block_hash.pp hash i j >>= fun () ->
            Block_hash.Table.add blocks hash (Some ((hash, i, j), 0)) ;
            if confirmations <= 0 then
              return (Some (hash, i, j))
            else begin
              return None
            end
          end in

  Shell_services.Monitor.heads ctxt chain >>=? fun (stream, stop) ->
  Lwt_stream.get stream >>= function
  | None -> assert false
  | Some (head, _) ->
      let rec loop n =
        if n >= 0 then
          let block = `Hash (head, n) in
          Shell_services.Blocks.hash ctxt ~chain ~block () >>=? fun hash ->
          Shell_services.Blocks.Header.shell_header ctxt
            ~chain ~block () >>=? fun shell ->
          process hash shell >>=? function
          | Some block ->
              stop () ;
              return block
          | None ->
              loop (n-1)
        else
          let exception WrapError of error list in
          Lwt.catch
            (fun () ->
               let stream = Lwt_stream.map_list_s fetch_predecessors stream in
               Lwt_stream.find_s
                 (fun (hash, header) ->
                    process hash header >>= function
                    | Ok None -> Lwt.return false
                    | Ok (Some _) -> Lwt.return true
                    | Error err ->
                        Lwt.fail (WrapError err)) stream >>= return)
            (function
              | WrapError e -> Lwt.return (Error e)
              | exn -> Lwt.fail exn) >>=? function
          | None ->
              failwith "..."
          | Some (hash, _) ->
              stop () ;
              match Block_hash.Table.find_opt blocks hash with
              | None | Some None -> assert false
              | Some (Some (hash, _)) ->
                  return hash in
      Block_services.Empty.hash
        ctxt ~block:(`Hash (head, predecessors+1)) () >>=? fun oldest ->
      Block_hash.Table.add blocks oldest None ;
      loop predecessors
