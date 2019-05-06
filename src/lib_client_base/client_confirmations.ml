(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let in_block operation_hash operations =
  let exception Found of int * int in
  try
    List.iteri
      (fun i ops ->
         List.iteri (fun j op ->
             if Operation_hash.equal operation_hash op then
               raise (Found (i,j))) ops)
      operations ;
    None
  with Found (i,j) -> Some (i, j)

type operation_status =
  | Confirmed of (Block_hash.t * int * int)
  | Pending
  | Still_not_found

let wait_for_operation_inclusion
    (ctxt : #Client_context.full)
    ~chain
    ?(predecessors = 10)
    ?(confirmations = 1)
    ?branch
    operation_hash =

  let exception WrapError of error list in
  let exception Outdated of Operation_hash.t in

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
        Lwt.return_nil in

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
          return Pending
        end else
          return (Confirmed block_with_op)
    | None ->
        Shell_services.Blocks.Operation_hashes.operation_hashes
          ctxt ~chain ~block () >>=? fun operations ->
        match in_block operation_hash operations with
        | None ->
            Block_hash.Table.add blocks hash None ;
            return Still_not_found
        | Some (i, j) -> begin
            ctxt#answer
              "Operation found in block: %a (pass: %d, offset: %d)"
              Block_hash.pp hash i j >>= fun () ->
            Block_hash.Table.add blocks hash (Some ((hash, i, j), 0)) ;
            if confirmations <= 0 then
              return (Confirmed (hash, i, j))
            else
              return Pending
          end in

  (* Checks if the given branch is considered alive.*)

  let check_branch_alive () =
    match branch with
    | Some branch_hash ->
        Shell_services.Blocks.live_blocks
          ctxt ~chain ~block:(`Head 0) () >>= begin function
          | Ok live_blocks ->
              if Block_hash.Set.mem branch_hash live_blocks then
                Lwt.return_unit
              else
                ctxt#error
                  "The operation %a is outdated and may \
                   never be included in the chain.@,\
                   We recommand to use an external block explorer."
                  Operation_hash.pp operation_hash >>= fun () ->
                Lwt.fail (Outdated operation_hash)
          | Error err -> Lwt.fail (WrapError err)
        end
    | None -> Lwt.return_unit
  in

  Shell_services.Monitor.heads ctxt chain >>=? fun (stream, stop) ->
  Lwt_stream.get stream >>= function
  | None -> assert false
  | Some (head, _) ->
      let rec loop n =
        if n >= 0 then
          (*Search for the operation in the n head predecessors*)
          let block = `Hash (head, n) in
          Shell_services.Blocks.hash ctxt ~chain ~block () >>=? fun hash ->
          Shell_services.Blocks.Header.shell_header ctxt
            ~chain ~block () >>=? fun shell ->
          process hash shell >>=? function
          | Confirmed block ->
              stop () ;
              return block
          | Pending | Still_not_found ->
              loop (n-1)
        else
          (*Search for the operation in new heads*)
          Lwt.catch
            (fun () ->
               (*Fetching potential unknown blocks from potential new heads*)
               let stream = Lwt_stream.map_list_s fetch_predecessors stream in
               Lwt_stream.find_s
                 (fun (hash, header) ->
                    process hash header >>= function
                    | Ok Pending ->
                        Lwt.return_false
                    | Ok Still_not_found ->
                        check_branch_alive () >>= fun () ->
                        Lwt.return_false
                    | Ok (Confirmed _) ->
                        Lwt.return_true
                    | Error err ->
                        Lwt.fail (WrapError err)) stream >>= return)
            (function
              | WrapError e -> Lwt.return_error e
              | exn -> Lwt.fail exn) >>=? function
          | None ->
              failwith "..."
          | Some (hash, _) ->
              stop () ;
              match Block_hash.Table.find_opt blocks hash with
              | None | Some None -> assert false
              | Some (Some (hash, _)) ->
                  return hash in
      begin
        match branch with
        | Some branch_hash ->
            Shell_services.Blocks.Header.shell_header
              ctxt ~chain ~block:(`Hash(branch_hash,0)) () >>=? fun branch_header ->
            let branch_level = branch_header.Block_header.level in
            Shell_services.Blocks.Header.shell_header
              ctxt ~chain ~block:(`Hash (head,0)) () >>=? fun head_shell ->
            let head_level = head_shell.Block_header.level in
            return (Int32.(to_int (sub head_level branch_level)))
        | None -> return predecessors
      end
      >>=? fun block_hook ->
      Block_services.Empty.hash
        ctxt ~chain ~block:(`Hash (head, block_hook+1)) () >>=? fun oldest ->
      Block_hash.Table.add blocks oldest None ;
      loop block_hook

let lookup_operation_in_previous_block ctxt chain operation_hash i =
  Block_services.Empty.hash ctxt ~block:(`Head i) ()
  >>=? fun block ->
  Shell_services.Blocks.Operation_hashes.operation_hashes ctxt ~chain
    ~block:(`Hash (block, 0)) ()
  >>=? fun operations ->
  match in_block operation_hash operations with
  | None -> return_none
  | Some (a, b) -> return_some (block, a, b)

let lookup_operation_in_previous_blocks
    (ctxt : #Client_context.full)
    ~chain
    ~predecessors
    operation_hash =
  let rec loop i =
    if i = predecessors + 1 then
      return_none
    else begin
      lookup_operation_in_previous_block ctxt chain operation_hash i >>=?
      function
      | None -> loop (i + 1)
      | Some (block, a, b) -> return_some (block, a, b)
    end
  in
  loop 0
