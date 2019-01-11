(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

let get_context index hash =
  Context.checkout index hash >>= function
  | None -> fail (Block_validator_errors.Failed_to_checkout_context hash)
  | Some ctx -> return ctx

(** The standard block validation method *)
module Seq_validator = struct

  include Internal_event.Legacy_logging.Make (struct
      let name = "validation_process.sequential"
    end)

  type validation_context = {
    context_index : Context.index ;
  }

  type t = validation_context

  let init context_index =
    lwt_log_notice "Intialized" >>= fun () ->
    Lwt.return { context_index }

  let close _ =
    lwt_log_notice "Shutting down ..." >>= fun () ->
    Lwt.return_unit

  let apply_block
      validator_process
      chain_id
      ~max_operations_ttl
      ~(predecessor_block_header : Block_header.t)
      ~block_header
      operations =
    get_context validator_process.context_index
      predecessor_block_header.shell.context >>=? fun predecessor_context ->
    Block_validation.apply
      chain_id
      ~max_operations_ttl
      ~predecessor_block_header
      ~predecessor_context
      ~block_header
      operations

end

type validator_kind =
  | Internal of Context.index

type t =
  | Sequential of Seq_validator.t

let init = function
  | Internal index ->
      Seq_validator.init index >>= fun v ->
      Lwt.return (Sequential v)

let close = function
  | Sequential vp -> Seq_validator.close vp

let apply_block bvp ~predecessor block_header operations =
  let chain_state = State.Block.chain_state predecessor in
  let chain_id = State.Block.chain_id predecessor in
  let predecessor_block_header = State.Block.header predecessor in
  let max_operations_ttl = State.Block.max_operations_ttl predecessor in
  let block_hash = Block_header.hash block_header in
  begin
    Chain.data chain_state >>= fun chain_data ->
    if State.Block.equal chain_data.current_head predecessor then
      Lwt.return (chain_data.live_blocks, chain_data.live_operations)
    else
      Chain_traversal.live_blocks
        predecessor (State.Block.max_operations_ttl predecessor)
  end >>= fun (live_blocks, live_operations) ->
  Block_validation.check_liveness
    ~live_operations ~live_blocks block_hash operations >>=? fun () ->
  match bvp with
  | Sequential vp ->
      Seq_validator.apply_block vp
        ~max_operations_ttl
        chain_id ~predecessor_block_header
        ~block_header operations
