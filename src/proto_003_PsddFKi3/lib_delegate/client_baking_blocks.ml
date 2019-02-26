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

open Proto_alpha
open Alpha_context
open Logging

include Tezos_stdlib.Logging.Make_semantic(struct let name = "client.blocks" end)

type block_info = {
  hash: Block_hash.t ;
  chain_id: Chain_id.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.t ;
  protocol: Protocol_hash.t ;
  next_protocol: Protocol_hash.t ;
  proto_level: int ;
  level: Raw_level.t ;
  context : Context_hash.t ;
}

let raw_info cctxt ?(chain = `Main) hash shell_header =
  let block = `Hash (hash, 0) in
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  Shell_services.Blocks.protocols
    cctxt ~chain ~block () >>=? fun { current_protocol = protocol ;
                                      next_protocol } ->
  let { Tezos_base.Block_header.predecessor ; fitness ;
        timestamp ; level ; context ; proto_level ; _ } =
    shell_header in
  match Raw_level.of_int32 level with
  | Ok level ->
      return { hash ; chain_id ; predecessor ; fitness ;
               timestamp ; protocol ; next_protocol ; proto_level ; level ; context }
  | Error _ ->
      failwith "Cannot convert level into int32"

let info cctxt ?(chain = `Main) block =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Shell_services.Blocks.Header.shell_header
    cctxt ~chain ~block () >>=? fun shell_header  ->
  raw_info cctxt ~chain hash shell_header

let monitor_valid_blocks cctxt ?chains ?protocols ~next_protocols () =
  Monitor_services.valid_blocks cctxt
    ?chains ?protocols ?next_protocols () >>=? fun (block_stream, _stop) ->
  return (Lwt_stream.map_s
            (fun ((chain, block), data) ->
               log_info Tag.DSL.(fun f ->
                   f "Saw block %a on chain %a"
                   -% t event "monitor_saw_valid_block"
                   -% a Block_hash.Logging.tag block
                   -% a State_logging.chain_id chain
                   -% t block_header_tag data) ;
               raw_info cctxt ~chain:(`Hash chain) block data.Tezos_base.Block_header.shell)
            block_stream)

let monitor_heads cctxt ~next_protocols chain =
  Monitor_services.heads
    cctxt ?next_protocols chain >>=? fun (block_stream, _stop) ->
  return (Lwt_stream.map_s
            (fun (block, data) ->
               log_info Tag.DSL.(fun f ->
                   f "Saw head %a"
                   -% t event "monitor_saw_head"
                   -% a Block_hash.Logging.tag block
                   -% t block_header_tag data) ;
               raw_info cctxt ~chain block data.Tezos_base.Block_header.shell)
            block_stream)

let blocks_from_current_cycle cctxt ?(chain = `Main) block ?(offset = 0l) () =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Shell_services.Blocks.Header.shell_header
    cctxt ~chain ~block () >>=? fun { level } ->
  Alpha_services.Helpers.levels_in_current_cycle
    cctxt ~offset (chain, block) >>= function
  | Error [RPC_context.Not_found _] ->
      return_nil
  | Error _ as err -> Lwt.return err
  | Ok (first, last) ->
      let length = Int32.to_int (Int32.sub level (Raw_level.to_int32 first)) in
      Shell_services.Blocks.list cctxt ~heads:[hash] ~length () >>=? fun blocks ->
      let blocks =
        List.remove
          (length - (Int32.to_int (Raw_level.diff last first)))
          (List.hd blocks) in
      if Int32.equal level (Raw_level.to_int32 last) then
        return (hash :: blocks)
      else
        return blocks
