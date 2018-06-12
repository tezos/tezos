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

type block_info = {
  hash: Block_hash.t ;
  chain_id: Chain_id.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.t ;
  protocol: Protocol_hash.t ;
  next_protocol: Protocol_hash.t ;
  level: Raw_level.t ;
}

let raw_info cctxt ?(chain = `Main) hash shell_header =
  let block = `Hash (hash, 0) in
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  Shell_services.Blocks.protocols
    cctxt ~chain ~block () >>=? fun { current_protocol = protocol ;
                                      next_protocol } ->
  let { Tezos_base.Block_header.predecessor ; fitness ; timestamp ; level ; _ } =
    shell_header in
  match Raw_level.of_int32 level with
  | Ok level ->
      return { hash ; chain_id ; predecessor ; fitness ;
               timestamp ; protocol ; next_protocol ; level }
  | Error _ ->
      failwith "Cannot convert level into int32"

let info cctxt ?(chain = `Main) block =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Shell_services.Blocks.Header.shell_header
    cctxt ~chain ~block () >>=? fun shell_header  ->
  raw_info cctxt ~chain hash shell_header

let monitor_valid_blocks cctxt ?chains ?protocols ?next_protocols () =
  Shell_services.Monitor.valid_blocks cctxt
    ?chains ?protocols ?next_protocols () >>=? fun (block_stream, _stop) ->
  return (Lwt_stream.map_s
            (fun ((chain, block), { Tezos_base.Block_header.shell }) ->
               raw_info cctxt ~chain:(`Hash chain) block shell)
            block_stream)

let monitor_heads cctxt ?next_protocols chain =
  Monitor_services.heads
    cctxt ?next_protocols chain >>=? fun (block_stream, _stop) ->
  return (Lwt_stream.map_s
            (fun (block, { Tezos_base.Block_header.shell }) -> raw_info cctxt ~chain block shell)
            block_stream)

let blocks_from_current_cycle cctxt ?(chain = `Main) block ?(offset = 0l) () =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Shell_services.Blocks.Header.shell_header
    cctxt ~chain ~block () >>=? fun { level } ->
  Alpha_services.Helpers.levels_in_current_cycle
    cctxt ~offset (chain, block) >>= function
  | Error [RPC_context.Not_found _] ->
      return []
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
