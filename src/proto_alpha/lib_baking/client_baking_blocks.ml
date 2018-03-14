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
  level: Level.t ;
}

let convert_block_info cctxt
    ( { hash ; chain_id ; predecessor ; fitness ; timestamp ; protocol }
      : Block_services.block_info ) =
  Alpha_services.Context.level cctxt (`Hash hash) >>= function
  | Ok level ->
      Lwt.return
        (Some { hash ; chain_id ; predecessor ;
                fitness ; timestamp ; protocol ; level })
  | Error _ ->
      (* TODO log error *)
      Lwt.return_none

let convert_block_info_err cctxt
    ( { hash ; chain_id ; predecessor ; fitness ; timestamp ; protocol }
      : Block_services.block_info ) =
  Alpha_services.Context.level cctxt (`Hash hash) >>=? fun level ->
  return { hash ; chain_id ; predecessor ; fitness ; timestamp ; protocol ; level }

let info cctxt ?include_ops block =
  Block_services.info cctxt ?include_ops block >>=? fun block ->
  convert_block_info_err cctxt block

let compare (bi1 : block_info) (bi2 : block_info) =
  match Fitness.compare bi1.fitness bi2.fitness with
  | 0 -> begin
      match compare bi1.level bi2.level with
      | 0 -> begin
          match Time.compare bi1.timestamp bi2.timestamp with
          | 0 -> Block_hash.compare bi1.predecessor bi2.predecessor
          | x -> - x
        end
      | x -> - x
    end
  | x -> x

let sort_blocks cctxt ?(compare = compare) blocks =
  Lwt_list.filter_map_p (convert_block_info cctxt) blocks >|= fun blocks ->
  List.sort compare blocks

let monitor cctxt
    ?include_ops ?length ?heads ?delay
    ?min_date ?min_heads ?compare () =
  Block_services.monitor
    ?include_ops ?length ?heads ?delay ?min_date ?min_heads
    cctxt >>=? fun (block_stream, _stop) ->
  let convert blocks =
    sort_blocks cctxt ?compare (List.flatten blocks) >>= return in
  return (Lwt_stream.map_s convert block_stream)

let blocks_from_cycle cctxt block cycle =
  Alpha_services.Context.level cctxt block >>=? fun level ->
  Alpha_services.Helpers.levels cctxt block cycle >>=? fun (first, last) ->
  let length = Int32.to_int (Raw_level.diff level.level first) in
  Block_services.predecessors cctxt block length >>=? fun blocks ->
  let blocks =
    List.remove
      (length - (1 + Int32.to_int (Raw_level.diff last first))) blocks in
  if Raw_level.(level.level = last) then
    Block_services.hash cctxt block >>=? fun last ->
    return (last :: blocks)
  else
    return blocks
