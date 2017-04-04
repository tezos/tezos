(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type block_info = {
  hash: Block_hash.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.t ;
  protocol: Protocol_hash.t option ;
  level: Level.t ;
}

let convert_block_info cctxt
    ( { hash ; predecessor ; fitness ; timestamp ; protocol }
      : Client_node_rpcs.Blocks.block_info ) =
  Client_proto_rpcs.Context.level cctxt (`Hash hash) >>= function
  | Ok level ->
      Lwt.return (Some { hash ; predecessor ; fitness ; timestamp ; protocol ; level })
  | Error _ ->
      (* TODO log error *)
      Lwt.return_none

let convert_block_info_err cctxt
    ( { hash ; predecessor ; fitness ; timestamp ; protocol }
      : Client_node_rpcs.Blocks.block_info ) =
  Client_proto_rpcs.Context.level cctxt (`Hash hash) >>=? fun level ->
  return { hash ; predecessor ; fitness ; timestamp ; protocol ; level }

let info cctxt ?operations block =
  Client_node_rpcs.Blocks.info cctxt ?operations block >>=? fun block ->
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
    ?operations ?length ?heads ?delay
    ?min_date ?min_heads ?compare () =
  Client_node_rpcs.Blocks.monitor cctxt
    ?operations ?length ?heads ?delay ?min_date ?min_heads
    () >>=? fun block_stream ->
  let convert blocks =
    Lwt.return blocks >>=? fun blocks ->
    sort_blocks cctxt ?compare (List.flatten blocks) >>= return in
  return (Lwt_stream.map_s convert block_stream)

let blocks_from_cycle cctxt block cycle =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | _ -> block in
  Client_proto_rpcs.Context.level cctxt block >>=? fun level ->
  Client_proto_rpcs.Helpers.levels cctxt block cycle >>=? fun (first, last) ->
  let length = Int32.to_int (Raw_level.diff level.level first) in
  Client_node_rpcs.Blocks.predecessors cctxt block length >>=? fun blocks ->
  let blocks =
    Utils.remove_elem_from_list
      (length - (1 + Int32.to_int (Raw_level.diff last first))) blocks in
  return blocks
