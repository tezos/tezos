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

let convert_block_info
    ( { hash ; predecessor ; fitness ; timestamp ; protocol }
      : Client_node_rpcs.Blocks.block_info ) =
  Client_proto_rpcs.Context.level (`Hash hash) >>= function
  | Ok level ->
      Lwt.return (Some { hash ; predecessor ; fitness ; timestamp ; protocol ; level })
  | Error _ ->
      (* TODO log error *)
      Lwt.return_none

let convert_block_info_err
    ( { hash ; predecessor ; fitness ; timestamp ; protocol }
      : Client_node_rpcs.Blocks.block_info ) =
  Client_proto_rpcs.Context.level (`Hash hash) >>=? fun level ->
  return { hash ; predecessor ; fitness ; timestamp ; protocol ; level }

let info ?operations block =
  Client_node_rpcs.Blocks.info ?operations block >>= fun block ->
  convert_block_info_err block

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

let sort_blocks ?(compare = compare) blocks =
  Lwt_list.map_p convert_block_info blocks >|= fun blocks ->
  let blocks = Utils.unopt_list blocks in
  List.sort compare blocks

let monitor
    ?operations ?length ?heads ?delay
    ?min_date ?min_heads ?compare () =
  Client_node_rpcs.Blocks.monitor
    ?operations ?length ?heads ?delay ?min_date ?min_heads
    () >>= fun block_stream ->
  let convert blocks = sort_blocks ?compare (List.flatten blocks) in
  Lwt.return (Lwt_stream.map_s convert block_stream)

let blocks_from_cycle block cycle =
  let block =
    match block with
    | `Prevalidation -> `Head 0
    | `Test_prevalidation -> `Test_head 0
    | _ -> block in
  Client_node_rpcs.Blocks.hash block >>= fun block_hash ->
  Client_proto_rpcs.Context.level block >>=? fun level ->
  Client_proto_rpcs.Helpers.levels block cycle >>=? fun block_levels ->
  begin
    match List.sort Level.compare block_levels with
    | [] -> failwith "Internal error"
    | hd :: _ -> return hd
  end >>=? fun min_level ->
  let length = 1 + Int32.to_int (Level.diff level min_level) in
  begin
    Client_node_rpcs.Blocks.list ~length ~heads:[block_hash] () >>= function
    | [] | _::_::_ -> failwith "Unexpected RPC result"
    | [blocks] -> return blocks
  end >>=? fun block_infos ->
  let block_infos =
    Utils.remove_elem_from_list (length - List.length block_levels) block_infos in
  map_s convert_block_info_err block_infos >>=? fun block_res ->
  return block_res
