(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type DISTRIBUTED_DB = sig
  type t
  type state
  type store
  type key
  type value
  val create: state -> store Persist.shared_ref -> t
  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value option Lwt.t
  val prefetch: t -> Store.net_id -> key -> unit
  val fetch: t -> Store.net_id -> key -> value Lwt.t
  val pending: t -> key -> bool
  val store: t -> key -> value -> bool Lwt.t
  val update: t -> key -> value -> bool Lwt.t
  val remove: t -> key -> bool Lwt.t
  val shutdown: t -> unit Lwt.t

  val keys: t -> key list Lwt.t
end

type operation_state = {
  request_operations: Store.net_id -> Operation_hash.t list -> unit ;
}

module Operation_scheduler = struct
  let name = "operation_scheduler"
  type rdata = Store.net_id
  type data = float ref
  type state = operation_state
  let init_request _ _ = Lwt.return (ref 0.0)
  let request net ~get:_ ~set:_ pendings =
    let current_time = Unix.gettimeofday () in
    let time = current_time -. (3. +. Random.float 8.) in
    let operations =
      List.fold_left
        (fun acc (hash, last_request, Store.Net net_id) ->
           if !last_request < time then begin
             last_request := current_time ;
             let prev =
               try Block_hash_map.find net_id acc
               with Not_found -> [] in
             Block_hash_map.add net_id (hash :: prev) acc
           end else
             acc)
        Block_hash_map.empty
        pendings in
    if Block_hash_map.is_empty operations then
      0.
    else begin
      Block_hash_map.iter
        (fun net_id -> net.request_operations (Net net_id))
        operations ;
      1. +. Random.float 4.
    end
end

module Operation =
  Persist.MakeImperativeProxy
    (Store.Faked_functional_operation)
    (Operation_hash_table) (Operation_scheduler)

type block_state = {
  request_blocks: Store.net_id -> Block_hash.t list -> unit ;
}

module Block_scheduler = struct
  let name = "block_scheduler"
  type rdata = Store.net_id
  type data = float ref
  type state = block_state
  let init_request _ _ = Lwt.return (ref 0.0)
  let request net ~get:_ ~set:_ pendings =
    let current_time = Unix.gettimeofday () in
    let limit = current_time -. (3. +. Random.float 8.) in
    let blocks =
      List.fold_left
        (fun acc (hash, last_request, Store.Net net_id) ->
           if !last_request < limit then begin
             last_request := current_time ;
             let prev =
               try Block_hash_map.find net_id acc
               with Not_found -> [] in
             Block_hash_map.add net_id (hash :: prev) acc
           end else
             acc)
        Block_hash_map.empty
        pendings in
    if Block_hash_map.is_empty blocks then
      0.
    else begin
      Block_hash_map.iter
        (fun net_id -> net.request_blocks (Net net_id))
        blocks ;
      1. +. Random.float 4.
    end
end

module Block =
  Persist.MakeImperativeProxy
    (Store.Faked_functional_block)
    (Block_hash_table) (Block_scheduler)

type protocol_state = {
  request_protocols: Protocol_hash.t list -> unit ;
}

module Protocol_scheduler = struct
  let name = "protocol_scheduler"
  type rdata = Store.net_id
  type data = float ref
  type state = protocol_state
  let init_request _ _ = Lwt.return (ref 0.0)
  let request net ~get:_ ~set:_ pendings =
    let current_time = Unix.gettimeofday () in
    let time = current_time -. (3. +. Random.float 8.) in
    let protocols =
      List.fold_left
        (fun acc (hash, last_request, Store.Net net_id) ->
           if !last_request < time then begin
             last_request := current_time ;
             let prev =
               try Block_hash_map.find net_id acc
               with Not_found -> [] in
             Block_hash_map.add net_id (hash :: prev) acc
           end else
             acc)
        Block_hash_map.empty
        pendings in
    if Block_hash_map.is_empty protocols then
      0.
    else begin
      Block_hash_map.iter (fun _net_id -> net.request_protocols) protocols ;
      1. +. Random.float 4.
    end
end

module Protocol =
  Persist.MakeImperativeProxy
    (Store.Faked_functional_protocol)
    (Protocol_hash_table) (Protocol_scheduler)
