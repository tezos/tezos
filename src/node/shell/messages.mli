(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** High level messages *)
type t =

  | Discover_blocks of Store.net_id * Block_hash.t list (* Block locator *)
  | Block_inventory of Store.net_id * Block_hash.t list

  | Get_blocks of Block_hash.t list
  | Block of MBytes.t

  | Current_operations of Store.net_id
  | Operation_inventory of Store.net_id * Operation_hash.t list

  | Get_operations of Operation_hash.t list
  | Operation of MBytes.t

  | Get_protocols of Protocol_hash.t list
  | Protocol of MBytes.t

val encoding : t P2p.msg_encoding list
