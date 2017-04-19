(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t =

  | Get_current_branch of Net_id.t
  | Current_branch of Net_id.t * Block_hash.t list (* Block locator *)
  | Deactivate of Net_id.t

  | Get_current_head of Net_id.t
  | Current_head of Net_id.t * Block_hash.t * Operation_hash.t list

  | Get_block_headers of Net_id.t * Block_hash.t list
  | Block_header of Block_header.t

  | Get_operations of Net_id.t * Operation_hash.t list
  | Operation of Operation.t

  | Get_protocols of Protocol_hash.t list
  | Protocol of Protocol.t

  | Get_operation_list of Net_id.t * (Block_hash.t * int) list
  | Operation_list of Net_id.t * Block_hash.t * int *
                      Operation_hash.t list * Operation_list_list_hash.path

val cfg : t P2p.message_config

val pp_json : Format.formatter -> t -> unit
