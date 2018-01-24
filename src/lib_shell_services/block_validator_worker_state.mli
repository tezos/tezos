(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Request : sig
  type view = {
    net_id : Net_id.t ;
    block : Block_hash.t ;
    peer: P2p_peer.Id.t option ;
  }
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type t =
    | Validation_success of Request.view * Worker_types.request_status
    | Validation_failure of Request.view * Worker_types.request_status * error list
    | Debug of string
  val level : t -> Logging.level
  val encoding : error list Data_encoding.encoding -> t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit
end

module Worker_state : sig
  type view = unit
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end
