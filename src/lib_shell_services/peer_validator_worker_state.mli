(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Request : sig
  type view =
    | New_head of Block_hash.t
    | New_branch of Block_hash.t * int
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end

module Event : sig
  type t =
    | Request of (Request.view * Worker_types.request_status * error list option)
    | Debug of string
  val level : t -> Logging.level
  val encoding : error list Data_encoding.encoding -> t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit
end

module Worker_state : sig
  type view =
    { bootstrapped : bool ;
      mutable last_validated_head: Block_hash.t ;
      mutable last_advertised_head: Block_hash.t }
  val encoding : view Data_encoding.encoding
  val pp : Format.formatter -> view -> unit
end
