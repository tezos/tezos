(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Pervasives
module Pervasives = Pervasives
module Compare = Compare
module Array = Array
module List = List
module Bytes = Bytes
module String = String
module Set = Set
module Map = Map
module Int32 = Int32
module Int64 = Int64
module Nativeint = Nativeint
module Buffer = Buffer
module Format = Format
module Hex_encode = Hex_encode
module Lwt_sequence = Lwt_sequence
module Lwt = Lwt
module Lwt_list = Lwt_list
module MBytes = MBytes
module Uri = Uri
module Data_encoding = Data_encoding
module Time = Time
module Base48 = Base48
module Hash = Hash
module Ed25519 = Ed25519
module Persist = Persist
module Context = Context
module RPC = RPC
module Fitness = Fitness
module Updater = Updater

(* Internal usage *)

module Error_monad_sig = Error_monad_sig
module Error_monad = Error_monad
module Logging = Logging

module type PACKED_PROTOCOL = sig
  val hash : Protocol_hash.t
  include Updater.PROTOCOL
  val error_encoding : error Data_encoding.t
  val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
  val pp : Format.formatter -> error -> unit
end
