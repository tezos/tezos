(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = private raw
(** A type for sparse block locator (/à la/ Bitcoin) *)

and raw = Block_header.t * Block_hash.t list
(** Non private version of Block_store_locator.t for coercions *)

val raw: t -> raw

val encoding: t Data_encoding.t

val compute: predecessor: (Block_hash.t -> int -> Block_hash.t option Lwt.t) ->
  genesis:Block_hash.t -> Block_hash.t -> Block_header.t -> int -> t Lwt.t
(** [compute block max_length] compute the sparse block locator for
    the [block]. The locator contains at most [max_length] elements. *)

type validity =
  | Unknown
  | Known_valid
  | Known_invalid

val unknown_prefix:
  (Block_hash.t -> validity Lwt.t) -> t -> (Block_hash.t * t) option Lwt.t
(** [unknown_prefix validity locator] keeps only the unknown part of
    the locator up to the first valid block. If there is no known valid
    block or there is a known invalid one, None is returned. *)
