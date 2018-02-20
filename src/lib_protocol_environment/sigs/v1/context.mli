(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

type t

(** Keys in (kex x value) database implementations *)
type key = string list

(** Values in (kex x value) database implementations *)
type value = MBytes.t

val mem: t -> key -> bool Lwt.t
val dir_mem: t -> key -> bool Lwt.t

val get: t -> key -> value option Lwt.t

val set: t -> key -> value -> t Lwt.t

(** [copy] returns None if the [from] key is not bound *)
val copy: t -> from:key -> to_:key -> t option Lwt.t

val del: t -> key -> t Lwt.t
val remove_rec: t -> key -> t Lwt.t

val fold:
  t -> key -> init:'a ->
  f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

val keys: t -> key -> key list Lwt.t
val fold_keys:
  t -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

val register_resolver:
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete: t -> string -> string list Lwt.t
