(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

include Persist.STORE

val register_resolver:
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete: t -> string -> string list Lwt.t
