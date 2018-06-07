(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** All the metadata associated to a running connection. *)

type t = {
  disable_mempool : bool ;
  private_node : bool ;
}

val encoding: t Data_encoding.t
val pp: Format.formatter -> t -> unit
