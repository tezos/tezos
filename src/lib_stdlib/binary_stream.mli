(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type buffer = {
  buffer : MBytes.t ;
  ofs : int ;
  len : int ;
}

exception Need_more_data

val is_empty: t -> bool
val empty: t
val of_buffer: buffer -> t
val read: t -> int -> buffer * t
val push: MBytes.t -> t -> t
