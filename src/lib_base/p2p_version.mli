(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Protocol version *)

type t = {
  name : string ;
  major : int ;
  minor : int ;
}
(** Type of a protocol version. *)

val pp : Format.formatter -> t -> unit
val encoding : t Data_encoding.t
val common : t list -> t list -> t option

