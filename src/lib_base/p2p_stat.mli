(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Bandwidth usage statistics *)

type t = {
  total_sent : int64 ;
  total_recv : int64 ;
  current_inflow : int ;
  current_outflow : int ;
}

val empty : t
val pp : Format.formatter -> t -> unit
val encoding : t Data_encoding.t
