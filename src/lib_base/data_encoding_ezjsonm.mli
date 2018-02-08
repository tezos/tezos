(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

(** Loads a JSON file in memory *)
val read_file : string -> Data_encoding.json tzresult Lwt.t

(** (Over)write a JSON file from in memory data *)
val write_file : string -> Data_encoding.json -> unit tzresult Lwt.t
