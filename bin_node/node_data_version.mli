(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type error += Invalid_data_dir_version of t * t
type error += Could_not_read_data_dir_version of string

val data_version : t
val default_identity_file_name : string

val pp : Format.formatter -> t -> unit

val version_encoding : t Data_encoding.encoding

val ensure_data_dir : string -> unit tzresult Lwt.t
