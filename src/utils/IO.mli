(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

val mkdir: string -> unit

val check_dir: string -> unit tzresult Lwt.t
val is_directory: string -> bool

val with_file_in: string -> (MBytes.t -> 'a Lwt.t) -> 'a Lwt.t
val with_file_out: string -> MBytes.t -> unit Lwt.t

val remove_file: ?cleanup:bool -> string -> unit Lwt.t

val fold: string -> init:'a -> f:(string -> 'a -> 'a Lwt.t) -> 'a Lwt.t

