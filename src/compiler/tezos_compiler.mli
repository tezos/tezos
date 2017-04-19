(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Tezos_data

(** Low-level part of the [Updater]. *)

module Meta : sig
  val to_file: Lwt_io.file_name -> ?hash:Protocol_hash.t -> string list -> unit
  val of_file: Lwt_io.file_name -> Protocol_hash.t option * string list
end

val read_dir: Lwt_io.file_name -> Protocol.t

val main: unit -> unit
