(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Low-level part of the [Updater]. *)

module Meta : sig

  val to_file: string -> Protocol_hash.t -> string list -> unit
  val of_file: string -> Protocol_hash.t * string list

end

val main: unit -> unit
