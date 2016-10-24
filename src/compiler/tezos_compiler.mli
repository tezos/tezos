(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Low-level part of the [Updater]. *)

module Protocol : sig
  type component = {
    name : string;
    interface : string option;
    implementation : string;
  }
  val component_encoding : component Data_encoding.encoding
  type t = component list
  val encoding : component list Data_encoding.encoding
  val to_bytes : component list -> MBytes.t
  val of_bytes : MBytes.t -> component list option
  val hash : component list -> Hash.Protocol_hash.t
end

module Meta : sig
  val to_file: string -> ?hash:Protocol_hash.t -> string list -> unit
  val of_file: string -> Protocol_hash.t option * string list
end

val main: unit -> unit
