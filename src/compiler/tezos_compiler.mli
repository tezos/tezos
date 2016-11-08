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
  val to_file: Lwt_io.file_name -> ?hash:Protocol_hash.t -> string list -> unit
  val of_file: Lwt_io.file_name -> Protocol_hash.t option * string list
end

module Protocol : sig
  type component = {
    name : string;
    interface : string option;
    implementation : string;
  }
  val find_component : Lwt_io.file_name -> string -> component
  val component_encoding : component Data_encoding.encoding

  type t = component list
  val encoding : t Data_encoding.encoding
  val to_bytes : t -> MBytes.t
  val of_bytes : MBytes.t -> t option
  val hash : t -> Hash.Protocol_hash.t

  val of_dir : Lwt_io.file_name -> t
end

val main: unit -> unit
