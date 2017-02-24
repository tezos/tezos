(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash

(** Low-level part of the [Updater]. *)

module Meta : sig
  val to_file: Lwt_io.file_name -> ?hash:Protocol_hash.t -> string list -> unit
  val of_file: Lwt_io.file_name -> Protocol_hash.t option * string list
end

module Protocol : sig

  type t = component list

  and component = {
    name: string ;
    interface: string option ;
    implementation: string ;
  }

  type protocol = t

  val compare: protocol -> protocol -> int
  val equal: protocol -> protocol -> bool

  val hash: protocol -> Protocol_hash.t
  val encoding: protocol Data_encoding.encoding

  val of_dir: Lwt_io.file_name -> protocol

end

val main: unit -> unit
