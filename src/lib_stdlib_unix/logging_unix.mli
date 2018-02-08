(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Output : sig
  type t =
    | Null
    | Stdout
    | Stderr
    | File of string
    | Syslog of Lwt_log.syslog_facility

  val encoding : t Data_encoding.t
  val of_string : string -> t option
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end


val init: ?template:Logging.template -> Output.t -> unit Lwt.t

val close: unit -> unit Lwt.t
