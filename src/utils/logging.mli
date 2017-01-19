(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type LOG = sig

  val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a
  val fatal_error: ('a, Format.formatter, unit, 'b) format4 -> 'a

  val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

end

module Core : LOG
module Net : LOG
module RPC : LOG
module Db : LOG
module Updater : LOG
module Node : sig
  module State : LOG
  module Validator : LOG
  module Prevalidator : LOG
  module Discoverer : LOG
  module Worker : LOG
  module Main : LOG
end
module Client : sig
  module Blocks : LOG
  module Mining : LOG
  module Endorsement : LOG
  module Revelation : LOG
  module Denunciation : LOG
end
module Webclient : LOG

module Make(S: sig val name: string end) : LOG

type kind =
  | Null
  | Stdout
  | Stderr
  | File of string
  | Syslog
  | Manual of Lwt_log.logger

val init: kind -> unit
