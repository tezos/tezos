(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
  val fatal_error: ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_fatal_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

end

module Core : LOG
module P2p : LOG
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
  module Baking : LOG
  module Endorsement : LOG
  module Revelation : LOG
  module Denunciation : LOG
end

module Make(S: sig val name: string end) : LOG

type level = Lwt_log_core.level =
  | Debug
  (** Debugging message. They can be automatically removed by the
      syntax extension. *)
  | Info
  (** Informational message. Suitable to be displayed when the
      program is in verbose mode. *)
  | Notice
  (** Same as {!Info}, but is displayed by default. *)
  | Warning
  (** Something strange happend *)
  | Error
  (** An error message, which should not means the end of the
      program. *)
  | Fatal

type template = Lwt_log.template
val default_template : template

val sections: string list ref
