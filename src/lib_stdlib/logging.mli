(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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

(** Unique tag for a logging module.
    Match against, e.g. `Logging.Core.Section`.  *)
type log_section = private ..

type log_message = {
  section : log_section ;
  level : level ;
  text : string option ;
  tags : Tag.set ;
}

type tap_id

(** Intercept events as they are logged.  All events will generate a call to
    your tap function, but `text` will only be included for events that
    actually print a message according to the active logging configuration.  *)
val tap : (log_message -> unit) -> tap_id

(** Remove a previously set tap by supplying its tap_id.  Does nothing if
    the tap was removed already.  *)
val untap : tap_id -> unit

type ('a,'b) msgf = (('a, Format.formatter, unit, 'b) format4 -> ?tags:Tag.set -> 'a) -> ?tags:Tag.set -> 'b
type ('a,'b) log = ('a,'b) msgf -> 'b

module type MESSAGE = sig
  val name: string
end

module type SEMLOG = sig

  type log_section += Section

  module Tag = Tag

  val debug: ('a, unit) log
  val log_info: ('a, unit) log
  val log_notice: ('a, unit) log
  val warn: ('a, unit) log
  val log_error: ('a, unit) log
  val fatal_error: ('a, unit) log

  val lwt_debug: ('a, unit Lwt.t) log
  val lwt_log_info: ('a, unit Lwt.t) log
  val lwt_log_notice: ('a, unit Lwt.t) log
  val lwt_warn: ('a, unit Lwt.t) log
  val lwt_log_error: ('a, unit Lwt.t) log
  val lwt_fatal_error: ('a, unit Lwt.t) log

  val event : string Tag.def
  val exn : exn Tag.def

end

module type LOG = sig

  type log_section += Section

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

module Core : sig
  include SEMLOG

  val worker : string Tag.def
end

module Make(S: MESSAGE) : LOG
module Make_unregistered(S: MESSAGE) : LOG

module Make_semantic(S: MESSAGE) : SEMLOG

type template = Lwt_log.template
val default_template : template

val sections: string list ref
