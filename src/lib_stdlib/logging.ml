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

let log_f
    ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
  if level < Lwt_log_core.Section.level section then
    Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
  else
    Format.kasprintf
      (fun msg -> Lwt_log_core.log ?exn ~section ?location ?logger ~level msg)
      format

let ign_log_f
    ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
  if level < Lwt_log_core.Section.level section then
    Format.ikfprintf (fun _ -> ()) Format.std_formatter format
  else
    Format.kasprintf
      (fun msg -> Lwt_log_core.ign_log ?exn ~section ?location ?logger ~level msg)
      format

let sections = ref []

module Make(S : sig val name: string end) : LOG = struct

  let () = sections := S.name :: !sections
  let section = Lwt_log_core.Section.make S.name

  let debug fmt = ign_log_f ~section ~level:Lwt_log_core.Debug fmt
  let log_info fmt = ign_log_f ~section ~level:Lwt_log_core.Info fmt
  let log_notice fmt = ign_log_f ~section ~level:Lwt_log_core.Notice fmt
  let warn fmt = ign_log_f ~section ~level:Lwt_log_core.Warning fmt
  let log_error fmt = ign_log_f ~section ~level:Lwt_log_core.Error fmt
  let fatal_error fmt = ign_log_f ~section ~level:Lwt_log_core.Fatal fmt

  let lwt_debug fmt = log_f ~section ~level:Lwt_log_core.Debug fmt
  let lwt_log_info fmt = log_f ~section ~level:Lwt_log_core.Info fmt
  let lwt_log_notice fmt = log_f ~section ~level:Lwt_log_core.Notice fmt
  let lwt_warn fmt = log_f ~section ~level:Lwt_log_core.Warning fmt
  let lwt_log_error fmt = log_f ~section ~level:Lwt_log_core.Error fmt
  let lwt_fatal_error fmt = log_f ~section ~level:Lwt_log_core.Fatal fmt

end

module Core = Make(struct let name = "core" end)

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

type template = Lwt_log_core.template
let default_template = "$(date) - $(section): $(message)"
