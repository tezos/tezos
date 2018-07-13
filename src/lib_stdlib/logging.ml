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

type ('a, 'b) msgf =
  (('a, Format.formatter, unit, 'b) format4 -> ?tags:Tag.set -> 'a) -> ?tags:Tag.set -> 'b

type ('a, 'b) log = ('a, 'b) msgf -> 'b

module type MESSAGE = sig
  val name: string
end

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

type log_section = ..

type log_message = {
  section : log_section ;
  level : level ;
  text : string option ;
  tags : Tag.set ;
}

type tap_id = int
let next_tap : int ref = ref 0

type tap = {
  id : tap_id ;
  process : log_message -> unit ;
}

let taps : tap list ref = ref []

let tap process = let id = !next_tap in
  begin
    next_tap := id + 1 ;
    taps := { id ; process } :: !taps ;
    id
  end

let untap x = taps := List.filter (fun tap -> tap.id <> x) !taps

let call_taps v = List.iter (fun tap -> tap.process v) !taps

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

let sections = ref []

let event = Tag.def ~doc:"String identifier for the class of event being logged" "event" Format.pp_print_text
let exn = Tag.def ~doc:"Exception which was detected" "exception" (fun f e -> Format.pp_print_text f (Printexc.to_string e))

module Make_semantic(S : MESSAGE) : SEMLOG = struct

  include S

  type log_section += Section

  module Tag = Tag

  let () = sections := S.name :: !sections
  let section = Lwt_log_core.Section.make S.name


  let log_f ~level =
    if level < Lwt_log_core.Section.level section then
      fun format ?(tags=Tag.empty) ->
        Format.ikfprintf
          (fun _ -> call_taps { section = Section ; level ; text = None ; tags }; Lwt.return_unit)
          Format.std_formatter
          format
    else
      fun format ?(tags=Tag.empty) ->
        Format.kasprintf
          (fun text ->
             call_taps { section = Section ; level ; text = Some text ; tags };
             Lwt_log_core.log ~section ~level text)
          format

  let ign_log_f ~level =
    if level < Lwt_log_core.Section.level section then
      fun format ?(tags=Tag.empty) ->
        Format.ikfprintf
          (fun _ -> call_taps { section = Section ; level ; text = None ; tags })
          Format.std_formatter
          format
    else
      fun format ?(tags=Tag.empty) ->
        Format.kasprintf
          (fun text ->
             call_taps { section = Section ; level ; text = Some text ; tags };
             Lwt_log_core.ign_log ~section ~level text)
          format

  let debug f = f (ign_log_f ~level:Lwt_log_core.Debug) ?tags:(Some Tag.empty)
  let log_info f = f (ign_log_f ~level:Lwt_log_core.Info) ?tags:(Some Tag.empty)
  let log_notice f = f (ign_log_f ~level:Lwt_log_core.Notice) ?tags:(Some Tag.empty)
  let warn f = f (ign_log_f ~level:Lwt_log_core.Warning) ?tags:(Some Tag.empty)
  let log_error f = f (ign_log_f ~level:Lwt_log_core.Error) ?tags:(Some Tag.empty)
  let fatal_error f = f (ign_log_f ~level:Lwt_log_core.Fatal) ?tags:(Some Tag.empty)

  let lwt_debug f = f (log_f ~level:Lwt_log_core.Debug) ?tags:(Some Tag.empty)
  let lwt_log_info f = f (log_f ~level:Lwt_log_core.Info) ?tags:(Some Tag.empty)
  let lwt_log_notice f = f (log_f ~level:Lwt_log_core.Notice) ?tags:(Some Tag.empty)
  let lwt_warn f = f (log_f ~level:Lwt_log_core.Warning) ?tags:(Some Tag.empty)
  let lwt_log_error f = f (log_f ~level:Lwt_log_core.Error) ?tags:(Some Tag.empty)
  let lwt_fatal_error f = f (log_f ~level:Lwt_log_core.Fatal) ?tags:(Some Tag.empty)

  let event = event
  let exn = exn

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

let sections = ref []

module Make_unregistered(S : MESSAGE) : LOG = struct

  let section = Lwt_log_core.Section.make S.name
  type log_section += Section

  let log_f
      ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
    if level < Lwt_log_core.Section.level section then
      Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
    else
      Format.kasprintf
        (fun msg ->
           call_taps { section = Section ; level ; text = Some msg ; tags = Tag.empty };
           Lwt_log_core.log ?exn ~section ?location ?logger ~level msg)
        format

  let ign_log_f
      ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
    if level < Lwt_log_core.Section.level section then
      Format.ikfprintf (fun _ -> ()) Format.std_formatter format
    else
      Format.kasprintf
        (fun msg ->
           call_taps { section = Section ; level ; text = Some msg ; tags = Tag.empty };
           Lwt_log_core.ign_log ?exn ~section ?location ?logger ~level msg)
        format

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

module Make(S : MESSAGE) : LOG = struct

  let () = sections := S.name :: !sections
  include Make_unregistered(S)

end

module Core = struct
  include Make_semantic(struct let name = "core" end)

  let worker = Tag.def ~doc:"Name of affected worker" "worker" Format.pp_print_text
end

type template = Lwt_log_core.template
let default_template = "$(date) - $(section): $(message)"
