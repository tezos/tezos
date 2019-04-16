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

(** An implementation of {!Tezos_base.Internal_event.SINK} which
    writes the events as JSON files in a given directory structure.

    It is registered with the URI scheme ["unix-files"], one can activate it
    with an URI containing the top-level directory in which the JSON
    files will be written, e.g.
    ["export TEZOS_EVENTS_CONFIG=unix-files:///the/path/to/write"]
    (the path should be inexistent or already a directory).

    The directory structure is as follows:
    ["<section-dirname>/<event-name>/<YYYYMMDD-string>/<HHMMSS-MMMMMM>/<YYYYMMDD-HHMMSS-MMMMMM-xxxx.json>"]
    where ["<section-dirname>"] is either ["no-section"] or
    ["section-<section-name>"].
*)

(** The module {!Query} provides a {!fold} function over the events
    stored by a given instantiation of the [SINK.t]. *)
module Query : sig

  module Time_constraint : sig
    type op = [ `Lt | `Le | `Ge | `Gt ]
    type t =
      [ `All
      | `And of t * t
      | `Or of t * t
      | `Date of op * float
      | `Time of op * float ]
  end

  (** The {!fold} function returns a list of non-fatal errors and
      warnings that happened during the scan, those are defined in
      {!Report.item}. *)
  module Report : sig
    type item = [
      | `Error of [
          | `Parsing_event of [
              | `Encoding of string * exn
              | `Json of string * error list
            ]
          | `Cannot_recognize_section of string
        ]
      | `Warning of [
          | `Expecting_regular_file_at of string
          | `Expecting_directory_at of string
          | `Unknown_event_name_at of string * string
        ]
    ]
    val pp: Format.formatter -> item -> unit
  end

  (** Scan a folder for events.

      - [?on_unknown] is a function which takes a path to a JSON file.
      - [?only_sections] is an optional filter on the sections in which the
        events have been emitted ({!Internal_event.Section.t}).
      - [?only_names] is an optional filter on the event names.
      - [?time_query] is a filter restricting the allowed events'
        emission dates (cf. {!Time_constraint}).

      See also an example of use in {!Client_event_logging_commands}
      (command ["tezos-client-admin query events from
      unix-files:///..."]).
  *)
  val fold :
    ?on_unknown:(string -> unit tzresult Lwt.t) ->
    ?only_sections:string option list ->
    ?only_names:string list ->
    ?time_query:Time_constraint.t ->
    Uri.t ->
    init:'a ->
    f:('a -> time_stamp:float -> Internal_event.Generic.event -> 'a tzresult Lwt.t) ->
    (Report.item list * 'a) tzresult Lwt.t
end
