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

(**
   This module defines a "structured event logging framework."

   Internal-Event streams are like traditional logs but they have a proper
   {!Data_encoding} format in order to be processed by software.

   The module defines "Sinks" {!SINK} as the receptacle for structured
   events: pluggable modules which can absorb (i.e. display, store,
   forward) the events emitted within the code-base.
*)

open Tezos_error_monad
open Error_monad

(** {3 Events Definitions and Registration } *)

type level = Debug | Info | Notice | Warning | Error | Fatal
(** The relative importance of a particular event (compatible with
    traditional logging systems, cf. {!Lwt_log_core.level}). *)

(** Module to manipulate values of type {!level}.  *)
module Level : sig

  type t = level
  (** Alias of {!level}. *)

  val default : t
  (** The default level is {!Info}, it is used in {!Event_defaults}. *)

  val to_lwt_log : t -> Lwt_log_core.level
  (** Cast the level to a value of {!Lwt_log_core.level}. *)

  val to_string : t -> string
  val of_string : string -> t option
  val encoding : t Data_encoding.t
end


(** Sections are a simple way of classifying events at the time of
    their emission. *)
module Section: sig
  type t = private string list

  val empty : t

  val make_sanitized : string list -> t
  (** Build a {!Section.t} by replacing special characters with ['_']. *)

  val to_lwt_log : t -> Lwt_log_core.section
  (** Make the equivalent {!Lwt_log} section.  *)

  val encoding : t Data_encoding.t

  val to_string_list : t -> string list

end

(** Parameters defining an inspectable type of events. *)
module type EVENT_DEFINITION = sig
  type t

  val name : string
  (** Defines the identifier for the event. Names should be unique and
      are restricted to alphanumeric characters or [".@-_+=,~"].*)

  val doc : string
  (** A display-friendly test which describes what the event means. *)

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val level : t -> level
  (** Return the prefered {!level} for a given event instance. *)
end

(** Default values for fields in {!EVENT_DEFINITION}. *)
module Event_defaults : sig
  (** Use this module as needed with [include Event_defaults]. *)

  val level : 'a -> level
end

(** Events created with {!Make} provide the {!EVENT} API. *)
module type EVENT = sig
  include EVENT_DEFINITION

  val emit : ?section: Section.t -> (unit -> t) -> unit tzresult Lwt.t
  (** Output an event of type {!t}, if no sinks are listening the
      function won't be applied. *)
end

(** Build an event from an event-definition. *)
module Make(E: EVENT_DEFINITION): EVENT with type t = E.t

type 'a event_definition = (module EVENT_DEFINITION with type t = 'a)
(** [event_definition] wraps {!EVENT_DEFINITION} as a first class module. *)

(** Helper functions to manipulate all kinds of events in a generic way. *)
module Generic : sig
  type definition = Definition : (string * 'a event_definition) -> definition

  type event = Event : (string * 'a event_definition * 'a) -> event

  type with_name = < doc : string; name : string >

  val json_schema : definition -> < schema : Json_schema.schema ; with_name >
  (** Get the JSON schema (together with [name] and [doc]) of a given
      event definition. *)

  val explode_event : event ->
    < pp : Format.formatter -> unit -> unit ;
      json : Data_encoding.json ;
      with_name >
    (** Get the JSON representation and a pretty-printer for a given
        event {i instance}. *)
end

(** Access to all the event definitions registered with {!Make}. *)
module All_definitions : sig

  (** Get the list of all the known definitions. *)
  val get : unit -> Generic.definition list

  (** Find the definition matching on the given name. *)
  val find: (string -> bool) -> Generic.definition option
end

(** {3 Sink Definitions and Registration } *)

(** An implementation of {!SINK} is responsible for handling/storing
    events, for instance, a sink could be output to a file, to a
    database, or a simple "memory-less" forwarding mechanism.  *)
module type SINK = sig

  (** A sink can store any required state, e.g. a database handle, in
      a value of the [t] type see {!configure}. *)
  type t

  val uri_scheme : string
  (** Registered sinks are a distinguished by their URI scheme. *)

  val configure : Uri.t -> t tzresult Lwt.t
  (** When a registered sink is activated the {!configure} function is
      called to initialize it. The parameters should be encoded or
      obtained from the URI (the scheme of the URI is already
      {!uri_scheme}). *)

  val handle :
    t -> 'a event_definition ->
    ?section: Section.t -> (unit -> 'a) -> unit tzresult Lwt.t
  (** A sink's main function is to {!handle} incoming events from the
      code base. *)

  val close : t -> unit tzresult Lwt.t
  (** A function to be called on graceful termination of processes
      (e.g. to flush file-descriptors, etc.). *)
end

type 'a sink_definition = (module SINK with type t = 'a)
(** [sink_definition] wraps {!SINK_DEFINITION} as a first class module. *)

(** Use {!All_sinks.register} to add a new {i inactive} sink, then
    {!All_sinks.activate} to make it handle events. *)
module All_sinks : sig

  val register : 'a sink_definition -> unit
  (** Register a new sink (e.g.
      [let () = Internal_event.All_sinks.register (module Sink_implementation)])
      for it to be available (but inactive) in the framework. *)

  val activate : Uri.t -> unit tzresult Lwt.t
  (** Make a registered sink active: the function finds it by URI
      scheme and calls {!configure}. *)

  val close : unit -> unit tzresult Lwt.t
  (** Call [close] on all the sinks. *)

  val pp_state : Format.formatter -> unit -> unit
  (** Display the state of registered/active sinks. *)
end

(** {3 Common Event Definitions } *)

(** {!Error_event.t} is a generic event to emit values of type
    {!Error_monad.error list}. *)
module Error_event : sig
  type t = {
    message : string option ;
    severity : [ `Fatal | `Recoverable ] ;
    trace : Error_monad.error list ;
  }
  (** Errors mainly store {!Error_monad.error list} values. One can
      attach a message and a severity (the default is [`Recoverable]
      which corresponds to the {!Error} {!level}, while [`Fatal]
      corresponds to {!Fatal}). *)

  val make :
    ?message: string ->
    ?severity:[ `Fatal | `Recoverable ] ->
    Error_monad.error list ->
    unit -> t

  include EVENT with type t := t

  val log_error_and_recover :
    ?section:Section.t ->
    ?message:string ->
    ?severity:[ `Fatal | `Recoverable ] ->
    (unit -> (unit, error list) result Lwt.t) -> unit Lwt.t
    (** [log_error_and_recover f] calls [f ()] and emits an {!Error_event.t}
        event if it results in an error. It then continues in the [_ Lwt.t]
        monad (e.g. there is no call to [Lwt.fail]). *)
end

(** The debug-event is meant for emitting (temporarily)
    semi-structured data in the event stream. *)
module Debug_event : sig
  type t = {
    message : string ;
    attachment : Data_encoding.Json.t ;
  }
  val make : ?attach: Data_encoding.Json.t -> string -> unit -> t
  include EVENT with type t := t
end

(** The worker event is meant for use with {!Lwt_utils.worker}. *)
module Lwt_worker_event : sig
  type t = {
    name : string;
    event : [ `Ended | `Failed of string | `Started ];
  }
  include EVENT with type t := t

  val on_event :
    string -> [ `Ended | `Failed of string | `Started ] -> unit Lwt.t
    (** [on_event msg status] emits an event of type [t] and matches
        the signature required by {!Lwt_utils.worker}.  *)
end

(** {3 Compatibility With Legacy Logging } *)

(** The module {!Legacy_logging} replaces the previous
    [Logging.Make_*] functors by injecting the non-structured logs
    into the event-logging framework.
    {b Please do not use for new modules.} *)
module Legacy_logging : sig

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
  open Tezos_stdlib
  type ('a, 'b) msgf =
    (('a, Format.formatter, unit, 'b) format4 -> ?tags:Tag.set -> 'a) ->
    ?tags:Tag.set -> 'b
  type ('a, 'b) log = ('a, 'b) msgf -> 'b
  module type SEMLOG = sig
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
  module Make :  (sig val name : string end) -> sig
      module Event : EVENT
      include LOG
    end
  module Make_semantic :  (sig val name : string end) -> sig
      module Event : EVENT
      include SEMLOG
    end

  val sections : string list ref
end


(** {3 Common Event-Sink Definitions } *)

(** The lwt-sink outputs pretty-printed renderings of events to the
    lwt-log logging framework (see the {!Lwt_log_core} module).

    It is activated {i by default} in {!Internal_event_unix.Configuration.default}
    (in any case it can be activated with [TEZOS_EVENTS_CONFIG="lwt-log://"]. To
    configure further how the sink outputs to a file or the user's
    terminal, one needs to use the [TEZOS_LOG] variable (see also the module
    {!Lwt_log_sink_unix}).
*)
module Lwt_log_sink: sig
  val uri_scheme : string
end
