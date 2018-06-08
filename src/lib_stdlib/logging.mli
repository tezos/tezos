(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Tag : sig
  type _ def

  val def : ?doc:string -> string -> (Format.formatter -> 'a -> unit) -> 'a def

  val name : 'a def -> string
  val doc : 'a def -> string
  val printer : 'a def -> (Format.formatter -> 'a -> unit)
  val pp_def : Format.formatter -> 'a def -> unit

  type t = V : 'a def * 'a -> t
  val pp : Format.formatter -> t -> unit

  module Key : sig
    type t = V : 'a def -> t
  end
  type set

  val empty : set
  val is_empty : set -> bool
  val mem : 'a def -> set -> bool
  val add : 'a def -> 'a -> set -> set
  val update : 'a def -> ('a option -> 'a option) -> (set -> set)
  val singleton : 'a def -> 'a -> set
  val remove : 'a def -> set -> set
  val rem : 'a def -> set -> set
  type merger = { merger : 'a. 'a def -> 'a option -> 'a option -> 'a option }
  val merge : merger -> set -> set -> set
  type unioner = { unioner : 'a. 'a def -> 'a -> 'a -> 'a }
  val union : unioner -> set -> set -> set
  val iter : (t -> unit) -> set -> unit
  val fold : (t -> 'b -> 'b) -> (set -> 'b -> 'b)
  val for_all : (t -> bool) -> (set -> bool)
  val exists : (t -> bool) -> (set -> bool)
  val filter : (t -> bool) -> set -> set
  val partition : (t -> bool) -> set -> (set * set)
  val cardinal : set -> int
  val min_binding : set -> t
  val min_binding_opt : set -> t option
  val max_binding : set -> t
  val max_binding_opt : set -> t option
  val choose : set -> t
  val choose_opt : set -> t option
  val split : 'a def -> set -> set * 'a option * set
  val find : 'a def -> set -> 'a option
  val get : 'a def -> set -> 'a
  val find_first : (Key.t -> bool) -> set -> t
  val find_first_opt : (Key.t -> bool) -> set -> t option
  val find_last : (Key.t -> bool) -> set -> t
  val find_last_opt : (Key.t -> bool) -> set -> t option
  val map : (t -> t) -> set -> set
  val mapi : (t -> t) -> set -> set
  val pp_set : Format.formatter -> set -> unit

  module DSL : sig
    type (_,_,_,_) arg
    val a : 'v def -> 'v -> (('b -> 'v -> 'c) -> 'v -> 'd, 'b, 'c, 'd) arg
    val s : 'v def -> 'v -> ('v -> 'd, 'b, 'c, 'd) arg
    val t : 'v def -> 'v -> ('d, 'b, 'c, 'd) arg
    val (-%) : (?tags:set -> 'a) -> ('a,Format.formatter,unit,'d) arg -> (?tags:set -> 'd)
  end

end

type log_section = private ..

type log_message = {
  section : log_section ;
  text : string ;
  tags : Tag.set ;
}

val tap : (log_message -> unit) -> unit

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

module Make(S: sig val name: string end) : LOG
module Make_unregistered(S: sig val name: string end) : LOG

module Make_semantic(S: MESSAGE) : SEMLOG

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
