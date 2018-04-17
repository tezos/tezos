(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Error Monad *)

(** Categories of error *)
type error_category =
  [ `Branch (** Errors that may not happen in another context *)
  | `Temporary (** Errors that may not happen in a later context *)
  | `Permanent (** Errors that will happen no matter the context *)
  ]

include Error_monad_sig.S

module type Wrapped_error_monad = sig
  type unwrapped = ..
  include Error_monad_sig.S with type error := unwrapped
  val unwrap : error -> unwrapped option
  val wrap : unwrapped -> error
end

val register_wrapped_error_kind :
  (module Wrapped_error_monad) ->
  id:string -> title:string -> description:string ->
  unit

(** Erroneous result (shortcut for generic errors) *)
val generic_error :
  ('a, Format.formatter, unit, 'b tzresult) format4 ->
  'a

(** Erroneous return (shortcut for generic errors) *)
val failwith :
  ('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 ->
  'a

val error_exn : exn -> 'a tzresult
val record_trace_exn : exn -> 'a tzresult -> 'a tzresult
val trace_exn : exn -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t
val generic_trace :
  ('a, Format.formatter, unit,
   ('b, error list) result Lwt.t -> ('b, error list) result Lwt.t) format4 -> 'a
val pp_exn : Format.formatter -> exn -> unit

val failure : ('a, Format.formatter, unit, error) format4 -> 'a

(** Wrapped OCaml/Lwt exception *)
type error += Exn of exn

type error += Canceled

val protect :
  ?on_error:(error list -> 'a tzresult Lwt.t) ->
  ?canceler:Lwt_canceler.t ->
  (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

type error += Timeout
val with_timeout:
  ?canceler:Lwt_canceler.t ->
  unit Lwt.t -> (Lwt_canceler.t -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

module Make(Prefix : sig val id : string end) : Error_monad_sig.S

(**/**)
val json_to_string : (Data_encoding.json -> string) ref
