(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

(** Erroneous result (shortcut for generic errors) *)
val generic_error : string -> 'a tzresult

(** Erroneous return (shortcut for generic errors) *)
val failwith :
  ('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 ->
  'a

val protect :
  ?on_error: (error list -> 'a tzresult Lwt.t) ->
  (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

val error_exn : exn -> 'a tzresult
val record_trace_exn : exn -> 'a tzresult -> 'a tzresult
val trace_exn : exn -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t
val pp_exn : Format.formatter -> exn -> unit

type error += Exn of exn
type error += Unclassified of string

module Make() : Error_monad_sig.S

(**/**)
val json_to_string : (Data_encoding.json -> string) ref
