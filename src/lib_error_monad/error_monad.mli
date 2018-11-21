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

(** [protect] is a wrapper around [Lwt.catch] where the error handler operates
    over `error list` instead of `exn`. Besides, [protect ~on_error ~canceler ~f]
    may *cancel* [f] via a [Lwt_canceler.t].

    More precisely, [protect ~on_error ~canceler f] runs [f ()]. An Lwt failure
    triggered by [f ()] is wrapped into an [Exn]. If a [canceler] is given and
    [Lwt_canceler.cancelation canceler] is determined before [f ()],
    a [Canceled] error is returned.

    Errors are caught by [~on_error] (if given), otherwise the previous value
    is returned. An Lwt failure triggered by [~on_error] is wrapped into an
    [Exn] *)
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

val errs_tag : error list Tag.def
