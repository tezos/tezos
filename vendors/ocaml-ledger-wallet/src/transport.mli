(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Status : sig
  type t =
    | Invalid_pin of int
    | Incorrect_length
    | Incompatible_file_structure
    | Security_status_unsatisfied
    | Conditions_of_use_not_satisfied
    | Incorrect_data
    | File_not_found
    | Incorrect_params
    | Ins_not_supported
    | Technical_problem of int
    | Ok

  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

val write_apdu :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> Apdu.t -> unit
(** [write_apdu ?pp ?buf ledger apdu] writes [apdu] to [ledger]. *)

val read : ?buf:Cstruct.t -> Hidapi.t -> Status.t * Cstruct.t
(** [read ?buf ledger] reads from [ledger] a status response and a
    payload. *)

val ping : ?buf:Cstruct.t -> Hidapi.t -> unit
(** [ping ?buf ledger] writes a ping packet to [ledger], optionally
    containing [buf]. *)

val apdu :
  ?pp:Format.formatter -> ?msg:string -> ?buf:Cstruct.t ->
  Hidapi.t -> Apdu.t -> Cstruct.t
(** [apdu ?pp ?msg ?buf ledger apdu] writes [apdu] to [ledger] and
    returns the response. *)

val write_payload :
  ?pp:Format.formatter -> ?msg:string -> ?buf:Cstruct.t ->
  ?mark_last:bool -> cmd:Apdu.cmd -> ?p1:int -> ?p2:int ->
  Hidapi.t -> Cstruct.t -> Cstruct.t
(** [write_payload ?pp ?msg ?buf ?mark_last ~cmd ?p1 ?p2 ledger
    payload] writes the [payload] of [cmd] into [ledger] and returns
    the response. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
