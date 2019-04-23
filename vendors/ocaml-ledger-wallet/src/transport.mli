(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Status : sig
  type t = ..
  type t +=
    | Invalid_pin of int
    | Incorrect_length
    | Incorrect_length_for_ins
    | Incompatible_file_structure
    | Security_status_unsatisfied
    | Hid_required
    | Conditions_of_use_not_satisfied
    | Incorrect_data
    | File_not_found
    | Parse_error
    | Incorrect_params
    | Incorrect_class
    | Ins_not_supported
    | Memory_error
    | Referenced_data_not_found
    | Technical_problem of int
    | Ok

  val register_string_f : (t -> string option) -> unit

  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Header : sig
  module Error : sig
    type t =
      | Header_too_short of int
      | Invalid_channel of int
      | Invalid_command_tag of int
      | Unexpected_sequence_number of { expected : int ;
                                        actual : int }
  end
end

type transport_error =
  | Hidapi of string
  | Incomplete_write of int
  | Incomplete_read of int

type error =
  | AppError of { status : Status.t ; msg : string }
  | ApduError of Header.Error.t
  | TransportError of transport_error

val app_error :
  msg:string -> ('a, Status.t) result -> ('a, error) result

val pp_error : Format.formatter -> error -> unit

val write_apdu :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> Apdu.t -> (unit, error) result
(** [write_apdu ?pp ?buf ledger apdu] writes [apdu] to [ledger]. *)

val read :
  ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (Status.t * Cstruct.t, error) result
(** [read ?pp ?buf ledger] reads from [ledger] a status response and a
    payload. *)

val ping : ?pp:Format.formatter -> ?buf:Cstruct.t ->
  Hidapi.t -> (unit, error) result
(** [ping ?pp ?buf ledger] writes a ping packet to [ledger],
    optionally containing [buf]. *)

val apdu :
  ?pp:Format.formatter -> ?msg:string -> ?buf:Cstruct.t ->
  Hidapi.t -> Apdu.t -> (Cstruct.t, error) result
(** [apdu ?pp ?msg ?buf ledger apdu] writes [apdu] to [ledger] and
    returns the response. *)

val write_payload :
  ?pp:Format.formatter -> ?msg:string -> ?buf:Cstruct.t ->
  ?mark_last:bool -> cmd:Apdu.cmd -> ?p1:int -> ?p2:int ->
  Hidapi.t -> Cstruct.t -> (Cstruct.t, error) result
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
