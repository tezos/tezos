(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type cmd = Apdu_command : {
    cmd : 'a ;
    cla_of_cmd : 'a -> int ;
    ins_of_cmd : 'a -> int ;
  } -> cmd
(** Arbitrary type of a command, with its converters. *)

val create_cmd :
  cmd:'a -> cla_of_cmd:('a -> int) -> ins_of_cmd:('a -> int) -> cmd

type t = {
  cmd : cmd ;
  p1 : int ;
  p2 : int ;
  lc : int ;
  le : int ;
  data : Cstruct.t ;
}
(** Type of an ADPU. *)

val max_data_length : int
(** [max_data_length] is the maximum data length of an APDU. *)

val create :
  ?p1:int -> ?p2:int -> ?lc:int -> ?le:int -> ?data:Cstruct.t -> cmd -> t
val create_string :
  ?p1:int -> ?p2:int -> ?lc:int -> ?le:int -> ?data:string -> cmd -> t

val length : t -> int
(** [length t] is the size of [t] in bytes. *)

val write : Cstruct.t -> t -> Cstruct.t
(** [write cs t] writes t at [cs] and returns [cs] shifted by [length
    t] bytes. *)

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
