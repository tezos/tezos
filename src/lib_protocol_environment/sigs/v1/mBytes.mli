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

type t

val create: int -> t

val length: t -> int

val copy: t -> t

val sub: t -> int -> int -> t
(** [sub src ofs len] extract a sub-array of [src] starting at [ofs]
    and of length [len]. No copying of elements is involved: the
    sub-array and the original array share the same storage space. *)

val blit: t -> int -> t -> int -> int -> unit
(** [blit src ofs_src dst ofs_dst len] copy [len] bytes from [src]
    starting at [ofs_src] into [dst] starting at [ofs_dst]. *)

val blit_of_string: string -> int -> t -> int -> int -> unit
(** See [blit] *)

val blit_to_bytes: t -> int -> bytes -> int -> int -> unit
(** See [blit] *)

val of_string: string -> t
(** [of_string s] create an byte array filled with the same content than [s]. *)

val to_string: t -> string
(** [to_string b] dump the array content in a [string]. *)

val sub_string: t -> int -> int -> string
(** [sub_string b ofs len] is equivalent to [to_string (sub b ofs len)]. *)



(** Functions reading and writing bytes  *)

val get_char: t -> int -> char
(** [get_char buff i] reads 1 byte at offset i as a char *)

val get_uint8: t -> int -> int
(** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
    bits. i.e. It returns a value between 0 and 2^8-1 *)

val get_int8: t -> int -> int
(** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
    bits. i.e. It returns a value between -2^7 and 2^7-1 *)

val set_char: t -> int -> char -> unit
(** [set_char buff i v] writes [v] to [buff] at offset [i] *)

val set_int8: t -> int -> int -> unit
(** [set_int8 buff i v] writes the least significant 8 bits of [v]
    to [buff] at offset [i] *)

(** Functions reading according to Big Endian byte order *)

val get_uint16: t -> int -> int
(** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)

val get_int16: t -> int -> int
(** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)

val get_int32: t -> int -> int32
(** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)

val get_int64: t -> int -> int64
(** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)

val set_int16: t -> int -> int -> unit
(** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] *)

val set_int32: t -> int -> int32 -> unit
(** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)

val set_int64: t -> int -> int64 -> unit
(** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)


module LE: sig

  (** Functions reading according to Little Endian byte order *)

  val get_uint16: t -> int -> int
  (** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)

  val get_int16: t -> int -> int
  (** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)

  val get_int32: t -> int -> int32
  (** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)

  val get_int64: t -> int -> int64
  (** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)

  val set_int16: t -> int -> int -> unit
  (** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] *)

  val set_int32: t -> int -> int32 -> unit
  (** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)

  val set_int64: t -> int -> int64 -> unit
  (** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)

end

val (=) : t -> t -> bool
val (<>) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
val (>=) : t -> t -> bool
val (>) : t -> t -> bool
val compare : t -> t -> int

val concat: string -> t list -> t

val to_hex: t -> [ `Hex of string ]
val of_hex: [ `Hex of string ] -> t
