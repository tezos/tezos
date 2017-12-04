(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Low-level byte growing byte vector. Internally uses operations in
    {!MBytes}. The vector is resized as needed.
    All operations append to the end of the vector. **)


(** Type of vectors *)
type t


(** Create a vector with an initial size of [initial_size]. *)
val create : ?initial_size:int -> unit -> t


(** [write_mbytes t src src_offset len]
    Copy a sequence of len bytes from [src] to [t] starting at [src_offset]. *)
val write_mbytes : t -> MBytes.t -> int -> int -> unit

(** Write a character to the vector *)
val write_char : t -> char -> unit

(** Write an 8-bit signed integer to the vector *)
val write_int8 : t -> int -> unit

(** Write an 16-bit signed integer to the vector *)
val write_int16 : t -> int -> unit

(** Write an 32-bit signed integer to the vector *)
val write_int32 : t -> int32 -> unit

(** Write an 64-bit signed integer to the vector *)
val write_int64 : t -> int64 -> unit

(** Write a single-precision float to the vector *)
val write_float : t -> float -> unit

(** Write a double-precision float to the vector *)
val write_double : t -> float -> unit

(** Write the characters from a string to the vector.
    This does not copy the size. *)
val write_string_data : t -> string -> unit

(** Write a sized amount of data to the vector.
    Size is a 32 bit integer.
    Do not use [to_mbytes] in the writer function. *)
val write_sized : t -> (unit -> unit) -> unit

(** Convert the buffer to mBytes.
    As all operations are append only, you can continue to add to the
    buffer after this function has been called.
    The buffer may be more than twice as large as necessary to contain
    the written data. Calling [MBytes.copy] will create a new buffer
    that is exactly the required size. *)
val to_mbytes : t -> MBytes.t
