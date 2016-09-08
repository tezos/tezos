(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A simple, portable implementation of network frames. *)

(** The type of a single datum in a network frame. The encoding of a
    datum is as follows: [[TYPE][CONTENTS]], where [[type]] is a
    single byte whose value is [1] for [S], [2] for [I], [3] for [L],
    [4] for B, [5] for [D], [6] for [F] and [7] for [C].
    For [S]. [I], [L] and [D]¸ the raw values are stored using big
    endianness. For [B], [F] and [C], the size is prefixed as a 16-bit,
    big endian, unsigned integer
    ([[SIZE][BYTES]]). *)
type chunk =
  | S of int (** A 16-bit integer *)
  | I of int32 (** A 32-bit integer *)
  | L of int64 (** A 64-bit integer *)
  | B of MBytes.t (** A series of bytes *)
  | D of float (** A 64-bits IEEE-754 floating point number *)
  | F of frame (** An encapsulated subframe *)
  | C of string (** A string *)

(** A network frame is a list of simple data. Its encoding on the
    network is as follows: [[SIZE][DATA]] where [[SIZE]] is the raw
    length of [[DATA]] in bytes as a big endian, 32-bit, unsigned
    integer. *)
and frame =
  chunk list

(** Writes a frame from to file descriptor Returns [true] if
    successful, [false] if an error happened, which means that the
    descriptor cannot accept any more data and should be closed. *)
val write : Lwt_unix.file_descr -> frame -> bool Lwt.t

(** Reads a frame from a file descriptor. Returns [Some frame] if
    successful, [None] if an error happened, which means either that
    that the descriptor cannot provide any more data or that corrupted
    bytes have been received, and in any case says that the descriptor
    should not be used anymore. The second parameter is the limit in
    bytes of the underlying representation, including the size. [None]
    is returned in case of overhead, and the bytes are not consumed
    from the descriptor. *)
val read : Lwt_unix.file_descr -> int -> frame option Lwt.t

(** Pretty printing of frames for debugging *)
val print : Format.formatter -> frame -> unit

(** Pretty prints of frames *)
val to_string : frame -> string

(** Encode a frame as raw bytes to send over the network *)
val to_raw : frame -> MBytes.t

(** Decode a complete raw frame as read from the network *)
val of_raw : MBytes.t -> frame option
