(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val length : 'a Encoding.t -> 'a -> int
val read : 'a Encoding.t -> MBytes.t -> int -> int -> (int * 'a) option
val write : 'a Encoding.t -> 'a -> MBytes.t -> int -> int option
val to_bytes : 'a Encoding.t -> 'a -> MBytes.t
val of_bytes : 'a Encoding.t -> MBytes.t -> 'a option
val of_bytes_exn : 'a Encoding.t -> MBytes.t -> 'a

(** [to_bytes_list ?copy_blocks blocks_size encod data] encode the
    given data as a list of successive blocks of length
    'blocks_size' at most.

    NB. If 'copy_blocks' is false (default), the blocks of the list
    can be garbage-collected only when all the blocks are
    unreachable (because of the 'optimized' implementation of
    MBytes.sub used internally *)
val to_bytes_list : ?copy_blocks:bool -> int  -> 'a Encoding.t -> 'a -> MBytes.t list

(** This type is used when decoding binary data incrementally.
    - In case of 'Success', the decoded data, the size of used data
     to decode the result, and the remaining data are returned
    - In case of error, 'Error' is returned
    - 'Await' status embeds a function that waits for additional data
     to continue decoding, when given data are not sufficient *)
type 'a status =
  | Success of { res : 'a ; res_len : int ; remaining : MBytes.t list }
  | Await of (MBytes.t -> 'a status)
  | Error

(** This function allows to decode (or to initialize decoding) a
    stream of 'MByte.t'. The given data encoding should have a
    'Fixed' or a 'Dynamic' size, otherwise an exception
    'Invalid_argument "streaming data with variable size"' is
    raised *)
val read_stream_of_bytes : ?init:MBytes.t list -> 'a Encoding.t -> 'a status

(** Like read_stream_of_bytes, but only checks that the stream can
    be read. Note that this is an approximation because failures
    that may come from conversion functions present in encodings are
    not checked *)
val check_stream_of_bytes : ?init:MBytes.t list -> 'a Encoding.t -> unit status

val fixed_length : 'a Encoding.t -> int option
val fixed_length_exn : 'a Encoding.t -> int
