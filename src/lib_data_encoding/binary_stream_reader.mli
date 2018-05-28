(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {Data_encoding.Binary}. *)

type 'ret status =
  | Success of { result : 'ret ; size : int ; stream : Binary_stream.t }
  | Await of (MBytes.t -> 'ret status)
  | Error of Binary_error.read_error

val read_stream: ?init:Binary_stream.t -> 'a Encoding.t -> 'a status
