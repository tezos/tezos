(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Return type for service handler *)
type 'o t =
  [ `Ok of 'o (* 200 *)
  | `OkStream of 'o stream (* 200 *)
  | `Created of string option (* 201 *)
  | `No_content (* 204 *)
  | `Unauthorized of error list option (* 401 *)
  | `Forbidden of error list option (* 403 *)
  | `Not_found of error list option (* 404 *)
  | `Conflict of error list option (* 409 *)
  | `Error of error list option (* 500 *)
  ]

and 'a stream = {
  next: unit -> 'a option Lwt.t ;
  shutdown: unit -> unit ;
}

val return: 'o -> 'o t Lwt.t
val return_stream: 'o stream -> 'o t Lwt.t
val not_found: 'o t Lwt.t
val fail: error list -> 'a t Lwt.t
