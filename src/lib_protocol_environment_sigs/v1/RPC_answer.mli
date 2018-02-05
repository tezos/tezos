(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Return type for service handler *)
type ('o, 'e) t =
  [ `Ok of 'o (* 200 *)
  | `OkStream of 'o stream (* 200 *)
  | `Created of string option (* 201 *)
  | `No_content (* 204 *)
  | `Unauthorized of 'e option (* 401 *)
  | `Forbidden of 'e option (* 403 *)
  | `Not_found of 'e option (* 404 *)
  | `Conflict of 'e option (* 409 *)
  | `Error of 'e option (* 500 *)
  ]

and 'a stream = {
  next: unit -> 'a option Lwt.t ;
  shutdown: unit -> unit ;
}

val return: 'o -> ('o, 'e) t Lwt.t
val return_stream: 'o stream -> ('o, 'e) t Lwt.t
