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
  | `Unauthorized of unit option (* 401 *)
  | `Forbidden of unit option (* 403 *)
  | `Not_found of unit option (* 404 *)
  | `Conflict of unit option (* 409 *)
  | `Error of unit option (* 500 *)
  ]

and 'a stream = 'a Resto_directory.Answer.stream = {
  next: unit -> 'a option Lwt.t ;
  shutdown: unit -> unit ;
}

let return x = Lwt.return (`Ok x)
let return_stream x = Lwt.return (`OkStream x)
