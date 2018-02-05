(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Notification callbacks} *)

type 'a input
type stopper

val create_input : unit -> 'a input
val notify : 'a input -> 'a -> unit
val create_stream : 'a input -> 'a Lwt_stream.t * stopper
val create_fake_stream : unit -> 'a Lwt_stream.t * stopper
val shutdown : stopper -> unit
