(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A 'dropbox' with a single element. *)

type 'a t
(** Type of dropbox holding a value of type ['a] *)

val create: unit -> 'a t
(** Create an empty dropbox. *)

val put: 'a t -> 'a -> unit
(** Put an element inside the dropbox. If the dropbox was already
    containing an element, the old element is replaced by the new one.
    The function might return [Closed] if the dropbox has been closed
    with [close]. *)

val take: 'a t -> 'a Lwt.t
(** Wait until the dropbox contains an element, then returns the elements.
    The elements is removed from the dropbox. The function might return
    [Closed] if the dropbox is empty and closed. *)

val take_with_timeout: unit Lwt.t -> 'a t -> 'a option Lwt.t
(** Like [take] except that it returns [None] after [timeout seconds]
    if the dropbox is still empty. *)

val peek: 'a t -> 'a option
(** Read the current element of the dropbox without removing it. It
    immediatly returns [None] if the dropbox is empty. *)

exception Closed
(** The exception returned when trying to access a 'closed' dropbox. *)

val close: 'a t -> unit
(** Close the dropox. It terminates all the waiting reader with the
    exception [Closed]. All further read or write will also immediatly
    fail with [Closed], except if the dropbox is not empty when
    [close] is called. In that can, a single (and last) [take] will
    succeed. *)

