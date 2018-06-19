(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

exception Closed

type 'a t =
  { mutable data : 'a option ;
    mutable closed : bool ;
    mutable put_waiter : (unit Lwt.t * unit Lwt.u) option ;
  }

let create () =
  { data = None ;
    closed = false ;
    put_waiter = None ;
  }

let notify_put dropbox =
  match dropbox.put_waiter with
  | None -> ()
  | Some (_waiter, wakener) ->
      dropbox.put_waiter <- None ;
      Lwt.wakeup_later wakener ()

let put dropbox elt =
  if dropbox.closed then
    raise Closed
  else begin
    dropbox.data <- Some elt ;
    notify_put dropbox
  end

let peek dropbox = dropbox.data

let close dropbox =
  if not dropbox.closed then begin
    dropbox.closed <- true ;
    notify_put dropbox ;
  end

let wait_put ~timeout dropbox =
  match dropbox.put_waiter with
  | Some (waiter, _wakener) ->
      Lwt.choose [
        timeout ;
        Lwt.protected waiter
      ]
  | None ->
      let waiter, wakener = Lwt.wait () in
      dropbox.put_waiter <- Some (waiter, wakener) ;
      Lwt.choose [
        timeout ;
        Lwt.protected waiter ;
      ]

let rec take dropbox =
  match dropbox.data with
  | Some elt ->
      dropbox.data <- None ;
      Lwt.return elt
  | None ->
      if dropbox.closed then
        Lwt.fail Closed
      else
        wait_put ~timeout:(Lwt_utils.never_ending ()) dropbox >>= fun () ->
        take dropbox

let rec take_with_timeout timeout dropbox =
  match dropbox.data with
  | Some elt ->
      Lwt.cancel timeout ;
      dropbox.data <- None ;
      Lwt.return (Some elt)
  | None ->
      if Lwt.is_sleeping timeout then
        if dropbox.closed then
          Lwt.fail Closed
        else
          wait_put ~timeout dropbox >>= fun () ->
          take_with_timeout timeout dropbox
      else
        Lwt.return_none
