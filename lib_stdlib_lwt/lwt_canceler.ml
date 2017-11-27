(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

type t = {
  cancelation: unit Lwt_condition.t ;
  cancelation_complete: unit Lwt_condition.t ;
  mutable cancel_hook: unit -> unit Lwt.t ;
  mutable canceling: bool ;
  mutable canceled: bool ;
}

let create () =
  let cancelation = Lwt_condition.create () in
  let cancelation_complete = Lwt_condition.create () in
  { cancelation ; cancelation_complete ;
    cancel_hook = (fun () -> Lwt.return ()) ;
    canceling = false ;
    canceled = false ;
  }

let cancel st =
  if st.canceled then
    Lwt.return ()
  else if st.canceling then
    Lwt_condition.wait st.cancelation_complete
  else begin
    st.canceling <- true ;
    Lwt_condition.broadcast st.cancelation () ;
    Lwt.finalize
      st.cancel_hook
      (fun () ->
         st.canceled <- true ;
         Lwt_condition.broadcast st.cancelation_complete () ;
         Lwt.return ())
  end

let on_cancel st cb =
  let hook = st.cancel_hook in
  st.cancel_hook <- (fun () -> hook () >>= cb)

let cancelation st =
  if st.canceling then Lwt.return ()
  else Lwt_condition.wait st.cancelation

let canceled st = st.canceling
