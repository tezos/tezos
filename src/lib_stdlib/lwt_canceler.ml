(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
    cancel_hook = (fun () -> Lwt.return_unit) ;
    canceling = false ;
    canceled = false ;
  }

let cancel st =
  if st.canceled then
    Lwt.return_unit
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
         Lwt.return_unit)
  end

let on_cancel st cb =
  let hook = st.cancel_hook in
  st.cancel_hook <- (fun () -> hook () >>= cb)

let cancelation st =
  if st.canceling then Lwt.return_unit
  else Lwt_condition.wait st.cancelation

let canceled st = st.canceling
