(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let wait_for_bootstrapped (ctxt : #Client_context.full) =
  let display = ref false in
  Lwt.async begin fun () ->
    Lwt_unix.sleep 0.3 >>= fun () ->
    if not !display then
      ctxt#answer "Waiting for the node to be bootstrapped before injection..." >>= fun () ->
      display := true ;
      Lwt.return_unit
    else
      Lwt.return_unit
  end ;
  Monitor_services.bootstrapped ctxt >>=? fun (stream, _stop) ->
  Lwt_stream.iter_s
    (fun (hash, time) ->
       if !display then
         ctxt#message "Current head: %a (timestamp: %a, validation: %a)"
           Block_hash.pp_short hash
           Time.System.pp_hum (Time.System.of_protocol_exn time)
           Time.System.pp_hum (Tezos_stdlib_unix.Systime_os.now ())
       else Lwt.return_unit) stream >>= fun () ->
  display := true ;
  ctxt#answer "Node is bootstrapped, ready for injecting operations." >>= fun () ->
  return_unit
