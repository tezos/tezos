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

exception Exit

let termination_thread, exit_wakener = Lwt.wait ()
let exit x = Lwt.wakeup exit_wakener x; raise Exit

let () =
  Lwt.async_exception_hook :=
    (function
      | Exit -> ()
      | e ->
          let backtrace = Printexc.get_backtrace () in
          let pp_exn_trace ppf backtrace =
            if String.length backtrace <> 0 then
              Format.fprintf ppf
                "@,Backtrace:@,  @[<h>%a@]"
                Format.pp_print_text backtrace
          in
          (* TODO Improve this *)
          Format.eprintf
            "@[<v 2>@[Uncaught (asynchronous) exception (%d):@ %s@]%a@]@.%!"
            (Unix.getpid ())
            (Printexc.to_string e)
            pp_exn_trace backtrace ;
          Lwt.wakeup exit_wakener 1)
