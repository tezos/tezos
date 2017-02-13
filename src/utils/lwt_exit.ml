(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception Exit

let termination_thread, exit_wakener = Lwt.wait ()
let exit x = Lwt.wakeup exit_wakener x; raise Exit

let () =
  Lwt.async_exception_hook :=
    (function
      | Exit -> ()
      (* BEGIN HACK
         ignore error unhandled by cohttp (until PR129 is merged) *)
      | Unix.Unix_error(Unix.ECONNRESET, _, _)
      | Unix.Unix_error(Unix.EPIPE, _, _) -> ()
      (* END HACK *)
      | exn ->
          Format.eprintf
            "@[Uncaught (asynchronous) exception (%d):@ %a@]"
            (Unix.getpid ())
            Error_monad.pp_exn exn ;
          let backtrace = Printexc.get_backtrace () in
          if String.length backtrace <> 0 then
            Format.eprintf "\n%s" backtrace ;
          Format.eprintf "@." ;
          Lwt.wakeup exit_wakener 1)
