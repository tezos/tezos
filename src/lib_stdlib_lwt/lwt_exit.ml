(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
      | exn ->
          let backtrace = Printexc.get_backtrace () in
          Logging.Node.Main.fatal_error "@[<v 2>%a%a@]"
            (fun ppf exn ->
               Format.fprintf ppf
                 "@[Uncaught (asynchronous) exception (%d):@ %a@]"
                 (Unix.getpid ())
                 Error_monad.pp_exn exn)
            exn
            (fun ppf backtrace ->
               if String.length backtrace <> 0 then
                 Format.fprintf ppf
                   "@,Backtrace:@,  @[<h>%a@]"
                   Format.pp_print_text backtrace)
            backtrace ;
          Lwt.wakeup exit_wakener 1)
