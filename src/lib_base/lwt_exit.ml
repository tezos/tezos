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
      | e ->
          let backtrace = Printexc.get_backtrace () in
          Base_logging.(fatal_error Tag.DSL.(fun f ->
              f "@[<v 2>@[Uncaught (asynchronous) exception (%d):@ %a@]%a@]"
              -% t event "uncaught_async_exception"
              -% s pid (Unix.getpid ())
              -% a exn e
              -% a exn_trace backtrace)) ;
          Lwt.wakeup exit_wakener 1)
