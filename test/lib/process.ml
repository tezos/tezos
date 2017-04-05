(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make (struct let name = "process" end)

open Error_monad

exception Exited of int

let detach ?(prefix = "") f =
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | 0 ->
      Random.self_init () ;
      let template = Format.asprintf "%s$(section): $(message)" prefix in
      Lwt_main.run begin
        Logging.init ~template Stderr >>= fun () ->
        lwt_log_notice "PID: %d" (Unix.getpid ()) >>= fun () ->
        f ()
      end ;
      exit 0
  | pid ->
      Lwt.catch
        (fun () ->
           Lwt_unix.waitpid [] pid >>= function
           | (_,Lwt_unix.WEXITED 0) ->
               Lwt.return_unit
           | (_,Lwt_unix.WEXITED n) ->
               Lwt.fail (Exited n)
           | (_,Lwt_unix.WSIGNALED _)
           | (_,Lwt_unix.WSTOPPED _) ->
               Lwt.fail Exit)
        (function
          | Lwt.Canceled ->
              Unix.kill pid Sys.sigkill ;
              Lwt.return_unit
          | exn -> Lwt.fail exn)

let handle_error f =
  Lwt.catch
    f
    (fun exn -> Lwt.return (error_exn exn)) >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      lwt_log_error "%a" Error_monad.pp_print_error err >>= fun () ->
      exit 1

let rec wait processes =
  Lwt.catch
    (fun () ->
       Lwt.nchoose_split processes >>= function
       | (_, []) -> lwt_log_notice "All done!"
       | (_, processes) -> wait processes)
    (function
      | Exited n ->
          lwt_log_notice "Early error!" >>= fun () ->
          List.iter Lwt.cancel processes ;
          Lwt.catch
            (fun () -> Lwt.join processes)
            (fun _ -> Lwt.return_unit) >>= fun () ->
          lwt_log_notice "A process finished with error %d !" n >>= fun () ->
          Pervasives.exit n
      | exn ->
          lwt_log_notice "Unexpected error!%a"
            Error_monad.pp_exn exn >>= fun () ->
          List.iter Lwt.cancel processes ;
          Lwt.catch
            (fun () -> Lwt.join processes)
            (fun _ -> Lwt.return_unit) >>= fun () ->
          Pervasives.exit 2)

