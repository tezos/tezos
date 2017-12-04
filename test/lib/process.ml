(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () = Lwt_unix.set_default_async_method Async_none

include Logging.Make (struct let name = "process" end)

exception Exited of int
exception Signaled of int
exception Stopped of int

let handle_error f =
  Lwt.catch
    f
    (fun exn -> Lwt.return (error_exn exn)) >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      lwt_log_error "%a" Error_monad.pp_print_error err >>= fun () ->
      exit 1

module Channel = struct
  type ('a, 'b) t = (Lwt_io.input_channel * Lwt_io.output_channel)
  let push (_, outch) v =
    Lwt.catch
      (fun () -> Lwt_io.write_value outch v >>= return)
      (fun exn -> fail (Exn exn))
  let pop (inch, _) =
    Lwt.catch
      (fun () -> Lwt_io.read_value inch >>= return)
      (fun exn -> fail (Exn exn))
end

let wait pid =
  Lwt.catch
    (fun () ->
       Lwt_unix.waitpid [] pid >>= function
       | (_,Lwt_unix.WEXITED 0) ->
           return ()
       | (_,Lwt_unix.WEXITED n) ->
           fail (Exn (Exited n))
       | (_,Lwt_unix.WSIGNALED n) ->
           fail (Exn (Signaled n))
       | (_,Lwt_unix.WSTOPPED n) ->
           fail (Exn (Stopped n)))
    (function
      | Lwt.Canceled ->
          Unix.kill pid Sys.sigkill ;
          return ()
      | exn ->
          fail (Exn exn))

type ('a, 'b) t = {
  termination: unit tzresult Lwt.t ;
  channel: ('b, 'a) Channel.t ;
}

let detach ?(prefix = "") f =
  Lwt_io.flush_all () >>= fun () ->
  let main_in, child_out = Lwt_io.pipe () in
  let child_in, main_out = Lwt_io.pipe () in
  match Lwt_unix.fork () with
  | 0 ->
      Logging.init Stderr >>= fun () ->
      Random.self_init () ;
      let template = Format.asprintf "%s$(message)" prefix in
      Lwt_main.run begin
        Lwt_io.close main_in >>= fun () ->
        Lwt_io.close main_out >>= fun () ->
        Logging.init ~template Stderr >>= fun () ->
        lwt_log_notice "PID: %d" (Unix.getpid ()) >>= fun () ->
        handle_error (fun () -> f (child_in, child_out))
      end ;
      exit 0
  | pid ->
      let termination = wait pid in
      Lwt_io.close child_in >>= fun () ->
      Lwt_io.close child_out >>= fun () ->
      Lwt.return ({ termination ; channel = (main_in, main_out) })

let signal_name =
  let names =
    [ Sys.sigabrt, "ABRT" ;
      Sys.sigalrm, "ALRM" ;
      Sys.sigfpe, "FPE" ;
      Sys.sighup, "HUP" ;
      Sys.sigill, "ILL" ;
      Sys.sigint, "INT" ;
      Sys.sigkill, "KILL" ;
      Sys.sigpipe, "PIPE" ;
      Sys.sigquit, "QUIT" ;
      Sys.sigsegv, "SEGV" ;
      Sys.sigterm, "TERM" ;
      Sys.sigusr1, "USR1" ;
      Sys.sigusr2, "USR2" ;
      Sys.sigchld, "CHLD" ;
      Sys.sigcont, "CONT" ;
      Sys.sigstop, "STOP" ;
      Sys.sigtstp, "TSTP" ;
      Sys.sigttin, "TTIN" ;
      Sys.sigttou, "TTOU" ;
      Sys.sigvtalrm, "VTALRM" ;
      Sys.sigprof, "PROF" ;
      Sys.sigbus, "BUS" ;
      Sys.sigpoll, "POLL" ;
      Sys.sigsys, "SYS" ;
      Sys.sigtrap, "TRAP" ;
      Sys.sigurg, "URG" ;
      Sys.sigxcpu, "XCPU" ;
      Sys.sigxfsz, "XFSZ" ] in
  fun n -> List.assoc n names

let wait_all processes =
  let rec loop processes =
    match processes with
    | [] -> Lwt.return_none
    | processes ->
        Lwt.nchoose_split processes >>= function
        | (finished, remaining) ->
            let rec handle = function
              | [] -> loop remaining
              | Ok () :: finished -> handle finished
              | Error err :: _ ->
                  Lwt.return (Some (err, remaining)) in
            handle finished in
  loop (List.map (fun p -> p.termination) processes) >>= function
  | None ->
      lwt_log_info "All done!" >>= fun () ->
      return ()
  | Some ([Exn (Exited n)], remaining) ->
      lwt_log_error "Early error!" >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join remaining >>= fun _ ->
      failwith "A process finished with error %d !" n
  | Some ([Exn (Signaled n)], remaining) ->
      lwt_log_error "Early error!" >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join remaining >>= fun _ ->
      failwith "A process was killed by a SIG%s !" (signal_name n)
  | Some ([Exn (Stopped n)], remaining) ->
      lwt_log_error "Early error!" >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join remaining >>= fun _ ->
      failwith "A process was stopped by a SIG%s !" (signal_name n)
  | Some (err, remaining) ->
      lwt_log_error "@[<v 2>Unexpected error!@,%a@]"
        pp_print_error err >>= fun () ->
      List.iter Lwt.cancel remaining ;
      join remaining >>= fun _ ->
      failwith "A process finished with an unexpected error !"

