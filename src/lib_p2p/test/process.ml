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

open Error_monad

let () = Lwt_unix.set_default_async_method Async_none

let section = Lwt_log.Section.make "process"
let log_f ~level format =
  if level < Lwt_log.Section.level section then
    Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
  else
    Format.kasprintf (fun msg -> Lwt_log.log ~section ~level msg) format
let lwt_debug fmt = log_f ~level:Lwt_log.Debug fmt
let lwt_log_notice fmt = log_f ~level:Lwt_log.Notice fmt
let lwt_log_info fmt = log_f ~level:Lwt_log.Info fmt
let lwt_log_error fmt = log_f ~level:Lwt_log.Error fmt

exception Exited of int
exception Signaled of int
exception Stopped of int

let handle_error f =
  Lwt.catch
    f
    (fun exn -> Lwt.return_error [Exn exn]) >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      lwt_debug "%a" pp_print_error err >>= fun () ->
      exit 1

module Channel = struct
  type ('a, 'b) t = (Lwt_io.input_channel * Lwt_io.output_channel)
  let push (_, outch) v =
    Lwt.catch
      (fun () -> Lwt_io.write_value outch v >>= Lwt.return_ok)
      (fun exn -> Lwt.return_error [Exn exn])
  let pop (inch, _) =
    Lwt.catch
      (fun () -> Lwt_io.read_value inch >>= Lwt.return_ok)
      (fun exn -> Lwt.return_error [Exn exn])
end

let wait pid =
  Lwt.catch
    (fun () ->
       Lwt_unix.waitpid [] pid >>= function
       | (_,Lwt_unix.WEXITED 0) ->
           Lwt.return_ok ()
       | (_,Lwt_unix.WEXITED n) ->
           Lwt.return_error [Exn (Exited n)]
       | (_,Lwt_unix.WSIGNALED n) ->
           Lwt.return_error [Exn (Signaled n)]
       | (_,Lwt_unix.WSTOPPED n) ->
           Lwt.return_error [Exn (Stopped n)])
    (function
      | Lwt.Canceled ->
          Unix.kill pid Sys.sigkill ;
          Lwt.return_ok ()
      | exn ->
          Lwt.return_error [Exn exn])

type ('a, 'b) t = {
  termination: unit tzresult Lwt.t ;
  channel: ('b, 'a) Channel.t ;
}

let template = "$(date) - $(section): $(message)"

let detach ?(prefix = "") f =
  Lwt_io.flush_all () >>= fun () ->
  let main_in, child_out = Lwt_io.pipe () in
  let child_in, main_out = Lwt_io.pipe () in
  match Lwt_unix.fork () with
  | 0 ->
      Lwt_log.default :=
        Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr () ;
      Random.self_init () ;
      let template = Format.asprintf "%s$(message)" prefix in
      Lwt_main.run begin
        Lwt_io.close main_in >>= fun () ->
        Lwt_io.close main_out >>= fun () ->
        Lwt_log.default :=
          Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr () ;
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
                  Lwt.return_some (err, remaining) in
            handle finished in
  loop (List.map (fun p -> p.termination) processes) >>= function
  | None ->
      lwt_log_info "All done!" >>= fun () ->
      Lwt.return_ok ()
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
