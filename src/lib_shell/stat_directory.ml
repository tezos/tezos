(*****************************************************************************)
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

type Error_monad.error += Mem_stat_failure of string

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"node.shell.stats"
    ~title:"Memory stats failure"
    ~description:"Memory stats failure"
    ~pp:begin fun ppf s ->
      Format.fprintf ppf
        "@[<v 2>Memory stat failure %s@]"
        s
    end
    Data_encoding.(obj1 (req "failure" string))
    (function Mem_stat_failure s -> Some s | _ -> None)
    (fun s -> Mem_stat_failure s)


let memory_stats () =
  let pid = string_of_int @@ Unix.getpid () in
  if Sys.os_type = "Unix" then
    Lwt.catch
      (fun () ->
         begin Lwt_process.with_process_in  ("uname", [| "uname" |])
             (fun pc -> Lwt_io.read_line pc#stdout) >>= function
           | "Linux" ->
               Lwt_process.with_process_in
                 ("getconf", [| "getconf";  "PAGE_SIZE" |])
                 (fun pc -> Lwt_io.read_line pc#stdout >>= fun ps ->
                   Lwt.return (int_of_string ps)) >>= fun ps -> return (ps, Stat_services.Linux)
           | "Darwin" ->
               Lwt_process.with_process_in ("pagesize", [| "pagesize" |])
                 (fun pc -> Lwt_io.read_line pc#stdout >>= fun ps ->
                   Lwt.return (int_of_string ps)) >>= fun ps -> return (ps, Stat_services.Darwin)
           | os ->
               Lwt.return
                 (error (Mem_stat_failure ("Unexpected os ("^os^")"))) end >>=? fun (page_size, os) ->
         match os with
         | Stat_services.Linux ->
             let fname = ("/proc/"^pid^"/statm") in
             begin Lwt_unix.file_exists fname >>= function
               | true ->
                   begin Lwt_io.open_file ~mode:Input fname >>= fun ic ->
                     Lwt_io.read_line ic >>= fun line ->
                     match List.map Int64.of_string @@ String.split ' ' line with
                     | size::resident::shared::text::lib::data::dt::_ ->
                         return
                           Stat_services.(Statm { page_size ; size ; resident ;
                                                  shared ; text ;
                                                  lib ; data ; dt ; })
                     | _ ->  Lwt.return (error (Mem_stat_failure "Unexpected proc/<pid>/statm format")) end
               | false ->
                   Lwt.return
                     (error (Mem_stat_failure (fname^" not found"))) end
         | Stat_services.Darwin ->
             Lwt_process.with_process_in ~env:[| "LC_ALL=C" |]
               ("ps", [|  "ps" ; "-o" ; "pid,%mem,rss" ; "-p"; pid |])
               (fun pc -> Lwt_io.read_line_opt pc#stdout >>= function
                  | None ->
                      Lwt.return (error (Mem_stat_failure "Unexpected ps answer (1st line)"))
                  | Some _ -> (* first line is useless *)
                      Lwt_io.read_line_opt pc#stdout >>= function
                      | None ->
                          Lwt.return (error (Mem_stat_failure "Unexpected ps answer (2nd line)"))
                      | Some ps_stats ->
                          match String.split ' ' ps_stats with
                          | _pid::mem::resident::_ ->
                              return
                                Stat_services.(Ps { page_size ;
                                                    mem = float_of_string mem ;
                                                    resident = Int64.of_string resident })
                          | _ -> Lwt.return (error (Mem_stat_failure "Unexpected ps answer"))
               ))
      (function _ ->
         Lwt.return (error (Mem_stat_failure "memory_stat failure")) )
  else Lwt.return (error (Mem_stat_failure ("cannot get stat from os_type : "^Sys.os_type)))


let rpc_directory () =
  let dir = RPC_directory.empty in
  RPC_directory.gen_register dir Stat_services.S.gc begin fun () () () ->
    RPC_answer.return @@ Gc.stat () end |> fun dir ->

  RPC_directory.gen_register dir Stat_services.S.memory begin fun () () () ->
    memory_stats () >>= function
    | Ok stats ->
        RPC_answer.return stats
    | Error errs -> RPC_answer.fail errs end
