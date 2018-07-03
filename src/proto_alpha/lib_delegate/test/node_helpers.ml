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

exception Node_exited_prematurely

let handle_error res log_file_name =
  match res with
  | 0, _ ->
      ()
  | pid, Unix.WEXITED x ->
      Printf.eprintf "Wait: %d, exit %d\n\nDumping log:\n\n%!" pid x ;
      ignore (Sys.command (Printf.sprintf "cat %s" log_file_name) : int) ;
      raise Node_exited_prematurely
  | pid, Unix.WSIGNALED x ->
      Printf.eprintf "Wait: %d, signaled %d\n\nDumping log:\n\n%!" pid x ;
      ignore (Sys.command (Printf.sprintf "cat %s" log_file_name) : int) ;
      raise Node_exited_prematurely
  | pid, Unix.WSTOPPED x ->
      Printf.eprintf "Wait: %d, stopped %d\n\nDumping log:\n\n%!" pid x ;
      ignore (Sys.command (Printf.sprintf "cat %s" log_file_name) : int) ;
      raise Node_exited_prematurely

let fork_node ?exe ?(timeout = 4) ?(port = 18732) ?sandbox () =
  let data_dir =
    Printf.sprintf
      "%s/tezos_node_%6X"
      (Filename.get_temp_dir_name ())
      (Random.int 0xFF_FF_FF) in
  let log_file_name, log_file =
    Filename.open_temp_file "tezos_node_" ".log" in
  let sandbox =
    match sandbox with
    | None -> None
    | Some json ->
        let file_name, ch =
          Filename.open_temp_file "tezos_node_" ".log" in
        Printf.fprintf ch "%s%!"
          (Data_encoding.Json.to_string json) ;
        close_out ch ;
        Some file_name in
  let log_fd = Unix.descr_of_out_channel log_file in
  let null_fd = Unix.(openfile "/dev/null" [O_RDONLY] 0o644) in
  let exe =
    match exe with
    | Some exe -> exe
    | None ->
        let (//) = Filename.concat in
        try
          let path = Sys.argv.(1) in
          if Filename.is_relative path then
            Sys.getcwd () // ".." // path
          else
            path
        with _ -> Sys.getcwd () // ".." // "bin_node" // "main.exe" in
  let pid =
    Unix.create_process exe
      [| "tezos-node" ;
         "run" ;
         "--data-dir"; data_dir ;
         (match sandbox with
          | None -> "--sandbox"
          | Some path -> "--sandbox=" ^ path);
         "--rpc-addr"; "[::]:" ^ string_of_int port |]
      null_fd log_fd log_fd in
  Printf.printf "Created node, pid: %d, log: %s\n%!" pid log_file_name ;
  Printf.printf "Waiting %d seconds for its initialisation\n%!" timeout ;
  Unix.sleep timeout ;
  match Unix.waitpid [Unix.WNOHANG] pid with
  | 0, _ ->
      Pervasives.at_exit begin fun () ->
        begin
          match Unix.waitpid [Unix.WNOHANG] pid with
          | 0, _ ->
              Unix.kill pid Sys.sigkill ;
              Unix.sleep 1
          | res ->
              handle_error res log_file_name
        end ;
        ignore (Sys.command (Printf.sprintf "rm -fr \"%s\"" data_dir)) ;
        match sandbox with
        | None -> ()
        | Some file -> ignore (Sys.command (Printf.sprintf "rm -f \"%s\"" file))
      end ;
      pid
  | res ->
      handle_error res log_file_name ;
      0
