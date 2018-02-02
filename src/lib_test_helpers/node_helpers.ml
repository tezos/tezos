(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  Format.eprintf "EXE %s@." exe ;
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
        ignore (Sys.command (Printf.sprintf "rm -fr \"%s\"" data_dir))
      end ;
      pid
  | res ->
      handle_error res log_file_name ;
      0
