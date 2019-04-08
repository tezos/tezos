(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Client_context
include Internal_event.Legacy_logging.Make_semantic
    (struct let name = "client.context.unix" end)

class unix_wallet ~base_dir ~password_filename : wallet = object (self)

  method load_passwords = match password_filename with
    | None -> None
    | Some filename ->
        if Sys.file_exists filename then
          Some (Lwt_io.lines_of_file filename)
        else
          None

  method read_file path =
    Lwt.catch
      (fun () ->
         Lwt_io.(with_file ~mode:Input path read) >>= fun content ->
         return content)
      (fun exn ->
         failwith
           "cannot read file (%s)" (Printexc.to_string exn))

  method private filename alias_name =
    Filename.concat
      base_dir
      (Str.(global_replace (regexp_string " ") "_" alias_name) ^ "s")

  method with_lock : type a. (unit -> a Lwt.t) -> a Lwt.t =
    (fun f ->
       let unlock fd =
         let fd = Lwt_unix.unix_file_descr fd in
         Unix.lockf fd Unix.F_ULOCK 0 ;
         Unix.close fd in
       let lock () =
         Lwt_unix.openfile (Filename.concat base_dir "wallet_lock")
           Lwt_unix.[ O_CREAT ; O_WRONLY ] 0o644 >>= fun fd ->
         Lwt_unix.lockf fd Unix.F_LOCK 0 >>= fun () ->
         Lwt.return (fd, (Lwt_unix.on_signal Sys.sigint
                            (fun _s ->
                               unlock fd ;
                               exit 0 (* exit code? *) ))) in
       lock () >>= fun (fd, sh) ->
       (* catch might be useless if f always uses the error monad *)
       Lwt.catch f (function e -> Lwt.return (unlock fd ; raise e)) >>= fun res ->
       Lwt.return (unlock fd) >>= fun () ->
       Lwt_unix.disable_signal_handler sh ;
       Lwt.return res)

  method load : type a. string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t =
    fun alias_name ~default encoding ->
      let filename = self#filename alias_name in
      if not (Sys.file_exists filename) then
        return default
      else
        Lwt_utils_unix.Json.read_file filename
        |> generic_trace
          "could not read the %s alias file" alias_name >>=? fun json ->
        match Data_encoding.Json.destruct encoding json with
        | exception e ->
            failwith "did not understand the %s alias file %s : %s"
              alias_name filename (Printexc.to_string e)
        | data ->
            return data

  method write :
    type a. string -> a -> a Data_encoding.encoding -> unit tzresult Lwt.t =
    fun alias_name list encoding ->
      Lwt.catch
        (fun () ->
           Lwt_utils_unix.create_dir base_dir >>= fun () ->
           let filename = self#filename alias_name in
           let json = Data_encoding.Json.construct encoding list in
           Lwt_utils_unix.Json.write_file filename json)
        (fun exn -> Lwt.return (error_exn exn))
      |> generic_trace "could not write the %s alias file." alias_name
end

class unix_prompter = object
  method prompt : type a. (a, string tzresult) lwt_format -> a =
    Format.kasprintf begin fun msg ->
      print_string msg ;
      let line = read_line () in
      return line
    end

  method prompt_password : type a. (a, MBytes.t tzresult) lwt_format -> a =
    Format.kasprintf begin fun msg ->
      print_string msg ;
      let line = Lwt_utils_unix.getpass () in
      return (MBytes.of_string line)
    end
end

class unix_logger ~base_dir =
  let startup = Format.asprintf "%a" Time.System.pp_hum (Systime_os.now ()) in
  let log channel msg = match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return_unit
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return_unit
    | log ->
        let (//) = Filename.concat in
        Lwt_utils_unix.create_dir (base_dir // "logs" // log) >>= fun () ->
        Lwt_io.with_file
          ~flags: Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ]
          ~mode: Lwt_io.Output
          (base_dir // "logs" // log // startup)
          (fun chan -> Lwt_io.write chan msg) in
  object
    inherit Client_context.simple_printer log
  end

class unix_ui = object
  method sleep = Lwt_unix.sleep
end

class unix_full ~base_dir ~chain ~block ~confirmations ~password_filename ~rpc_config : Client_context.full =
  object
    inherit unix_logger ~base_dir
    inherit unix_prompter
    inherit unix_wallet ~base_dir ~password_filename
    inherit RPC_client.http_ctxt rpc_config Media_type.all_media_types
    inherit unix_ui
    method chain = chain
    method block = block
    method confirmations = confirmations
  end
