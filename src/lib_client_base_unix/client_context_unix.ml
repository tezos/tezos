(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_context

class unix_wallet ~base_dir : wallet = object (self)
  method private filename alias_name =
    Filename.concat
      base_dir
      (Str.(global_replace (regexp_string " ") "_" alias_name) ^ "s")

  method load : type a. string -> default:a -> a Data_encoding.encoding -> a tzresult Lwt.t =
    fun alias_name ~default encoding ->
      let filename = self#filename alias_name in
      if not (Sys.file_exists filename) then
        return default
      else
        Lwt_utils_unix.Json.read_file filename
        |> generic_trace
          "couldn't to read the %s file" alias_name >>=? fun json ->
        match Data_encoding.Json.destruct encoding json with
        | exception _ -> (* TODO print_error *)
            failwith "didn't understand the %s file" alias_name
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
  let startup =
    CalendarLib.Printer.Precise_Calendar.sprint
      "%Y-%m-%dT%H:%M:%SZ"
      (CalendarLib.Calendar.Precise.now ()) in
  let log channel msg = match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return ()
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return ()
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

class unix_full ~base_dir ~block ~rpc_config : Client_context.full =
  object
    inherit unix_logger ~base_dir
    inherit unix_prompter
    inherit unix_wallet ~base_dir
    inherit RPC_client.http_ctxt rpc_config Media_type.all_media_types
    method block = block
  end
