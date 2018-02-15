(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_context

class file_wallet dir : wallet = object (self)
  method private filename alias_name =
    Filename.concat
      dir
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
           Lwt_utils_unix.create_dir dir >>= fun () ->
           let filename = self#filename alias_name in
           let json = Data_encoding.Json.construct encoding list in
           Lwt_utils_unix.Json.write_file filename json)
        (fun exn -> Lwt.return (error_exn exn))
      |> generic_trace "could not write the %s alias file." alias_name
end

(* Default config *)

let (//) = Filename.concat

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

let default_base_dir = home // ".tezos-client"

let default_block = `Prevalidation

let default_log ~base_dir channel msg =
  let startup =
    CalendarLib.Printer.Precise_Calendar.sprint
      "%Y-%m-%dT%H:%M:%SZ"
      (CalendarLib.Calendar.Precise.now ()) in
  match channel with
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
        (fun chan -> Lwt_io.write chan msg)

let make_context
    ?(base_dir = default_base_dir)
    ?(block = default_block)
    ?(rpc_config = RPC_client.default_config)
    log =
  object
    inherit Client_context.logger log
    inherit file_wallet base_dir
    inherit RPC_client.http_ctxt rpc_config Media_type.all_media_types
    method block = block
  end

let ignore_context =
  make_context (fun _ _ -> Lwt.return ())
