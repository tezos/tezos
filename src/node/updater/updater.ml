(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Updater

let (//) = Filename.concat

type validation_result = Protocol_sigs.validation_result = {
  context: Context.t ;
  fitness: Fitness.t ;
  message: string option ;
  max_operations_ttl: int ;
}

type rpc_context = Protocol_sigs.rpc_context = {
  block_hash: Block_hash.t ;
  block_header: Block_header.t ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> Operation.t list list Lwt.t ;
  context: Context.t ;
}

module type PROTOCOL = Protocol_sigs.PROTOCOL
module type PACKED_PROTOCOL = Protocol_sigs.PACKED_PROTOCOL
module type REGISTRED_PROTOCOL = sig
  val hash: Protocol_hash.t
  include PROTOCOL with type error := error
                             and type 'a tzresult := 'a tzresult
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

(** Version table *)

module VersionTable = Protocol_hash.Table

let versions : ((module REGISTRED_PROTOCOL)) VersionTable.t =
  VersionTable.create 20

let register hash proto =
  VersionTable.add versions hash proto

let activate = Context.set_protocol
let fork_test_network = Context.fork_test_network

let get_exn hash = VersionTable.find versions hash
let get hash =
  try Some (get_exn hash)
  with Not_found -> None

(** Compiler *)

let datadir = ref None
let get_datadir () =
  match !datadir with
  | None -> fatal_error "not initialized"
  | Some m -> m

let init dir =
  datadir := Some dir

let create_files dir units =
  Lwt_utils.remove_dir dir >>= fun () ->
  Lwt_utils.create_dir dir >>= fun () ->
  Lwt_list.map_s
    (fun { Protocol.name; interface; implementation } ->
       let name = String.lowercase_ascii name in
       let ml = dir // (name ^ ".ml") in
       let mli = dir // (name ^ ".mli") in
       Lwt_utils.create_file ml implementation >>= fun () ->
       match interface with
       | None -> Lwt.return [ml]
       | Some content ->
           Lwt_utils.create_file mli content >>= fun () ->
           Lwt.return [mli;ml])
    units >>= fun files ->
  let files = List.concat files in
  Lwt.return files

let extract dirname hash units =
  let source_dir = dirname // Protocol_hash.to_short_b58check hash // "src" in
  create_files source_dir units >|= fun _files ->
  Tezos_compiler.Meta.to_file source_dir ~hash
    (List.map (fun {Protocol.name} -> String.capitalize_ascii name) units)

let do_compile hash units =
  let datadir = get_datadir () in
  let source_dir = datadir // Protocol_hash.to_short_b58check hash // "src" in
  let log_file = datadir // Protocol_hash.to_short_b58check hash // "LOG" in
  let plugin_file = datadir // Protocol_hash.to_short_b58check hash //
                    Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp hash
  in
  create_files source_dir units >>= fun _files ->
  Tezos_compiler.Meta.to_file source_dir ~hash
    (List.map (fun {Protocol.name} -> String.capitalize_ascii name) units);
  let compiler_command =
    (Sys.executable_name,
     Array.of_list [Node_compiler_main.compiler_name; plugin_file; source_dir]) in
  let fd = Unix.(openfile log_file [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
  let pi =
    Lwt_process.exec
      ~stdin:`Close ~stdout:(`FD_copy fd) ~stderr:(`FD_move fd)
      compiler_command in
  pi >>= function
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      log_error "INTERRUPTED COMPILATION (%s)" log_file;
      Lwt.return false
  | Unix.WEXITED x when x <> 0 ->
      log_error "COMPILATION ERROR (%s)" log_file;
      Lwt.return false
  | Unix.WEXITED _ ->
      try Dynlink.loadfile_private plugin_file; Lwt.return true
      with Dynlink.Error err ->
        log_error "Can't load plugin: %s (%s)"
          (Dynlink.error_message err) plugin_file;
        Lwt.return false

let compile hash units =
  if VersionTable.mem versions hash then
    Lwt.return true
  else begin
    do_compile hash units >>= fun success ->
    let loaded = VersionTable.mem versions hash in
    if success && not loaded then
      log_error "Internal error while compiling %a" Protocol_hash.pp hash;
    Lwt.return loaded
  end
