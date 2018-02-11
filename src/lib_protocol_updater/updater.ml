(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Updater

let (//) = Filename.concat

module Raw = struct

  type validation_result = {
    context: Context.t ;
    fitness: Fitness.t ;
    message: string option ;
    max_operation_data_length: int ;
    max_operations_ttl: int ;
  }

  type quota = {
    max_size: int ;
    max_op: int option ;
  }

  type rpc_context = {
    block_hash: Block_hash.t ;
    block_header: Block_header.t ;
    operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
    operations: unit -> Operation.t list list Lwt.t ;
    context: Context.t ;
  }

  let activate = Context.set_protocol
  let fork_test_network = Context.fork_test_network

  (** Compiler *)

  let datadir = ref None
  let get_datadir () =
    match !datadir with
    | None ->
        fatal_error "Node not initialized" ;
        Lwt_exit.exit 1
    | Some m -> m

  let init dir =
    datadir := Some dir

  let compiler_name = "tezos-protocol-compiler"

  let do_compile hash p =
    assert (p.Protocol.expected_env = V1) ;
    let datadir = get_datadir () in
    let source_dir = datadir // Protocol_hash.to_short_b58check hash // "src" in
    let log_file = datadir // Protocol_hash.to_short_b58check hash // "LOG" in
    let plugin_file = datadir // Protocol_hash.to_short_b58check hash //
                      Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp hash
    in
    begin
      Lwt_utils_unix.Protocol.write_dir source_dir ~hash p >>=? fun () ->
      let compiler_command =
        (Sys.executable_name,
         Array.of_list [compiler_name; "-register"; plugin_file; source_dir]) in
      let fd = Unix.(openfile log_file [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
      Lwt_process.exec
        ~stdin:`Close ~stdout:(`FD_copy fd) ~stderr:(`FD_move fd)
        compiler_command >>= return
    end >>= function
    | Error err ->
        log_error "Error %a" pp_print_error err ;
        Lwt.return false
    | Ok (Unix.WSIGNALED _ | Unix.WSTOPPED _) ->
        log_error "INTERRUPTED COMPILATION (%s)" log_file;
        Lwt.return false
    | Ok (Unix.WEXITED x) when x <> 0 ->
        log_error "COMPILATION ERROR (%s)" log_file;
        Lwt.return false
    | Ok (Unix.WEXITED _) ->
        try Dynlink.loadfile_private plugin_file; Lwt.return true
        with Dynlink.Error err ->
          log_error "Can't load plugin: %s (%s)"
            (Dynlink.error_message err) plugin_file;
          Lwt.return false

  let compile hash p =
    if Tezos_protocol_registerer.Registerer.mem hash then
      Lwt.return true
    else begin
      do_compile hash p >>= fun success ->
      let loaded = Tezos_protocol_registerer.Registerer.mem hash in
      if success && not loaded then
        log_error "Internal error while compiling %a" Protocol_hash.pp hash;
      Lwt.return loaded
    end

end

include Raw

module type NODE_PROTOCOL = Protocol_environment.T
  with type context := Context.t
   and type validation_result := validation_result
   and type quota := quota
   and type rpc_context := rpc_context
   and type 'a tzresult := 'a tzresult

module Node_protocol_environment_sigs = struct

  module type V1 = Protocol_environment.V1
    with type Context.t = Context.t
     and type Updater.validation_result = validation_result
     and type Updater.quota = quota
     and type Updater.rpc_context = rpc_context

end

module MakeV1(Name : sig val name: string end)()
  : Node_protocol_environment_sigs.V1 =
  Protocol_environment.MakeV1(Name)(Context)(Raw)()

