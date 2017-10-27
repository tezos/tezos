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

type validation_result = {
  context: Context.t ;
  fitness: Fitness.t ;
  message: string option ;
  max_operations_ttl: int ;
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

let extract dir ?hash (p: Protocol.t) =
  create_files dir p.components >>= fun _files ->
  Tezos_compiler.Meta.to_file dir
    ?hash
    ~env_version:p.expected_env
    (List.map (fun {Protocol.name} -> String.capitalize_ascii name) p.components) ;
  Lwt.return_unit

let do_compile hash p =
  assert (p.Protocol.expected_env = V1) ;
  let units = p.components in
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

let compile hash p =
  if Tezos_protocol_registerer.mem hash then
    Lwt.return true
  else begin
    do_compile hash p >>= fun success ->
    let loaded = Tezos_protocol_registerer.mem hash in
    if success && not loaded then
      log_error "Internal error while compiling %a" Protocol_hash.pp hash;
    Lwt.return loaded
  end

module Node_protocol_environment_sigs = struct

  module type V1 = sig

    include Tezos_protocol_environment_sigs_v1.T
      with type Format.formatter = Format.formatter
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Hash.Net_id.t = Hash.Net_id.t
       and type Hash.Block_hash.t = Hash.Block_hash.t
       and type Hash.Operation_hash.t = Hash.Operation_hash.t
       and type Hash.Operation_list_list_hash.t = Hash.Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Time.t = Time.t
       and type MBytes.t = MBytes.t
       and type Tezos_data.Operation.shell_header = Tezos_data.Operation.shell_header
       and type Tezos_data.Operation.t = Tezos_data.Operation.t
       and type Tezos_data.Block_header.shell_header = Tezos_data.Block_header.shell_header
       and type Tezos_data.Block_header.t = Tezos_data.Block_header.t
       and type 'a RPC.directory = 'a RPC.directory
       and type Updater.validation_result = validation_result
       and type Updater.rpc_context = rpc_context

    type error += Ecoproto_error of Error_monad.error list
    val wrap_error : 'a Error_monad.tzresult -> 'a tzresult

  end

end

module type RAW_PROTOCOL = sig
  type error = ..
  type 'a tzresult = ('a, error list) result
  type operation
  val max_operation_data_length: int
  val max_block_length: int
  val max_number_of_operations: int
  val parse_operation:
    Operation_hash.t -> Operation.t -> operation tzresult
  val compare_operations: operation -> operation -> int
  type validation_state
  val current_context: validation_state -> Context.t tzresult Lwt.t
  val precheck_block:
    ancestor_context: Context.t ->
    ancestor_timestamp: Time.t ->
    Block_header.t ->
    unit tzresult Lwt.t
  val begin_application:
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    Block_header.t ->
    validation_state tzresult Lwt.t
  val begin_construction:
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?proto_header: MBytes.t ->
    unit -> validation_state tzresult Lwt.t
  val apply_operation:
    validation_state -> operation -> validation_state tzresult Lwt.t
  val finalize_block:
    validation_state -> validation_result tzresult Lwt.t
  val rpc_services: rpc_context RPC.directory
  val configure_sandbox:
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t
end

module type NODE_PROTOCOL =
  RAW_PROTOCOL with type error := error
                and type 'a tzresult := 'a tzresult


module LiftProtocol
    (Name : sig val name: string end)
    (Env : Node_protocol_environment_sigs.V1)
    (P : Env.Updater.PROTOCOL) = struct
  include P
  let precheck_block
      ~ancestor_context ~ancestor_timestamp
      raw_block =
    precheck_block
      ~ancestor_context ~ancestor_timestamp
      raw_block >|= Env.wrap_error
  let begin_application
      ~predecessor_context ~predecessor_timestamp
      ~predecessor_fitness
      raw_block =
    begin_application
      ~predecessor_context ~predecessor_timestamp
      ~predecessor_fitness
      raw_block >|= Env.wrap_error
  let begin_construction
      ~predecessor_context ~predecessor_timestamp
      ~predecessor_level ~predecessor_fitness
      ~predecessor ~timestamp ?proto_header () =
    begin_construction
      ~predecessor_context ~predecessor_timestamp
      ~predecessor_level ~predecessor_fitness
      ~predecessor ~timestamp ?proto_header () >|= Env.wrap_error
  let current_context c =
    current_context c >|= Env.wrap_error
  let apply_operation c o =
    apply_operation c o >|= Env.wrap_error
  let finalize_block c = finalize_block c >|= Env.wrap_error
  let parse_operation h b = parse_operation h b |> Env.wrap_error
  let configure_sandbox c j =
    configure_sandbox c j >|= Env.wrap_error
end
