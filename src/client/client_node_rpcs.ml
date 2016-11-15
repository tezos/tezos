(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - RPC Calls *)

open Lwt
open Cli_entries
open Logging.RPC

let log_file =
  let open CalendarLib in
  Printer.Precise_Calendar.sprint
    "%Y-%m-%dT%H:%M:%SZ.log"
    (Calendar.Precise.now ())

let with_log_file f =
  Utils.create_dir Client_config.(base_dir#get // "logs") >>= fun () ->
  Lwt_io.with_file
    ~flags: Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ]
    ~mode: Lwt_io.Output
    Client_config.(base_dir#get // "logs" // log_file)
    f

let log_request cpt url req =
  with_log_file
    (fun fp ->
       Lwt_io.fprintf fp">>>>%d: %s\n%s\n" cpt url req >>= fun () ->
       Lwt_io.flush fp)

let log_response cpt code ans =
  with_log_file
    (fun fp ->
       Lwt_io.fprintf fp"<<<<%d: %s\n%s\n" cpt (Cohttp.Code.string_of_status code) ans >>= fun () ->
       Lwt_io.flush fp)

let cpt = ref 0
let make_request service json =
  incr cpt ;
  let cpt = !cpt in
  let serv = "http://" ^ Client_config.incoming_addr#get
             ^ ":" ^ string_of_int Client_config.incoming_port#get in
  let string_uri = String.concat "/" (serv :: service) in
  let uri = Uri.of_string string_uri in
  let reqbody = Data_encoding.Json.to_string json in
  let tzero = Unix.gettimeofday () in
  catch
    (fun () ->
       let body = Cohttp_lwt_body.of_string reqbody in
       Cohttp_lwt_unix.Client.post ~body uri >>= fun (code, ansbody) ->
       log_request cpt string_uri reqbody >>= fun () ->
       return (cpt, Unix.gettimeofday () -. tzero,
               code.Cohttp.Response.status, ansbody))
    (fun e ->
       let msg = match e with
         | Unix.Unix_error (e, _, _) -> Unix.error_message e
         | e -> Printexc.to_string e in
       error "cannot connect to the RPC server (%s)" msg)

let get_streamed_json service json =
  make_request service json >>= fun (_cpt, time, code, ansbody) ->
  let ansbody = Cohttp_lwt_body.to_stream ansbody in
  match code, ansbody with
  | #Cohttp.Code.success_status, ansbody ->
      if Client_config.print_timings#get then
        message "Request to /%s succeeded in %gs"
          (String.concat "/" service) time ;
      Lwt.return (
        Lwt_stream.filter_map_s
          (function
            | Ok v -> Lwt.return (Some v)
            | Error msg ->
                lwt_log_error
                  "Failed to parse json: %s" msg >>= fun () ->
                Lwt.return None)
          (Data_encoding.Json.from_stream ansbody))
  | err, _ansbody ->
      if Client_config.print_timings#get then
        message "Request to /%s failed in %gs"
          (String.concat "/" service) time ;
      message "Request to /%s failed, server returned %s"
        (String.concat "/" service) (Cohttp.Code.string_of_status err) ;
      error "the RPC server returned a non-success status (%s)"
        (Cohttp.Code.string_of_status err)

let get_json service json =
  make_request service json >>= fun (cpt, time, code, ansbody) ->
  Cohttp_lwt_body.to_string ansbody >>= fun ansbody ->
  match code, ansbody with
  | #Cohttp.Code.success_status, ansbody -> begin
      if Client_config.print_timings#get then
        message "Request to /%s succeeded in %gs"
          (String.concat "/" service) time ;
      log_response cpt code ansbody >>= fun () ->
      if ansbody = "" then Lwt.return `Null
      else match Data_encoding.Json.from_string ansbody with
        | Error _ -> error "the RPC server returned malformed JSON"
        | Ok res -> Lwt.return res
    end
  | err, _ansbody ->
      if Client_config.print_timings#get then
        message "Request to /%s failed in %gs"
          (String.concat "/" service) time ;
      message "Request to /%s failed, server returned %s"
        (String.concat "/" service) (Cohttp.Code.string_of_status err) ;
      error "the RPC server returned a non-success status (%s)"
        (Cohttp.Code.string_of_status err)

exception Unknown_error of Data_encoding.json

let parse_answer service path json =
  match RPC.read_answer service json with
  | Error msg -> (* TODO print_error *)
      error "request to /%s returned wrong JSON (%s)\n%s"
        (String.concat "/" path) msg (Data_encoding.Json.to_string json)
  | Ok v -> return v

let call_service0 service arg =
  let path, arg = RPC.forge_request service () arg in
  get_json path arg >>= parse_answer service path

let call_service1 service a1 arg =
  let path, arg = RPC.forge_request service ((), a1) arg in
  get_json path arg >>= parse_answer service path

let call_service2 service a1 a2 arg =
  let path, arg = RPC.forge_request service (((), a1), a2) arg in
  get_json path arg >>= parse_answer service path

let call_streamed_service0 service arg =
  let path, arg = RPC.forge_request service () arg in
  get_streamed_json path arg >|= fun st ->
  Lwt_stream.map_s (parse_answer service path) st

module Services = Node_rpc_services
let errors = call_service0 RPC.Error.service
let forge_block ?net ?predecessor ?timestamp fitness ops header =
  call_service0 Services.forge_block
    (net, predecessor, timestamp, fitness, ops, header)
let validate_block net block =
  call_service0 Services.validate_block (net, block)
let inject_block ?(wait = true) ?force block =
  call_service0 Services.inject_block (block, wait, force)
let inject_operation ?(wait = true) ?force operation =
  call_service0 Services.inject_operation (operation, wait, force)
let inject_protocol ?(wait = true) ?force protocol =
  call_service0 Services.inject_protocol (protocol, wait, force)
let complete ?block prefix =
  match block with
  | None ->
      call_service1 Services.complete prefix ()
  | Some block ->
      call_service2 Services.Blocks.complete block prefix ()
let describe ?recurse path =
  let prefix, arg = RPC.forge_request Services.describe () recurse in
  get_json (prefix @ path) arg >>=
  parse_answer Services.describe prefix

type net = Services.Blocks.net = Net of Block_hash.t

module Blocks = struct
  type block = Services.Blocks.block

  type block_info = Services.Blocks.block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: net ;
    test_protocol: Protocol_hash.t option ;
    test_network: (net * Time.t) option ;
  }
  type preapply_param = Services.Blocks.preapply_param = {
    operations: Operation_hash.t list ;
    sort: bool ;
    timestamp: Time.t option ;
  }
  type preapply_result = Services.Blocks.preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }
  let net h = call_service1 Services.Blocks.net h ()
  let predecessor h = call_service1 Services.Blocks.predecessor h ()
  let hash h = call_service1 Services.Blocks.hash h ()
  let timestamp h = call_service1 Services.Blocks.timestamp h ()
  let fitness h = call_service1 Services.Blocks.fitness h ()
  let operations h = call_service1 Services.Blocks.operations h ()
  let protocol h = call_service1 Services.Blocks.protocol h ()
  let test_protocol h = call_service1 Services.Blocks.test_protocol h ()
  let test_network h = call_service1 Services.Blocks.test_network h ()
  let preapply h ?timestamp ?(sort = false) operations =
    call_service1 Services.Blocks.preapply h { operations ; sort ; timestamp }
  let pending_operations block =
    call_service1 Services.Blocks.pending_operations block ()
  let info ?(operations = false) h =
    call_service1 Services.Blocks.info h operations
  let complete block prefix =
    call_service2 Services.Blocks.complete block prefix ()
  let list ?operations ?length ?heads ?delay ?min_date ?min_heads () =
    call_service0 Services.Blocks.list
      { operations; length ; heads ; monitor = Some false ; delay ;
        min_date ; min_heads }
  let monitor ?operations ?length ?heads ?delay ?min_date ?min_heads () =
    call_streamed_service0 Services.Blocks.list
      { operations; length ; heads ; monitor = Some true ; delay ;
        min_date ; min_heads }
end

module Operations = struct
  let monitor ?contents () =
    call_streamed_service0 Services.Operations.list
      { monitor = Some true ; contents }
end

module Protocols = struct
  let bytes hash =
    call_service1 Services.Protocols.bytes hash ()
  let list ?contents () =
    call_service0 Services.Protocols.list { contents; monitor = Some false }
end
