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
open Client_commands
open Logging.RPC

let log_request { log } cpt url req =
  log "requests" ">>>>%d: %s\n%s\n" cpt url req

let log_response { log } cpt code ans =
  log "requests" "<<<<%d: %s\n%s\n" cpt (Cohttp.Code.string_of_status code) ans

let cpt = ref 0
let make_request cctxt meth service json =
  incr cpt ;
  let cpt = !cpt in
  let scheme = if cctxt.config.tls then "https" else "http" in
  let host = cctxt.config.incoming_addr in
  let port = cctxt.config.incoming_port in
  let path = String.concat "/" service in
  let uri = Uri.make ~scheme ~host ~port ~path () in
  let string_uri = Uri.to_string uri in
  let reqbody = Data_encoding_ezjsonm.to_string json in
  let tzero = Unix.gettimeofday () in
  catch
    (fun () ->
       let body = Cohttp_lwt_body.of_string reqbody in
       Cohttp_lwt_unix.Client.call meth ~body uri >>= fun (code, ansbody) ->
       log_request cctxt cpt string_uri reqbody >>= fun () ->
       return (cpt, Unix.gettimeofday () -. tzero,
               code.Cohttp.Response.status, ansbody))
    (fun e ->
       let msg = match e with
         | Unix.Unix_error (e, _, _) -> Unix.error_message e
         | e -> Printexc.to_string e in
       cctxt.error "cannot connect to the RPC server (%s)" msg)

let get_streamed_json cctxt meth service json =
  make_request cctxt meth service json >>= fun (_cpt, time, code, ansbody) ->
  let ansbody = Cohttp_lwt_body.to_stream ansbody in
  match code, ansbody with
  | #Cohttp.Code.success_status, ansbody ->
      (if cctxt.config.print_timings then
         cctxt.message "Request to /%s succeeded in %gs"
           (String.concat "/" service) time
       else Lwt.return ()) >>= fun () ->
      Lwt.return (
        Lwt_stream.filter_map_s
          (function
            | Ok v -> Lwt.return (Some v)
            | Error msg ->
                lwt_log_error
                  "Failed to parse json: %s" msg >>= fun () ->
                Lwt.return None)
          (Data_encoding_ezjsonm.from_stream ansbody))
  | err, _ansbody ->
      (if cctxt.config.print_timings then
         cctxt.message "Request to /%s failed in %gs"
           (String.concat "/" service) time
       else Lwt.return ()) >>= fun () ->
      cctxt.message "Request to /%s failed, server returned %s"
        (String.concat "/" service) (Cohttp.Code.string_of_status err) >>= fun () ->
      cctxt.error "the RPC server returned a non-success status (%s)"
        (Cohttp.Code.string_of_status err)

let get_json cctxt meth service json =
  make_request cctxt meth service json >>= fun (cpt, time, code, ansbody) ->
  Cohttp_lwt_body.to_string ansbody >>= fun ansbody ->
  match code, ansbody with
  | #Cohttp.Code.success_status, ansbody -> begin
      (if cctxt.config.print_timings then
         cctxt.message "Request to /%s succeeded in %gs"
           (String.concat "/" service) time
       else Lwt.return ()) >>= fun () ->
      log_response cctxt cpt code ansbody >>= fun () ->
      if ansbody = "" then Lwt.return `Null
      else match Data_encoding_ezjsonm.from_string ansbody with
        | Error _ -> cctxt.error "the RPC server returned malformed JSON"
        | Ok res -> Lwt.return res
    end
  | err, _ansbody ->
      (if cctxt.config.print_timings then
         cctxt.message "Request to /%s failed in %gs"
           (String.concat "/" service) time
       else Lwt.return ()) >>= fun () ->
      cctxt.message "Request to /%s failed, server returned %s"
        (String.concat "/" service) (Cohttp.Code.string_of_status err) >>= fun () ->
      cctxt.error "the RPC server returned a non-success status (%s)"
        (Cohttp.Code.string_of_status err)

exception Unknown_error of Data_encoding.json

let parse_answer cctxt service path json =
  match RPC.read_answer service json with
  | Error msg -> (* TODO print_error *)
      cctxt.error "request to /%s returned wrong JSON (%s)\n%s"
        (String.concat "/" path) msg (Data_encoding_ezjsonm.to_string json)
  | Ok v -> return v

let call_service0 cctxt service arg =
  let meth, path, arg = RPC.forge_request service () arg in
  get_json cctxt meth path arg >>= fun json ->
  parse_answer cctxt service path json

let call_service1 cctxt service a1 arg =
  let meth, path, arg = RPC.forge_request service ((), a1) arg in
  get_json cctxt meth path arg >>= fun json ->
  parse_answer cctxt service path json

let call_service2 cctxt service a1 a2 arg =
  let meth, path, arg = RPC.forge_request service (((), a1), a2) arg in
  get_json cctxt meth path arg >>= fun json ->
  parse_answer cctxt service path json

let call_streamed_service0 cctxt service arg =
  let meth, path, arg = RPC.forge_request service () arg in
  get_streamed_json cctxt meth path arg >|= fun st ->
  Lwt_stream.map_s (parse_answer cctxt service path) st

module Services = Node_rpc_services
let errors cctxt =
  call_service0 cctxt Services.Error.service ()
let forge_block cctxt ?net ?predecessor ?timestamp fitness ops header =
  call_service0 cctxt Services.forge_block
    (net, predecessor, timestamp, fitness, ops, header)
let validate_block cctxt net block =
  call_service0 cctxt Services.validate_block (net, block)
let inject_block cctxt ?(wait = true) ?force block =
  call_service0 cctxt Services.inject_block (block, wait, force)
let inject_operation cctxt ?(wait = true) ?force operation =
  call_service0 cctxt Services.inject_operation (operation, wait, force)
let inject_protocol cctxt ?(wait = true) ?force protocol =
  call_service0 cctxt Services.inject_protocol (protocol, wait, force)
let bootstrapped cctxt =
  call_streamed_service0 cctxt Services.bootstrapped ()
let complete cctxt ?block prefix =
  match block with
  | None ->
      call_service1 cctxt Services.complete prefix ()
  | Some block ->
      call_service2 cctxt Services.Blocks.complete block prefix ()
let describe cctxt ?recurse path =
  let meth, prefix, arg = RPC.forge_request Services.describe () recurse in
  get_json cctxt meth (prefix @ path) arg >>=
  parse_answer cctxt Services.describe prefix

module Blocks = struct
  type block = Services.Blocks.block

  type block_info = Services.Blocks.block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: Updater.Net_id.t ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Updater.Net_id.t * Time.t) option ;
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
  let net cctxt h = call_service1 cctxt Services.Blocks.net h ()
  let predecessor cctxt h = call_service1 cctxt Services.Blocks.predecessor h ()
  let predecessors cctxt h l = call_service1 cctxt Services.Blocks.predecessors h l
  let hash cctxt h = call_service1 cctxt Services.Blocks.hash h ()
  let timestamp cctxt h = call_service1 cctxt Services.Blocks.timestamp h ()
  let fitness cctxt h = call_service1 cctxt Services.Blocks.fitness h ()
  let operations cctxt h = call_service1 cctxt Services.Blocks.operations h ()
  let protocol cctxt h = call_service1 cctxt Services.Blocks.protocol h ()
  let test_protocol cctxt h = call_service1 cctxt Services.Blocks.test_protocol h ()
  let test_network cctxt h = call_service1 cctxt Services.Blocks.test_network h ()
  let preapply cctxt h ?timestamp ?(sort = false) operations =
    call_service1 cctxt Services.Blocks.preapply h { operations ; sort ; timestamp }
  let pending_operations cctxt block =
    call_service1 cctxt Services.Blocks.pending_operations block ()
  let info cctxt ?(operations = false) h =
    call_service1 cctxt Services.Blocks.info h operations
  let complete cctxt block prefix =
    call_service2 cctxt Services.Blocks.complete block prefix ()
  let list cctxt ?operations ?length ?heads ?delay ?min_date ?min_heads () =
    call_service0 cctxt Services.Blocks.list
      { operations; length ; heads ; monitor = Some false ; delay ;
        min_date ; min_heads }
  let monitor cctxt ?operations ?length ?heads ?delay ?min_date ?min_heads () =
    call_streamed_service0 cctxt Services.Blocks.list
      { operations; length ; heads ; monitor = Some true ; delay ;
        min_date ; min_heads }
end

module Operations = struct
  let monitor cctxt ?contents () =
    call_streamed_service0 cctxt Services.Operations.list
      { monitor = Some true ; contents }
end

module Protocols = struct
  let bytes cctxt hash =
    call_service1 cctxt Services.Protocols.bytes hash ()
  let list cctxt ?contents () =
    call_service0 cctxt Services.Protocols.list { contents; monitor = Some false }
end

module Network = struct
  let stat cctxt =
    call_service0 cctxt Services.Network.stat ()
  let connections cctxt =
    call_service0 cctxt Services.Network.Connection.list ()
  let peers cctxt =
    call_service0 cctxt Services.Network.Peer_id.list []
  let points cctxt =
    call_service0 cctxt Services.Network.Point.list []
end
