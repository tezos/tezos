(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open Lwt.Infix

type logger = Logger : {
    log_request: Uri.t -> Data_encoding.json -> 'a Lwt.t ;
    log_success:
      'a -> Cohttp.Code.status_code -> Data_encoding.json  -> unit Lwt.t ;
    log_error:
      'a -> Cohttp.Code.status_code -> string  -> unit Lwt.t ;
  } -> logger

type config = {
  host : string ;
  port : int ;
  tls : bool ;
  logger : logger ;
}

let null_logger =
  Logger {
    log_request = (fun _ _ -> Lwt.return_unit) ;
    log_success = (fun _ _ _ -> Lwt.return_unit) ;
    log_error = (fun _ _ _ -> Lwt.return_unit) ;
  }

let timings_logger ppf =
  Logger {
    log_request = begin fun url _body ->
      let tzero = Unix.gettimeofday () in
      let url = Uri.to_string url in
      Lwt.return (url, tzero)
    end ;
    log_success = begin fun (url, tzero) _code _body ->
      let time = Unix.gettimeofday () -. tzero in
      Format.fprintf ppf "Request to %s succeeded in %gs" url time ;
      Lwt.return_unit
    end ;
    log_error = begin fun (url, tzero) _code _body ->
      let time = Unix.gettimeofday () -. tzero in
      Format.fprintf ppf "Request to %s failed in %gs" url time ;
      Lwt.return_unit
    end ;
  }

let full_logger ppf =
  let cpt = ref 0 in
  Logger {
    log_request = begin fun url body ->
      let id = !cpt in
      let url = Uri.to_string url in
      let body = Data_encoding_ezjsonm.to_string body in
      incr cpt ;
      Format.fprintf ppf ">>>>%d: %s\n%s@." id url body ;
      Lwt.return (id, url)
    end ;
    log_success = begin fun (id, _url) code body ->
      let code = Cohttp.Code.string_of_status code in
      let body = Data_encoding_ezjsonm.to_string body in
      Format.fprintf ppf "<<<<%d: %s\n%s@." id code body ;
      Lwt.return_unit
    end ;
    log_error = begin fun (id, _url) code body ->
      let code = Cohttp.Code.string_of_status code in
      Format.fprintf ppf "<<<<%d: %s\n%s@." id code body ;
      Lwt.return_unit
    end ;
}

let default_config = {
  host = "localhost" ;
  port = 8732 ;
  tls = false ;
  logger = null_logger ;
}

type rpc_error =
  | Cannot_connect_to_RPC_server of string
  | Request_failed of string list * Cohttp.Code.status_code
  | Malformed_json of string list * string * string
  | Unexpected_json of string list * Data_encoding.json * string

type error += RPC_error of config * rpc_error

let fail config err = fail (RPC_error (config, err))

let make_request config log_request meth service json =
  let scheme = if config.tls then "https" else "http" in
  let path = String.concat "/" service in
  let uri =
    Uri.make ~scheme ~host:config.host ~port:config.port ~path () in
  let reqbody = Data_encoding_ezjsonm.to_string json in
  Lwt.catch begin fun () ->
    let body = Cohttp_lwt_body.of_string reqbody in
    Cohttp_lwt_unix.Client.call meth ~body uri >>= fun (code, ansbody) ->
    log_request uri json >>= fun reqid ->
    return (reqid, code.Cohttp.Response.status, ansbody)
  end begin fun e ->
    let msg = match e with
      | Unix.Unix_error (e, _, _) -> Unix.error_message e
      | e -> Printexc.to_string e in
    fail config (Cannot_connect_to_RPC_server msg)
  end

let get_streamed_json config meth service json =
  let Logger logger = config.logger in
  make_request config logger.log_request
    meth service json >>=? fun (reqid, code, ansbody) ->
  match code with
  | #Cohttp.Code.success_status ->
      let ansbody = Cohttp_lwt_body.to_stream ansbody in
      let json_st = Data_encoding_ezjsonm.from_stream ansbody in
      let parsed_st, push = Lwt_stream.create () in
      let rec loop () =
        Lwt_stream.get json_st >>= function
        | Some (Ok json) as v ->
            push v ;
            logger.log_success reqid code json >>= fun () ->
            loop ()
        | None ->
            push None ;
            Lwt.return_unit
        | Some (Error msg) ->
            let error =
              RPC_error (config, Malformed_json (service, "", msg)) in
            push (Some (Error [error])) ;
            push None ;
            Lwt.return_unit
      in
      Lwt.async loop ;
      return parsed_st
  | err ->
      Cohttp_lwt_body.to_string ansbody >>= fun ansbody ->
      logger.log_error reqid code ansbody >>= fun () ->
      fail config (Request_failed (service, err))

let get_json config meth service json =
  let Logger logger = config.logger in
  make_request config logger.log_request
    meth service json >>=? fun (reqid, code, ansbody) ->
  Cohttp_lwt_body.to_string ansbody >>= fun ansbody ->
  match code with
  | #Cohttp.Code.success_status -> begin
      if ansbody = "" then
        return `Null
      else
        match Data_encoding_ezjsonm.from_string ansbody with
        | Error msg ->
            logger.log_error reqid code ansbody >>= fun () ->
            fail config (Malformed_json (service, ansbody, msg))
        | Ok json ->
            logger.log_success reqid code json >>= fun () ->
            return json
    end
  | err ->
      logger.log_error reqid code ansbody >>= fun () ->
      fail config (Request_failed (service, err))

let parse_answer config service path json =
  match RPC.read_answer service json with
  | Error msg -> (* TODO print_error *)
      fail config (Unexpected_json (path, json, msg))
  | Ok v -> return v

let call_service0 cctxt service arg =
  let meth, path, arg = RPC.forge_request service () arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_answer cctxt service path json

let call_service1 cctxt service a1 arg =
  let meth, path, arg = RPC.forge_request service ((), a1) arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_answer cctxt service path json

let call_service2 cctxt service a1 a2 arg =
  let meth, path, arg = RPC.forge_request service (((), a1), a2) arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_answer cctxt service path json

let call_streamed_service0 cctxt service arg =
  let meth, path, arg = RPC.forge_request service () arg in
  get_streamed_json cctxt meth path arg >>=? fun json_st ->
  let parsed_st, push = Lwt_stream.create () in
  let rec loop () =
    Lwt_stream.get json_st >>= function
    | Some (Ok json) -> begin
        parse_answer cctxt service path json >>= function
        | Ok v -> push (Some (Ok v)) ; loop ()
        | Error _ as err ->
            push (Some err) ; push None ; Lwt.return_unit
        end
    | Some (Error _) as v ->
        push v ; push None ; Lwt.return_unit
    | None -> push None ; Lwt.return_unit
  in
  Lwt.async loop ;
  return parsed_st

let parse_err_answer config service path json =
  match RPC.read_answer service json with
  | Error msg -> (* TODO print_error *)
      fail config (Unexpected_json (path, json, msg))
  | Ok v -> Lwt.return v

let call_err_service0 cctxt service arg =
  let meth, path, arg = RPC.forge_request service () arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_err_answer cctxt service path json

let call_err_service1 cctxt service a1 arg =
  let meth, path, arg = RPC.forge_request service ((), a1) arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_err_answer cctxt service path json

let call_err_service2 cctxt service a1 a2 arg =
  let meth, path, arg = RPC.forge_request service (((), a1), a2) arg in
  get_json cctxt meth path arg >>=? fun json ->
  parse_err_answer cctxt service path json

let call_describe0 cctxt service path arg =
  let meth, prefix, arg = RPC.forge_request service () arg in
  get_json cctxt meth (prefix @ path) arg >>=? fun json ->
  parse_answer cctxt service prefix json
