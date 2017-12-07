(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

let config_encoding =
  let open Data_encoding in
  conv
    (fun { host ; port ; tls } -> (host, port, tls))
    (fun (host, port, tls) -> { host ; port ; tls ; logger = null_logger})
    (obj3
       (req "host" string)
       (req "port" uint16)
       (req "tls" bool))

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
  | Connection_failed of string
  | Request_failed of string list * Cohttp.Code.status_code
  | Malformed_json of string list * string * string
  | Unexpected_json of string list * Data_encoding.json * string

type error += RPC_error of config * rpc_error

let rpc_error_encoding =
  let open Data_encoding in
  union
    [ case ~tag: 1
        (obj2
           (req "rpc_error_kind" (constant "connection_failed"))
           (req "message" string))
        (function Connection_failed msg -> Some ((), msg) | _ -> None)
        (function (), msg -> Connection_failed msg) ;
      case ~tag: 2
        (obj3
           (req "rpc_error_kind" (constant "request_failed"))
           (req "path" (list string))
           (req "http_code" (conv Cohttp.Code.code_of_status Cohttp.Code.status_of_code uint16)))
        (function Request_failed (path, code) -> Some ((), path, code) | _ -> None)
        (function (), path, code -> Request_failed (path, code)) ;
      case ~tag: 3
        (obj4
           (req "rpc_error_kind" (constant "malformed_json"))
           (req "path" (list string))
           (req "message" string)
           (req "text" string))
        (function Malformed_json (path, json, msg) -> Some ((), path, msg, json) | _ -> None)
        (function (), path, msg, json -> Malformed_json (path, json, msg)) ;
      case ~tag: 4
        (obj4
           (req "rpc_error_kind" (constant "unexpected_json"))
           (req "path" (list string))
           (req "message" string)
           (req "json" json))
        (function Unexpected_json (path, json, msg) -> Some ((), path, msg, json) | _ -> None)
        (function (), path, msg, json -> Unexpected_json (path, json, msg)) ]

let pp_error ppf (config, err) =
  let pp_path ppf path =
    Format.fprintf ppf "%s://%s:%d/%s"
      (if config.tls then "https" else "http")
      config.host config.port
      (String.concat "/" path) in
  match err with
  | Connection_failed msg ->
      Format.fprintf ppf
        "@[<v 2>Unable to connect to the node:@,\
         Node's address: %s@,\
         Node's RPC port: %d@,\
         Error: %s@]"
        config.host config.port msg
  | Request_failed (path, code) ->
      let code = Cohttp.Code.code_of_status code in
      Format.fprintf ppf "@[<v 2>RPC Request failed:@,\
                          Path: %a@,\
                          HTTP status: %d (%s)@]"
        pp_path path
        code (Cohttp.Code.reason_phrase_of_code code)
  | Malformed_json (path, json, msg) ->
      Format.fprintf ppf "@[<v 2>RPC request returned malformed JSON:@,\
                          Path: %a@,\
                          Error: %s@,\
                          @[<v 2>JSON data:@,%a@]@]"
        pp_path path
        msg
        (Format.pp_print_list
           (fun ppf s -> Format.fprintf ppf "> %s" s))
        (String.split '\n' json)
  | Unexpected_json (path, json, msg) ->
      Format.fprintf ppf "@[<v 2>RPC request returned unexpected JSON:@,\
                          Path: %a@,\
                          @[<v 2>Error:@,%a@]@,\
                          @[<v 2>JSON data:@,%a@]@]"
        pp_path path
        (Format.pp_print_list (fun ppf s -> Format.fprintf ppf "%s" s))
        (String.split '\n' msg)
        Json_repr.(pp (module Ezjsonm)) json

let () =
  register_error_kind
    `Branch
    ~id: "client_rpc"
    ~title: "Client side RPC error"
    ~description: "An RPC call failed"
    ~pp: pp_error
    Data_encoding.(obj2
                     (req "config" config_encoding)
                     (req "error" rpc_error_encoding))
    (function RPC_error (config, err) -> Some (config, err) | _ -> None)
    (fun (config, err) -> RPC_error (config, err))

let fail config err = fail (RPC_error (config, err))

class type rpc_sig = object
  method get_json :
    RPC.meth ->
    string list -> Data_encoding.json -> Data_encoding.json tzresult Lwt.t
  method get_streamed_json :
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    Data_encoding.json tzresult Lwt_stream.t tzresult Lwt.t
  method make_request :
    (Uri.t -> Data_encoding.json -> 'a Lwt.t) ->
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    ('a * Cohttp.Code.status_code * Cohttp_lwt.Body.t) tzresult Lwt.t
  method parse_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output, unit) RPC.Service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
  method parse_err_answer :
    'meth 'params 'input 'output.
    ([< Resto.meth ] as 'meth, unit, 'params, unit, 'input, 'output tzresult, unit) RPC.Service.t ->
    string list ->
    Data_encoding.json -> 'output tzresult Lwt.t
end

class rpc config : rpc_sig = object (self)
  val config = config
  method make_request :
    type a. (Uri.t -> Data_encoding.json -> a Lwt.t) ->
    RPC.meth ->
    string list ->
    Data_encoding.json ->
    (a * Cohttp.Code.status_code * Cohttp_lwt.Body.t) tzresult Lwt.t =
    fun log_request meth service json ->
      let scheme = if config.tls then "https" else "http" in
      let path = String.concat "/" service in
      let uri =
        Uri.make ~scheme ~host:config.host ~port:config.port ~path () in
      let reqbody = Data_encoding_ezjsonm.to_string json in
      Lwt.catch begin fun () ->
        let body = Cohttp_lwt.Body.of_string reqbody in
        Cohttp_lwt_unix.Client.call
          (meth :> Cohttp.Code.meth) ~body uri >>= fun (code, ansbody) ->
        log_request uri json >>= fun reqid ->
        return (reqid, code.Cohttp.Response.status, ansbody)
      end begin fun exn ->
        let msg = match exn with
          | Unix.Unix_error (e, _, _) -> Unix.error_message e
          | Failure msg -> msg
          | Invalid_argument msg -> msg
          | e -> Printexc.to_string e in
        fail config (Connection_failed msg)
      end

  method get_streamed_json meth service json =
    let Logger logger = config.logger in
    self#make_request logger.log_request
      meth service json >>=? fun (reqid, code, ansbody) ->
    match code with
    | #Cohttp.Code.success_status ->
        let ansbody = Cohttp_lwt.Body.to_stream ansbody in
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
        Cohttp_lwt.Body.to_string ansbody >>= fun ansbody ->
        logger.log_error reqid code ansbody >>= fun () ->
        fail config (Request_failed (service, err))

  method parse_answer
    : 'm 'p 'i 'o.
      ([< Resto.meth ] as 'm, unit, 'p, unit, 'i, 'o, unit) RPC.Service.t ->
      string list ->
      Data_encoding.json -> 'o tzresult Lwt.t =
    fun service path json ->
      match Data_encoding.Json.destruct (RPC.Service.output_encoding service) json with
      | exception msg ->
          let msg =
            Format.asprintf "%a" (fun x -> Data_encoding.Json.print_error x) msg in
          fail config (Unexpected_json (path, json, msg))
      | v -> return v


  method get_json : RPC.meth ->
    string list -> Data_encoding.json -> Data_encoding.json tzresult Lwt.t =
    fun meth service json ->
      let Logger logger = config.logger in
      self#make_request logger.log_request
        meth service json >>=? fun (reqid, code, ansbody) ->
      Cohttp_lwt.Body.to_string ansbody >>= fun ansbody ->
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

  method parse_err_answer
    : 'm 'p 'i 'o.
      ([< Resto.meth ] as 'm, unit, 'p, unit, 'i, 'o tzresult, unit) RPC.Service.t ->
      string list ->
      Data_encoding.json -> 'o tzresult Lwt.t =
    fun service path json ->
      match Data_encoding.Json.destruct (RPC.Service.output_encoding service) json with
      | exception msg -> (* TODO print_error *)
          let msg =
            Format.asprintf "%a" (fun x -> Data_encoding.Json.print_error x) msg in
          fail config (Unexpected_json (path, json, msg))
      | v -> Lwt.return v
end

let make_request config log_request meth service json =
  let scheme = if config.tls then "https" else "http" in
  let path = String.concat "/" service in
  let uri =
    Uri.make ~scheme ~host:config.host ~port:config.port ~path () in
  let reqbody = Data_encoding_ezjsonm.to_string json in
  Lwt.catch begin fun () ->
    let body = Cohttp_lwt.Body.of_string reqbody in
    Cohttp_lwt_unix.Client.call
      (meth :> Cohttp.Code.meth)
      ~body uri >>= fun (code, ansbody) ->
    log_request uri json >>= fun reqid ->
    return (reqid, code.Cohttp.Response.status, ansbody)
  end begin fun exn ->
    let msg = match exn with
      | Unix.Unix_error (e, _, _) -> Unix.error_message e
      | e -> Printexc.to_string e in
    fail config (Connection_failed msg)
  end

let forge_request (type i) (service: (_,_,_,_,i,_,_) RPC.Service.t) params body =
  let { RPC.Service.meth ; uri } =
    RPC.Service.forge_request service params () in
  let json =
    match RPC.Service.input_encoding service with
    | RPC.Service.No_input -> assert false (* TODO *)
    | RPC.Service.Input input -> Data_encoding.Json.construct input body in
  let path = String.split_path (Uri.path uri) in (* Temporary *)
  meth, path, json

let call_service0 (rpc : #rpc_sig) service arg =
  let meth, path, arg = forge_request service () arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_answer service path json

let call_service1 (rpc : #rpc_sig) service a1 arg =
  let meth, path, arg = forge_request service ((), a1) arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_answer service path json

let call_service2 (rpc : #rpc_sig) service a1 a2 arg =
  let meth, path, arg = forge_request service (((), a1), a2) arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_answer service path json

let call_streamed (rpc : #rpc_sig) service (meth, path, arg) =
  rpc#get_streamed_json meth path arg >>=? fun json_st ->
  let parsed_st, push = Lwt_stream.create () in
  let rec loop () =
    Lwt_stream.get json_st >>= function
    | Some (Ok json) -> begin
        rpc#parse_answer service path json >>= function
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

let call_streamed_service0 (rpc : #rpc_sig) service arg =
  call_streamed rpc service (forge_request service () arg)

let call_streamed_service1 cctxt service arg1 arg2 =
  call_streamed cctxt service (forge_request service ((), arg1) arg2)

let call_err_service0 (rpc : #rpc_sig) service arg =
  let meth, path, arg = forge_request service () arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_err_answer service path json

let call_err_service1 (rpc : #rpc_sig) service a1 arg =
  let meth, path, arg = forge_request service ((), a1) arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_err_answer service path json

let call_err_service2 (rpc : #rpc_sig) service a1 a2 arg =
  let meth, path, arg = forge_request service (((), a1), a2) arg in
  rpc#get_json meth path arg >>=? fun json ->
  rpc#parse_err_answer service path json

type block = Node_rpc_services.Blocks.block

let last_baked_block = function
  | `Prevalidation -> `Head 0
  | `Test_prevalidation -> `Test_head 0
  | `Genesis | `Head _ | `Test_head _ | `Hash _ as block -> block
