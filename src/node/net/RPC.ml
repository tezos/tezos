(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.RPC

module Arg = Resto.Arg
module Path = Resto.Path
module Description = Resto.Description
let read_answer = Resto.read_answer
let forge_request = Resto.forge_request
let service ?description ~input ~output path =
  Resto.service
    ?description
    ~input:(Data_encoding.Json.convert input)
    ~output:(Data_encoding.Json.convert output)
    path
type ('prefix, 'params, 'input, 'output) service =
  ('prefix, 'params, 'input, 'output) Resto.service

include RestoDirectory

(* public types *)
type server = (* hidden *)
  { shutdown : unit -> unit Lwt.t ;
    mutable root : unit directory }

module ConnectionMap = Map.Make(Cohttp.Connection)

exception Invalid_method
exception Cannot_parse_body of string

(* Promise a running RPC server. Takes the port. *)
let launch port ?pre_hook ?post_hook root =
  (* launch the worker *)
  let cancelation, canceler, _ = Lwt_utils.canceler () in
  let open Cohttp_lwt_unix in
  let streams = ref ConnectionMap.empty in
  let create_stream _io con to_string (s: _ Answer.stream) =
    let running = ref true in
    let stream =
      Lwt_stream.from
        (fun () ->
           if not !running then Lwt.return None else
             s.next () >|= function
             | None -> None
             | Some x -> Some (to_string x)) in
    let shutdown () = running := false ; s.shutdown () in
    streams := ConnectionMap.add con shutdown !streams ;
    stream
  in
  let shutdown_stream con =
    try ConnectionMap.find con !streams ()
    with Not_found -> () in
  let call_hook (io, con) req ?(answer_404 = false) hook =
    match hook with
    | None -> Lwt.return None
    | Some hook ->
        Lwt.catch
          (fun () ->
             hook (Uri.path (Cohttp.Request.uri req))
             >>= fun { Answer.code ; body } ->
             if code = 404 && not answer_404 then
               Lwt.return None
             else
               let body = match body with
                 | Answer.Empty ->
                     Cohttp_lwt_body.empty
                 | Single body ->
                     Cohttp_lwt_body.of_string body
                 | Stream s ->
                     let stream =
                       create_stream io con (fun s -> s) s in
                     Cohttp_lwt_body.of_stream stream in
               Lwt.return_some
                 (Response.make ~flush:true ~status:(`Code code) (),
                  body))
          (function
            | Not_found -> Lwt.return None
            | exn -> Lwt.fail exn) in
  let callback (io, con) req body =
    (* FIXME: check inbound adress *)
    let path = Utils.split_path (Uri.path (Cohttp.Request.uri req)) in
    lwt_log_info "(%s) receive request to %s"
      (Cohttp.Connection.to_string con) (Uri.path (Cohttp.Request.uri req)) >>= fun () ->
    Lwt.catch
      (fun () ->
         call_hook (io, con) req pre_hook >>= function
         | Some res ->
             Lwt.return res
         | None ->
             lookup root () path >>= fun handler ->
             begin
               match req.meth with
               | `POST -> begin
                   Cohttp_lwt_body.to_string body >>= fun body ->
                   match Data_encoding.Json.from_string body with
                   | Error msg -> Lwt.fail (Cannot_parse_body msg)
                   | Ok body -> Lwt.return (Some body)
                 end
               | `GET -> Lwt.return None
               | _ -> Lwt.fail Invalid_method
             end >>= fun body ->
             handler body >>= fun { Answer.code ; body } ->
             let body = match body with
               | Empty ->
                   Cohttp_lwt_body.empty
               | Single json ->
                   Cohttp_lwt_body.of_string (Data_encoding.Json.to_string json)
               | Stream s ->
                   let stream =
                     create_stream io con Data_encoding.Json.to_string s in
                   Cohttp_lwt_body.of_stream stream in
             lwt_log_info "(%s) RPC %s"
               (Cohttp.Connection.to_string con)
               (if Cohttp.Code.is_error code
                then "failed"
                else "success") >>= fun () ->
             Lwt.return (Response.make ~flush:true ~status:(`Code code) (),
                         body))
      (function
        | Not_found | Cannot_parse _ ->
            lwt_log_info "(%s) not found"
              (Cohttp.Connection.to_string con) >>= fun () ->
            (call_hook (io, con) req ~answer_404: true post_hook >>= function
              | Some res -> Lwt.return res
              | None ->
                  Lwt.return (Response.make ~flush:true ~status:`Not_found (),
                              Cohttp_lwt_body.empty))
        | Invalid_method ->
            lwt_log_info "(%s) bad method"
              (Cohttp.Connection.to_string con) >>= fun () ->
            let headers =
              Cohttp.Header.add_multi (Cohttp.Header.init ())
                "Allow" ["POST"] in
            Lwt.return (Response.make
                          ~flush:true ~status:`Method_not_allowed
                          ~headers (),
                        Cohttp_lwt_body.empty)
        | Cannot_parse_body msg ->
            lwt_log_info "(%s) can't parse RPC body"
              (Cohttp.Connection.to_string con) >>= fun () ->
            Lwt.return (Response.make ~flush:true ~status:`Bad_request (),
                        Cohttp_lwt_body.of_string msg)
        | e -> Lwt.fail e)
  and conn_closed (_, con) =
    log_info "connection close %s" (Cohttp.Connection.to_string con) ;
    shutdown_stream con  in
  lwt_log_info "create server listening on port %d" port >>= fun () ->
  let ctx = Cohttp_lwt_unix_net.init () in
  let mode = `TCP (`Port port) in
  let stop = cancelation () in
  let _server =
    Server.create
      ~stop ~ctx ~mode
      (Server.make ~callback ~conn_closed ()) in
  let shutdown () =
    canceler () >>= fun () ->
    lwt_log_info "server not really stopped (cohttp bug)" >>= fun () ->
    Lwt.return () (* server *) (* FIXME: bug in cohttp *) in
  Lwt.return { shutdown ; root }

let root_service { root } = root

let set_root_service server root = server.root <- root

let shutdown server =
  server.shutdown ()

module Error = struct

  let service =
    service
      ~description: "Schema for all the RPC errors from the shell"
      ~input: Data_encoding.empty
      ~output: Data_encoding.json_schema
      Path.(root / "errors")

  let encoding =
    let open Data_encoding in
    let path, _ = forge_request service () () in
    describe
      ~description:
        (Printf.sprintf
           "The full list of error is available with \
            the global RPC `/%s`" (String.concat "/" path))
      (conv
         ~schema:Json_schema.any
         (fun exn -> `A (List.map json_of_error exn))
         (function `A exns -> List.map error_of_json exns | _ -> [])
         json)

  let wrap param_encoding =
    let open Data_encoding in
    union [
      case
        (obj1 (req "ok" param_encoding))
        (function Ok x -> Some x | _ -> None)
        (fun x -> Ok x) ;
      case
        (obj1 (req "error" encoding))
        (function Error x -> Some x | _ -> None)
        (fun x -> Error x) ;
    ]

end
