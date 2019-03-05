(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Lwt.Infix

module ConnectionMap = Map.Make(Cohttp.Connection)

module type LOGGING = sig

  val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

end

module Make (Encoding : Resto.ENCODING)(Log : LOGGING) = struct

  open Log
  open Cohttp

  module Service = Resto.MakeService(Encoding)
  module Directory = Resto_directory.Make(Encoding)

  module Media_type = Media_type.Make(Encoding)

  type server = {
    root : unit Directory.directory ;
    mutable streams : (unit -> unit) ConnectionMap.t ;
    cors : Cors.t ;
    media_types : Media_type.t list ;
    default_media_type : string * Media_type.t ;
    stopper : unit Lwt.u ;
    mutable worker : unit Lwt.t ;
  }

  let create_stream server con to_string s =
    let running = ref true in
    let stream =
      Lwt_stream.from
        (fun () ->
           if not !running then
             Lwt.return None
           else
             s.Resto_directory.Answer.next () >|= function
             | None -> None
             | Some x -> Some (to_string x)) in
    let shutdown () =
      running := false ;
      s.shutdown () ;
      server.streams <- ConnectionMap.remove con server.streams in
    server.streams <- ConnectionMap.add con shutdown server.streams ;
    stream

  let (>>=?) m f =
    m >>= function
    | Ok x -> f x
    | Error err -> Lwt.return_error err

  let callback server ((_io, con) : Cohttp_lwt_unix.Server.conn) req body =
    let uri = Request.uri req in
    let path = Uri.pct_decode (Uri.path uri) in
    lwt_log_info "(%s) receive request to %s"
      (Connection.to_string con) path >>= fun () ->
    let path = Utils.split_path path in
    let req_headers = Request.headers req in
    begin
      match Request.meth req with
      | #Resto.meth when server.cors.allowed_origins <> [] &&
                         not (Cors.check_host req_headers server.cors) ->
          let headers =
            Cohttp.Header.init_with "X-OCaml-Resto-CORS-Error" "invalid host" in
          Lwt.return_ok
            (Response.make ~headers ~status:`Forbidden (),
             Cohttp_lwt.Body.empty)
      | #Resto.meth as meth -> begin
          Directory.lookup server.root ()
            meth path >>=? fun (Directory.Service s) ->
          begin
            match Header.get req_headers "content-type" with
            | None -> Lwt.return_ok (snd server.default_media_type)
            | Some content_type ->
                match Utils.split_path content_type with
                | [x ; y] -> begin
                    match Media_type.find_media (x, y) server.media_types with
                    | None ->
                        Lwt.return_error (`Unsupported_media_type content_type)
                    | Some media_type ->
                        Lwt.return_ok media_type
                  end
                | _ ->
                    Lwt.return_error (`Unsupported_media_type content_type)
          end >>=? fun input_media_type ->
          lwt_debug "(%s) input media type %s"
            (Connection.to_string con)
            (Media_type.name input_media_type) >>= fun () ->
          begin
            match Header.get req_headers "accept" with
            | None -> Lwt.return_ok server.default_media_type
            | Some accepted ->
                match Media_type.resolve_accept_header
                        server.media_types (Some accepted) with
                | None -> Lwt.return_error `Not_acceptable
                | Some media_type -> Lwt.return_ok media_type
          end >>=? fun (output_content_type, output_media_type) ->
          begin
            match Resto.Query.parse s.types.query
                    (List.map
                       (fun (k, l) -> (k, String.concat "," l))
                       (Uri.query uri)) with
            | exception (Resto.Query.Invalid s) ->
                Lwt.return_error (`Cannot_parse_query s)
            | query -> Lwt.return_ok query
          end >>=? fun query ->
          lwt_debug "(%s) ouput media type %s"
            (Connection.to_string con)
            (Media_type.name output_media_type) >>= fun () ->
          let output = output_media_type.construct s.types.output
          and error = function
            | None -> Cohttp_lwt.Body.empty, Transfer.Fixed 0L
            | Some e ->
                let s = output_media_type.construct s.types.error e in
                Cohttp_lwt.Body.of_string s,
                Transfer.Fixed (Int64.of_int (String.length s)) in
          let headers = Header.init () in
          let headers =
            Header.add headers "content-type" output_content_type in
          let headers = Cors.add_allow_origin
              headers server.cors (Header.get req_headers "origin") in
          begin
            match s.types.input with
            | Service.No_input ->
                s.handler query () >>= Lwt.return_ok
            | Service.Input input ->
                Cohttp_lwt.Body.to_string body >>= fun body ->
                match
                  input_media_type.destruct input body
                with
                | Error s ->
                    Lwt.return_error (`Cannot_parse_body s)
                | Ok body ->
                    s.handler query body >>= Lwt.return_ok
          end >>=? function
          | `Ok o ->
              let body = output o in
              let encoding =
                Transfer.Fixed (Int64.of_int (String.length body)) in
              Lwt.return_ok
                (Response.make ~status:`OK ~encoding ~headers (),
                 Cohttp_lwt.Body.of_string body)
          | `OkStream o ->
              let body = create_stream server con output o in
              let encoding = Transfer.Chunked in
              Lwt.return_ok
                (Response.make ~status:`OK ~encoding ~headers (),
                 Cohttp_lwt.Body.of_stream body)
          | `Created s ->
              let headers = Header.init () in
              let headers =
                match s with
                | None -> headers
                | Some s -> Header.add headers "location" s in
              Lwt.return_ok
                (Response.make ~status:`Created  ~headers (),
                 Cohttp_lwt.Body.empty)
          | `No_content ->
              Lwt.return_ok
                (Response.make ~status:`No_content (),
                 Cohttp_lwt.Body.empty)
          | `Unauthorized e ->
              let body, encoding = error e in
              let status = `Unauthorized in
              Lwt.return_ok
                (Response.make ~status ~encoding ~headers (), body)
          | `Forbidden e ->
              let body, encoding = error e in
              let status = `Forbidden in
              Lwt.return_ok
                (Response.make ~status ~encoding ~headers (), body)
          | `Not_found e ->
              let body, encoding = error e in
              let status = `Not_found in
              Lwt.return_ok
                (Response.make ~status ~encoding ~headers (), body)
          | `Conflict e ->
              let body, encoding = error e in
              let status = `Conflict in
              Lwt.return_ok
                (Response.make ~status ~encoding ~headers (), body)
          | `Error e ->
              let body, encoding = error e in
              let status = `Internal_server_error in
              Lwt.return_ok
                (Response.make ~status ~encoding ~headers (), body)
        end
      | `HEAD ->
          (* TODO ??? *)
          Lwt.return_error `Not_implemented
      | `OPTIONS ->
          let req_headers = Request.headers req in
          let origin_header = Header.get req_headers "origin" in
          begin
            (* Default OPTIONS handler for CORS preflight *)
            if origin_header = None then
              Directory.allowed_methods server.root () path
            else
              match Header.get req_headers
                      "Access-Control-Request-Method" with
              | None ->
                  Directory.allowed_methods server.root () path
              | Some meth ->
                  match Code.method_of_string meth with
                  | #Resto.meth as meth ->
                      Directory.lookup server.root () meth path >>=? fun _handler ->
                      Lwt.return_ok [ meth ]
                  | _ ->
                      Lwt.return_error `Not_found
          end >>=? fun cors_allowed_meths ->
          lwt_log_info "(%s) RPC preflight"
            (Connection.to_string con) >>= fun () ->
          let headers = Header.init () in
          let headers =
            Header.add_multi headers
              "Access-Control-Allow-Methods"
              (List.map Resto.string_of_meth cors_allowed_meths) in
          let headers = Cors.add_headers headers server.cors origin_header in
          Lwt.return_ok
            (Response.make ~flush:true ~status:`OK ~headers (),
             Cohttp_lwt.Body.empty)
      | _ ->
          Lwt.return_error `Not_implemented
    end >>= function
    | Ok answer -> Lwt.return answer
    | Error `Not_implemented ->
        Lwt.return
          (Response.make ~status:`Not_implemented (),
           Cohttp_lwt.Body.empty)
    | Error `Method_not_allowed methods ->
        let headers = Header.init () in
        let headers =
          Header.add_multi headers "allow"
            (List.map Resto.string_of_meth methods) in
        Lwt.return
          (Response.make ~status:`Method_not_allowed ~headers (),
           Cohttp_lwt.Body.empty)
    | Error `Cannot_parse_path (context, arg, value) ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        Lwt.return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_lwt.Body.of_string
             "Failed to parsed an argument in path. After \"%s\", \
              the value \"%s\" is not acceptable for type \"%s\""
             (String.concat "/" context) value arg.name)
    | Error `Cannot_parse_body s ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        Lwt.return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_lwt.Body.of_string
             "Failed to parse the request body: %s" s)
    | Error `Cannot_parse_query s ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        Lwt.return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_lwt.Body.of_string
             "Failed to parse the query string: %s" s)
    | Error `Not_acceptable ->
        let accepted_encoding =
          Media_type.acceptable_encoding server.media_types in
        Lwt.return
          (Response.make ~status:`Not_acceptable (),
           Cohttp_lwt.Body.of_string accepted_encoding)
    | Error `Unsupported_media_type _ ->
        Lwt.return
          (Response.make ~status:`Unsupported_media_type (),
           Cohttp_lwt.Body.empty)
    | Error `Not_found ->
        Lwt.return
          (Response.make ~status:`Not_found (),
           Cohttp_lwt.Body.empty)

  (* Promise a running RPC server. *)

  let launch
      ?(host="::")
      ?(cors = Cors.default)
      ~media_types
      mode root =
    let default_media_type =
      match Media_type.first_complete_media media_types with
      | None -> invalid_arg "Resto_directory_cohttp.launch(empty media type list)"
      | Some ((l, r), m) -> l^"/"^r, m in
    let stop, stopper = Lwt.wait () in
    let server = {
      root ;
      streams = ConnectionMap.empty ;
      cors ;
      media_types ;
      default_media_type ;
      stopper ;
      worker = Lwt.return_unit ;
    } in
    Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
    let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
    server.worker <- begin
      let conn_closed (_, con) =
        log_info "connection closed %s" (Connection.to_string con) ;
        try ConnectionMap.find con server.streams ()
        with Not_found -> ()
      and on_exn = function
        | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
            log_error "RPC server port already taken, \
                       the node will be shutdown" ;
            exit 1
        | Unix.Unix_error (ECONNRESET, _, _)
        | Unix.Unix_error (EPIPE, _, _)  -> ()
        | exn ->
            Format.eprintf "@[<v 2>Uncaught (asynchronous) exception:@ %s@ %s@]%!"
              (Printexc.to_string exn)
              (Printexc.get_backtrace ())
      and callback (io, con) req body =
        Lwt.catch
          begin fun () -> callback server (io, con) req body end
          begin function
            | Not_found ->
                let status = `Not_found in
                let body = Cohttp_lwt.Body.empty in
                Lwt.return (Response.make ~status (), body)
            | exn ->
                let headers = Header.init () in
                let headers =
                  Header.add headers "content-type" "text/ocaml.exception" in
                let status = `Internal_server_error in
                let body = Cohttp_lwt.Body.of_string (Printexc.to_string exn) in
                Lwt.return (Response.make ~status ~headers (), body)
          end
      in
      Cohttp_lwt_unix.Server.create ~stop ~ctx ~mode ~on_exn
        (Cohttp_lwt_unix.Server.make ~callback ~conn_closed ())
    end ;
    Lwt.return server

  let shutdown server =
    Lwt.wakeup_later server.stopper () ;
    server.worker >>= fun () ->
    ConnectionMap.iter (fun _ f -> f ()) server.streams ;
    Lwt.return_unit

end
