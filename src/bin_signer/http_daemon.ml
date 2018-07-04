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

let log = Signer_logging.lwt_log_notice
open Signer_logging

let run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~check_high_watermark ~require_auth mode =
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register1 dir Signer_services.sign begin fun pkh signature data ->
      Handler.sign cctxt { pkh ; data ; signature } ?magic_bytes ~check_high_watermark ~require_auth
    end in
  let dir =
    RPC_directory.register1 dir Signer_services.public_key begin fun pkh () () ->
      Handler.public_key cctxt pkh
    end in
  let dir =
    RPC_directory.register0 dir Signer_services.authorized_keys begin fun () () ->
      if require_auth then
        Handler.Authorized_key.load cctxt >>=? fun keys ->
        return_some (keys |> List.split |> snd |> List.map Signature.Public_key.hash)
      else
        return_none
    end in
  Lwt.catch
    (fun () ->
       List.map
         (fun host ->
            let host = Ipaddr.V6.to_string host in
            log Tag.DSL.(fun f ->
                f "Listening on address %s"
                -% t event "signer_listening"
                -% s host_name host) >>= fun () ->
            RPC_server.launch ~host mode dir
              ~media_types:Media_type.all_media_types
            >>= fun _server ->
            fst (Lwt.wait ()))
         hosts |> Lwt.choose)
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind","") ->
          failwith "Port already in use."
      | exn -> Lwt.return (error_exn exn))

let run_https (cctxt : #Client_context.wallet) ~host ~port ~cert ~key ?magic_bytes ~check_high_watermark ~require_auth =
  Lwt_utils_unix.getaddrinfo ~passive:true ~node:host ~service:(string_of_int port) >>= function
  | []->
      failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = fst (List.split points) in
      log Tag.DSL.(fun f ->
          f "Accepting HTTPS requests on port %d"
          -% t event "accepting_https_requests"
          -% s port_number port) >>= fun () ->
      let mode : Conduit_lwt_unix.server =
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
      run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~check_high_watermark ~require_auth mode

let run_http (cctxt : #Client_context.wallet) ~host ~port ?magic_bytes ~check_high_watermark ~require_auth =
  Lwt_utils_unix.getaddrinfo ~passive:true ~node:host ~service:(string_of_int port) >>= function
  | [] ->
      failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = fst (List.split points) in
      log Tag.DSL.(fun f ->
          f "Accepting HTTP requests on port %d"
          -% t event "accepting_http_requests"
          -% s port_number port) >>= fun () ->
      let mode : Conduit_lwt_unix.server =
        `TCP (`Port port) in
      run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~check_high_watermark ~require_auth mode
