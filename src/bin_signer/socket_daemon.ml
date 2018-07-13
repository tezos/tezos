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

open Signer_logging
open Signer_messages

let log = lwt_log_notice

let run (cctxt : #Client_context.wallet) path ?magic_bytes ~require_auth =
  Lwt_utils_unix.Socket.bind path >>=? fun fd ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async begin fun () ->
      Lwt_utils_unix.Socket.recv fd Request.encoding >>=? function
      | Sign req ->
          let encoding = result_encoding Sign.Response.encoding in
          Handler.sign cctxt req ?magic_bytes ~require_auth >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return_unit
      | Public_key pkh ->
          let encoding = result_encoding Public_key.Response.encoding in
          Handler.public_key cctxt pkh >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return_unit
      | Authorized_keys ->
          let encoding = result_encoding Authorized_keys.Response.encoding in
          begin if require_auth then
              Handler.Authorized_key.load cctxt >>=? fun keys ->
              return (Authorized_keys.Response.Authorized_keys
                        (keys |> List.split |> snd |> List.map Signature.Public_key.hash))
            else return Authorized_keys.Response.No_authentication
          end >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return_unit
    end ;
    loop ()
  in
  begin
    match path with
    | Tcp (host, port) ->
        log Tag.DSL.(fun f ->
            f "Accepting TCP requests on port %s:%d"
            -% t event "accepting_tcp_requests"
            -% s host_name host
            -% s port_number port)
    | Unix path ->
        Sys.set_signal Sys.sigint (Signal_handle begin fun _ ->
            Format.printf "Removing the local socket file and quitting.@." ;
            Unix.unlink path ;
            exit 0
          end) ;
        log Tag.DSL.(fun f ->
            f "Accepting UNIX requests on %s"
            -% t event "accepting_unix_requests"
            -% s unix_socket_path path)
  end >>= fun () ->
  loop ()
