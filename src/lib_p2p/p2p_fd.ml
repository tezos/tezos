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

(* logging facility to monitor sockets *)

let is_not_windows = Sys.os_type <> "Win32"
let () =
  (* Otherwise some writes trigger a SIGPIPE instead of raising an
     Lwt_unit exception. In the node, this is already done by
     Cohttp, so this is only useful when using the P2P layer as a
     stand alone library.  *)
  if is_not_windows  then
    Sys.(set_signal sigpipe Signal_ignore)

(* Logging facility for the P2P layer *)
module Log = Internal_event.Legacy_logging.Make(struct let name = "p2p.fd" end)

type t = {
  fd : Lwt_unix.file_descr ;
  id : int ;
  mutable nread : int ;
  mutable nwrit : int ;
}

(* we use a prefix ' cnx:' that allows easy grepping in the log to lookup
   everything related to a particular connection. *)
let log t fmt =
  Format.kasprintf (fun s -> Log.debug "cnx:%d:%s" t.id s) fmt

let create =
  let counter = ref 0 in
  function fd ->
    incr counter;
    let t = { fd ; id = !counter ; nread = 0 ; nwrit = 0 } in
    log t "create: fd %d" t.id ;
    t

let string_of_sockaddr addr =
  match addr with
  | Lwt_unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
  | Lwt_unix.ADDR_UNIX file ->
      Printf.sprintf "@%s" file

let id t = t.id

let socket proto kind arg =
  create (Lwt_unix.socket proto kind arg)

let close t =
  log t "close: stats %d/%d" t.nread t.nwrit ;
  Lwt_utils_unix.safe_close t.fd

let read t buf pos len =
  log t "try-read: %d" len;
  Lwt_bytes.read t.fd buf pos len >>= fun nread ->
  t.nread <- t.nread + nread ;
  log t "read: %d (%d)" nread t.nread ;
  Lwt.return nread

let write t buf =
  let len = MBytes.length buf in
  log t "try-write: %d" len;
  Lwt_utils_unix.write_mbytes t.fd buf >>= fun () ->
  t.nwrit <- t.nwrit + len ;
  log t "written: %d (%d)" len t.nwrit ;
  Lwt.return_unit

let connect t saddr =
  log t "connect: %s" (string_of_sockaddr saddr);
  Lwt_unix.connect t.fd saddr

let accept sock =
  Lwt_unix.accept sock >>= fun (fd, saddr) ->
  let t = create fd in
  log t "accept: %s" (string_of_sockaddr saddr);
  Lwt.return (t, saddr)

module Table =
  Hashtbl.Make(struct
    type nonrec t = t
    let equal { id = x ; _ } { id = y ; _ } = x = y
    let hash { id ; _ } = Hashtbl.hash id
  end)

