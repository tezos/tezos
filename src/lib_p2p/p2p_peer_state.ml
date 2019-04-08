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

open P2p_peer

type ('conn, 'conn_meta) t =
  | Accepted of { current_point: P2p_connection.Id.t ;
                  cancel: Lwt_canceler.t }
  | Running of { data: 'conn ;
                 conn_metadata: 'conn_meta ;
                 current_point: P2p_connection.Id.t }
  | Disconnected
type ('conn, 'conn_meta) state = ('conn, 'conn_meta) t

let pp ppf = function
  | Accepted { current_point ; _ } ->
      Format.fprintf ppf "accepted %a" P2p_connection.Id.pp current_point
  | Running { current_point ; _ } ->
      Format.fprintf ppf "running %a" P2p_connection.Id.pp current_point
  | Disconnected ->
      Format.fprintf ppf "disconnected"

module Info = struct

  type ('conn, 'peer_meta, 'conn_meta) t = {
    peer_id : Id.t ;
    created : Time.System.t ;
    mutable state : ('conn, 'conn_meta) state ;
    mutable peer_metadata : 'peer_meta ;
    mutable trusted : bool ;
    mutable last_failed_connection : (P2p_connection.Id.t * Time.System.t) option ;
    mutable last_rejected_connection : (P2p_connection.Id.t * Time.System.t) option ;
    mutable last_established_connection : (P2p_connection.Id.t * Time.System.t) option ;
    mutable last_disconnection : (P2p_connection.Id.t * Time.System.t) option ;
    events : Pool_event.t Ring.t ;
    watchers : Pool_event.t Lwt_watcher.input ;
  }
  type ('conn, 'peer_meta, 'conn_meta) peer_info = ('conn, 'peer_meta, 'conn_meta) t

  let compare gi1 gi2 = Id.compare gi1.peer_id gi2.peer_id

  let log_size = 100

  let create ?(created = Systime_os.now ()) ?(trusted = false) ~peer_metadata peer_id =
    { peer_id ;
      created ;
      state = Disconnected ;
      peer_metadata ;
      trusted ;
      last_failed_connection = None ;
      last_rejected_connection = None ;
      last_established_connection = None ;
      last_disconnection = None ;
      events = Ring.create log_size ;
      watchers = Lwt_watcher.create_input () ;
    }

  let encoding peer_metadata_encoding =
    let open Data_encoding in
    conv
      (fun { peer_id ; trusted ; peer_metadata ; events ; created ;
             last_failed_connection ; last_rejected_connection ;
             last_established_connection ; last_disconnection ; _ } ->
        (peer_id, created, trusted, peer_metadata, Ring.elements events,
         last_failed_connection, last_rejected_connection,
         last_established_connection, last_disconnection))
      (fun (peer_id, created, trusted, peer_metadata, event_list,
            last_failed_connection, last_rejected_connection,
            last_established_connection, last_disconnection) ->
        let info = create ~trusted ~peer_metadata peer_id in
        let events = Ring.create log_size in
        Ring.add_list info.events event_list ;
        { state = Disconnected ;
          trusted ; peer_id ; peer_metadata ; created ;
          last_failed_connection ;
          last_rejected_connection ;
          last_established_connection ;
          last_disconnection ;
          events ;
          watchers = Lwt_watcher.create_input () ;
        })
      (obj9
         (req "peer_id" Id.encoding)
         (req "created" Time.System.encoding)
         (dft "trusted" bool false)
         (req "peer_metadata" peer_metadata_encoding)
         (dft "events" (list Pool_event.encoding) [])
         (opt "last_failed_connection"
            (tup2 P2p_connection.Id.encoding Time.System.encoding))
         (opt "last_rejected_connection"
            (tup2 P2p_connection.Id.encoding Time.System.encoding))
         (opt "last_established_connection"
            (tup2 P2p_connection.Id.encoding Time.System.encoding))
         (opt "last_disconnection"
            (tup2 P2p_connection.Id.encoding Time.System.encoding)))

  let peer_id { peer_id ; _ } = peer_id
  let created { created ; _ } = created
  let peer_metadata { peer_metadata ; _ } = peer_metadata
  let set_peer_metadata gi peer_metadata = gi.peer_metadata <- peer_metadata
  let trusted { trusted ; _ } = trusted
  let set_trusted gi = gi.trusted <- true
  let unset_trusted gi = gi.trusted <- false
  let last_established_connection s = s.last_established_connection
  let last_disconnection s = s.last_disconnection
  let last_failed_connection s = s.last_failed_connection
  let last_rejected_connection s = s.last_rejected_connection

  let last_seen s =
    Time.System.recent
      s.last_established_connection
      (Time.System.recent s.last_rejected_connection s.last_disconnection)
  let last_miss s =
    Time.System.recent
      s.last_failed_connection
      (Time.System.recent s.last_rejected_connection s.last_disconnection)

  let log { events ; watchers ; _ } ?(timestamp = Systime_os.now ()) point kind =
    let event = { Pool_event.kind ; timestamp ; point } in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ?timestamp peer_info point =
    log peer_info ?timestamp point Rejecting_request

  module File = struct

    let load path peer_metadata_encoding =
      let enc =
        Data_encoding.list (encoding peer_metadata_encoding) in
      if path <> "/dev/null" && Sys.file_exists path then
        Lwt_utils_unix.Json.read_file path >>=? fun json ->
        return (Data_encoding.Json.destruct enc json)
      else
        return_nil

    let save path peer_metadata_encoding peers =
      let open Data_encoding in
      Lwt_utils_unix.Json.write_file path @@
      Json.construct (list (encoding peer_metadata_encoding)) peers

  end

  let watch { watchers ; _ } = Lwt_watcher.create_stream watchers
  let fold { events ; _ } ~init ~f = Ring.fold events ~init ~f

end

let get { Info.state ; _ } = state

let is_disconnected { Info.state ; _ } =
  match state with
  | Disconnected -> true
  | Accepted _ | Running _ -> false

let set_accepted
    ?(timestamp = Systime_os.now ())
    peer_info current_point cancel =
  assert begin
    match peer_info.Info.state with
    | Accepted _ | Running _ -> false
    | Disconnected -> true
  end ;
  peer_info.state <- Accepted { current_point ; cancel } ;
  Info.log peer_info ~timestamp current_point Accepting_request

let set_running
    ?(timestamp = Systime_os.now ())
    peer_info point data conn_metadata =
  assert begin
    match peer_info.Info.state with
    | Disconnected -> true (* request to unknown peer_id. *)
    | Running _ -> false
    | Accepted { current_point ; _ } ->
        P2p_connection.Id.equal point current_point
  end ;
  peer_info.state <- Running { data ; conn_metadata ; current_point = point } ;
  peer_info.last_established_connection <- Some (point, timestamp) ;
  Info.log peer_info ~timestamp point Connection_established

let set_disconnected
    ?(timestamp = Systime_os.now ()) ?(requested = false) peer_info =
  let current_point, (event : Pool_event.kind) =
    match peer_info.Info.state with
    | Accepted { current_point ; _ } ->
        peer_info.last_rejected_connection <-
          Some (current_point, timestamp) ;
        current_point, Request_rejected
    | Running { current_point ; _ } ->
        peer_info.last_disconnection <-
          Some (current_point, timestamp) ;
        current_point,
        if requested then Disconnection else External_disconnection
    | Disconnected -> assert false
  in
  peer_info.state <- Disconnected ;
  Info.log peer_info ~timestamp current_point event
