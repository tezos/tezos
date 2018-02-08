(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_peer

type 'data t =
  | Accepted of { current_point: P2p_connection.Id.t ;
                  cancel: Lwt_canceler.t }
  | Running of { data: 'data ;
                 current_point: P2p_connection.Id.t }
  | Disconnected
type 'data state = 'data t

let pp ppf = function
  | Accepted { current_point ; _ } ->
      Format.fprintf ppf "accepted %a" P2p_connection.Id.pp current_point
  | Running { current_point ; _ } ->
      Format.fprintf ppf "running %a" P2p_connection.Id.pp current_point
  | Disconnected ->
      Format.fprintf ppf "disconnected"

module Info = struct

  type ('conn, 'meta) t = {
    peer_id : Id.t ;
    created : Time.t ;
    mutable state : 'conn state ;
    mutable metadata : 'meta ;
    mutable trusted : bool ;
    mutable last_failed_connection : (P2p_connection.Id.t * Time.t) option ;
    mutable last_rejected_connection : (P2p_connection.Id.t * Time.t) option ;
    mutable last_established_connection : (P2p_connection.Id.t * Time.t) option ;
    mutable last_disconnection : (P2p_connection.Id.t * Time.t) option ;
    events : Pool_event.t Ring.t ;
    watchers : Pool_event.t Lwt_watcher.input ;
  }
  type ('conn, 'meta) peer_info = ('conn, 'meta) t

  let compare gi1 gi2 = Id.compare gi1.peer_id gi2.peer_id

  let log_size = 100

  let create ?(created = Time.now ()) ?(trusted = false) ~metadata peer_id =
    { peer_id ;
      created ;
      state = Disconnected ;
      metadata ;
      trusted ;
      last_failed_connection = None ;
      last_rejected_connection = None ;
      last_established_connection = None ;
      last_disconnection = None ;
      events = Ring.create log_size ;
      watchers = Lwt_watcher.create_input () ;
    }

  let encoding metadata_encoding =
    let open Data_encoding in
    conv
      (fun { peer_id ; trusted ; metadata ; events ; created ;
             last_failed_connection ; last_rejected_connection ;
             last_established_connection ; last_disconnection ; _ } ->
        (peer_id, created, trusted, metadata, Ring.elements events,
         last_failed_connection, last_rejected_connection,
         last_established_connection, last_disconnection))
      (fun (peer_id, created, trusted, metadata, event_list,
            last_failed_connection, last_rejected_connection,
            last_established_connection, last_disconnection) ->
        let info = create ~trusted ~metadata peer_id in
        let events = Ring.create log_size in
        Ring.add_list info.events event_list ;
        { state = Disconnected ;
          trusted ; peer_id ; metadata ; created ;
          last_failed_connection ;
          last_rejected_connection ;
          last_established_connection ;
          last_disconnection ;
          events ;
          watchers = Lwt_watcher.create_input () ;
        })
      (obj9
         (req "peer_id" Id.encoding)
         (req "created" Time.encoding)
         (dft "trusted" bool false)
         (req "metadata" metadata_encoding)
         (dft "events" (list Pool_event.encoding) [])
         (opt "last_failed_connection"
            (tup2 P2p_connection.Id.encoding Time.encoding))
         (opt "last_rejected_connection"
            (tup2 P2p_connection.Id.encoding Time.encoding))
         (opt "last_established_connection"
            (tup2 P2p_connection.Id.encoding Time.encoding))
         (opt "last_disconnection"
            (tup2 P2p_connection.Id.encoding Time.encoding)))

  let peer_id { peer_id ; _ } = peer_id
  let created { created ; _ } = created
  let metadata { metadata ; _ } = metadata
  let set_metadata gi metadata = gi.metadata <- metadata
  let trusted { trusted ; _ } = trusted
  let set_trusted gi = gi.trusted <- true
  let unset_trusted gi = gi.trusted <- false
  let last_established_connection s = s.last_established_connection
  let last_disconnection s = s.last_disconnection
  let last_failed_connection s = s.last_failed_connection
  let last_rejected_connection s = s.last_rejected_connection

  let last_seen s =
    Time.recent
      s.last_established_connection
      (Time.recent s.last_rejected_connection s.last_disconnection)
  let last_miss s =
    Time.recent
      s.last_failed_connection
      (Time.recent s.last_rejected_connection s.last_disconnection)

  let log { events ; watchers ; _ } ?(timestamp = Time.now ()) point kind =
    let event = { Pool_event.kind ; timestamp ; point } in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ?timestamp peer_info point =
    log peer_info ?timestamp point Rejecting_request

  module File = struct

    let load path metadata_encoding =
      let enc = Data_encoding.list (encoding metadata_encoding) in
      if path <> "/dev/null" && Sys.file_exists path then
        Data_encoding_ezjsonm.read_file path >>=? fun json ->
        return (Data_encoding.Json.destruct enc json)
      else
        return []

    let save path metadata_encoding peers =
      let open Data_encoding in
      Data_encoding_ezjsonm.write_file path @@
      Json.construct (list (encoding metadata_encoding)) peers

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
    ?(timestamp = Time.now ())
    peer_info current_point cancel =
  assert begin
    match peer_info.Info.state with
    | Accepted _ | Running _ -> false
    | Disconnected -> true
  end ;
  peer_info.state <- Accepted { current_point ; cancel } ;
  Info.log peer_info ~timestamp current_point Accepting_request

let set_running
    ?(timestamp = Time.now ())
    peer_info point data =
  assert begin
    match peer_info.Info.state with
    | Disconnected -> true (* request to unknown peer_id. *)
    | Running _ -> false
    | Accepted { current_point ; _ } ->
        P2p_connection.Id.equal point current_point
  end ;
  peer_info.state <- Running { data ; current_point = point } ;
  peer_info.last_established_connection <- Some (point, timestamp) ;
  Info.log peer_info ~timestamp point Connection_established

let set_disconnected
    ?(timestamp = Time.now ()) ?(requested = false) peer_info =
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
