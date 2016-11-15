(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type worker = {
  shutdown: unit -> unit Lwt.t;
}

let create_worker p2p state =

  let cancelation, cancel, _on_cancel = Lwt_utils.canceler () in

  let broadcast m = Tezos_p2p.broadcast p2p m in

  let discovery_worker =
    let rec worker_loop () =
      let nets = State.Net.active state in
      Lwt_list.iter_p
        (fun net ->
           State.Net.Blockchain.head net >>= fun head ->
           State.Valid_block.block_locator state 50 head >>= fun locator ->
           broadcast Tezos_p2p.(Discover_blocks (State.Net.id net, locator)) ;
           broadcast Tezos_p2p.(Current_operations (State.Net.id net)) ;
           Lwt.return_unit)
        nets >>= fun () ->
      let timeout = 15. +. Random.float 15. in
      Lwt.pick [(Lwt_unix.sleep timeout >|= fun () -> `Process);
                (cancelation () >|= fun () -> `Cancel)] >>= function
      | `Cancel -> Lwt.return_unit
      | `Process ->
          worker_loop ()
    in
    Lwt_utils.worker "discoverer" ~run:worker_loop ~cancel in

  let shutdown () =
    cancel () >>= fun () -> discovery_worker in

  { shutdown;
  }

let shutdown t = t.shutdown ()
