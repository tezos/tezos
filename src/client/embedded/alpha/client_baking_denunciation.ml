(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Client.Denunciation

let create cctxt endorsement_stream =
  let last_get_endorsement = ref None in
  let get_endorsement () =
    match !last_get_endorsement with
    | None ->
        let t = Lwt_stream.get endorsement_stream in
        last_get_endorsement := Some t ;
        t
    | Some t -> t in
  let rec worker_loop () =
    (* let timeout = compute_timeout state in *)
    Lwt.choose [
      (* (timeout >|= fun () -> `Timeout) ; *)
      (get_endorsement () >|= fun e -> `Endorsement e) ;
    ] >>= function
    | `Endorsement (None | Some (Error _)) ->
        Lwt.return_unit
    | `Endorsement (Some (Ok e)) ->
        last_get_endorsement := None ;
        Client_keys.Public_key_hash.name cctxt
          e.Client_baking_operations.source >>= function
        | Ok source ->
            lwt_debug
              "Discovered endorsement for block %a by %s (slot @[<h>%a@])"
              Block_hash.pp_short e.block
              source
              Format.(pp_print_list pp_print_int) e.slots >>= fun () ->
            worker_loop ()
        | Error _ ->
            (* TODO log *)
            worker_loop ()
  in
  lwt_log_info "Starting denunciation daemon" >>= fun () ->
  worker_loop ()
