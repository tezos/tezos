(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "client.denunciation" end)

let create cctxt endorsement_stream =

  let never_ends = Lwt_utils.never_ending () in

  let event_k cctxt () e =
    (* TODO: more than just logging *)
    Client_keys.Public_key_hash.name
      cctxt
      e.Client_baking_operations.source >>= function
    | Ok source ->
        lwt_debug
          "Discovered endorsement for block %a by %s (slot @[<h>%a@])"
          Block_hash.pp_short e.block
          source
          Format.(pp_print_list pp_print_int) e.slots >>= fun () ->
        return ()
    | Error errs ->
        lwt_log_error "Error whilst checking the endorsment %a/%a:@\n%a"
          Block_hash.pp_short e.block
          Format.(pp_print_list pp_print_int) e.slots
          pp_print_error errs >>= fun () ->
        return ()
  in

  Client_baking_scheduling.main
    ~name:"denunciator"
    ~cctxt
    ~stream:endorsement_stream
    ~state_maker:(fun _ _ -> return ())
    ~pre_loop:(fun _ _ _ -> return ())
    ~compute_timeout:(fun () -> never_ends)
    ~timeout_k:(fun _ _ () -> return ())
    ~event_k

