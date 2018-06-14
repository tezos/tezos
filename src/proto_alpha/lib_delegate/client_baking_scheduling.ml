(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let sleep_until time =
  let delay = Time.diff time (Time.now ()) in
  if delay < 0L then
    None
  else
    Some (Lwt_unix.sleep (Int64.to_float delay))

let wait_for_first_block
    ?(info = fun (_: (unit Lwt.t, unit) Client_context.lwt_format) -> Lwt.return_unit)
    (block_stream: Client_baking_blocks.block_info tzresult Lwt_stream.t)
    k =
  let rec wait_for_first_block () =
    Lwt_stream.get block_stream >>= function
    | None | Some (Error _) ->
        info "Can't fetch the current block head. Retrying soon." >>= fun () ->
        (* NOTE: this is not a tight loop because of Lwt_stream.get *)
        wait_for_first_block ()
    | Some (Ok bi) ->
        k bi
  in
  wait_for_first_block ()
