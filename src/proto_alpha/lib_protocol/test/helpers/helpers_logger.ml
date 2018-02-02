(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions , Inc.< contact@tezos.com >                *)
(*                                                                        *)
(*    All rights reserved.No warranty , explicit or implicit , provided.  *)
(*                                                                        *)
(**************************************************************************)

let name = "Isolate Helpers"
module Logger = Logging.Make(struct let name = name end)
let section = Lwt_log.Section.make name
let () =
  Lwt_log.Section.set_level section Lwt_log.Debug

include Logger
