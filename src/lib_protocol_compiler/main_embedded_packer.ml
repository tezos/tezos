(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let srcdir = Sys.argv.(1)
let version = Sys.argv.(2)

let hash, sources =
  match Lwt_main.run (Lwt_utils_unix.Protocol.read_dir srcdir) with
  | Ok v -> v
  | Error err ->
      Format.kasprintf Pervasives.failwith
        "Failed to read TEZOS_PROTOCOL: %a" pp_print_error err

let () =
  Format.printf {|
module Source = struct
  let hash =
    Some (Tezos_base.Protocol_hash.of_b58check_exn %S)
  let sources = Tezos_base.Protocol.%a
end
@.|}
    (Protocol_hash.to_b58check hash)
    Protocol.pp_ocaml sources

let () =
  Format.printf {|
let () =
  let module Ignored = Tezos_protocol_updater.Registered_protocol.Register
    (Tezos_embedded_protocol_environment_%s.Environment)
    (Tezos_embedded_raw_protocol_%s.Main)
    (Source) in
    ()
@.|}
    version version
