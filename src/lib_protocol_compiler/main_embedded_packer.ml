(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let srcdir = Sys.argv.(1)
let version = Sys.argv.(2)

let hash, sources = Protocol.read_dir srcdir

let () =
  Format.printf {|
module Source = struct
  let hash =
    Some (Tezos_crypto.Protocol_hash.of_b58check_exn %S)
  let sources = Tezos_base.Protocol.%a
end
@.|}
    (Protocol_hash.to_b58check hash)
    Protocol.pp_ocaml sources

let () =
  Format.printf {|
let () =
  let module Ignored = Tezos_shell.State.Register_embedded_protocol
    (Tezos_embedded_protocol_environment_%s.Environment)
    (Tezos_embedded_raw_protocol_%s.Main)
    (Source) in
    ()
@.|}
    version version
