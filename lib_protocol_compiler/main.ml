(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  try
    Tezos_protocol_compiler.Native.main ();
    Pervasives.exit 0
  with exn ->
    Format.eprintf "%a\n%!" Opterrors.report_error exn;
    Pervasives.exit 1
