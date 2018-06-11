(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_stdlib.Logging.Make_semantic(struct let name = "base" end)

let pp_exn_trace ppf backtrace =
  if String.length backtrace <> 0 then
    Format.fprintf ppf
      "@,Backtrace:@,  @[<h>%a@]"
      Format.pp_print_text backtrace

let pid = Tag.def ~doc:"unix process ID where problem occurred" "pid" Format.pp_print_int
let exn_trace = Tag.def ~doc:"backtrace from native Ocaml exception" "exn_trace" pp_exn_trace
