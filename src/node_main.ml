(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let description = [
  `S "DESCRIPTION" ;
  `P "Entry point for initializing, configuring and running a Tezos node." ;
  `P Node_identity_command.Manpage.command_description ;
  `P Node_run_command.Manpage.command_description ;
  `P Node_config_command.Manpage.command_description ;
]

let man =
  description @
  Node_run_command.Manpage.examples

let info =
  Cmdliner.Term.info
    ~doc:"The Tezos node"
    ~man
    "tezos-node"

let commands = [
  Node_run_command.cmd ;
  Node_config_command.cmd ;
  Node_identity_command.cmd ;
]

let () =
  match Cmdliner.Term.eval_choice (term, info) commands with
  | `Error _ -> exit 1
  | `Help -> exit 0
  | `Version -> exit 1
  | `Ok () -> exit 0
