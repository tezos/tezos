(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let commands () =
  let open Cli_entries in
  let group = { name = "admin" ;
                title = "commands to perform privileged operations on the node" } in
  [
    command ~group ~desc: "unmark invalid"
      no_options
      (prefixes [ "unmark" ; "invalid" ]
       @@ Block_hash.param ~name:"block" ~desc:"block to remove from invalid list"
       @@ stop)
      (fun () block (cctxt : #Client_commands.full_context) ->
         Block_services.unmark_invalid cctxt block >>=? fun () ->
         cctxt#message "Block %a no longer marked invalid" Block_hash.pp block >>= return) ;
  ]
