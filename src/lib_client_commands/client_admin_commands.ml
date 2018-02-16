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
                title = "Commands to perform privileged operations on the node" } in
  [
    command ~group
      ~desc: "Make the node forget its decision of rejecting a block."
      no_options
      (prefixes [ "unmark" ; "invalid" ]
       @@ seq_of_param (Block_hash.param ~name:"block" ~desc:"block to remove from invalid list"))
      (fun () blocks (cctxt : #Client_context.full) ->
         iter_s
           (fun block ->
              Block_services.unmark_invalid cctxt block >>=? fun () ->
              cctxt#message
                "Block %a no longer marked invalid."
                Block_hash.pp block >>= fun () ->
              return ())
           blocks) ;
  ]
