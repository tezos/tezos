(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_commands
open Client_config

let unique_switch =
  Cli_entries.switch
    ~parameter:"-unique"
    ~doc:"Fail when there is more than one possible completion."

let commands () = Cli_entries.[
    command
      ~desc: "Lookup for the possible completion of a \
              given prefix of Base58Check-encoded hash. This actually \
              works only for blocks, operations, public key and contract \
              identifiers."
      (args1 unique_switch)
      (prefixes [ "complete" ] @@
       string
         ~name: "prefix"
         ~desc: "the prefix of the Base58Check-encoded hash to be completed" @@
       stop)
      (fun unique prefix cctxt ->
         Client_node_rpcs.complete
           cctxt.rpc_config ~block:cctxt.config.block prefix >>=? fun completions ->
         match completions with
         | [] -> Pervasives.exit 3
         | _ :: _ :: _ when unique -> Pervasives.exit 3
         | completions ->
             List.iter print_endline completions ;
             return ()) ;
    command
      ~desc: "Wait for the node to be bootstrapped."
      no_options
      (prefixes [ "bootstrapped" ] @@
       stop)
      (fun () cctxt ->
         Client_node_rpcs.bootstrapped cctxt.rpc_config >>=? fun stream ->
         Lwt_stream.iter_s (function
             | Ok (hash, time) ->
                 cctxt.message "Current head: %a (%a)"
                   Block_hash.pp_short hash
                   Time.pp_hum time
             | Error err ->
                 cctxt.error "Error: %a"
                   pp_print_error err
           ) stream >>= fun () ->
         cctxt.answer "Bootstrapped." >>= fun () ->
         return ()
)
  ]
