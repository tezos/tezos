(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_config

let unique = ref false
let unique_arg =
  "-unique",
  Arg.Set unique,
  "Fail when there is more than one possible completion."

let commands () = Cli_entries.[
    command
      ~desc: "Lookup for the possible completion of a \
              given prefix of Base58Check-encoded hash. This actually \
              works only for blocks, operations, public key and contract \
              identifiers."
      ~args: [unique_arg]
      (prefixes [ "complete" ] @@
       string
         ~name: "prefix"
         ~desc: "the prefix of the Base58Check-encoded hash to be completed" @@
       stop)
      (fun prefix cctxt ->
         Client_node_rpcs.complete cctxt ~block:(block ()) prefix >>= fun completions ->
         match completions with
         | [] -> Pervasives.exit 3
         | _ :: _ :: _ when !unique -> Pervasives.exit 3
         | completions ->
             List.iter print_endline completions ;
             Lwt.return_unit)
  ]
