(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

open Lwt

(* Main (lwt) entry *)
let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  catch
    (fun () ->
       let block = Client_config.preparse_args () in
       Lwt.catch
         (fun () ->
            Client_node_rpcs.Blocks.protocol block)
         (fun _ ->
            Cli_entries.message "\n\
                                 The connection to the RPC server failed, \
                                 using the default protocol version.\n" ;
            Lwt.return Client_bootstrap.Client_proto_main.protocol)
       >>= fun version ->
       let commands =
         Client_generic_rpcs.commands @
         Client_keys.commands () @
         Client_protocols.commands () @
         Client_version.commands_for_version version in
       Client_config.parse_args ~version
         (Cli_entries.usage commands)
         (Cli_entries.inline_dispatcher commands))
    (function
      | Arg.Help help ->
          Format.printf "%s%!" help ;
          Pervasives.exit 0
      | Arg.Bad help ->
          Format.eprintf "%s%!" help ;
          Pervasives.exit 1
      | Cli_entries.Command_not_found ->
          Format.eprintf "Unkonwn command, try `-help`.\n%!" ;
          Pervasives.exit 1
      | Client_version.Version_not_found ->
          Format.eprintf "Unkonwn protocol version, try `list versions`.\n%!" ;
          Pervasives.exit 1
      | Cli_entries.Bad_argument (idx, _n, v) ->
          Format.eprintf "There's a problem with argument %d, %s.\n%!" idx v ;
          Pervasives.exit 1
      | Cli_entries.Command_failed message ->
          Format.eprintf "Command failed, %s.\n%!" message ;
          Pervasives.exit 1
      | exn ->
          Format.printf "Fatal internal error: %s\n%!"
            (Printexc.to_string exn) ;
          Pervasives.exit 1)

(* Where all the user friendliness starts *)
let () = Lwt_main.run (main ())
