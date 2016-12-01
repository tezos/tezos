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

let () =
  let startup =
    CalendarLib.Printer.Precise_Calendar.sprint
      "%Y-%m-%dT%H:%M:%SZ"
      (CalendarLib.Calendar.Precise.now ()) in
  let log channel msg = match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return ()
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return ()
    | log ->
        Lwt_utils.create_dir Client_config.(base_dir#get // "logs" // log) >>= fun () ->
        Lwt_io.with_file
          ~flags: Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ]
          ~mode: Lwt_io.Output
          Client_config.(base_dir#get // "logs" // log // startup)
          (fun chan -> Lwt_io.write chan msg) in
  Cli_entries.log_hook := Some log

(* Main (lwt) entry *)
let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  catch
    (fun () ->
       Client_config.preparse_args Sys.argv >>= fun block ->
       Lwt.catch
         (fun () ->
            Client_node_rpcs.Blocks.protocol block)
         (fun _ ->
            Cli_entries.message "\n\
                                 The connection to the RPC server failed, \
                                 using the default protocol version.\n" >>= fun () ->
            Lwt.return Client_bootstrap.Client_proto_main.protocol)
       >>= fun version ->
       let commands =
         Client_generic_rpcs.commands @
         Client_keys.commands () @
         Client_protocols.commands () @
         Client_helpers.commands () @
         Client_version.commands_for_version version in
       Client_config.parse_args ~version
         (Cli_entries.usage commands)
         (Cli_entries.inline_dispatch commands)
         Sys.argv >>= fun command ->
       command () >>= fun () ->
       Lwt.return 0)
    (function
      | Arg.Help help ->
          Format.printf "%s%!" help ;
          Lwt.return 0
      | Arg.Bad help ->
          Format.eprintf "%s%!" help ;
          Lwt.return 1
      | Cli_entries.Command_not_found ->
          Format.eprintf "Unkonwn command, try `-help`.\n%!" ;
          Lwt.return 1
      | Client_version.Version_not_found ->
          Format.eprintf "Unkonwn protocol version, try `list versions`.\n%!" ;
          Lwt.return 1
      | Cli_entries.Bad_argument (idx, _n, v) ->
          Format.eprintf "There's a problem with argument %d, %s.\n%!" idx v ;
          Lwt.return 1
      | Cli_entries.Command_failed message ->
          Format.eprintf "Command failed, %s.\n%!" message ;
          Lwt.return 1
      | Failure message ->
          Format.eprintf "%s%!" message ;
          Lwt.return 1
      | exn ->
          Format.printf "Fatal internal error: %s\n%!"
            (Printexc.to_string exn) ;
          Lwt.return 1)

(* Where all the user friendliness starts *)
let () = Pervasives.exit (Lwt_main.run (main ()))
