(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

open Lwt.Infix

let cctxt =
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
  Client_commands.make_context log

(* Main (lwt) entry *)
let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  Lwt.catch begin fun () ->
    Client_config.preparse_args Sys.argv cctxt >>= fun block ->
    Lwt.catch begin fun () ->
      Client_node_rpcs.Blocks.protocol cctxt block >>= fun version ->
      Lwt.return (Some version, Client_commands.commands_for_version version)
    end begin fun exn ->
      cctxt.warning
        "Failed to acquire the protocol version from the node: %s."
        (match exn with
         | Failure msg -> msg
         | exn -> Printexc.to_string exn) >>= fun () ->
      Lwt.return (None, [])
    end >>= fun (version, commands_for_version)  ->
    let commands =
      Client_generic_rpcs.commands @
      Client_network.commands () @
      Client_keys.commands () @
      Client_protocols.commands () @
      Client_helpers.commands () @
      commands_for_version in
    Client_config.parse_args ?version
      (Cli_entries.usage ~commands)
      (Cli_entries.inline_dispatch commands)
      Sys.argv cctxt >>= fun command ->
    command cctxt >>= fun () ->
    Lwt.return 0
  end begin function
    | Arg.Help help ->
        Format.printf "%s%!" help ;
        Lwt.return 0
    | Arg.Bad help ->
        Format.eprintf "%s%!" help ;
        Lwt.return 1
    | Cli_entries.Command_not_found ->
        Format.eprintf "Unknown command, try `-help`.\n%!" ;
        Lwt.return 1
    | Client_commands.Version_not_found ->
        Format.eprintf "Unknown protocol version, try `list versions`.\n%!" ;
        Lwt.return 1
    | Cli_entries.Bad_argument (idx, _n, v) ->
        Format.eprintf "There's a problem with argument %d, %s.\n%!" idx v ;
        Lwt.return 1
    | Cli_entries.Command_failed message ->
        Format.eprintf "Command failed, %s.\n%!" message ;
        Lwt.return 1
    | Failure message ->
        Format.eprintf "Fatal error: %s\n%!" message ;
        Lwt.return 1
    | exn ->
        Format.printf "Fatal internal error: %s\n%!"
          (Printexc.to_string exn) ;
        Lwt.return 1
  end

(* Where all the user friendliness starts *)
let () = Pervasives.exit (Lwt_main.run (main ()))
