(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Commands *)

let show (args : Node_shared_arg.t) =
  if not @@ Sys.file_exists args.config_file then
    Format.eprintf
      "\n\
       Warning: no config file at %s,\n\
      \         displaying the default configuration.\n@."
      args.config_file ;
  Node_shared_arg.read_and_patch_config_file args >>=? fun cfg ->
  Node_config_file.check cfg >>= fun () ->
  print_endline @@ Node_config_file.to_string cfg ;
  return ()

let reset (args : Node_shared_arg.t) =
  if Sys.file_exists args.config_file then
    Format.eprintf
      "Ignoring previous configuration file: %s.@."
      args.config_file ;
  Node_shared_arg.read_and_patch_config_file args >>=? fun cfg ->
  Node_config_file.check cfg >>= fun () ->
  Node_config_file.write args.config_file cfg

let init (args : Node_shared_arg.t) =
  if Sys.file_exists args.config_file then
    failwith
      "Pre-existant config file at %s, use `reset`."
      args.config_file
  else
    Node_shared_arg.read_and_patch_config_file args >>=? fun cfg ->
    Node_config_file.check cfg >>= fun () ->
    Node_config_file.write args.config_file cfg

let update (args : Node_shared_arg.t) =
  if not (Sys.file_exists args.config_file) then
    failwith
      "Missing configuration file at %s. \
       Use `%s config init [options]` to generate a new file"
      args.config_file Sys.argv.(0)
  else
    Node_shared_arg.read_and_patch_config_file args >>=? fun cfg ->
    Node_config_file.check cfg >>= fun () ->
    Node_config_file.write args.config_file cfg

(** Main *)

module Term = struct

  type subcommand = Show | Reset | Init | Update

  let process subcommand args  =
    let res =
      match subcommand with
      | Show -> show args
      | Reset -> reset args
      | Init -> init args
      | Update -> update args in
    match Lwt_main.run res with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "show" -> `Ok Show
      | "reset" -> `Ok Reset
      | "init" -> `Ok Init
      | "update" -> `Ok Update
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Show -> Format.fprintf ppf "show"
      | Reset -> Format.fprintf ppf "reset"
      | Init -> Format.fprintf ppf "init"
      | Update -> Format.fprintf ppf "update" in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. \
       Possible values: $(b,show), $(b,reset), $(b,init), $(b,update)." in
    value & pos 0 (parser, printer) Show & info [] ~docv:"OPERATION" ~doc

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg $ Node_shared_arg.Term.args)

end

module Manpage = struct

  let command_description =
    "The $(b,config) command is meant to inspect and amend the \
     configuration of the Tezos node. \
     This command is complementary to manually editing the tezos \
     node configuration file. Its arguments are a subset of the $(i,run) \
     command ones."

  let description = [
    `S "DESCRIPTION" ;
    `P (command_description ^ " Several operations are possible: ");
    `P "$(b,show) reads, parses and displays Tezos current config file. \
        Use this command to see exactly what config file will be used by \
        Tezos. If additional command-line arguments are provided, \
        the displayed configuration will be amended accordingly. \
        This is the default operation." ;
    `P "$(b,reset) will overwrite the current configuration file with a \
        factory default one. \
        If additional command-line arguments are provided, \
        they will amend the generated file. \
        It assumes that a configuration file already exists \
        and will abort otherwise." ;
    `P "$(b,init) is like reset but assumes that \
        no configuration file is present \
        and will abort otherwise." ;
    `P "$(b,update) is the main option to edit the configuration file of Tezos. \
        It will parse command line arguments and add or replace corresponding \
        entries in the Tezos configuration file."
  ]

  let man =
    description @
    Node_shared_arg.Manpage.args @
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Manage node configuration"
      ~man
      "config"

end

let cmd =
  Term.term, Manpage.info
