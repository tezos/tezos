(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let (//) = Filename.concat

(** Commands *)

let identity_file data_dir = data_dir // Node_identity_file.default_name

let show { Node_config_file.data_dir } =
  Node_identity_file.read (identity_file data_dir) >>=? fun id ->
  Format.printf "Peer_id: %a.@." P2p_types.Peer_id.pp id.peer_id ;
  return ()

let generate { Node_config_file.data_dir ; net } =
  let identity_file = identity_file data_dir in
  if Sys.file_exists identity_file then
    fail (Node_identity_file.Existent_identity_file identity_file)
  else
    let target = Crypto_box.make_target net.expected_pow in
    Format.eprintf "Generating a new identity... (level: %.2f) " net.expected_pow ;
    let id =
      P2p.Identity.generate_with_animation Format.err_formatter target in
    Node_identity_file.write identity_file id >>=? fun () ->
    Format.eprintf
      "Stored the new identity (%a) into '%s'.@."
      P2p.Peer_id.pp id.peer_id identity_file ;
    return ()

let check { Node_config_file.data_dir ; net = { expected_pow } } =
  Node_identity_file.read
    ~expected_pow (identity_file data_dir) >>=? fun id ->
  Format.printf
    "Peer_id: %a. Proof of work is higher than %.2f.@."
    P2p_types.Peer_id.pp id.peer_id expected_pow ;
  return ()

(** Main *)

module Term = struct

  type subcommand = Show | Generate | Check

  let process subcommand data_dir config_file expected_pow =
    let res =
      begin
        match data_dir, config_file with
        | None, None ->
            let default_config =
              Node_config_file.default_data_dir // "config.json" in
            if Sys.file_exists default_config then
              Node_config_file.read default_config
            else
              return Node_config_file.default_config
        | None, Some config_file ->
            Node_config_file.read config_file
        | Some data_dir, None ->
            Node_config_file.read (data_dir // "config.json") >>=? fun cfg ->
            return { cfg with data_dir }
        | Some data_dir, Some config_file ->
            Node_config_file.read config_file >>=? fun cfg ->
            return { cfg with data_dir }
      end >>=? fun cfg ->
      let cfg = Node_config_file.update ?expected_pow cfg in
      match subcommand with
      | Show -> show cfg
      | Generate -> generate cfg
      | Check -> check cfg in
    match Lwt_main.run res with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "show" -> `Ok Show
      | "generate" -> `Ok Generate
      | "check" -> `Ok Check
      | s -> `Error ("invalid argument: " ^ s)
    and printer fmt = function
      | Show -> Format.fprintf fmt "show"
      | Generate -> Format.fprintf fmt "generate"
      | Check -> Format.fprintf fmt "check" in
    let doc =
      "Operation to perform. \
       Possible values: $(b,show), $(b,generate), $(b,check)." in
    let open Cmdliner.Arg in
    value & pos 0 (parser, printer) Show & info [] ~docv:"OPERATION" ~doc

  let expected_pow =
    let open Cmdliner in
    let doc =
      "Expected amount of proof-of-work for the node identity. \
       The optional parameter should be a float between 0 and 256, where
       0 disables the proof-of-work mechanism." in
    Arg.(value & pos 1 (some float) None & info [] ~docv:"DIFFICULTY" ~doc)

  let term =
    Cmdliner.Term.(ret (const process
                        $ subcommand_arg
                        $ Node_shared_arg.Term.data_dir
                        $ Node_shared_arg.Term.config_file
                        $ expected_pow))
end

module Manpage = struct

  let command_description =
    "The $(b,identity) command is meant to create and manage node \
     identities. An $(i,identity) uniquely identifies a peer on the \
     network and consists of a cryptographic key pair as well as a \
     proof-of-work stamp that certifies \
     that enough CPU time has been dedicated to produce the identity, \
     to avoid sybil attacks. An identity with enough proof-of-work is \
     required to participate in the Tezos network, therefore this command \
     is necessary to launch Tezos the first time."

  let description = [
    `S "DESCRIPTION" ;
    `P (command_description ^ " Several options are possible:");
    `P "$(b,show) reads, parses and displays the current identity of the node. \
        Use this command to see what identity will be used by Tezos. \
        This is the default operation." ;
    `P "$(b,generate [difficulty]) generates an identity whose \
        proof of work stamp difficulty is at least equal to $(i,difficulty). \
        The value provided must be a floating point number between 0 and 256. \
        It roughly reflects the numbers of expected leading zeroes in the hash \
        of the identity data-structure. \
        Therefore, a value of 0 means no proof-of-work, and the difficulty \
        doubles for each increment of 1 in the difficulty value." ;
    `P "$(b,check [difficulty]) checks that an identity is valid and that its \
        proof of work stamp difficulty is at least equal to $(i,difficulty)." ;
  ]

  let man =
    description @
    (* [ `S misc_docs ] @ *)
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc: "Manage node identities"
      ~man
      "identity"

end

let cmd =
  Term.term, Manpage.info

