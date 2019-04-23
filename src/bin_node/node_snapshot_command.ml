(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Node_logging

let (//) = Filename.concat
let context_dir data_dir = data_dir // "context"
let store_dir data_dir = data_dir // "store"

(** Main *)

module Term = struct

  type subcommand = Export

  let dir_cleaner data_dir =
    lwt_log_notice "Cleaning directory %s" data_dir >>= fun () ->
    Lwt_utils_unix.remove_dir @@ store_dir data_dir >>= fun () ->
    Lwt_utils_unix.remove_dir @@ context_dir data_dir

  let process subcommand config_file file block export_rolling =
    let data_dir =
      match config_file with
      | None -> Node_config_file.default_data_dir
      | Some dir -> dir in
    let genesis = Genesis_chain.genesis in
    let res =
      match subcommand with
      | Export ->
          Node_data_version.ensure_data_dir data_dir >>=? fun () ->
          Snapshots.export
            ~export_rolling ~data_dir
            ~genesis:genesis.block file block
    in
    match Lwt_main.run res with
    | Ok () -> `Ok ()
    | Error err ->
        `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "export" -> `Ok Export
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Export -> Format.fprintf ppf "export"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. \
       Possible value: $(b,export)." in
    required & pos 0 (some (parser, printer)) None & info [] ~docv:"OPERATION" ~doc

  let file_arg =
    let open Cmdliner.Arg in
    required & pos 1 (some string) None & info [] ~docv:"FILE"

  let blocks =
    let open Cmdliner.Arg in
    let doc ="Block hash of the block to export." in
    value & opt (some string) None & info ~docv:"<block_hash>" ~doc ["block"]

  let export_rolling =
    let open Cmdliner in
    let doc =
      "Force export command to dump a minimal snapshot based on the rolling mode." in
    Arg.(value & flag &
         info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["rolling"])

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg
         $ Node_shared_arg.Term.data_dir
         $ file_arg
         $ blocks
         $ export_rolling)

end

module Manpage = struct

  let command_description =
    "The $(b,snapshot) command is meant to export snapshots files."

  let description = [
    `S "DESCRIPTION" ;
    `P (command_description ^ " Several operations are possible: ");
    `P "$(b,export) allows to export a snapshot of the current node state into a file." ;
  ]

  let options = [
    `S "OPTIONS" ;
  ]

  let examples =
    [
      `S "EXAMPLES" ;
      `I ("$(b,Export a snapshot using the rolling mode)",
          "$(mname) snapshot export latest.rolling --rolling") ;
    ]

  let man =
    description @
    options @
    examples @
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Manage snapshots"
      ~man
      "snapshot"

end

let cmd =
  Term.term, Manpage.info
