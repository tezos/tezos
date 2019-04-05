(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let (//) = Filename.concat

(** Main *)

module Term = struct

  type upgrade = Storage

  let subcommand_arg =
    let parser = function
      | "storage" -> `Ok Storage
      | s -> `Error ("invalid argument: "^s)
    and printer ppf = function
      | Storage -> Format.fprintf ppf "storage"
    in
    let open Cmdliner.Arg in
    let doc =
      "Upgrade to perform. \
       Possible values: $(b, storage)." in
    required & pos 0 (some (parser, printer)) None & info [] ~docv:"UPGRADE" ~doc

  let process subcommand data_dir config_file =
    match subcommand with
    | Storage ->
        let run =
          begin
            match data_dir with
            | Some data_dir -> return data_dir
            | None ->
                match config_file with
                | None ->
                    let default_config =
                      Node_config_file.default_data_dir // "config.json" in
                    if Sys.file_exists default_config then
                      Node_config_file.read default_config >>=? fun cfg ->
                      return cfg.data_dir
                    else
                      return Node_config_file.default_data_dir
                | Some config_file ->
                    Node_config_file.read config_file >>=? fun cfg ->
                    return cfg.data_dir
          end >>=? fun data_dir ->
          begin
            trace (failure "Fail to lock the data directory. Is a `tezos-node` running?") @@
            Lwt_lock_file.create
              ~unlink_on_exit:true (Node_data_version.lock_file data_dir)
          end >>=? fun () ->
          Node_data_version.upgrade_data_dir data_dir in
        match Lwt_main.run run with
        | Ok () -> `Ok ()
        | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let term =
    Cmdliner.Term.(ret (const process $ subcommand_arg
                        $ Node_shared_arg.Term.data_dir
                        $ Node_shared_arg.Term.config_file))

end

module Manpage = struct

  let command_description =
    "The $(b,upgrade) command is meant to manage upgrades of the node."

  let description = [
    `S "DESCRIPTION" ;
    `P command_description ;
    `P "Available ugrades are:";
    `P "$(b,storage) will upgrade the node disk storage (if needed)."
  ]

  let man =
    description @
    (* [ `S misc_docs ] @ *)
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc: "Upgrade of the node"
      ~man
      "upgrade"

end

let cmd =
  Term.term, Manpage.info
