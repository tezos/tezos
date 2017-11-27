(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

type cfg = {
  base_dir : string ;
  force : bool ;
  block : Node_rpc_services.Blocks.block ;
}

type context = {
  rpc_config : Client_rpcs.config ;
  config : cfg ;
  error : 'a 'b. ('a, 'b) lwt_format -> 'a ;
  warning : 'a. ('a, unit) lwt_format -> 'a ;
  message : 'a. ('a, unit) lwt_format -> 'a ;
  answer : 'a. ('a, unit) lwt_format -> 'a ;
  log : 'a. string -> ('a, unit) lwt_format -> 'a ;
}

type command = (context, unit) Cli_entries.command

(* Default config *)

let (//) = Filename.concat

let default_cfg_of_base_dir base_dir = {
  base_dir ;
  force = false ;
  block = `Prevalidation ;
}

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

let default_base_dir = home // ".tezos-client"

let default_cfg = default_cfg_of_base_dir default_base_dir

let make_context
    ?(config = default_cfg)
    ?(rpc_config = Client_rpcs.default_config)
    log =
  let error fmt =
    Format.kasprintf
      (fun msg ->
         Lwt.fail (Failure msg))
      fmt in
  let warning fmt =
    Format.kasprintf
      (fun msg -> log "stderr" msg)
      fmt in
  let message fmt =
    Format.kasprintf
      (fun msg -> log "stdout" msg)
      fmt in
  let answer =
    message in
  let log name fmt =
    Format.kasprintf
      (fun msg -> log name msg)
      fmt in
  { config ; rpc_config ; error ; warning ; message ; answer ; log }

let ignore_context =
  make_context (fun _ _ -> Lwt.return ())

exception Version_not_found

let versions = Protocol_hash.Table.create 7

let get_versions () =
  Protocol_hash.Table.fold
    (fun k c acc -> (k, c) :: acc)
    versions
    []

let register name commands =
  let previous =
    try Protocol_hash.Table.find versions name
    with Not_found -> [] in
  Protocol_hash.Table.replace versions name (commands @ previous)

let commands_for_version version =
  try Protocol_hash.Table.find versions version
  with Not_found -> raise Version_not_found
