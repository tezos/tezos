(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_context

type command = full_context Cli_entries.command

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
