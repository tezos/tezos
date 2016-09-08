(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


(* A global store for version indexed commands. *)

exception Version_not_found

let versions = Protocol_hash_table.create 7

let get_versions () =
  Protocol_hash_table.fold
    (fun k c acc -> (k, c) :: acc)
    versions
    []

let register name commands =
  let previous =
    try Protocol_hash_table.find versions name
    with Not_found -> [] in
  Protocol_hash_table.add versions name (commands @ previous)

let commands_for_version version =
  try Protocol_hash_table.find versions version
  with Not_found -> raise Version_not_found
