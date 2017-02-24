(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'b) lwt_format =
  ('a, Format.formatter, unit, 'b Lwt.t) format4

type context =
  { error : 'a 'b. ('a, 'b) lwt_format -> 'a ;
    warning : 'a. ('a, unit) lwt_format -> 'a ;
    message : 'a. ('a, unit) lwt_format -> 'a ;
    answer : 'a. ('a, unit) lwt_format -> 'a ;
    log : 'a. string -> ('a, unit) lwt_format -> 'a }

type command = (context, unit) Cli_entries.command

let make_context log =
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
  { error ; warning ; message ; answer ; log }

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
  Protocol_hash.Table.add versions name (commands @ previous)

let commands_for_version version =
  try Protocol_hash.Table.find versions version
  with Not_found -> raise Version_not_found
