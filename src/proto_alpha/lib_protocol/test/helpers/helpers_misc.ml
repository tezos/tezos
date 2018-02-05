(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_shell

exception Unknown_protocol

let no_ops_hash =
  Operation_list_list_hash.compute
    [Operation_list_hash.empty]


let get_protocol hash : (module Registred_protocol.T) =
  let (module Protocol): (module Registred_protocol.T) =
    Option.unopt_exn
      Unknown_protocol
    @@ Registred_protocol.get hash
  in
  (module Protocol)


let get_shell_header head =
  let shell_header : Operation.shell_header = {
    branch = State.Block.hash head
  } in
  shell_header


let get_block_header pred operations_hash fitness context timestamp =
  let pred_header = State.Block.header pred in
  let pred_hash = State.Block.hash pred in
  let shell_header : Block_header.shell_header = {
    level = Int32.succ pred_header.shell.level ;
    proto_level = 0 ;
    predecessor = pred_hash ;
    timestamp ;
    validation_passes = 1 ;
    operations_hash ;
    context ;
    fitness
  } in
  shell_header


let find_account accounts hpub =
  let hpub_pred (x : Helpers_account.t) =
    Ed25519.Public_key_hash.equal x.hpub hpub in
  List.find hpub_pred accounts


let get_dummy_tezos_context context =
  Proto_alpha.Tezos_context.init
    context
    ~level: Int32.one
    ~timestamp:(Time.now ())
    ~fitness:([])


let read_file path =
  let (//) = Filename.concat in
  let executable_path = Sys.getcwd () in
  let path =
    if Filename.is_relative path
    then executable_path // path
    else path
  in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  (Bytes.to_string s)
