(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception Unknown_protocol

let no_ops_hash =
  Operation_list_list_hash.compute
    (List.map (fun _ -> Operation_list_hash.empty)
       Proto_alpha.Main.validation_passes)

let find_account accounts hpub =
  let hpub_pred (x : Helpers_account.t) =
    Ed25519.Public_key_hash.equal x.hpub hpub in
  List.find hpub_pred accounts

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
