(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

(* TODO locking... *)

type t = (Block_hash.t * Nonce.t) list

let encoding : t Data_encoding.t =
  let open Data_encoding in
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let name = "nonces"

let load (wallet : #Client_commands.wallet) =
  wallet#load ~default:[] name encoding

let save (wallet : #Client_commands.wallet) list =
  wallet#write name list encoding

let mem (wallet : #Client_commands.wallet) block_hash =
  load wallet >>|? fun data ->
  List.mem_assoc block_hash data

let find wallet block_hash =
  load wallet >>|? fun data ->
  try Some (List.assoc block_hash data)
  with Not_found -> None

let add wallet block_hash nonce =
  load wallet >>=? fun data ->
  save wallet ((block_hash, nonce) ::
               List.remove_assoc block_hash data)

let del wallet block_hash =
  load wallet >>=? fun data ->
  save wallet (List.remove_assoc block_hash data)

let dels wallet hashes =
  load wallet >>=? fun data ->
  save wallet @@
  List.fold_left
    (fun data hash -> List.remove_assoc hash data)
    data hashes
