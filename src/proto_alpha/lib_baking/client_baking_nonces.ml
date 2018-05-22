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


type t = (Block_hash.t * Nonce.t) list

let encoding : t Data_encoding.t =
  let open Data_encoding in
  def "seed_nonce" @@
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let name = "nonces"

let load (wallet : #Client_context.wallet) =
  wallet#load ~default:[] name encoding

let save (wallet : #Client_context.wallet) list =
  wallet#with_lock (fun () ->
      wallet#write name list encoding)

let mem (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock (fun () ->
      load wallet >>|? fun data ->
      List.mem_assoc block_hash data)

let find (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock ( fun () ->
    load wallet >>|? fun data ->
    try Some (List.assoc block_hash data)
    with Not_found -> None)


let add (wallet : #Client_context.wallet) block_hash nonce =
  wallet#with_lock ( fun () ->
  load wallet >>=? fun data ->
  save wallet ((block_hash, nonce) ::
               List.remove_assoc block_hash data))

let del (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock ( fun () ->
    load wallet >>=? fun data ->
    save wallet (List.remove_assoc block_hash data))

let dels (wallet : #Client_context.wallet) hashes =
  wallet#with_lock ( fun () ->
  load wallet >>=? fun data ->
  save wallet @@
  List.fold_left
    (fun data hash -> List.remove_assoc hash data)
    data hashes)
