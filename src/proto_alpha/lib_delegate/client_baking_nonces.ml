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

let name = "nonce"

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
