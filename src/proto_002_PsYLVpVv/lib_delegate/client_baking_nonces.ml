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


type t = Nonce.t Block_hash.Map.t

let encoding : t Data_encoding.t =
  let open Data_encoding in
  def "seed_nonce" @@
  conv
    (fun m ->
       Block_hash.Map.fold (fun hash nonce acc -> (hash, nonce) :: acc) m [])
    (fun l ->
       List.fold_left
         (fun map (hash, nonce) -> Block_hash.Map.add hash nonce map)
         Block_hash.Map.empty l) @@
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let name = "nonce"

let load (wallet : #Client_context.wallet) =
  wallet#load ~default:Block_hash.Map.empty name encoding

let save (wallet : #Client_context.wallet) list =
  wallet#with_lock begin fun () ->
    wallet#write name list encoding
  end

let mem (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock begin fun () ->
    load wallet >>|? fun data ->
    Block_hash.Map.mem block_hash data
  end

let find (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock begin fun () ->
    load wallet >>|? fun data ->
    try Some (Block_hash.Map.find block_hash data)
    with Not_found -> None
  end


let add (wallet : #Client_context.wallet) block_hash nonce =
  wallet#with_lock begin fun () ->
    load wallet >>=? fun data ->
    save wallet (Block_hash.Map.add block_hash nonce data)
  end

let del (wallet : #Client_context.wallet) block_hash =
  wallet#with_lock begin fun () ->
    load wallet >>=? fun data ->
    save wallet (Block_hash.Map.remove block_hash data)
  end

let dels (wallet : #Client_context.wallet) hashes =
  wallet#with_lock begin fun () ->
    load wallet >>=? fun data ->
    save wallet @@
    List.fold_left
      (fun data hash -> Block_hash.Map.remove hash data)
      data hashes
  end
