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

type t = Nonce.t Block_hash.Map.t Chain_id.Map.t


let per_chain_encoding =
  let open Data_encoding in
  list
    (obj2
       (req "block" Block_hash.encoding)
       (req "nonce" Nonce.encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun map ->
       (Chain_id.Map.fold (fun chain nonces acc ->
            (chain, (Block_hash.Map.bindings nonces)) :: acc)
           map []))
    (function l ->
       List.fold_left (fun acc (chain, nonces) ->
           let nonces =
             List.fold_left (fun acc (hash, nonce) ->
                 Block_hash.Map.add hash nonce acc)
               Block_hash.Map.empty nonces in
           Chain_id.Map.add chain nonces acc
         ) Chain_id.Map.empty l)
    (list
       (obj2
          (req "chain" Chain_id.encoding)
          (req "nonces" per_chain_encoding)))

let transition_encoding =
  let open Data_encoding in
  let legacy_case =
    case
      ~title:"legacy_encoding"
      ~description:"Old version encoding"
      (Tag 0)
      per_chain_encoding
      (function
        | `Legacy _ -> assert false (* never write in the legacy format *)
        | `Current _ -> None)
      (fun l ->
         `Legacy (List.fold_left (fun acc (hash, nonce) ->
             Block_hash.Map.add hash nonce acc)
             Block_hash.Map.empty l))
  in
  let current_case =
    case
      ~title:"current_encoding"
      ~description:"`Current version encoding"
      (Tag 1)
      encoding
      (function
        | `Current map -> Some map
        | `Legacy _ -> assert false (* never write in the legacy format *))
      (fun map -> `Current map)
  in
  def "seed_nonce" @@
  union ~tag_size:`Uint8 [ legacy_case ; current_case ]

let name = "nonce"

let load ~main_chain_id (wallet : #Client_context.wallet) =
  wallet#load name ~default:(`Current Chain_id.Map.empty) transition_encoding >>=? function
  | `Current map -> return map
  | `Legacy map ->
      return (Chain_id.Map.add main_chain_id map Chain_id.Map.empty)

let save (wallet : #Client_context.wallet) t =
  wallet#write name t encoding

let mem t chain hash =
  try
    let nonces = Chain_id.Map.find chain t in
    Block_hash.Map.mem hash nonces
  with
  | Not_found -> false

let find_opt t chain hash =
  try
    let nonces = Chain_id.Map.find chain t in
    Block_hash.Map.find_opt hash nonces
  with
  | Not_found -> None


let add t chain hash nonce =
  Chain_id.Map.update chain
    (function
      | None ->
          Some Block_hash.Map.(add hash nonce empty)
      | Some nonces ->
          Some Block_hash.Map.(add hash nonce nonces))
    t

let remove t chain hash =
  Chain_id.Map.update chain
    (function
      | None -> None
      | Some map ->
          Some (Block_hash.Map.remove hash map))
    t

let find_chain_nonces_opt t chain =
  Chain_id.Map.find_opt chain t
