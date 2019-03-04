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

type _ location =
  { filename : string ;
    chain : Chain_services.chain }

let resolve_location (cctxt : #Client_context.full) ~chain (kind : 'a) : 'a location tzresult Lwt.t =
  let basename = match kind with
    | `Block -> "block"
    | `Endorsement -> "endorsement"
    | `Nonce -> "nonce" in
  let test_filename chain_id =
    Format.kasprintf return "test_%a_%s" Chain_id.pp_short chain_id basename in
  begin match chain with
    | `Main -> return basename
    | `Test ->
        Chain_services.chain_id cctxt ~chain:`Test () >>=? fun chain_id ->
        test_filename chain_id
    | `Hash chain_id ->
        Chain_services.chain_id cctxt ~chain:`Main () >>=? fun main_chain_id ->
        if Chain_id.(chain_id = main_chain_id) then
          return basename
        else
          test_filename chain_id
  end >>=? fun filename -> return { filename ; chain }

let filename { filename ; _ } = filename
let chain { chain ; _ } = chain
