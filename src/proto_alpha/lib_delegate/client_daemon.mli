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

module Endorser : sig
  val run:
    #Proto_alpha.full ->
    chain: Chain_services.chain ->
    delay: int ->
    public_key_hash list -> unit tzresult Lwt.t
end

module Baker : sig
  val run:
    #Proto_alpha.full ->
    ?minimal_fees: Tez.t ->
    ?minimal_nanotez_per_gas_unit: Z.t ->
    ?minimal_nanotez_per_byte: Z.t ->
    ?await_endorsements: bool ->
    ?max_priority: int ->
    chain: Chain_services.chain ->
    context_path: string ->
    public_key_hash list -> unit tzresult Lwt.t
end

module Accuser : sig
  val run:
    #Proto_alpha.full ->
    chain: Chain_services.chain ->
    preserved_levels: int ->
    unit tzresult Lwt.t
end
