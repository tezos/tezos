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

type error +=
  | Unknown of { oldest : Cycle_repr.t ;
                 cycle : Cycle_repr.t ;
                 latest : Cycle_repr.t } (* `Permanent *)

(** Generates the first [preserved_cycles+2] seeds for which
    there are no nonces. *)
val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val for_cycle:
  Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

(** If it is the end of the cycle, computes and stores the seed of cycle at
    distance [preserved_cycle+2] in the future using the seed of the previous
    cycle and the revelations of the current one.  *)
val cycle_end:
  Raw_context.t -> Cycle_repr.t ->
  (Raw_context.t * Nonce_storage.unrevealed list) tzresult Lwt.t
