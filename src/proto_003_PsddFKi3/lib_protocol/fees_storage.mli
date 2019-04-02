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

type error += Cannot_pay_storage_fee (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)
type error += Storage_limit_too_high (* `Permanent *)

(** Does not burn, only adds the burn to storage space to be paid *)
val origination_burn:
  Raw_context.t -> (Raw_context.t * Tez_repr.t) tzresult Lwt.t

(** The returned Tez quantity is for logging purpose only *)
val record_paid_storage_space:
  Raw_context.t -> Contract_repr.t ->
  (Raw_context.t * Z.t * Z.t * Tez_repr.t) tzresult Lwt.t

val check_storage_limit:
  Raw_context.t -> storage_limit:Z.t -> unit tzresult

val start_counting_storage_fees :
  Raw_context.t -> Raw_context.t

val burn_storage_fees:
  Raw_context.t -> storage_limit:Z.t -> payer:Contract_repr.t -> Raw_context.t tzresult Lwt.t
