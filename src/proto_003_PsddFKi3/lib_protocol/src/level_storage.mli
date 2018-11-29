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

val current: Raw_context.t -> Level_repr.t
val previous: Raw_context.t -> Level_repr.t

val root: Raw_context.t -> Level_repr.t

val from_raw: Raw_context.t -> ?offset:int32 -> Raw_level_repr.t -> Level_repr.t
val pred: Raw_context.t -> Level_repr.t -> Level_repr.t option
val succ: Raw_context.t -> Level_repr.t -> Level_repr.t

val first_level_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t
val last_level_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t
val levels_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t list
val levels_in_current_cycle:
  Raw_context.t -> ?offset:int32 -> unit -> Level_repr.t list

val levels_with_commitments_in_cycle:
  Raw_context.t -> Cycle_repr.t -> Level_repr.t list

val last_allowed_fork_level: Raw_context.t -> Raw_level_repr.t
