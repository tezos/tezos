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

open Alpha_context

val ballots :
  'a #RPC_context.simple -> 'a -> Vote.ballots shell_tzresult Lwt.t

val ballot_list :
  'a #RPC_context.simple -> 'a -> (Signature.Public_key_hash.t * Vote.ballot) list shell_tzresult Lwt.t

val current_period_kind :
  'a #RPC_context.simple -> 'a -> Voting_period.kind shell_tzresult Lwt.t

val current_quorum :
  'a #RPC_context.simple -> 'a -> Int32.t shell_tzresult Lwt.t

val listings :
  'a #RPC_context.simple -> 'a -> (Signature.Public_key_hash.t * int32) list shell_tzresult Lwt.t

val proposals :
  'a #RPC_context.simple -> 'a -> Int32.t Protocol_hash.Map.t shell_tzresult Lwt.t

val current_proposal :
  'a #RPC_context.simple -> 'a -> Protocol_hash.t option shell_tzresult Lwt.t

val register : unit -> unit
