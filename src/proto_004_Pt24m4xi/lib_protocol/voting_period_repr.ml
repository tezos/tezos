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

type t = int32
type voting_period = t
include (Compare.Int32 : Compare.S with type t := t)
let encoding = Data_encoding.int32
let pp ppf level = Format.fprintf ppf "%ld" level
let rpc_arg =
  let construct voting_period = Int32.to_string voting_period in
  let destruct str =
    match Int32.of_string str with
    | exception _ -> Error "Cannot parse voting period"
    | voting_period -> Ok voting_period in
  RPC_arg.make
    ~descr:"A voting period"
    ~name: "voting_period"
    ~construct
    ~destruct
    ()

let root = 0l
let succ = Int32.succ

let to_int32 l = l
let of_int32_exn l =
  if Compare.Int32.(l >= 0l)
  then l
  else invalid_arg "Voting_period_repr.of_int32"

type kind =
  | Proposal
  | Testing_vote
  | Testing
  | Promotion_vote

let kind_encoding =
  let open Data_encoding in
  union ~tag_size:`Uint8 [
    case (Tag 0)
      ~title:"Proposal"
      (constant "proposal")
      (function Proposal -> Some () | _ -> None)
      (fun () -> Proposal) ;
    case (Tag 1)
      ~title:"Testing_vote"
      (constant "testing_vote")
      (function Testing_vote -> Some () | _ -> None)
      (fun () -> Testing_vote) ;
    case (Tag 2)
      ~title:"Testing"
      (constant "testing")
      (function Testing -> Some () | _ -> None)
      (fun () -> Testing) ;
    case (Tag 3)
      ~title:"Promotion_vote"
      (constant "promotion_vote")
      (function Promotion_vote -> Some () | _ -> None)
      (fun () -> Promotion_vote) ;
  ]
