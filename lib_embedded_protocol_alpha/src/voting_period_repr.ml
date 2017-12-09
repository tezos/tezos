(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = int32
type voting_period = t
include (Compare.Int32 : Compare.S with type t := t)
let encoding = Data_encoding.int32
let pp ppf level = Format.fprintf ppf "%ld" level
let arg =
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
    case ~tag:0
      (constant "proposal")
      (function Proposal -> Some () | _ -> None)
      (fun () -> Proposal) ;
    case ~tag:1
      (constant "testing_vote")
      (function Testing_vote -> Some () | _ -> None)
      (fun () -> Testing_vote) ;
    case ~tag:2
      (constant "testing")
      (function Testing -> Some () | _ -> None)
      (fun () -> Testing) ;
    case ~tag:3
      (constant "promotion_vote")
      (function Promotion_vote -> Some () | _ -> None)
      (fun () -> Promotion_vote) ;
  ]
