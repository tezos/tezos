(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Unaccounted
  | Limited of { remaining : Int64.t }

type error += Block_quota_exceeded (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"storage_exhausted.operation"
    ~title: "Storage quota exceeded for the operation"
    ~description:
      "A script or one of its callee wrote more \
       bytes than the operation said it would"
    empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Temporary
    ~id:"storage_exhausted.block"
    ~title: "Storage quota exceeded for the block"
    ~description:
      "The sum of storage consumed by all the operations in the block \
       exceeds the hard storage limit per block"
    empty
    (function Block_quota_exceeded -> Some () | _ -> None)
    (fun () -> Block_quota_exceeded)

let consume block_storage operation_storage ~bytes = match operation_storage with
  | Unaccounted -> ok (block_storage, Unaccounted)
  | Limited { remaining } ->
      let remaining =
        Int64.sub remaining bytes in
      let block_remaining =
        Int64.sub block_storage bytes in
      if Compare.Int64.(remaining < 0L)
      then error Operation_quota_exceeded
      else if Compare.Int64.(block_remaining < 0L)
      then error Block_quota_exceeded
      else ok (block_remaining, Limited { remaining })
