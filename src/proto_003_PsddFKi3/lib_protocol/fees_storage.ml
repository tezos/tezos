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

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:"contract.cannot_pay_storage_fee"
    ~title:"Cannot pay storage fee"
    ~description:"The storage fee is higher than the contract balance"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot pay storage storage fee")
    Data_encoding.empty
    (function Cannot_pay_storage_fee -> Some () | _ -> None)
    (fun () -> Cannot_pay_storage_fee) ;
  register_error_kind
    `Temporary
    ~id:"storage_exhausted.operation"
    ~title: "Storage quota exceeded for the operation"
    ~description:
      "A script or one of its callee wrote more \
       bytes than the operation said it would"
    Data_encoding.empty
    (function Operation_quota_exceeded -> Some () | _ -> None)
    (fun () -> Operation_quota_exceeded) ;
  register_error_kind
    `Permanent
    ~id:"storage_limit_too_high"
    ~title: "Storage limit out of protocol hard bounds"
    ~description:
      "A transaction tried to exceed the hard limit on storage"
    empty
    (function Storage_limit_too_high -> Some () | _ -> None)
    (fun () -> Storage_limit_too_high)

let origination_burn c =
  let origination_size = Constants_storage.origination_size c in
  let cost_per_byte = Constants_storage.cost_per_byte c in
  (* the origination burn, measured in bytes *)
  Lwt.return
    Tez_repr.(cost_per_byte *? (Int64.of_int origination_size)) >>=? fun to_be_paid ->
  return (Raw_context.update_allocated_contracts_count c,
          to_be_paid)

let record_paid_storage_space c contract =
  Contract_storage.used_storage_space c contract >>=? fun size ->
  Contract_storage.set_paid_storage_space_and_return_fees_to_pay c contract size >>=? fun (to_be_paid, c) ->
  let c = Raw_context.update_storage_space_to_pay c to_be_paid in
  let cost_per_byte = Constants_storage.cost_per_byte c in
  Lwt.return (Tez_repr.(cost_per_byte *? (Z.to_int64 to_be_paid))) >>=? fun to_burn ->
  return (c, size, to_be_paid, to_burn)

let burn_storage_fees c ~storage_limit ~payer =
  let origination_size = Constants_storage.origination_size c in
  let c, storage_space_to_pay, allocated_contracts =
    Raw_context.clear_storage_space_to_pay c in
  let storage_space_for_allocated_contracts =
    Z.mul (Z.of_int allocated_contracts) (Z.of_int origination_size) in
  let consumed =
    Z.add storage_space_to_pay storage_space_for_allocated_contracts in
  let remaining = Z.sub storage_limit consumed in
  if Compare.Z.(remaining < Z.zero) then
    fail Operation_quota_exceeded
  else
    let cost_per_byte = Constants_storage.cost_per_byte c in
    Lwt.return (Tez_repr.(cost_per_byte *? (Z.to_int64 consumed))) >>=? fun to_burn ->
    (* Burning the fees... *)
    if Tez_repr.(to_burn = Tez_repr.zero) then
      (* If the payer was was deleted by transfering all its balance, and no space was used,
         burning zero would fail *)
      return c
    else
      trace Cannot_pay_storage_fee
        (Contract_storage.must_exist c payer >>=? fun () ->
         Contract_storage.spend_from_script c payer to_burn) >>=? fun c ->
      return c

let check_storage_limit c ~storage_limit =
  if Compare.Z.(storage_limit > (Raw_context.constants c).hard_storage_limit_per_operation)
  || Compare.Z.(storage_limit < Z.zero)then
    error Storage_limit_too_high
  else
    ok ()

let start_counting_storage_fees c =
  Raw_context.init_storage_space_to_pay c
