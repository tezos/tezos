(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type error += Cannot_pay_storage_fee

let () =
  register_error_kind
    `Temporary
    ~id:"contract.cannot_pay_storage_fee"
    ~title:"Cannot pay storage fee"
    ~description:"The storage fee is higher than the contract balance"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot pay storage storage fee")
    Data_encoding.empty
    (function Cannot_pay_storage_fee -> Some () | _ -> None)
    (fun () -> Cannot_pay_storage_fee)


let origination_burn c ~source contract =
  Contract.spend_from_script c source Constants.origination_burn >>=? fun c ->
  Contract.code_and_storage_fee c contract >>=? fun storage_fee ->
  Contract.spend_from_script c source storage_fee
  |> trace Cannot_pay_storage_fee

let update_script_storage c ~source contract storage_fees =
  Contract.code_and_storage_fee c contract >>=? fun paid_fees ->
  Contract.update_storage_fee c contract storage_fees >>=? fun c ->
  Contract.code_and_storage_fee c contract >>=? fun fee ->
  match Tez.(fee -? paid_fees) with
  | Error _ ->
      (* Previously paid fees are greater than required fees. *)
      return c
  | Ok to_be_paid ->
      (* Burning the fees... *)
      Contract.spend_from_script c source to_be_paid
      |> trace Cannot_pay_storage_fee
