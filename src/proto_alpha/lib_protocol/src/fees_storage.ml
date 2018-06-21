(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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


let origination_burn c ~payer =
  let origination_burn = Constants_storage.origination_burn c in
  Contract_storage.spend_from_script c payer origination_burn >>=? fun c ->
  return (c, origination_burn)

let record_paid_storage_space c contract =
  Contract_storage.used_storage_space c contract >>=? fun size ->
  Contract_storage.record_paid_storage_space c contract size >>=? fun (to_be_paid, c) ->
  Lwt.return (Raw_context.update_storage_space_to_pay c to_be_paid) >>=? fun c ->
  let cost_per_byte = Constants_storage.cost_per_byte c in
  Lwt.return (Tez_repr.(cost_per_byte *? (Z.to_int64 to_be_paid))) >>=? fun to_burn ->
  return (c, size, to_burn)

let burn_fees_for_storage c ~payer =
  let c, storage_space_to_pay = Raw_context.clear_storage_space_to_pay c in
  let cost_per_byte = Constants_storage.cost_per_byte c in
  Lwt.return (Tez_repr.(cost_per_byte *? (Z.to_int64 storage_space_to_pay))) >>=? fun to_burn ->
  (* Burning the fees... *)
  trace Cannot_pay_storage_fee
    (Contract_storage.spend_from_script c payer to_burn) >>=? fun c ->
  return c

let with_fees_for_storage c ~payer f =
  Lwt.return (Raw_context.init_storage_space_to_pay c) >>=? fun c ->
  f c >>=? fun (c, ret) ->
  burn_fees_for_storage c ~payer >>=? fun c ->
  return (c, ret)
