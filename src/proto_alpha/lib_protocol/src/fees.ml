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


let origination_burn c ~payer contract =
  let origination_burn = Constants.origination_burn c in
  Contract.spend_from_script c payer origination_burn >>=? fun c ->
  Contract.used_storage_space c contract >>=? fun size ->
  let cost_per_byte = Constants.cost_per_byte c in
  Lwt.return (Tez.(cost_per_byte *? size)) >>=? fun fees ->
  trace Cannot_pay_storage_fee
    (Contract.spend_from_script c payer fees >>=? fun c ->
     Contract.pay_for_storage_space c contract fees) >>=? fun c ->
  return (c, size, fees)

let update_script_storage c ~payer contract =
  Contract.paid_storage_space_fees c contract >>=? fun paid_fees ->
  Contract.used_storage_space c contract >>=? fun size ->
  let cost_per_byte = Constants.cost_per_byte c in
  Lwt.return (Tez.(cost_per_byte *? size)) >>=? fun fees ->
  match Tez.(fees -? paid_fees) with
  | Error _ ->
      (* Previously paid fees are greater than required fees. *)
      return (c, size, Tez.zero)
  | Ok to_be_paid ->
      (* Burning the fees... *)
      trace Cannot_pay_storage_fee
        (Contract.spend_from_script c payer to_be_paid >>=? fun c ->
         Contract.pay_for_storage_space c contract to_be_paid) >>=? fun c ->
      return (c, size, to_be_paid)
