(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val endorsement:
  ?delegate:public_key_hash ->
  ?level:Raw_level.t ->
  Context.t -> ?signing_context:Context.t ->
  int list -> Kind.endorsements Operation.t tzresult Lwt.t

val miss_signed_endorsement:
  ?level:Raw_level.t ->
  Context.t -> int -> Kind.endorsements Operation.t tzresult Lwt.t

val transaction:
  ?fee:Tez.tez ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  ?parameters:Script.lazy_expr ->
  Context.t ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  Operation.packed tzresult Lwt.t

val delegation:
  ?fee:Tez.tez -> Context.t ->
  Contract.t -> public_key_hash option ->
  Operation.packed tzresult Lwt.t

val revelation:
  Context.t -> public_key -> Operation.packed tzresult Lwt.t

val origination:
  ?delegate:public_key_hash ->
  ?script:Script.t ->
  ?spendable:bool ->
  ?delegatable:bool ->
  ?preorigination: Contract.contract option ->
  ?public_key:public_key ->
  ?manager:public_key_hash ->
  ?credit:Tez.tez ->
  ?fee:Tez.tez ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.contract ->
  (Operation.packed * Contract.contract) tzresult Lwt.t

val originated_contract:
  Operation.packed -> Contract.contract

val double_endorsement:
  Context.t ->
  Kind.endorsements Operation.t ->
  Kind.endorsements Operation.t ->
  Operation.packed tzresult Lwt.t

val double_baking:
  Context.t ->
  Block_header.block_header ->
  Block_header.block_header ->
  Operation.packed tzresult Lwt.t

val activation:
  Context.t ->
  Signature.Public_key_hash.t -> Blinded_public_key_hash.activation_code ->
  Operation.packed tzresult Lwt.t
