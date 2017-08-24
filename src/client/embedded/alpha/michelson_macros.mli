(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Script_located_ir

val expand : node -> node

val expand_caddadr : node -> node option
val expand_set_caddadr : node -> node option
val expand_map_caddadr : node -> node option
val expand_dxiiivp : node -> node option
val expand_paaiair : node -> node option
val expand_duuuuup : node -> node option
val expand_compare : node -> node option
val expand_asserts : node -> node option
val expand_unpaaiair : node -> node option
val expand_if_some : node -> node option
val expand_if_right : node -> node option

open Script

val unexpand : expr -> expr

val unexpand_caddadr : expr -> expr option
val unexpand_set_caddadr : expr -> expr option
val unexpand_map_caddadr : expr -> expr option
val unexpand_dxiiivp : expr -> expr option
val unexpand_paaiair : expr -> expr option
val unexpand_duuuuup : expr -> expr option
val unexpand_compare : expr -> expr option
val unexpand_asserts : expr -> expr option
val unexpand_unpaaiair : expr -> expr option
val unexpand_if_some : expr -> expr option
val unexpand_if_right : expr -> expr option
