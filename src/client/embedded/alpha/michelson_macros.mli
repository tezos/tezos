(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'l node = ('l, string) Micheline.node

val expand : 'l node -> 'l node

val expand_caddadr : 'l node -> 'l node option
val expand_set_caddadr : 'l node -> 'l node option
val expand_map_caddadr : 'l node -> 'l node option
val expand_dxiiivp : 'l node -> 'l node option
val expand_paaiair : 'l node -> 'l node option
val expand_duuuuup : 'l node -> 'l node option
val expand_compare : 'l node -> 'l node option
val expand_asserts : 'l node -> 'l node option
val expand_unpaaiair : 'l node -> 'l node option
val expand_if_some : 'l node -> 'l node option
val expand_if_right : 'l node -> 'l node option

val unexpand : 'l node -> 'l node

val unexpand_caddadr : 'l node -> 'l node option
val unexpand_set_caddadr : 'l node -> 'l node option
val unexpand_map_caddadr : 'l node -> 'l node option
val unexpand_dxiiivp : 'l node -> 'l node option
val unexpand_paaiair : 'l node -> 'l node option
val unexpand_duuuuup : 'l node -> 'l node option
val unexpand_compare : 'l node -> 'l node option
val unexpand_asserts : 'l node -> 'l node option
val unexpand_unpaaiair : 'l node -> 'l node option
val unexpand_if_some : 'l node -> 'l node option
val unexpand_if_right : 'l node -> 'l node option
