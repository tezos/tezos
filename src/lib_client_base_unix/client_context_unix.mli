(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

class unix_wallet :
  base_dir:string ->
  Client_context.wallet
class unix_prompter :
  Client_context.prompter
class unix_logger :
  base_dir:string ->
  Client_context.printer
class unix_full :
  base_dir:string ->
  block:Shell_services.block ->
  confirmations:int option ->
  rpc_config:RPC_client.config ->
  Client_context.full
