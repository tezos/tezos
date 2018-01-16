(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module RPCs = Client_rpcs

module Contracts : module type of Client_proto_contracts

module Context : module type of Client_proto_context

module Programs : module type of Client_proto_programs
