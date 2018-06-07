(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Protocol Signature Instance *)

type block_header_data = {
  command: Data.Command.t ;
  signature: Signature.t ;
}

include Updater.PROTOCOL with type block_header_data := block_header_data
