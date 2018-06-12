(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_stdlib.Logging.SEMLOG

val host_name: string Tag.def
val magic_byte: int Tag.def
val num_bytes: int Tag.def
val port_number: int Tag.def
val unix_socket_path: string Tag.def
