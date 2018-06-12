(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_stdlib.Logging.Make_semantic(struct let name = "client.signer" end)

let host_name = Tag.def ~doc:"Host name" "host" Format.pp_print_text
let magic_byte = Tag.def ~doc:"Magic byte" "magic_byte" Format.pp_print_int
let num_bytes = Tag.def ~doc:"Number of bytes" "num_bytes" Format.pp_print_int
let port_number = Tag.def ~doc:"Port number" "port" Format.pp_print_int
let unix_socket_path = Tag.def ~doc:"UNIX socket file path" "unix_socket" Format.pp_print_text
