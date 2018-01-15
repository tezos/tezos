(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Constants used for testing *)

val genesis : Tezos_shell.State.Net.genesis
val alpha_hash : Protocol_hash.t

(** Folder in which the temporary files for testing are put *)
val test_folder : string

val store_root : string
val context_root : string
