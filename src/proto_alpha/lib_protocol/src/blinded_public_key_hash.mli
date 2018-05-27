(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include S.HASH

val encoding : t Data_encoding.t
val rpc_arg : t RPC_arg.t

type activation_code
val activation_code_encoding : activation_code Data_encoding.t

val of_ed25519_pkh : activation_code -> Ed25519.Public_key_hash.t -> t

val activation_code_of_hex : string -> activation_code

module Index : sig
  type nonrec t = t
  val path_length : int
  val to_path : t -> string list -> string list
  val of_path : string list -> t option
end
