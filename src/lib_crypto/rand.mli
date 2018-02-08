(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val generate : int -> Cstruct.buffer
(** [generate len] is [len] random bytes. *)

val generate_into : ?pos:int -> ?len:int -> Cstruct.buffer -> unit
(** [generate_into ?pos ?len buf] writes [len] (default:
    [MBytes.length buf]) bytes in [buf] starting at [pos] (default:
    [0]). *)
