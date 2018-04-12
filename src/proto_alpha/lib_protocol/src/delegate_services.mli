(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val frozen_balances:
  'a #RPC_context.simple -> 'a ->
  Signature.Public_key_hash.t ->
  Delegate.frozen_balances shell_tzresult Lwt.t

module Baker : sig

  val rights:
    'a #RPC_context.simple -> ?max_priority:int -> 'a ->
    (Raw_level.t * (Signature.Public_key_hash.t * Time.t) list) shell_tzresult Lwt.t

  val rights_for_level:
    'a #RPC_context.simple -> ?max_priority:int -> 'a -> Raw_level.t ->
    (Raw_level.t * Signature.Public_key_hash.t list) shell_tzresult Lwt.t

  val rights_for_delegate:
    'a #RPC_context.simple ->
    ?max_priority:int -> ?first_level:Raw_level.t -> ?last_level:Raw_level.t ->
    'a -> Signature.Public_key_hash.t ->
    (Raw_level.t * int * Time.t) list shell_tzresult Lwt.t

end

module Endorser : sig

  val rights:
    'a #RPC_context.simple -> ?max_priority:int -> 'a ->
    (Raw_level.t * Signature.Public_key_hash.t list) shell_tzresult Lwt.t

  val rights_for_level:
    'a #RPC_context.simple -> ?max_priority:int -> 'a -> Raw_level.t ->
    (Raw_level.t * Signature.Public_key_hash.t list) shell_tzresult Lwt.t

  val rights_for_delegate:
    'a #RPC_context.simple ->
    ?max_priority:int -> ?first_level:Raw_level.t -> ?last_level:Raw_level.t ->
    'a -> Signature.Public_key_hash.t ->
    (Raw_level.t * int) list shell_tzresult Lwt.t

end

(* temporary export *)
val endorsement_rights:
  Alpha_context.t ->
  Level.t ->
  int option -> (Raw_level.t * public_key_hash list) tzresult Lwt.t

val baking_rights:
  Alpha_context.t ->
  unit ->
  int option ->
  (Raw_level.t * (public_key_hash * Time.t) list) tzresult Lwt.t
