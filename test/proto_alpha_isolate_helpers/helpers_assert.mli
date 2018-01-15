(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context

include module type of Assert

(** Functions capturing common assertion scenarios and error monads helpers *)

(** Converts a tzresult from the Environment's error monad to a tzresult of the
    top level error monad *)
val wrap_result : 'a Proto_alpha.tzresult -> 'a tzresult

(** Converts a tzresult Lwt.t from the Environment's error monad to a tzresult Lwt.t
    of the top level error monad *)
val wrap : 'a Proto_alpha.tzresult -> 'a tzresult Lwt.t

(** Binds a top level error monad function with an Environment's error monad
    tzresult Lwt.t *)
val ( >>=?? ) :
  'a Proto_alpha.tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

(** Binds a top level error monad function with an Environment's error monad
    tzresult *)
val ( >>?? ) : 'a Proto_alpha.tzresult -> ('a -> 'b tzresult) -> 'b tzresult

(** Partially binds a top level error monad function with an Environment's
    error monad tzresult *)
val ( >>?= ) : 'a Proto_alpha.tzresult Lwt.t -> ('a tzresult -> 'b Lwt.t) -> 'b Lwt.t

val tmp_map : ('a -> 'b Proto_alpha.tzresult) -> 'a list -> 'b list Proto_alpha.tzresult

val ok : ?msg:string -> 'a Proto_alpha.tzresult -> 'a Proto_alpha.tzresult Lwt.t

val ok_contract : ?msg:string ->
  (('a * 'b option) * 'c) Proto_alpha.tzresult ->
  (('a * 'b option) * 'c) Proto_alpha.tzresult Lwt.t

exception No_error

val no_error : ?msg:string -> ('a, 'b) result -> 'a
val equal_pkh :
  ?msg:string -> Ed25519.Public_key_hash.t option ->
  Ed25519.Public_key_hash.t option -> unit
val equal_int64 : ?msg:string -> Int64.t -> Int64.t -> unit
val equal_int : ?msg:string -> int -> int -> unit
val equal_tez : ?msg:string -> Tez.t -> Tez.t -> unit
val equal_balance :
  tc:context -> ?msg:string ->
  Contract.contract * Tez.t ->
  unit Proto_alpha.tzresult Lwt.t
val equal_cents_balance :
  tc:context -> ?msg:string ->
  Contract.contract * int ->
  unit Proto_alpha.tzresult Lwt.t
val ecoproto_error :
  (Proto_alpha.error -> bool) -> Error_monad.error -> bool

val generic_economic_error : msg:string -> 'a tzresult -> unit
val economic_error :
  msg:string -> (Proto_alpha.error -> bool) -> 'a tzresult -> unit
val ill_typed_data_error : msg:string -> 'a tzresult -> unit
val ill_typed_return_error : msg:string -> 'a tzresult -> unit
val double_endorsement : msg:string -> 'a tzresult -> unit
val contain_error_alpha :
  ?msg:string -> f:('a -> bool) -> ('b, 'a list) result -> unit
val unknown_contract : msg:string -> 'a Proto_alpha.tzresult -> unit
val non_existing_contract : msg:string -> 'a Proto_alpha.tzresult -> unit
val balance_too_low : msg:string -> 'a Proto_alpha.tzresult -> unit
val non_spendable : msg:string -> 'a tzresult -> unit
val inconsistent_pkh : msg:string -> 'a tzresult -> unit
val initial_amount_too_low : msg:string -> 'a tzresult -> unit
val non_delegatable : msg:string -> 'a tzresult -> unit
val wrong_delegate : msg:string -> 'a tzresult -> unit

