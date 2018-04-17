(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val fail : string -> string -> string -> 'a
(** Raises [Failed] with the passed parameters
    (expected value, actual value, and message). *)

val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_msg m] is equivalent to [fail "" "" m]. *)

val equal_string : ?msg:string -> string -> string -> unit
(** Same as [equal], but specialized for [string] values. *)

val equal : ?eq:('a -> 'a -> bool) -> ?prn:('a -> string) -> ?msg:string -> 'a -> 'a -> unit

(** Functions capturing common assertion scenarios and error monads helpers *)

(** Converts a tzresult from the Environment's error monad to a tzresult of the
    top level error monad *)
val wrap_result : 'a proto_tzresult -> 'a tzresult

(** Converts a tzresult Lwt.t from the Environment's error monad to a tzresult Lwt.t
    of the top level error monad *)
val wrap : 'a proto_tzresult -> 'a tzresult Lwt.t

(** Binds a top level error monad function with an Environment's error monad
    tzresult Lwt.t *)
val ( >>=?? ) :
  'a proto_tzresult Lwt.t -> ('a -> 'b tzresult Lwt.t) -> 'b tzresult Lwt.t

(** Binds a top level error monad function with an Environment's error monad
    tzresult *)
val ( >>?? ) : 'a proto_tzresult -> ('a -> 'b tzresult) -> 'b tzresult

(** Partially binds a top level error monad function with an Environment's
    error monad tzresult *)
val ( >>?= ) : 'a proto_tzresult Lwt.t -> ('a tzresult -> 'b Lwt.t) -> 'b Lwt.t

val tmp_map : ('a -> 'b proto_tzresult) -> 'a list -> 'b list proto_tzresult

val ok : ?msg:string -> 'a proto_tzresult -> 'a proto_tzresult Lwt.t

val ok_contract : ?msg:string ->
  (('a * 'b option) * 'c) proto_tzresult ->
  (('a * 'b option) * 'c) proto_tzresult Lwt.t

exception No_error

val no_error : ?msg:string -> ('a, 'b) result -> 'a
val equal_pkh :
  ?msg:string -> Signature.Public_key_hash.t option ->
  Signature.Public_key_hash.t option -> unit
val equal_int64 : ?msg:string -> Int64.t -> Int64.t -> unit
val equal_int : ?msg:string -> int -> int -> unit
val equal_tez : ?msg:string -> Tez.t -> Tez.t -> unit
val equal_balance :
  tc:context -> ?msg:string ->
  Contract.contract * Tez.t ->
  unit proto_tzresult Lwt.t
val equal_cents_balance :
  tc:context -> ?msg:string ->
  Contract.contract * int ->
  unit proto_tzresult Lwt.t
val ecoproto_error :
  (proto_error -> bool) -> error -> bool

val generic_economic_error : msg:string -> 'a tzresult -> unit
val economic_error :
  msg:string -> (proto_error -> bool) -> 'a tzresult -> unit
val ill_typed_data_error : msg:string -> 'a tzresult -> unit
val ill_typed_return_error : msg:string -> 'a tzresult -> unit
val double_endorsement_evidence : msg:string -> 'a tzresult -> unit
val contain_error_alpha :
  ?msg:string -> f:('a -> bool) -> ('b, 'a list) result -> unit
val unknown_contract : msg:string -> 'a proto_tzresult -> unit
val non_existing_contract : msg:string -> 'a proto_tzresult -> unit
val balance_too_low : msg:string -> 'a proto_tzresult -> unit
val non_spendable : msg:string -> 'a tzresult -> unit
val inconsistent_pkh : msg:string -> 'a tzresult -> unit
val non_delegatable : msg:string -> 'a tzresult -> unit
