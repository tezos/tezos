(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

type t
type cost

val consume : t -> cost -> t

val encoding : t Data_encoding.encoding
val pp : Format.formatter -> t -> unit

val encoding_cost : cost Data_encoding.encoding
val pp_cost : Format.formatter -> cost -> unit

val check : t -> unit tzresult Lwt.t
type error += Quota_exceeded

val of_int : int -> t

module Cost_of : sig
  val cycle : cost
  val loop_cycle : cost
  val list_size : cost
  val nop : cost
  val stack_op : cost
  val bool_binop : 'a -> 'b -> cost
  val bool_unop : 'a -> cost
  val pair : cost
  val pair_access : cost
  val cons : cost
  val variant_no_data : cost
  val branch : cost
  val concat : string -> string -> cost
  val map_mem :
    'a -> ('b, 'c) Script_typed_ir.map -> cost
  val map_to_list :
    ('b, 'c) Script_typed_ir.map -> cost
  val map_get :
    'a -> ('b, 'c) Script_typed_ir.map -> cost
  val map_update :
    'a -> 'b -> ('c, 'd) Script_typed_ir.map -> cost
  val map_size : cost
  val set_to_list : 'a Script_typed_ir.set -> cost
  val set_update : 'a -> 'b -> 'a Script_typed_ir.set -> cost
  val set_mem : 'a -> 'a Script_typed_ir.set -> cost
  val mul : 'a Script_int.num -> 'b Script_int.num -> cost
  val div : 'a Script_int.num -> 'b Script_int.num -> cost
  val add : 'a Script_int.num -> 'b Script_int.num -> cost
  val sub : 'a Script_int.num -> 'b Script_int.num -> cost
  val abs : 'a Script_int.num -> cost
  val neg : 'a Script_int.num -> cost
  val int : 'a -> cost
  val add_timestamp : Script_timestamp.t -> 'a Script_int.num -> cost
  val sub_timestamp : Script_timestamp.t -> 'a Script_int.num -> cost
  val diff_timestamps : Script_timestamp.t -> Script_timestamp.t -> cost
  val empty_set : cost
  val set_size : cost
  val empty_map : cost
  val int64_op : cost
  val z_to_int64 : cost
  val int64_to_z : cost
  val bitwise_binop : 'a Script_int.num -> 'b Script_int.num -> cost
  val logor : 'a Script_int.num -> 'b Script_int.num -> cost
  val logand : 'a Script_int.num -> 'b Script_int.num -> cost
  val logxor : 'a Script_int.num -> 'b Script_int.num -> cost
  val lognot : 'a Script_int.num -> cost
  val shift_left : 'a Script_int.num -> 'b Script_int.num -> cost
  val shift_right : 'a Script_int.num -> 'b Script_int.num -> cost
  val exec : cost
  val push : cost
  val compare_res : cost
  val manager : cost
  val transfer : cost
  val create_account : cost
  val create_contract : cost
  val default_account : cost
  val balance : cost
  val now : cost
  val check_signature : cost
  val hash_key : cost
  val hash : 'a -> cost
  val get_steps_to_quota : t -> Script_int.n Script_int.num
  val steps_to_quota : cost
  val source : cost
  val self : cost
  val amount : cost
  val wrap : cost
  val compare_bool : 'a -> 'b -> cost
  val compare_string : string -> string -> cost
  val compare_tez : 'a -> 'b -> cost
  val compare_int : 'a Script_int.num -> 'b Script_int.num -> cost
  val compare_nat : 'a Script_int.num -> 'b Script_int.num -> cost
  val compare_key_hash : 'a -> 'b -> cost
  val compare_timestamp : Script_timestamp.t -> Script_timestamp.t -> cost
end

