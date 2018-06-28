(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

module Cost_of : sig
  val cycle : Gas.cost
  val loop_cycle : Gas.cost
  val list_size : Gas.cost
  val nop : Gas.cost
  val stack_op : Gas.cost
  val bool_binop : 'a -> 'b -> Gas.cost
  val bool_unop : 'a -> Gas.cost
  val pair : Gas.cost
  val pair_access : Gas.cost
  val cons : Gas.cost
  val variant_no_data : Gas.cost
  val branch : Gas.cost
  val concat : string -> string -> Gas.cost
  val map_mem :
    'a -> ('b, 'c) Script_typed_ir.map -> Gas.cost
  val map_to_list :
    ('b, 'c) Script_typed_ir.map -> Gas.cost
  val map_get :
    'a -> ('b, 'c) Script_typed_ir.map -> Gas.cost
  val map_update :
    'a -> 'b -> ('c, 'd) Script_typed_ir.map -> Gas.cost
  val map_size : Gas.cost
  val big_map_mem : 'key -> ('key, 'value) Script_typed_ir.big_map -> Gas.cost
  val big_map_get : 'key -> ('key, 'value) Script_typed_ir.big_map -> Gas.cost
  val big_map_update : 'key -> 'value option -> ('key, 'value) Script_typed_ir.big_map -> Gas.cost
  val set_to_list : 'a Script_typed_ir.set -> Gas.cost
  val set_update : 'a -> bool -> 'a Script_typed_ir.set -> Gas.cost
  val set_mem : 'a -> 'a Script_typed_ir.set -> Gas.cost
  val mul : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val div : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val add : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val sub : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val abs : 'a Script_int.num -> Gas.cost
  val neg : 'a Script_int.num -> Gas.cost
  val int : 'a -> Gas.cost
  val add_timestamp : Script_timestamp.t -> 'a Script_int.num -> Gas.cost
  val sub_timestamp : Script_timestamp.t -> 'a Script_int.num -> Gas.cost
  val diff_timestamps : Script_timestamp.t -> Script_timestamp.t -> Gas.cost
  val empty_set : Gas.cost
  val set_size : Gas.cost
  val empty_map : Gas.cost
  val int64_op : Gas.cost
  val z_to_int64 : Gas.cost
  val int64_to_z : Gas.cost
  val bitwise_binop : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val logor : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val logand : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val logxor : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val lognot : 'a Script_int.num -> Gas.cost
  val shift_left : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val shift_right : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val exec : Gas.cost
  val push : Gas.cost
  val compare_res : Gas.cost
  val pack : MBytes.t -> Gas.cost
  val unpack : MBytes.t -> Gas.cost
  val address : Gas.cost
  val contract : Gas.cost
  val manager : Gas.cost
  val transfer : Gas.cost
  val create_account : Gas.cost
  val create_contract : Gas.cost
  val implicit_account : Gas.cost
  val set_delegate : Gas.cost
  val balance : Gas.cost
  val now : Gas.cost
  val check_signature : Gas.cost
  val hash_key : Gas.cost
  val hash : MBytes.t -> int -> Gas.cost
  val steps_to_quota : Gas.cost
  val source : Gas.cost
  val self : Gas.cost
  val amount : Gas.cost
  val wrap : Gas.cost
  val compare_bool : 'a -> 'b -> Gas.cost
  val compare_string : string -> string -> Gas.cost
  val compare_bytes : MBytes.t -> MBytes.t -> Gas.cost
  val compare_tez : 'a -> 'b -> Gas.cost
  val compare_int : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val compare_nat : 'a Script_int.num -> 'b Script_int.num -> Gas.cost
  val compare_key_hash : 'a -> 'b -> Gas.cost
  val compare_timestamp : Script_timestamp.t -> Script_timestamp.t -> Gas.cost
  val compare_address : Contract.t -> Contract.t -> Gas.cost

  module Typechecking : sig
    val cycle : Gas.cost
    val unit : Gas.cost
    val bool : Gas.cost
    val tez : Gas.cost
    val string : int -> Gas.cost
    val int_of_string : string -> Gas.cost
    val string_timestamp : Gas.cost
    val key : Gas.cost
    val key_hash : Gas.cost
    val signature : Gas.cost

    val contract : Gas.cost

    (** Gas.Cost of getting the code for a contract *)
    val get_script : Gas.cost

    val contract_exists : Gas.cost

    (** Additional Gas.cost of parsing a pair over the Gas.cost of parsing each type  *)
    val pair : Gas.cost

    val union : Gas.cost

    val lambda : Gas.cost

    val some : Gas.cost
    val none : Gas.cost

    val list_element : Gas.cost
    val set_element : Gas.cost
    val map_element : Gas.cost

    val primitive_type : Gas.cost
    val one_arg_type : Gas.cost
    val two_arg_type : Gas.cost

    val operation : MBytes.t -> Gas.cost

    (** Cost of parsing a type *)
    val type_ : int -> Gas.cost
  end

  module Unparse : sig
    val prim_cost : int -> Gas.cost
    val seq_cost : int -> Gas.cost
    val cycle : Gas.cost
    val unit : Gas.cost
    val bool : Gas.cost
    val z : Z.t -> Gas.cost
    val int : 'a Script_int.num -> Gas.cost
    val tez : Gas.cost
    val string : string -> Gas.cost
    val bytes : MBytes.t -> Gas.cost
    val timestamp : Script_timestamp.t -> Gas.cost
    val key : Gas.cost
    val key_hash : Gas.cost
    val signature : Gas.cost
    val operation : MBytes.t -> Gas.cost

    val contract : Gas.cost

    (** Additional Gas.cost of parsing a pair over the Gas.cost of parsing each type  *)
    val pair : Gas.cost

    val union : Gas.cost

    val some : Gas.cost
    val none : Gas.cost

    val list_element : Gas.cost
    val set_element : Gas.cost
    val map_element : Gas.cost

    val one_arg_type : Gas.cost
    val two_arg_type : Gas.cost
    val set_to_list : 'a Script_typed_ir.set -> Gas.cost
    val map_to_list : ('a, 'b) Script_typed_ir.map -> Gas.cost
  end
end
