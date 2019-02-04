(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_int

(* ---- Auxiliary types -----------------------------------------------------*)

type var_annot = [ `Var_annot of string ]
type type_annot = [ `Type_annot of string ]
type field_annot = [ `Field_annot of string ]

type annot = [ var_annot | type_annot | field_annot ]

type 'ty comparable_ty =
  | Int_key : type_annot option -> (z num) comparable_ty
  | Nat_key : type_annot option -> (n num) comparable_ty
  | String_key : type_annot option -> string comparable_ty
  | Bytes_key : type_annot option -> MBytes.t comparable_ty
  | Mutez_key : type_annot option -> Tez.t comparable_ty
  | Bool_key : type_annot option -> bool comparable_ty
  | Key_hash_key : type_annot option -> public_key_hash comparable_ty
  | Timestamp_key : type_annot option -> Script_timestamp.t comparable_ty
  | Address_key : type_annot option -> Contract.t comparable_ty


module type Boxed_set = sig
  type elt
  module OPS : S.SET with type elt = elt
  val boxed : OPS.t
  val size : int
end

type 'elt set = (module Boxed_set with type elt = 'elt)

module type Boxed_map = sig
  type key
  type value
  val key_ty : key comparable_ty
  module OPS : S.MAP with type key = key
  val boxed : value OPS.t * int
end

type ('key, 'value) map = (module Boxed_map with type key = 'key and type value = 'value)

type ('arg, 'storage) script =
  { code : (('arg, 'storage) pair, (packed_internal_operation list, 'storage) pair) lambda ;
    arg_type : 'arg ty ;
    storage : 'storage ;
    storage_type : 'storage ty }

and ('a, 'b) pair = 'a * 'b

and ('a, 'b) union = L of 'a | R of 'b

and end_of_stack = unit

and ('arg, 'ret) lambda =
    Lam of ('arg * end_of_stack, 'ret * end_of_stack) descr * Script.expr

and 'arg typed_contract =
  'arg ty * Contract.t

and 'ty ty =
  | Unit_t : type_annot option -> unit ty
  | Int_t : type_annot option -> z num ty
  | Nat_t : type_annot option -> n num ty
  | Signature_t : type_annot option -> signature ty
  | String_t : type_annot option -> string ty
  | Bytes_t : type_annot option -> MBytes.t ty
  | Mutez_t : type_annot option -> Tez.t ty
  | Key_hash_t : type_annot option -> public_key_hash ty
  | Key_t : type_annot option -> public_key ty
  | Timestamp_t : type_annot option -> Script_timestamp.t ty
  | Address_t : type_annot option -> Contract.t ty
  | Bool_t : type_annot option -> bool ty
  | Pair_t :
      ('a ty * field_annot option * var_annot option) *
      ('b ty * field_annot option * var_annot option) *
      type_annot option -> ('a, 'b) pair ty
  | Union_t : ('a ty * field_annot option) * ('b ty * field_annot option) * type_annot option  -> ('a, 'b) union ty
  | Lambda_t : 'arg ty * 'ret ty * type_annot option  -> ('arg, 'ret) lambda ty
  | Option_t : ('v ty * field_annot option) * field_annot option * type_annot option  -> 'v option ty
  | List_t : 'v ty * type_annot option -> 'v list ty
  | Set_t : 'v comparable_ty * type_annot option -> 'v set ty
  | Map_t : 'k comparable_ty * 'v ty * type_annot option -> ('k, 'v) map ty
  | Big_map_t : 'k comparable_ty * 'v ty * type_annot option -> ('k, 'v) big_map ty
  | Contract_t : 'arg ty * type_annot option -> 'arg typed_contract ty
  | Operation_t : type_annot option -> packed_internal_operation ty

and 'ty stack_ty =
  | Item_t : 'ty ty * 'rest stack_ty * var_annot option -> ('ty * 'rest) stack_ty
  | Empty_t : end_of_stack stack_ty

and ('key, 'value) big_map = { diff : ('key, 'value option) map ;
                               key_type : 'key ty ;
                               value_type : 'value ty }

(* ---- Instructions --------------------------------------------------------*)

(* The low-level, typed instructions, as a GADT whose parameters
   encode the typing rules. The left parameter is the typed shape of
   the stack before the instruction, the right one the shape
   after. Any program whose construction is accepted by OCaml's
   type-checker is guaranteed to be type-safe.  Overloadings of the
   concrete syntax are already resolved in this representation, either
   by using different constructors or type witness parameters. *)
and ('bef, 'aft) instr =
  (* stack ops *)
  | Drop :
      (_ * 'rest, 'rest) instr
  | Dup :
      ('top * 'rest, 'top * ('top * 'rest)) instr
  | Swap :
      ('tip * ('top * 'rest), 'top * ('tip * 'rest)) instr
  | Const : 'ty ->
    ('rest, ('ty * 'rest)) instr
  (* pairs *)
  | Cons_pair :
      (('car * ('cdr * 'rest)), (('car, 'cdr) pair * 'rest)) instr
  | Car :
      (('car, _) pair * 'rest, 'car * 'rest) instr
  | Cdr :
      ((_, 'cdr) pair * 'rest, 'cdr * 'rest) instr
  (* options *)
  | Cons_some :
      ('v * 'rest, 'v option * 'rest) instr
  | Cons_none : 'a ty ->
    ('rest, 'a option * 'rest) instr
  | If_none : ('bef, 'aft) descr * ('a * 'bef, 'aft) descr ->
    ('a option * 'bef, 'aft) instr
  (* unions *)
  | Left :
      ('l * 'rest, (('l, 'r) union * 'rest)) instr
  | Right :
      ('r * 'rest, (('l, 'r) union * 'rest)) instr
  | If_left : ('l * 'bef, 'aft) descr * ('r * 'bef, 'aft) descr ->
    (('l, 'r) union * 'bef, 'aft) instr
  (* lists *)
  | Cons_list :
      ('a * ('a list * 'rest), ('a list * 'rest)) instr
  | Nil :
      ('rest, ('a list * 'rest)) instr
  | If_cons : ('a * ('a list * 'bef), 'aft) descr * ('bef, 'aft) descr ->
    ('a list * 'bef, 'aft) instr
  | List_map : ('a * 'rest, 'b * 'rest) descr ->
    ('a list * 'rest, 'b list * 'rest) instr
  | List_iter : ('a * 'rest, 'rest) descr ->
    ('a list * 'rest, 'rest) instr
  | List_size : ('a list * 'rest, n num * 'rest) instr
  (* sets *)
  | Empty_set : 'a comparable_ty ->
    ('rest, 'a set * 'rest) instr
  | Set_iter : ('a * 'rest, 'rest) descr ->
    ('a set * 'rest, 'rest) instr
  | Set_mem :
      ('elt * ('elt set * 'rest), bool * 'rest) instr
  | Set_update :
      ('elt * (bool * ('elt set * 'rest)), 'elt set * 'rest) instr
  | Set_size : ('a set * 'rest, n num * 'rest) instr
  (* maps *)
  | Empty_map : 'a comparable_ty * 'v ty ->
    ('rest, ('a, 'v) map * 'rest) instr
  | Map_map : (('a * 'v) * 'rest, 'r * 'rest) descr ->
    (('a, 'v) map * 'rest, ('a, 'r) map * 'rest) instr
  | Map_iter : (('a * 'v) * 'rest, 'rest) descr ->
    (('a, 'v) map * 'rest, 'rest) instr
  | Map_mem :
      ('a * (('a, 'v) map * 'rest), bool * 'rest) instr
  | Map_get :
      ('a * (('a, 'v) map * 'rest), 'v option * 'rest) instr
  | Map_update :
      ('a * ('v option * (('a, 'v) map * 'rest)), ('a, 'v) map * 'rest) instr
  | Map_size : (('a, 'b) map * 'rest, n num * 'rest) instr
  (* big maps *)
  | Big_map_mem :
      ('a * (('a, 'v) big_map * 'rest), bool * 'rest) instr
  | Big_map_get :
      ('a * (('a, 'v) big_map * 'rest), 'v option * 'rest) instr
  | Big_map_update :
      ('key * ('value option * (('key, 'value) big_map * 'rest)), ('key, 'value) big_map * 'rest) instr
  (* string operations *)
  | Concat_string :
      (string list * 'rest, string * 'rest) instr
  | Concat_string_pair :
      (string * (string * 'rest), string * 'rest) instr
  | Slice_string :
      (n num * (n num * (string * 'rest)), string option * 'rest) instr
  | String_size :
      (string * 'rest, n num * 'rest) instr
  (* bytes operations *)
  | Concat_bytes :
      (MBytes.t list * 'rest, MBytes.t * 'rest) instr
  | Concat_bytes_pair :
      (MBytes.t * (MBytes.t * 'rest), MBytes.t * 'rest) instr
  | Slice_bytes :
      (n num * (n num * (MBytes.t * 'rest)), MBytes.t option * 'rest) instr
  | Bytes_size :
      (MBytes.t * 'rest, n num * 'rest) instr
  (* timestamp operations *)
  | Add_seconds_to_timestamp :
      (z num * (Script_timestamp.t * 'rest),
       Script_timestamp.t * 'rest) instr
  | Add_timestamp_to_seconds :
      (Script_timestamp.t * (z num * 'rest),
       Script_timestamp.t * 'rest) instr
  | Sub_timestamp_seconds :
      (Script_timestamp.t * (z num * 'rest),
       Script_timestamp.t * 'rest) instr
  | Diff_timestamps :
      (Script_timestamp.t * (Script_timestamp.t * 'rest),
       z num * 'rest) instr
  (* currency operations *)
  (* TODO: we can either just have conversions to/from integers and
     do all operations on integers, or we need more operations on
     Tez. Also Sub_tez should return Tez.t option (if negative) and *)
  | Add_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Sub_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Mul_teznat :
      (Tez.t * (n num * 'rest), Tez.t * 'rest) instr
  | Mul_nattez :
      (n num * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Ediv_teznat :
      (Tez.t * (n num * 'rest), ((Tez.t, Tez.t) pair) option * 'rest) instr
  | Ediv_tez :
      (Tez.t * (Tez.t * 'rest), ((n num, Tez.t) pair) option * 'rest) instr
  (* boolean operations *)
  | Or :
      (bool * (bool * 'rest), bool * 'rest) instr
  | And :
      (bool * (bool * 'rest), bool * 'rest) instr
  | Xor :
      (bool * (bool * 'rest), bool * 'rest) instr
  | Not :
      (bool * 'rest, bool * 'rest) instr
  (* integer operations *)
  | Is_nat :
      (z num * 'rest, n num option * 'rest) instr
  | Neg_nat :
      (n num * 'rest, z num * 'rest) instr
  | Neg_int :
      (z num * 'rest, z num * 'rest) instr
  | Abs_int :
      (z num * 'rest, n num * 'rest) instr
  | Int_nat :
      (n num * 'rest, z num * 'rest) instr
  | Add_intint :
      (z num * (z num * 'rest), z num * 'rest) instr
  | Add_intnat :
      (z num * (n num * 'rest), z num * 'rest) instr
  | Add_natint :
      (n num * (z num * 'rest), z num * 'rest) instr
  | Add_natnat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Sub_int :
      ('s num * ('t num * 'rest), z num * 'rest) instr
  | Mul_intint :
      (z num * (z num * 'rest), z num * 'rest) instr
  | Mul_intnat :
      (z num * (n num * 'rest), z num * 'rest) instr
  | Mul_natint :
      (n num * (z num * 'rest), z num * 'rest) instr
  | Mul_natnat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Ediv_intint :
      (z num * (z num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_intnat :
      (z num * (n num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_natint :
      (n num * (z num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_natnat :
      (n num * (n num * 'rest), ((n num, n num) pair) option * 'rest) instr
  | Lsl_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Lsr_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Or_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | And_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | And_int_nat :
      (z num * (n num * 'rest), n num * 'rest) instr
  | Xor_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Not_nat :
      (n num * 'rest, z num * 'rest) instr
  | Not_int :
      (z num * 'rest, z num * 'rest) instr
  (* control *)
  | Seq : ('bef, 'trans) descr * ('trans, 'aft) descr ->
    ('bef, 'aft) instr
  | If : ('bef, 'aft) descr * ('bef, 'aft) descr ->
    (bool * 'bef, 'aft) instr
  | Loop : ('rest, bool * 'rest) descr ->
    (bool * 'rest, 'rest) instr
  | Loop_left : ('a * 'rest, ('a, 'b) union * 'rest) descr ->
    (('a, 'b) union * 'rest, 'b * 'rest) instr
  | Dip : ('bef, 'aft) descr ->
    ('top * 'bef, 'top * 'aft) instr
  | Exec :
      ('arg * (('arg, 'ret) lambda * 'rest), 'ret * 'rest) instr
  | Lambda : ('arg, 'ret) lambda ->
    ('rest, ('arg, 'ret) lambda * 'rest) instr
  | Failwith :
      'a ty -> ('a * 'rest, 'aft) instr
  | Nop :
      ('rest, 'rest) instr
  (* comparison *)
  | Compare : 'a comparable_ty ->
    ('a * ('a * 'rest), z num * 'rest) instr
  (* comparators *)
  | Eq :
      (z num * 'rest, bool * 'rest) instr
  | Neq :
      (z num * 'rest, bool * 'rest) instr
  | Lt :
      (z num * 'rest, bool * 'rest) instr
  | Gt :
      (z num * 'rest, bool * 'rest) instr
  | Le :
      (z num * 'rest, bool * 'rest) instr
  | Ge :
      (z num * 'rest, bool * 'rest) instr

  (* protocol *)
  | Address :
      (_ typed_contract * 'rest, Contract.t * 'rest) instr
  | Contract : 'p ty ->
    (Contract.t * 'rest, 'p typed_contract option * 'rest) instr
  | Transfer_tokens :
      ('arg * (Tez.t * ('arg typed_contract * 'rest)), packed_internal_operation * 'rest) instr
  | Create_account :
      (public_key_hash * (public_key_hash option * (bool * (Tez.t * 'rest))),
       packed_internal_operation * (Contract.t * 'rest)) instr
  | Implicit_account :
      (public_key_hash * 'rest, unit typed_contract * 'rest) instr
  | Create_contract : 'g ty * 'p ty * ('p * 'g, packed_internal_operation list * 'g) lambda  ->
    (public_key_hash * (public_key_hash option * (bool * (bool * (Tez.t * ('g * 'rest))))),
     packed_internal_operation * (Contract.t * 'rest)) instr
  | Set_delegate :
      (public_key_hash option * 'rest, packed_internal_operation * 'rest) instr
  | Now :
      ('rest, Script_timestamp.t * 'rest) instr
  | Balance :
      ('rest, Tez.t * 'rest) instr
  | Check_signature :
      (public_key * (signature * (MBytes.t * 'rest)), bool * 'rest) instr
  | Hash_key :
      (public_key * 'rest, public_key_hash * 'rest) instr
  | Pack : 'a ty ->
    ('a * 'rest, MBytes.t * 'rest) instr
  | Unpack : 'a ty ->
    (MBytes.t * 'rest, 'a option * 'rest) instr
  | Blake2b :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Sha256 :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Sha512 :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Steps_to_quota : (* TODO: check that it always returns a nat *)
      ('rest, n num * 'rest) instr
  | Source :
      ('rest, Contract.t * 'rest) instr
  | Sender :
      ('rest, Contract.t * 'rest) instr
  | Self : 'p ty ->
    ('rest, 'p typed_contract * 'rest) instr
  | Amount :
      ('rest, Tez.t * 'rest) instr

and ('bef, 'aft) descr =
  { loc : Script.location ;
    bef : 'bef stack_ty ;
    aft : 'aft stack_ty ;
    instr : ('bef, 'aft)  instr }

type ex_big_map = Ex_bm : ('key, 'value) big_map -> ex_big_map
