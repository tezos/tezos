(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Script_int


(* ---- Auxiliary types -----------------------------------------------------*)

type 'ty comparable_ty =
  | Int_key : (z num) comparable_ty
  | Nat_key : (n num) comparable_ty
  | String_key : string comparable_ty
  | Tez_key : Tez.t comparable_ty
  | Bool_key : bool comparable_ty
  | Key_hash_key : public_key_hash comparable_ty
  | Timestamp_key : Script_timestamp.t comparable_ty

module type Boxed_set = sig
  type elt
  module OPS : Set.S with type elt = elt
  val boxed : OPS.t
end

type 'elt set = (module Boxed_set with type elt = 'elt)

module type Boxed_map = sig
  type key
  type value
  val key_ty : key comparable_ty
  module OPS : Map.S with type key = key
  val boxed : value OPS.t
end

type ('key, 'value) map = (module Boxed_map with type key = 'key and type value = 'value)

type ('arg, 'ret, 'storage) script =
  { code : (('arg, 'storage) pair, ('ret, 'storage) pair) lambda ;
    arg_type : 'arg ty ;
    ret_type : 'ret ty ;
    storage : 'storage ;
    storage_type : 'storage ty }

and ('a, 'b) pair = 'a * 'b

and ('a, 'b) union = L of 'a | R of 'b

and end_of_stack = unit

and ('arg, 'ret) lambda =
    Lam of ('arg * end_of_stack, 'ret * end_of_stack) descr * Script.expr

and ('arg, 'ret) typed_contract =
  'arg ty * 'ret ty * Contract.t

and annot = string option

and 'ty ty =
  | Unit_t : unit ty
  | Int_t : z num ty
  | Nat_t : n num ty
  | Signature_t : signature ty
  | String_t : string ty
  | Tez_t : Tez.t ty
  | Key_hash_t : public_key_hash ty
  | Key_t : public_key ty
  | Timestamp_t : Script_timestamp.t ty
  | Bool_t : bool ty
  | Pair_t : ('a ty * annot) * ('b ty * annot) -> ('a, 'b) pair ty
  | Union_t : ('a ty * annot) * ('b ty * annot) -> ('a, 'b) union ty
  | Lambda_t : 'arg ty * 'ret ty -> ('arg, 'ret) lambda ty
  | Option_t : 'v ty -> 'v option ty
  | List_t : 'v ty -> 'v list ty
  | Set_t : 'v comparable_ty -> 'v set ty
  | Map_t : 'k comparable_ty * 'v ty -> ('k, 'v) map ty
  | Contract_t : 'arg ty * 'ret ty -> ('arg, 'ret) typed_contract ty

and 'ty stack_ty =
  | Item_t : 'ty ty * 'rest stack_ty * annot -> ('ty * 'rest) stack_ty
  | Empty_t : end_of_stack stack_ty

(* ---- Instructions --------------------------------------------------------*)

(* The low-level, typed instructions, as a GADT whose parameters
   encode the typing rules. The eft parameter is the typed shape of
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
  | List_map :
      (('param, 'ret) lambda * ('param list * 'rest), 'ret list * 'rest) instr
  | List_map_body : ('a * 'rest, 'b * 'rest) descr ->
    ('a list * 'rest, 'b list * 'rest) instr
  | List_reduce :
      (('param * 'res, 'res) lambda *
       ('param list * ('res * 'rest)), 'res * 'rest) instr
  | List_size : ('a list * 'rest, n num * 'rest) instr
  | List_iter :
      ('a * 'rest, 'rest) descr ->
    ('a list * 'rest, 'rest) instr
  (* sets *)
  | Empty_set : 'a comparable_ty ->
    ('rest, 'a set * 'rest) instr
  | Set_map : 'ret comparable_ty ->
    (('param, 'ret) lambda * ('param set * 'rest), 'ret set * 'rest) instr
  | Set_reduce :
      (('param * 'res, 'res) lambda *
       ('param set * ('res * 'rest)), 'res * 'rest) instr
  | Set_iter :
      ('a * 'rest, 'rest) descr ->
    ('a set * 'rest, 'rest) instr
  | Set_mem :
      ('elt * ('elt set * 'rest), bool * 'rest) instr
  | Set_update :
      ('elt * (bool * ('elt set * 'rest)), 'elt set * 'rest) instr
  | Set_size : ('a set * 'rest, n num * 'rest) instr
  (* maps *)
  | Empty_map : 'a comparable_ty * 'v ty ->
    ('rest, ('a, 'v) map * 'rest) instr
  | Map_map :
      (('a * 'v, 'r) lambda * (('a, 'v) map * 'rest), ('a, 'r) map * 'rest) instr
  | Map_reduce :
      ((('a * 'v) * 'res, 'res) lambda *
       (('a, 'v) map * ('res * 'rest)), 'res * 'rest) instr
  | Map_iter :
      (('a * 'v) * 'rest, 'rest) descr ->
    (('a, 'v) map * 'rest, 'rest) instr
  | Map_mem :
      ('a * (('a, 'v) map * 'rest), bool * 'rest) instr
  | Map_get :
      ('a * (('a, 'v) map * 'rest), 'v option * 'rest) instr
  | Map_update :
      ('a * ('v option * (('a, 'v) map * 'rest)), ('a, 'v) map * 'rest) instr
  | Map_size : (('a, 'b) map * 'rest, n num * 'rest) instr
  (* string operations *)
  | Concat :
      (string * (string * 'rest), string * 'rest) instr
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
  | Fail :
      ('bef, 'aft) instr
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
  | Manager :
      (('arg, 'ret) typed_contract * 'rest, public_key_hash * 'rest) instr
  | Transfer_tokens : 'sto ty ->
    ('arg * (Tez.t * (('arg, 'ret) typed_contract * ('sto * end_of_stack))), 'ret * ('sto * end_of_stack)) instr
  | Create_account :
      (public_key_hash * (public_key_hash option * (bool * (Tez.t * 'rest))),
       (unit, unit) typed_contract * 'rest) instr
  | Default_account :
      (public_key_hash * 'rest, (unit, unit) typed_contract * 'rest) instr
  | Create_contract : 'g ty * 'p ty * 'r ty ->
    (public_key_hash * (public_key_hash option * (bool * (bool * (Tez.t *
                                                                  (('p * 'g, 'r * 'g) lambda * ('g * 'rest)))))),
     ('p, 'r) typed_contract * 'rest) instr
  | Now :
      ('rest, Script_timestamp.t * 'rest) instr
  | Balance :
      ('rest, Tez.t * 'rest) instr
  | Check_signature :
      (public_key * ((signature * string) * 'rest), bool * 'rest) instr
  | Hash_key :
      (public_key * 'rest, public_key_hash * 'rest) instr
  | H : 'a ty ->
    ('a * 'rest, string * 'rest) instr
  | Steps_to_quota : (* TODO: check that it always returns a nat *)
      ('rest, n num * 'rest) instr
  | Source : 'p ty * 'r ty ->
    ('rest, ('p, 'r) typed_contract * 'rest) instr
  | Amount :
      ('rest, Tez.t * 'rest) instr

and ('bef, 'aft) descr =
  { loc : Script.location ;
    bef : 'bef stack_ty ;
    aft : 'aft stack_ty ;
    instr : ('bef, 'aft)  instr }
