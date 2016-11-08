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

type ('arg, 'ret, 'storage) script =
  { code : (((Tez.t, 'arg) pair, 'storage) pair, ('ret, 'storage) pair) lambda ;
    arg_type : 'arg ty ;
    ret_type : 'ret ty ;
    storage : 'storage ;
    storage_type : 'storage ty }

(* ---- Auxiliary types -----------------------------------------------------*)

and ('a, 'b) pair = 'a * 'b

and ('a, 'b) union = L of 'a | R of 'b

and end_of_stack = unit

and ('arg, 'ret) lambda =
    Lam of ('arg * end_of_stack, 'ret * end_of_stack) instr * Script.expr

and ('arg, 'ret) typed_contract =
  'arg ty * 'ret ty * Contract.t

and 'ty comparable_ty =
  | Int_key : ('s, 'l) int_kind -> ('s, 'l) int_val comparable_ty
  | String_key : string comparable_ty
  | Tez_key : Tez.t comparable_ty
  | Bool_key : bool comparable_ty
  | Key_key : public_key_hash comparable_ty
  | Timestamp_key : Timestamp.t comparable_ty

and 'ty ty =
  | Void_t : unit ty
  | Int_t : ('s, 'l) int_kind -> ('s, 'l) int_val ty
  | Signature_t : signature ty
  | String_t : string ty
  | Tez_t : Tez.t ty
  | Key_t : public_key_hash ty
  | Timestamp_t : Timestamp.t ty
  | Bool_t : bool ty
  | Pair_t : 'a ty * 'b ty -> ('a, 'b) pair ty
  | Union_t : 'a ty * 'b ty -> ('a, 'b) union ty
  | Lambda_t : 'arg ty * 'ret ty -> ('arg, 'ret) lambda ty
  | Option_t : 'v ty -> 'v option ty
  | Ref_t : 'v ty -> 'v ref ty
  | List_t : 'v ty -> 'v list ty
  | Set_t : 'v comparable_ty -> 'v set ty
  | Map_t : 'k comparable_ty * 'v ty -> ('k, 'v) map ty
  | Contract_t : 'arg ty * 'ret ty -> ('arg, 'ret) typed_contract ty

and 'a set =
  'a list ref * 'a comparable_ty (* FIXME: ok, this is bad *)

and ('a, 'b) map =
  ('a * 'b) list ref * 'a comparable_ty (* FIXME: we'll have to do better *)

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
  | If_none : ('bef, 'aft) instr * ('a * 'bef, 'aft) instr ->
    ('a option * 'bef, 'aft) instr
  (* unions *)
  | Left :
      ('l * 'rest, (('l, 'r) union * 'rest)) instr
  | Right :
      ('r * 'rest, (('l, 'r) union * 'rest)) instr
  | If_left : ('l * 'bef, 'aft) instr * ('r * 'bef, 'aft) instr ->
    (('l, 'r) union * 'bef, 'aft) instr
  (* lists *)
  | Cons_list :
      ('a * ('a list * 'rest), ('a list * 'rest)) instr
  | Nil :
      ('rest, ('a list * 'rest)) instr
  | If_cons : ('a * ('a list * 'bef), 'aft) instr * ('bef, 'aft) instr ->
    ('a list * 'bef, 'aft) instr
  | List_iter :
      (('param, unit) lambda * ('param list * 'rest), 'rest) instr
  | List_map :
      (('param, 'ret) lambda * ('param list * 'rest), 'ret list * 'rest) instr
  | List_reduce :
      (('param * 'res, 'res) lambda *
       ('param list * ('res * 'rest)), 'res * 'rest) instr
  (* sets *)
  | Empty_set : 'a comparable_ty ->
    ('rest, 'a set * 'rest) instr
  | Set_iter :
      (('param, unit) lambda * ('param set * 'rest), 'rest) instr
  | Set_map : 'ret comparable_ty ->
    (('param, 'ret) lambda * ('param set * 'rest), 'ret set * 'rest) instr
  | Set_reduce :
      (('param * 'res, 'res) lambda *
       ('param set * ('res * 'rest)), 'res * 'rest) instr
  | Set_mem :
      ('elt * ('elt set * 'rest), bool * 'rest) instr
  | Set_update :
      ('elt * (bool * ('elt set * 'rest)), 'rest) instr
  (* maps *)
  | Empty_map : 'a comparable_ty * 'v ty ->
    ('rest, ('a, 'v) map * 'rest) instr
  | Map_iter :
      (('a * 'v, unit) lambda * (('a, 'v) map * 'rest), 'rest) instr
  | Map_map :
      (('a * 'v, 'r) lambda * (('a, 'v) map * 'rest), ('a, 'r) map * 'rest) instr
  | Map_reduce :
      ((('a * 'v) * 'res, 'res) lambda *
       (('a, 'v) map * ('res * 'rest)), 'res * 'rest) instr
  | Map_mem :
      ('a * (('a, 'v) map * 'rest), bool * 'rest) instr
  | Map_get :
      ('a * (('a, 'v) map * 'rest), 'v option * 'rest) instr
  | Map_update :
      ('a * ('v option * (('a, 'v) map * 'rest)), 'rest) instr
  (* reference cells *)
  | Ref :
      ('v * 'rest, 'v ref * 'rest) instr
  | Deref :
      ('v ref * 'rest, 'v * 'rest) instr
  | Set :
      ('v ref * ('v * 'rest), 'rest) instr
  (* string operations *)
  | Concat :
      (string * (string * 'rest), string * 'rest) instr
  (* timestamp operations *)
  | Add_seconds_to_timestamp : (unsigned, 'l) int_kind * Script.location ->
    ((unsigned, 'l) int_val * (Timestamp.t * 'rest), Timestamp.t * 'rest) instr
  | Add_timestamp_to_seconds : (unsigned, 'l) int_kind * Script.location ->
    (Timestamp.t * ((unsigned, 'l) int_val * 'rest), Timestamp.t * 'rest) instr
  (* currency operations *)
  | Add_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Sub_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Mul_tez : (unsigned, 'l) int_kind ->
    (Tez.t * ((unsigned, 'l) int_val * 'rest), Tez.t * 'rest) instr
  | Mul_tez' : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * (Tez.t * 'rest), Tez.t * 'rest) instr
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
  | Checked_neg_int : (signed, 'l) int_kind * Script.location ->
    ((signed, 'l) int_val * 'rest, (signed, 'l) int_val * 'rest) instr
  | Checked_abs_int : (signed, 'l) int_kind * Script.location ->
    ((signed, 'l) int_val * 'rest, (signed, 'l) int_val * 'rest) instr
  | Checked_add_int : ('s, 'l) int_kind * Script.location ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Checked_sub_int : ('s, 'l) int_kind * Script.location ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Checked_mul_int : ('s, 'l) int_kind * Script.location ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Neg_int : (signed, 'l) int_kind ->
    ((signed, 'l) int_val * 'rest, (signed, 'l) int_val * 'rest) instr
  | Abs_int : (signed, 'l) int_kind ->
    ((signed, 'l) int_val * 'rest, (signed, 'l) int_val * 'rest) instr
  | Add_int : ('s, 'l) int_kind ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Sub_int : ('s, 'l) int_kind ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Mul_int : ('s, 'l) int_kind ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Div_int : ('s, 'l) int_kind * Script.location ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Mod_int : ('s, 'l) int_kind * Script.location ->
    (('s, 'l) int_val * (('s, 'l) int_val * 'rest), ('s, 'l) int_val * 'rest) instr
  | Lsl_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * ((unsigned, eight) int_val * 'rest), (unsigned, 'l) int_val * 'rest) instr
  | Lsr_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * ((unsigned, eight) int_val * 'rest), (unsigned, 'l) int_val * 'rest) instr
  | Or_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * ((unsigned, 'l) int_val * 'rest), (unsigned, 'l) int_val * 'rest) instr
  | And_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * ((unsigned, 'l) int_val * 'rest), (unsigned, 'l) int_val * 'rest) instr
  | Xor_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * ((unsigned, 'l) int_val * 'rest), (unsigned, 'l) int_val * 'rest) instr
  | Not_int : (unsigned, 'l) int_kind ->
    ((unsigned, 'l) int_val * 'rest, (unsigned, 'l) int_val * 'rest) instr
  (* control *)
  | Seq : ('bef, 'trans) instr * ('trans, 'aft) instr ->
    ('bef, 'aft) instr
  | If : ('bef, 'aft) instr * ('bef, 'aft) instr ->
    (bool * 'bef, 'aft) instr
  | Loop : ('rest, bool * 'rest) instr ->
    (bool * 'rest, 'rest) instr
  | Dip : ('bef, 'aft) instr ->
    ('top * 'bef, 'top * 'aft) instr
  | Exec :
      ('arg * (('arg, 'ret) lambda * 'rest), 'ret * 'rest) instr
  | Lambda : ('arg, 'ret) lambda ->
    ('rest, ('arg, 'ret) lambda * 'rest) instr
  | Fail : Script.location ->
    ('rest, 'rest) instr
  | Nop :
      ('rest, 'rest) instr
  (* comparison *)
  | Compare : 'a comparable_ty ->
    ('a * ('a * 'rest), (signed, sixtyfour) int_val * 'rest) instr
  (* comparators *)
  | Eq :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  | Neq :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  | Lt :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  | Gt :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  | Le :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  | Ge :
      ((signed, sixtyfour) int_val * 'rest, bool * 'rest) instr
  (* casts *)
  | Int_of_int : ('sf, 'lf) int_kind * ('st, 'lt) int_kind ->
    (('sf, 'lf) int_val * 'rest, ('st, 'lt) int_val * 'rest) instr
  | Checked_int_of_int : ('sf, 'lf) int_kind * ('st, 'lt) int_kind * Script.location ->
    (('sf, 'lf) int_val * 'rest, ('st, 'lt) int_val * 'rest) instr
  (* protocol *)
  | Manager :
      (('arg, 'ret) typed_contract * 'rest, public_key_hash * 'rest) instr
  | Transfer_tokens : 'sto ty * Script.location ->
    ('arg * (Tez.t * (('arg, 'ret) typed_contract * ('sto * end_of_stack))), 'ret * ('sto * end_of_stack)) instr
  | Create_account :
      (public_key_hash * (public_key_hash option * (bool * (Tez.t * 'rest))),
       (unit, unit) typed_contract * 'rest) instr
  | Create_contract : 'g ty * 'p ty * 'r ty ->
    (public_key_hash * (public_key_hash option * (bool * (Tez.t *
                                                          (((Tez.t * 'p) * 'g, 'r * 'g) lambda * ('g * 'rest))))),
     ('p, 'r) typed_contract * 'rest) instr
  | Now :
      ('rest, Timestamp.t * 'rest) instr
  | Balance :
      ('rest, Tez.t * 'rest) instr
  | Check_signature :
      (public_key_hash * ((signature * string) * 'rest), bool * 'rest) instr
  | H : 'a ty ->
    ('a * 'rest, string * 'rest) instr
  | Steps_to_quota :
      ('rest, (unsigned, thirtytwo) int_val * 'rest) instr
  | Source : 'p ty * 'r ty ->
    ('rest, ('p, 'r) typed_contract * 'rest) instr
  | Amount :
      ('rest, Tez.t * 'rest) instr
