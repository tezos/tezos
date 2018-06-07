module Num = struct
  type t = Cstruct.buffer

  external size :
    unit -> int = "sizeof_secp256k1_num" [@@noalloc]
  external copy :
    t -> t -> unit = "ml_secp256k1_num_copy" [@@noalloc]
  external get_bin :
    Cstruct.buffer -> int -> t -> unit = "ml_secp256k1_num_get_bin" [@@noalloc]
  external set_bin :
    t -> Cstruct.buffer -> int -> unit = "ml_secp256k1_num_set_bin" [@@noalloc]
  external mod_inverse :
    t -> t -> t -> unit = "ml_secp256k1_num_mod_inverse" [@@noalloc]
  external jacobi :
    t -> t -> int = "ml_secp256k1_num_jacobi" [@@noalloc]
  external compare :
    t -> t -> int = "ml_secp256k1_num_cmp" [@@noalloc]
  external equal :
    t -> t -> bool = "ml_secp256k1_num_eq" [@@noalloc]
  external add :
    t -> t -> t -> unit = "ml_secp256k1_num_add" [@@noalloc]
  external sub :
    t -> t -> t -> unit = "ml_secp256k1_num_sub" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_num_mul" [@@noalloc]
  external modulo :
    t -> t -> unit = "ml_secp256k1_num_mod" [@@noalloc]
  external shift :
    t -> int -> unit = "ml_secp256k1_num_shift" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_num_is_zero" [@@noalloc]
  external is_one :
    t -> bool = "ml_secp256k1_num_is_one" [@@noalloc]
  external is_neg :
    t -> bool = "ml_secp256k1_num_is_neg" [@@noalloc]
  external negate :
    t -> unit = "ml_secp256k1_num_negate" [@@noalloc]

  let size = size ()

  let get_bin cs =
    Cstruct.(get_bin (to_bigarray cs) (len cs))
  let set_bin r cs =
    Cstruct.(set_bin r (to_bigarray cs) (len cs))

  let of_uint16 i =
    let t = Cstruct.create size in
    let cs = Cstruct.create 2 in
    Cstruct.BE.set_uint16 cs 0 i ;
    set_bin t.buffer cs ;
    t.buffer

  let zero () = of_uint16 0
  let one () = of_uint16 1

  let of_uint32 i =
    let t = Cstruct.create size in
    let cs = Cstruct.create 4 in
    Cstruct.BE.set_uint32 cs 0 i ;
    set_bin t.buffer cs ;
    t.buffer

  let of_uint64 i =
    let t = Cstruct.create size in
    let cs = Cstruct.create 8 in
    Cstruct.BE.set_uint64 cs 0 i ;
    set_bin t.buffer cs ;
    t.buffer
end

module Scalar = struct
  type t = Cstruct.buffer

  let size = 32

  external const :
    t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_scalar_const_bytecode" "ml_secp256k1_scalar_const" [@@noalloc]

  let const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
    let buf = Cstruct.create size in
    const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
    buf.buffer

  let zero () = const ()
  let one () = const ~d0:1L ()
  let copy t =
    let ret = Cstruct.create size in
    Cstruct.(blit (of_bigarray t) 0 ret 0 size) ;
    ret.buffer

  external clear :
    t -> unit = "ml_secp256k1_scalar_clear" [@@noalloc]
  external get_bits :
    t -> int -> int -> int = "ml_secp256k1_scalar_get_bits" [@@noalloc]
  external get_bits_var :
    t -> int -> int -> int = "ml_secp256k1_scalar_get_bits_var" [@@noalloc]
  external set_b32 :
    t -> Cstruct.buffer -> bool = "ml_secp256k1_scalar_set_b32" [@@noalloc]
  external set_int :
    Cstruct.buffer -> int -> unit = "ml_secp256k1_scalar_set_int" [@@noalloc]
  external get_b32 :
    Cstruct.buffer -> t -> unit = "ml_secp256k1_scalar_get_b32" [@@noalloc]
  external add :
    t -> t -> t -> bool = "ml_secp256k1_scalar_add" [@@noalloc]
  external cadd_bit :
    t -> int -> bool -> unit = "ml_secp256k1_scalar_cadd_bit" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_scalar_mul" [@@noalloc]
  external shr_int :
    t -> int -> int = "ml_secp256k1_scalar_shr_int" [@@noalloc]
  external sqr :
    t -> t -> unit = "ml_secp256k1_scalar_sqr" [@@noalloc]
  external inverse :
    t -> t -> unit = "ml_secp256k1_scalar_inverse" [@@noalloc]
  external inverse_var :
    t -> t -> unit = "ml_secp256k1_scalar_inverse_var" [@@noalloc]
  external negate :
    t -> t -> unit = "ml_secp256k1_scalar_negate" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_scalar_is_zero" [@@noalloc]
  external is_one :
    t -> bool = "ml_secp256k1_scalar_is_one" [@@noalloc]
  external is_even :
    t -> bool = "ml_secp256k1_scalar_is_even" [@@noalloc]
  external is_high :
    t -> bool = "ml_secp256k1_scalar_is_high" [@@noalloc]
  external cond_negate :
    t -> bool -> bool = "ml_secp256k1_scalar_cond_negate" [@@noalloc]
  external get_num :
    Num.t -> t -> unit = "ml_secp256k1_scalar_get_num" [@@noalloc]
  external order_get_num :
    Num.t -> unit = "ml_secp256k1_scalar_order_get_num" [@@noalloc]
  external equal :
    t -> t -> bool = "ml_secp256k1_scalar_eq" [@@noalloc]
  external mul_shift_var :
    t -> t -> t -> int -> unit = "ml_secp256k1_mul_shift_var" [@@noalloc]

  let set_b32 t buf = set_b32 t (Cstruct.to_bigarray buf)
  let get_b32 buf t = get_b32 (Cstruct.to_bigarray buf) t
end

module Field = struct
  type t = Cstruct.buffer

  module Storage = struct
    type t = Cstruct.buffer
    let size = 32
    let to_cstruct t = Cstruct.of_bigarray t
    let of_cstruct cs =
      let res = Cstruct.create size in
      try
        Cstruct.blit cs 0 res 0 size ;
        Some res.buffer
      with _ -> None
    let of_cstruct_exn cs =
      match of_cstruct cs with
      | Some t -> t
      | None -> invalid_arg "Field.Storage.of_cstruct_exn"

    external const :
      t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
      "ml_secp256k1_fe_storage_const_bytecode" "ml_secp256k1_fe_storage_const" [@@noalloc]

    let const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
      let buf = Cstruct.create size in
      const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
      buf.buffer

    external cmov :
      t -> t -> bool -> unit = "ml_secp256k1_fe_storage_cmov" [@@noalloc]
  end

  let size = 40

  external const :
    t -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> unit =
    "ml_secp256k1_fe_const_bytecode" "ml_secp256k1_fe_const" [@@noalloc]

  let const ?(d7=0L) ?(d6=0L) ?(d5=0L) ?(d4=0L) ?(d3=0L) ?(d2=0L) ?(d1=0L) ?(d0=0L) () =
    let buf = Cstruct.create size in
    const buf.buffer d7 d6 d5 d4 d3 d2 d1 d0 ;
    buf.buffer

  external normalize :
    t -> unit = "ml_secp256k1_fe_normalize" [@@noalloc]
  external normalize_weak :
    t -> unit = "ml_secp256k1_fe_normalize_weak" [@@noalloc]
  external normalize_var :
    t -> unit = "ml_secp256k1_fe_normalize_var" [@@noalloc]
  external normalizes_to_zero :
    t -> bool = "ml_secp256k1_fe_normalizes_to_zero" [@@noalloc]
  external normalizes_to_zero_var :
    t -> bool = "ml_secp256k1_fe_normalizes_to_zero_var" [@@noalloc]
  external set_int :
    t -> int -> unit = "ml_secp256k1_fe_set_int" [@@noalloc]
  external clear :
    t -> unit = "ml_secp256k1_fe_clear" [@@noalloc]
  external is_zero :
    t -> bool = "ml_secp256k1_fe_is_zero" [@@noalloc]
  external is_odd :
    t -> bool = "ml_secp256k1_fe_is_odd" [@@noalloc]
  external equal :
    t -> t -> bool = "ml_secp256k1_fe_equal" [@@noalloc]
  external equal_var :
    t -> t -> bool = "ml_secp256k1_fe_equal_var" [@@noalloc]
  external cmp_var :
    t -> t -> int = "ml_secp256k1_fe_cmp_var" [@@noalloc]
  external set_b32 :
    t -> Cstruct.buffer -> bool = "ml_secp256k1_fe_set_b32" [@@noalloc]
  external get_b32 :
    Cstruct.buffer -> t -> unit = "ml_secp256k1_fe_get_b32" [@@noalloc]
  external negate :
    t -> t -> int -> unit = "ml_secp256k1_fe_negate" [@@noalloc]
  external mul_int :
    t -> int -> unit = "ml_secp256k1_fe_mul_int" [@@noalloc]
  external add :
    t -> t -> unit = "ml_secp256k1_fe_add" [@@noalloc]
  external mul :
    t -> t -> t -> unit = "ml_secp256k1_fe_mul" [@@noalloc]
  external sqr :
    t -> t -> unit = "ml_secp256k1_fe_sqr" [@@noalloc]
  external sqrt :
    t -> t -> int = "ml_secp256k1_fe_sqrt" [@@noalloc]
  external is_quad_var :
    t -> bool = "ml_secp256k1_fe_is_quad_var" [@@noalloc]
  external inv :
    t -> t -> unit = "ml_secp256k1_fe_inv" [@@noalloc]
  external inv_var :
    t -> t -> unit = "ml_secp256k1_fe_inv_var" [@@noalloc]
  external inv_all_var :
    t -> Cstruct.buffer -> int -> unit = "ml_secp256k1_fe_inv_all_var" [@@noalloc]
  external to_storage :
    Storage.t -> t -> unit = "ml_secp256k1_fe_to_storage" [@@noalloc]
  external from_storage :
    t -> Storage.t -> unit = "ml_secp256k1_fe_from_storage" [@@noalloc]
  external cmov :
    t -> t -> bool -> unit = "ml_secp256k1_fe_cmov" [@@noalloc]

  let inv_all_var r fes =
    let nb_fe = List.length fes in
    let cs = Cstruct.create (nb_fe * size) in
    List.iteri
      (fun i fe -> Cstruct.(blit (of_bigarray fe) 0 cs (i*size) size)) fes ;
    inv_all_var r cs.buffer nb_fe ;
    Cstruct.memset cs 0

  let set_b32 t buf = set_b32 t (Cstruct.to_bigarray buf)
  let get_b32 buf t = get_b32 (Cstruct.to_bigarray buf) t

  let compare = cmp_var
end

module Group = struct
  type t = Cstruct.buffer
  type ge = t

  let size = 2 * Field.size + 8

  module Storage = struct
    type t = Cstruct.buffer
    let size = 2 * Field.Storage.size
    let to_cstruct t = Cstruct.of_bigarray t
    let of_cstruct cs =
      let res = Cstruct.create size in
      try
        Cstruct.blit cs 0 res 0 size ;
        Some res.buffer
      with _ -> None
    let of_cstruct_exn cs =
      match of_cstruct cs with
      | Some t -> t
      | None -> invalid_arg "Group.Storage.of_cstruct_exn"
    external of_fields :
      t -> Field.Storage.t -> Field.Storage.t -> unit =
      "ml_secp256k1_ge_storage_of_fields" [@@noalloc]
    let of_fields ?(x=Field.const ()) ?(y=Field.const ()) () =
      let cs = Cstruct.create size in
      of_fields cs.buffer x y ;
      cs.buffer
    external cmov : t -> t -> bool -> unit =
      "ml_secp256k1_ge_storage_cmov" [@@noalloc]
  end

  module Jacobian = struct
    type t = Cstruct.buffer

    let size = 3 * Field.size + 8

    external of_fields :
      t -> Field.t -> Field.t -> Field.t -> bool -> unit =
      "ml_secp256k1_gej_of_fields" [@@noalloc]

    external set_infinity : t -> unit =
      "ml_secp256k1_gej_set_infinity" [@@noalloc]

    external set_ge : t -> ge -> unit =
      "ml_secp256k1_gej_set_ge" [@@noalloc]

    external get_ge : ge -> t -> unit =
      "ml_secp256k1_ge_set_gej" [@@noalloc]

    external eq_x_var : Field.t -> t -> int =
      "ml_secp256k1_gej_eq_x_var" [@@noalloc]

    external neg : t -> t -> unit =
      "ml_secp256k1_gej_neg" [@@noalloc]

    external is_infinity : t -> bool =
      "ml_secp256k1_gej_is_infinity" [@@noalloc]

    external has_quad_y_var : t -> bool =
      "ml_secp256k1_gej_has_quad_y_var" [@@noalloc]

    external double_nonzero : t -> t -> Field.t option -> unit =
      "ml_secp256k1_gej_double_nonzero" [@@noalloc]

    external double_var : t -> t -> Field.t option -> unit =
      "ml_secp256k1_gej_double_var" [@@noalloc]

    external add_var : t -> t -> t -> Field.t option -> unit =
      "ml_secp256k1_gej_add_var" [@@noalloc]

    external add_ge : t -> t -> ge -> unit =
      "ml_secp256k1_gej_add_ge" [@@noalloc]

    external add_ge_var : t -> t -> ge -> Field.t option -> unit =
      "ml_secp256k1_gej_add_ge_var" [@@noalloc]

    external add_zinv_var : t -> t -> ge -> Field.t -> unit =
      "ml_secp256k1_gej_add_zinv_var" [@@noalloc]

    external mul : t -> ge -> Scalar.t -> unit =
      "ml_secp256k1_ecmult_const" [@@noalloc]

    external clear : t -> unit =
      "ml_secp256k1_gej_clear" [@@noalloc]

    external rescale : t -> Field.t -> unit =
      "ml_secp256k1_gej_rescale" [@@noalloc]

    let of_fields ?(x=Field.const ()) ?(y=Field.const ()) ?(z=Field.const ()) ?(infinity=false) () =
      let cs = Cstruct.create size in
      of_fields cs.buffer x y z infinity ;
      cs.buffer

    let double_nonzero ?rzr r a = double_nonzero r a rzr
    let double_var ?rzr r a = double_var r a rzr
    let add_var ?rzr r a b = add_var r a b rzr
    let add_ge_var ?rzr r a b = add_ge_var r a b rzr
  end

  external of_fields :
    t -> Field.t -> Field.t -> bool -> unit =
    "ml_secp256k1_ge_of_fields" [@@noalloc]

  external set_xy : t -> Field.t -> Field.t -> unit =
    "ml_secp256k1_ge_set_xy" [@@noalloc]

  external set_xquad : t -> Field.t -> unit =
    "ml_secp256k1_ge_set_xquad" [@@noalloc]

  external set_xovar : t -> Field.t -> int -> bool =
    "ml_secp256k1_ge_set_xquad" [@@noalloc]

  external is_infinity : t -> bool =
    "ml_secp256k1_ge_is_infinity" [@@noalloc]

  external is_valid_var : t -> bool =
    "ml_secp256k1_ge_is_valid_var" [@@noalloc]

  external neg : t -> t -> unit =
    "ml_secp256k1_ge_neg" [@@noalloc]

  external clear : t -> unit =
    "ml_secp256k1_ge_clear" [@@noalloc]

  external to_storage : Storage.t -> t -> unit =
    "ml_secp256k1_ge_to_storage" [@@noalloc]

  external from_storage : t -> Storage.t -> unit =
    "ml_secp256k1_ge_from_storage" [@@noalloc]

  let of_fields ?(x=Field.const ()) ?(y=Field.const ()) ?(infinity=false) () =
    let cs = Cstruct.create size in
    of_fields cs.buffer x y infinity ;
    cs.buffer

  let g =
    let x = Field.const
        ~d7:0x79BE667EL ~d6:0xF9DCBBACL ~d5:0x55A06295L ~d4:0xCE870B07L
        ~d3:0x029BFCDBL ~d2:0x2DCE28D9L ~d1:0x59F2815BL ~d0:0x16F81798L () in
    let y = Field.const
        ~d7:0x483ADA77L ~d6:0x26A3C465L ~d5:0x5DA4FBFCL ~d4:0x0E1108A8L
        ~d3:0xFD17B448L ~d2:0xA6855419L ~d1:0x9C47D08FL ~d0:0xFB10D4B8L () in
    of_fields ~x ~y ~infinity:false ()

  external serialize : t -> Cstruct.buffer -> int -> bool -> int =
    "ml_secp256k1_eckey_pubkey_serialize" [@@noalloc]

  external parse : t -> Cstruct.buffer -> int -> bool =
    "ml_secp256k1_eckey_pubkey_parse" [@@noalloc]

  let to_pubkey ?(compress=true) cs e =
    match serialize e cs.Cstruct.buffer cs.len compress with
    | 0 -> failwith "Group.to_pubkey"
    | len -> Cstruct.sub cs 0 len

  let from_pubkey t cs =
    match parse t cs.Cstruct.buffer cs.len with
    | false -> failwith "Group.from_pubkey"
    | true -> ()
end
