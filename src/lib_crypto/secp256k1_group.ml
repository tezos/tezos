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

module Sp = Libsecp256k1.Internal

module type SCALAR_SIG = sig
  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t
  val zero : t
  val one : t
  val of_Z : Z.t -> t
  val to_Z : t -> Z.t
  val of_int : int -> t
  val add: t -> t -> t
  val mul: t -> t -> t
  val negate: t -> t
  val sub : t -> t -> t
  val of_bits_exn: string -> t
  val to_bits: t -> string
  val inverse: t -> t option
  val pow: t -> Z.t -> t
  val equal : t -> t -> bool
end

module Group : sig
  val order: Z.t
  module Scalar : SCALAR_SIG
  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t
  val e: t
  val g: t
  val h: t
  val of_coordinates: x:Z.t -> y:Z.t -> t
  val of_bits_exn: string -> t
  val to_bits: t -> string
  val mul: Scalar.t -> t -> t
  val (+): t -> t -> t
  val (-): t -> t -> t
  val (=): t -> t -> bool
end = struct

  let order = Z.of_string_base 16 "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"

  let string_rev s =
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i])

  let b32_of_Z z =
    let cs = Cstruct.create 32 in
    let bits = Z.to_bits z in
    let length = (min 32 (String.length bits)) in
    let bits =  String.sub bits 0 length in
    let bits = string_rev bits in
    Cstruct.blit_from_string bits 0 cs (32 - length) length;
    cs

  let z_of_b32 b =
    b |> Cstruct.to_string |> string_rev |> Z.of_bits

  module Scalar : SCALAR_SIG with type t = Sp.Scalar.t = struct
    type t = Sp.Scalar.t

    let zero = Sp.Scalar.zero ()
    let one  = Sp.Scalar.one  ()
    let equal x y = Sp.Scalar.equal x y


    let of_Z z =
      let z = Z.erem z order in
      let r = Sp.Scalar.const () in
      let cs = b32_of_Z z in
      let _ = Sp.Scalar.set_b32 r cs in r

    let to_Z s =
      let cs = Cstruct.create 32 in
      Sp.Scalar.get_b32 cs s; cs |> z_of_b32

    let of_int i = i |> Z.of_int |> of_Z

    let pow t n =
      Z.powm (to_Z t) n order |> of_Z

    let add x y =
      let r = Sp.Scalar.const () in
      let _ = Sp.Scalar.add r x y in r

    let mul x y =
      let r = Sp.Scalar.const () in
      Sp.Scalar.mul r x y; r

    let negate x =
      let r = Sp.Scalar.const () in
      Sp.Scalar.negate r x; r

    let sub x y =
      add x (negate y)

    let of_bits_exn bits =
      let r = Sp.Scalar.const () in
      (* trim to 32 bytes *)
      let cs = Cstruct.create 32 in
      Cstruct.blit_from_string bits 0 cs 0 (min (String.length bits) 32);
      (* ignore overflow condition, it's always 0 based on the c-code *)
      let _ = Sp.Scalar.set_b32 r cs in r

    (* TODO, check that we are less than the order *)

    let to_bits x =
      let c = Cstruct.create 32 in
      Sp.Scalar.get_b32 c x; Cstruct.to_string c

    let inverse x =
      if x = zero then
        None else
        let r = Sp.Scalar.const () in
        Sp.Scalar.inverse r x; Some r

    type Base58.data +=
      | Data of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.secp256k1_scalar
        ~length: 32
        ~to_raw: to_bits
        ~of_raw: (fun s -> try Some (of_bits_exn s) with _ -> None)
        ~wrap: (fun x -> Data x)

    let title = "Secp256k1_group.Scalar"
    let name =  "Anscalar for the secp256k1 group"

    include Helpers.MakeB58(struct
        type nonrec t = t
        let name = name
        let b58check_encoding = b58check_encoding
      end)

    include Helpers.MakeEncoder(struct
        type nonrec t = t
        let name = name
        let title = title
        let raw_encoding = Data_encoding.(conv to_bits of_bits_exn string)
        let to_b58check = to_b58check
        let to_short_b58check = to_short_b58check
        let of_b58check = of_b58check
        let of_b58check_opt = of_b58check_opt
        let of_b58check_exn = of_b58check_exn
      end)
  end

  type t = Sp.Group.Jacobian.t
  (* type ge = Sp.Group.ge *)

  let field_of_Z z =
    let fe = Sp.Field.const () in
    let cs = b32_of_Z z in
    let _ = Sp.Field.set_b32 fe cs in fe

  let group_of_jacobian j =
    let r = Sp.Group.of_fields () in
    Sp.Group.Jacobian.get_ge r j; r

  let jacobian_of_group g =
    let j = Sp.Group.Jacobian.of_fields () in
    Sp.Group.Jacobian.set_ge j g; j


  let of_coordinates ~x ~y =
    Sp.Group.of_fields
      ~x:(field_of_Z x) ~y:(field_of_Z y) () |> jacobian_of_group

  let e =
    Sp.Group.Jacobian.of_fields ~infinity:true ()

  let g =
    let gx = Z.of_string "55066263022277343669578718895168534326250603453777594175500187360389116729240"
    and gy = Z.of_string "32670510020758816978083085130507043184471273380659243275938904335757337482424" in
    of_coordinates ~x:gx ~y:gy

  (* To obtain the second generator, take the sha256 hash of the decimal representation of g1_y
       python -c "import hashlib;print int(hashlib.sha256('32670510020758816978083085130507043184471273380659243275938904335757337482424').hexdigest(),16)"
  *)
  let h =
    let hx = Z.of_string "54850469061264194188802857211425616972714231399857248865148107587305936171824"
    and hy = Z.of_string "6558914719042992724977242403721980463337660510165027616783569279181206179101" in
    of_coordinates ~x:hx ~y:hy

  let (+) x y =
    let r = Sp.Group.Jacobian.of_fields () in
    Sp.Group.Jacobian.add_var r x y; r

  let (-) x y =
    let neg_y = Sp.Group.Jacobian.of_fields () in
    Sp.Group.Jacobian.neg neg_y y; x + neg_y

  let (=) x y = Sp.Group.Jacobian.is_infinity (x - y)

  let mul s g =
    let r = Sp.Group.Jacobian.of_fields () in
    Sp.Group.Jacobian.mul r (group_of_jacobian g) s; r

  let to_bits j =
    let x = group_of_jacobian j
    and buf = Cstruct.create 33 in
    let cs =  (Sp.Group.to_pubkey ~compress:true buf x) in
    Cstruct.to_string cs

  let of_bits_exn bits =
    let buf = Cstruct.of_string bits
    and x = Sp.Group.of_fields () in
    Sp.Group.from_pubkey x buf;
    x |> jacobian_of_group


  module Encoding = struct
    type Base58.data +=
      | Data of t

    let title = "Secp256k1_group.Group"
    let name =  "An element of secp256k1"

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.secp256k1_element
        ~length: 33
        ~to_raw: to_bits
        ~of_raw: (fun s -> try Some (of_bits_exn s) with _ -> None)
        ~wrap: (fun x -> Data x)

    include Helpers.MakeB58(
      struct
        type nonrec t = t
        let name = name
        let b58check_encoding = b58check_encoding
      end)

    include Helpers.MakeEncoder(
      struct
        type nonrec t = t
        let name = name
        let title = title
        let raw_encoding = Data_encoding.(conv to_bits of_bits_exn string)
        let to_b58check = to_b58check
        let to_short_b58check = to_short_b58check
        let of_b58check = of_b58check
        let of_b58check_opt = of_b58check_opt
        let of_b58check_exn = of_b58check_exn
      end
      )
  end

  include Encoding

end
