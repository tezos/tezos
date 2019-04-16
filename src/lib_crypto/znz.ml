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

module type ZN = sig
  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t
  val zero : t
  val one  : t
  val n : Z.t
  val (+)       : t -> t -> t
  val ( * )     : t -> t -> t
  val (-)       : t -> t -> t
  val (=)       : t -> t -> bool
  val of_int    : int -> t
  val of_Z      : Z.t -> t
  val to_Z      : t -> Z.t
  val of_bits_exn : String.t -> t
  val to_bits   : t -> String.t
  val pow       : t -> Z.t -> t
  val inv       : t -> t option
end

module type INT = sig
  val n : Z.t
end

module MakeZn (N : INT) (B : sig val b58_prefix : string end) : ZN = struct

  type t = Z.t
  let n = N.n
  let max_char_length = 2 * (Z.numbits n)
  let zero = Z.zero
  let one = Z.one
  let of_Z r = Z.(erem r n)
  let to_Z a = a
  let of_int u = u |> Z.of_int |> of_Z

  let to_bits h = h |> Zplus.serialize |> (fun s -> String.sub s 0 (String.length s - 1))
  let of_bits_exn bits =
    (* Do not process oversized inputs. *)
    if Compare.Int.((String.length bits) > max_char_length) then
      failwith "input too long"
    else
      (* Make sure the input is in the range [0, N.n-1]. Do not reduce modulo
         N.n for free! *)
      let x = Zplus.deserialize (bits) in
      if Zplus.(x < zero || x >= N.n) then
        failwith "out of range"
      else
        of_Z x

  let pow a x = Z.powm a Z.(erem x (sub n one)) n
  let (+) x y = Z.(erem (add x y)  n)
  let ( * )  x y = Z.(erem (mul x y) n)
  let (-) x y = Z.(erem (sub x y) n)
  let (=) x y = Z.equal x y

  let inv a = Zplus.invert a n

  let title = Format.sprintf "Znz.Make(%s)" (Z.to_string N.n)
  let name =  Format.sprintf "An element of Z/nZ for n = %s" (Z.to_string N.n)

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: B.b58_prefix
      ~length: 32
      ~to_raw: to_bits
      ~of_raw: (fun s -> try Some (of_bits_exn s) with _ -> None)
      ~wrap: (fun x -> Data x)

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
