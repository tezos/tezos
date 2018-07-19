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

module type QTY = sig
  val id : string
end

module type S = sig
  type qty

  type error +=
    | Addition_overflow of qty * qty (* `Temporary *)
    | Subtraction_underflow of qty * qty (* `Temporary *)
    | Multiplication_overflow of qty * int64 (* `Temporary *)
    | Negative_multiplicator of qty * int64 (* `Temporary *)
    | Invalid_divisor of qty * int64 (* `Temporary *)

  val id : string
  val zero : qty
  val one_mutez : qty
  val one_cent : qty
  val fifty_cents : qty
  val one : qty

  val ( -? ) : qty -> qty -> qty tzresult
  val ( +? ) : qty -> qty -> qty tzresult
  val ( *? ) : qty -> int64 -> qty tzresult
  val ( /? ) : qty -> int64 -> qty tzresult

  val to_mutez : qty -> int64

  (** [of_mutez n] (micro tez) is None if n is negative *)
  val of_mutez : int64 -> qty option

  (** [of_mutez_exn n] fails if n is negative.
      It should only be used at toplevel for constants. *)
  val of_mutez_exn : int64 -> qty

  (** It should only be used at toplevel for constants. *)
  val add_exn : qty -> qty -> qty

  (** It should only be used at toplevel for constants. *)
  val mul_exn : qty -> int -> qty

  val encoding : qty Data_encoding.t

  val to_int64 : qty -> int64

  include Compare.S with type t := qty

  val pp: Format.formatter -> qty -> unit

  val of_string: string -> qty option
  val to_string: qty -> string

end

module Make (T: QTY) : S = struct

  type qty = int64 (* invariant: positive *)

  type error +=
    | Addition_overflow of qty * qty (* `Temporary *)
    | Subtraction_underflow of qty * qty (* `Temporary *)
    | Multiplication_overflow of qty * int64 (* `Temporary *)
    | Negative_multiplicator of qty * int64 (* `Temporary *)
    | Invalid_divisor of qty * int64 (* `Temporary *)

  include Compare.Int64
  let zero = 0L
  (* all other constant are defined from the value of one micro tez *)
  let one_mutez = 1L
  let one_cent = Int64.mul one_mutez 10_000L
  let fifty_cents = Int64.mul one_cent 50L
  (* 1 tez = 100 cents = 1_000_000 mutez *)
  let one = Int64.mul one_cent 100L
  let id = T.id

  let of_string s =
    let triplets = function
      | hd :: tl ->
          let len = String.length hd in
          Compare.Int.(
            len <= 3 && len > 0 &&
            List.for_all (fun s -> String.length s = 3) tl
          )
      | [] -> false in
    let integers s = triplets (String.split_on_char ',' s) in
    let decimals s =
      let l = String.split_on_char ',' s in
      if Compare.Int.(List.length l > 2) then
        false
      else
        triplets (List.rev l) in
    let parse left right =
      let remove_commas s = String.concat "" (String.split_on_char ',' s) in
      let pad_to_six s =
        let len = String.length s in
        String.init 6 (fun i -> if Compare.Int.(i < len) then String.get s i else '0') in
      try
        Some (Int64.of_string (remove_commas left ^ pad_to_six (remove_commas right)))
      with _ -> None in
    match String.split_on_char '.' s with
    | [ left ; right ] ->
        if String.contains s ',' then
          if integers left && decimals right then
            parse left right
          else
            None
        else if Compare.Int.(String.length right > 0)
             && Compare.Int.(String.length right <= 6) then
          parse left right
        else None
    | [ left ] ->
        if not (String.contains s ',') || integers left then
          parse left ""
        else None
    | _ -> None

  let pp ppf amount =
    let mult_int = 1_000_000L in
    let rec left ppf amount =
      let d, r = Int64.(div amount 1000L), Int64.(rem amount 1000L) in
      if d > 0L then
        Format.fprintf ppf "%a%03Ld" left d r
      else
        Format.fprintf ppf "%Ld" r in
    let right ppf amount =
      let triplet ppf v =
        if Compare.Int.(v mod 10 > 0) then
          Format.fprintf ppf "%03d" v
        else if Compare.Int.(v mod 100 > 0) then
          Format.fprintf ppf "%02d" (v / 10)
        else
          Format.fprintf ppf "%d" (v / 100) in
      let hi, lo = amount / 1000, amount mod 1000 in
      if Compare.Int.(lo = 0) then
        Format.fprintf ppf "%a" triplet hi
      else
        Format.fprintf ppf "%03d%a" hi triplet lo in
    let ints, decs =
      Int64.(div amount mult_int),
      Int64.(to_int (rem amount mult_int)) in
    Format.fprintf ppf "%a" left ints ;
    if Compare.Int.(decs > 0) then
      Format.fprintf ppf ".%a" right decs

  let to_string t =
    Format.asprintf "%a" pp t

  let (-) t1 t2 =
    if t2 <= t1
    then Some (Int64.sub t1 t2)
    else None

  let ( -? ) t1 t2 =
    match t1 - t2 with
    | None -> error (Subtraction_underflow (t1, t2))
    | Some v -> ok v

  let ( +? ) t1 t2 =
    let t = Int64.add t1 t2 in
    if t < t1
    then error (Addition_overflow (t1, t2))
    else ok t

  let ( *? ) t m =
    let open Compare.Int64 in
    let open Int64 in
    let rec step cur pow acc =
      if cur = 0L then
        ok acc
      else
        pow +? pow >>? fun npow ->
        if logand cur 1L = 1L then
          acc +? pow >>? fun nacc ->
          step (shift_right_logical cur 1) npow nacc
        else
          step (shift_right_logical cur 1) npow acc in
    if m < 0L then
      error (Negative_multiplicator (t, m))
    else
      match step m t 0L with
      | Ok res -> Ok res
      | Error ([ Addition_overflow _ ] as errs) ->
          Error (Multiplication_overflow (t, m) :: errs)
      | Error errs -> Error errs

  let ( /? ) t d =
    if d <= 0L then
      error (Invalid_divisor (t, d))
    else
      ok (Int64.div t d)

  let add_exn t1 t2 =
    let t = Int64.add t1 t2 in
    if t <= 0L
    then invalid_arg "add_exn"
    else t

  let mul_exn t m =
    match t *? Int64.(of_int m) with
    | Ok v -> v
    | Error _ -> invalid_arg "mul_exn"

  let of_mutez t =
    if t < 0L then None
    else Some t

  let of_mutez_exn x =
    match of_mutez x with
    | None -> invalid_arg "Qty.of_mutez"
    | Some v -> v

  let to_int64 t = t
  let to_mutez t = t

  let encoding =
    let open Data_encoding in
    (check_size 10 (conv Z.of_int64 (Json.wrap_error Z.to_int64) n))

  let () =
    let open Data_encoding in
    register_error_kind
      `Temporary
      ~id:(T.id ^ ".addition_overflow")
      ~title:("Overflowing " ^ T.id ^ " addition")
      ~pp: (fun ppf (opa, opb) ->
          Format.fprintf ppf "Overflowing addition of %a %s and %a %s"
            pp opa T.id pp opb T.id)
      ~description:
        ("An addition of two " ^ T.id ^ " amounts overflowed")
      (obj1 (req "amounts" (tup2 encoding encoding)))
      (function Addition_overflow (a, b) -> Some (a, b) | _ -> None)
      (fun (a, b) -> Addition_overflow (a, b)) ;
    register_error_kind
      `Temporary
      ~id:(T.id ^ ".subtraction_underflow")
      ~title:("Underflowing " ^ T.id ^ " subtraction")
      ~pp: (fun ppf (opa, opb) ->
          Format.fprintf ppf "Underflowing subtraction of %a %s and %a %s"
            pp opa T.id pp opb T.id)
      ~description:
        ("An subtraction of two " ^ T.id ^ " amounts underflowed")
      (obj1 (req "amounts" (tup2 encoding encoding)))
      (function Subtraction_underflow (a, b) -> Some (a, b) | _ -> None)
      (fun (a, b) -> Subtraction_underflow (a, b)) ;
    register_error_kind
      `Temporary
      ~id:(T.id ^ ".multiplication_overflow")
      ~title:("Overflowing " ^ T.id ^ " multiplication")
      ~pp: (fun ppf (opa, opb) ->
          Format.fprintf ppf "Overflowing multiplication of %a %s and %Ld"
            pp opa T.id opb)
      ~description:
        ("A multiplication of a " ^ T.id ^ " amount by an integer overflowed")
      (obj2
         (req "amount" encoding)
         (req "multiplicator" int64))
      (function Multiplication_overflow (a, b) -> Some (a, b) | _ -> None)
      (fun (a, b) -> Multiplication_overflow (a, b)) ;
    register_error_kind
      `Temporary
      ~id:(T.id ^ ".negative_multiplicator")
      ~title:("Negative " ^ T.id ^ " multiplicator")
      ~pp: (fun ppf (opa, opb) ->
          Format.fprintf ppf "Multiplication of %a %s by negative integer %Ld"
            pp opa T.id opb)
      ~description:
        ("Multiplication of a " ^ T.id ^ " amount by a negative integer")
      (obj2
         (req "amount" encoding)
         (req "multiplicator" int64))
      (function Negative_multiplicator (a, b) -> Some (a, b) | _ -> None)
      (fun (a, b) -> Negative_multiplicator (a, b)) ;
    register_error_kind
      `Temporary
      ~id:(T.id ^ ".invalid_divisor")
      ~title:("Invalid " ^ T.id ^ " divisor")
      ~pp: (fun ppf (opa, opb) ->
          Format.fprintf ppf "Division of %a %s by non positive integer %Ld"
            pp opa T.id opb)
      ~description:
        ("Multiplication of a " ^ T.id ^ " amount by a non positive integer")
      (obj2
         (req "amount" encoding)
         (req "divisor" int64))
      (function Invalid_divisor (a, b) -> Some (a, b) | _ -> None)
      (fun (a, b) -> Invalid_divisor (a, b))

end
