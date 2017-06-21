(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
  val one_cent : qty
  val fifty_cents : qty
  val one : qty

  val ( -? ) : qty -> qty -> qty tzresult
  val ( +? ) : qty -> qty -> qty tzresult
  val ( *? ) : qty -> int64 -> qty tzresult
  val ( /? ) : qty -> int64 -> qty tzresult

  val to_cents : qty -> int64

  (** [of_cents n] is None if n is negative *)
  val of_cents : int64 -> qty option

  (** [of_cents_exn n] fails if n is negative.
      It should only be used at toplevel for constants. *)
  val of_cents_exn : int64 -> qty

  (** It should only be used at toplevel for constants. *)
  val add_exn : qty -> qty -> qty

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
  let one_cent = 1L
  let fifty_cents = 50L
  let one = 100L
  let id = T.id

  let of_cents t =
    if t < 0L
    then None
    else Some t

  let of_string s =
    let len = String.length s in
    let rec dec i len acc =
      if Compare.Int.(i = len) then acc
      else
        dec (succ i) len
          (Int64.add (Int64.mul 10L acc)
             (match String.get s i with
              | '0' -> 0L | '1' -> 1L | '2' -> 2L | '3' -> 3L | '4' -> 4L
              | '5' -> 5L | '6' -> 6L | '7' -> 7L | '8' -> 8L | '9' -> 9L
              | _ -> raise Exit)) in
    let rec loop acc m len =
      if Compare.Int.(len >= 4) && Compare.Char.(String.get s (len - 4) = ',') then
        let acc = Int64.add acc Int64.(mul (dec (len - 3) len 0L) m) in
        loop acc Int64.(mul 1000L m) (len - 4)
      else
        Int64.add acc Int64.(mul (dec 0 len 0L) m) in
    let cents, len =
      if Compare.Int.(len >= 3) && Compare.Char.(String.get s (len - 3) = '.') then
        dec (len - 2) len 0L, len - 3
      else
        0L, len in
    let res =
      if Compare.Int.(len >= 4) && Compare.Char.(String.get s (len - 4) = ',') then
        loop cents 100L len
      else if Compare.Int.(len = 0) && Compare.Int.(String.length s = 3) then
        cents
      else
        try
          Int64.(add (mul 100L (of_string (String.sub s 0 len))) cents)
        with _ -> raise Exit in
    match of_cents res with
    | None -> raise Exit
    | Some tez -> tez

  let of_string s =
    try Some (of_string s) with Exit -> None

  let pp ppf amount =
    let rec loop ppf amount=
      let d, r = Int64.div amount 1000L, Int64.rem amount 1000L in
      if d > 0L then
        Format.fprintf ppf "%a,%03Ld" loop d r
      else
        Format.fprintf ppf "%Ld" r in
    let i, c = Int64.div amount 100L, Int64.rem amount 100L in
    Format.fprintf ppf "%a.%02Ld" loop i c

  let to_string t =
    Format.asprintf "%a" pp t

  let (-) t1 t2 =
    if t2 <= t1
    then Some (Int64.sub t1 t2)
    else None

  let (-?) t1 t2 =
    match t1 - t2 with
    | None -> error (Subtraction_underflow (t1, t2))
    | Some v -> ok v

  let (+?) t1 t2 =
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

  let to_cents t = t

  let of_cents_exn x =
    match of_cents x with
    | None -> invalid_arg "Qty.of_cents"
    | Some v -> v

  let to_int64 t = t

  let encoding =
    let open Data_encoding in
    describe
      ~title: "Amount in centiles"
      (conv to_int64 (Json.wrap_error of_cents_exn) int64)

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
