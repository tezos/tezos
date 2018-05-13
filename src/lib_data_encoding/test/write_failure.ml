(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Test expected errors while serializing data. *)

open Data_encoding
open Types

let check_raises expected f =
  match f () with
  | exception exn when expected exn -> ()
  | exception exn ->
      Alcotest.failf "Unexpected exception: %s." (Printexc.to_string exn)
  | _ -> Alcotest.failf "Expecting exception, got success."

let json ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Json.construct encoding value : Json.t) ;
  end

let bson ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Bson.construct encoding value : Bson.t) ;
  end

let binary ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Binary.to_bytes encoding value : MBytes.t) ;
  end

let all name encoding value =
  [ name ^ ".json", `Quick, json encoding value ;
    name ^ ".bson", `Quick, bson encoding value ;
    name ^ ".bytes", `Quick, binary encoding value ]

let all_ranged_int minimum maximum =
  let encoding = ranged_int minimum maximum in
  let name = Format.asprintf "ranged_int.%d" minimum in
  all (name ^ ".min") encoding (minimum - 1) @
  all (name ^ ".max") encoding (maximum + 1)

let all_ranged_float minimum maximum =
  let encoding = ranged_float minimum maximum in
  let name = Format.asprintf "ranged_float.%f" minimum in
  all (name ^ ".min") encoding (minimum -. 1.) @
  all (name ^ ".max") encoding (maximum +. 1.)

let tests =
  all_ranged_int 100 400 @
  all_ranged_int 19000 19254 @
  all_ranged_int ~-100 300 @
  all_ranged_int ~-300_000_000 300_000_000 @
  all_ranged_float ~-. 100. 300. @
  all "string.fixed" (Fixed.string 4) "turlututu" @
  all "bytes.fixed" (Fixed.bytes 4) (MBytes.of_string "turlututu") @
  all "unknown_case.B" mini_union_enc (B "2") @
  all "unknown_case.E" mini_union_enc E @
  []
