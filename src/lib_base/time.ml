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

open CalendarLib

module T = struct
  include Int64

  let diff a b =
    let sign = a >= b in
    let res = Int64.sub a b in
    let res_sign = res >= 0L in
    if sign = res_sign then res else invalid_arg "Time.diff" ;;

  let add a d =
    let sign = d >= 0L in
    let res = Int64.add a d in
    let incr_sign = res >= a in
    if sign = incr_sign then res else invalid_arg "Time.add" ;;

  let recent a1 a2 =
    match a1, a2 with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some (_, t1), Some (_, t2)) ->
        if compare t1 t2 < 0 then a2 else a1

  let hash = to_int
  let min_value = min_int
  let epoch = 0L
  let max_value = max_int

  let now () = Int64.of_float (Unix.gettimeofday ())

  let of_seconds x = x
  let to_seconds x = x

  let formats =
    [ "%Y-%m-%dT%H:%M:%SZ" ; "%Y-%m-%d %H:%M:%SZ";
      "%Y-%m-%dT%H:%M:%S%:z"; "%Y-%m-%d %H:%M:%S%:z"; ]

  let int64_of_calendar c =
    let round fc =
      let f, i = modf fc in
      Int64.(add (of_float i) Pervasives.(if f < 0.5 then 0L else 1L)) in
    round @@ Calendar.Precise.to_unixfloat c

  let rec iter_formats s = function
    | [] -> None
    | f :: fs ->
        try
          Some (int64_of_calendar @@ Printer.Precise_Calendar.from_fstring f s)
        with _ -> iter_formats s fs

  let of_notation s =
    iter_formats s formats
  let of_notation_exn s =
    match of_notation s with
    | None -> invalid_arg "Time.of_notation: can't parse."
    | Some t -> t

  let to_notation t =
    let ft = Int64.to_float t in
    if Int64.of_float ft <> t then
      "out_of_range"
    else
      Printer.Precise_Calendar.sprint
        "%Y-%m-%dT%H:%M:%SZ"
        (Calendar.Precise.from_unixfloat ft)

  let rfc_encoding =
    let open Data_encoding in
    def
      "timestamp.rfc"
      ~title:
        "RFC 3339 formatted timestamp"
      ~description:
        "A date in human readble form as specified in RFC 3339." @@
    conv
      to_notation
      (fun s -> match of_notation s with
         | Some s -> s
         | None -> Data_encoding.Json.cannot_destruct "Time.of_notation")
      string

  let encoding =
    let open Data_encoding in
    def "timestamp" @@
    splitted
      ~binary: int64
      ~json:
        (union [
            case Json_only
              ~title:"RFC encoding"
              rfc_encoding
              (fun i -> Some i)
              (fun i -> i) ;
            case Json_only
              ~title:"Second since epoch"
              int64
              (fun _ -> None)
              (fun i -> i) ;
          ])

  let rpc_arg =
    RPC_arg.make
      ~name:(Format.asprintf "date")
      ~descr:(Format.asprintf "A date in seconds from epoch")
      ~destruct:
        (fun s ->
           if s = "none" || s = "epoch" then
             Ok epoch
           else
             match of_notation s with
             | None -> begin
                 match Int64.of_string s with
                 | exception _ -> begin
                     Error (Format.asprintf "failed to parse time (epoch): %S" s)
                   end
                 | t -> Ok t
               end
             | Some t -> Ok t)
      ~construct:Int64.to_string
      ()

  type 'a timed_data = {
    data: 'a ;
    time: t ;
  }

  let timed_encoding arg_encoding =
    let open Data_encoding in
    conv
      (fun {time; data} -> (time, data))
      (fun (time, data) -> {time; data})
      (tup2 encoding arg_encoding)

  let make_timed data = {
    data ; time = now () ;
  }

  let pp_hum ppf t = Format.pp_print_string ppf (to_notation t)
end

include T
include Compare.Make (T)
module Set = Set.Make (T)
module Map = Map.Make (T)
module Table = Hashtbl.Make (T)
