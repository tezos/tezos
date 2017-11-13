(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

  let hash = to_int
  let (=) = equal
  let (<>) x y = compare x y <> 0
  let (<) x y = compare x y < 0
  let (<=) x y = compare x y <= 0
  let (>=) x y = compare x y >= 0
  let (>) x y = compare x y > 0
  let min x y = if x <= y then x else y
  let max x y = if x <= y then y else x

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
      "timestamp" @@
    describe
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
    splitted
      ~binary: int64
      ~json:
        (union [
            case
              rfc_encoding
              (fun i -> Some i)
              (fun i -> i) ;
            case
              int64
              (fun _ -> None)
              (fun i -> i) ;
          ])

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
module Set = Set.Make(T)
module Map = Map.Make(T)
module Table = Hashtbl.Make(T)
