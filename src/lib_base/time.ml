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

module Protocol = struct

  type t = int64
  let epoch = 0L

  let diff = Int64.sub
  let add = Int64.add

  let of_ptime t =
    Option.map
      ~f:Int64.of_int
      (Ptime.Span.to_int_s (Ptime.to_span t))
  let to_ptime t =
    match Ptime.of_span (Ptime.Span.of_int_s (Int64.to_int t)) with
    | None -> invalid_arg ("Time.Protocol.to_ptime")
    | Some ptime -> ptime

  let of_notation s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> of_ptime t
    | Error _ -> None
  let of_notation_exn s =
    match Ptime.(rfc3339_error_to_msg (of_rfc3339 s)) with
    | Error (`Msg msg) -> invalid_arg ("Time.Protocol.of_notation: " ^ msg)
    | Ok (t, _, _) ->
        match of_ptime t with
        | None -> invalid_arg "Time.Protocol.of_notation"
        | Some t -> t
  let to_notation t =
    match Ptime.of_span (Ptime.Span.of_int_s (Int64.to_int t)) with
    | None -> invalid_arg ("Time.Protocol.to_notation")
    | Some ptime -> Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 ptime

  let of_seconds x = x
  let to_seconds x = x

  let encoding = Data_encoding.int64
  let rfc_encoding =
    let open Data_encoding in
    def
      "timestamp.rfc"
      ~title: "RFC 3339 formatted timestamp"
      ~description: "A date in RFC 3339 notation." @@
    conv
      to_notation
      (fun s -> match of_notation s with
         | Some s -> s
         | None -> Data_encoding.Json.cannot_destruct "Time.Protocol.of_notation")
      string

  let rpc_arg =
    RPC_arg.make
      ~name:(Format.asprintf "date")
      ~descr:(Format.asprintf "A date in seconds from epoch")
      ~destruct:
        (fun s ->
           if s = "none" || s = "epoch" then
             Ok epoch
           else
             match Int64.of_string s with
             | t -> Ok t
             | exception _ ->
                 Error (Format.asprintf "failed to parse time (epoch): %S" s))
      ~construct:Int64.to_string
      ()

  let pp_hum ppf t = Ptime.pp_rfc3339 () ppf (to_ptime t)

  include Compare.Make(Int64)

end

module System = struct

  type t = Ptime.t
  let epoch = Ptime.epoch

  module Span = struct
    type t = Ptime.Span.t
    let multiply_exn f s =
      let open Ptime.Span in
      Option.unopt_exn
        (Failure "Time.System.Span.multiply_exn")
        (of_float_s (f *. Ptime.Span.to_float_s s))
    let of_seconds_exn f =
      match Ptime.Span.of_float_s f with
      | None -> invalid_arg "Time.System.Span.of_seconds_exn"
      | Some s -> s
    let encoding =
      let open Data_encoding in
      conv
        Ptime.Span.to_float_s
        (fun f -> match Ptime.Span.of_float_s f with
           | None -> invalid_arg "Time.System.Span.encoding"
           | Some s -> s)
        float
    let rpc_arg =
      RPC_arg.make
        ~name:(Format.asprintf "timespan")
        ~descr:(Format.asprintf "A span of time in seconds")
        ~destruct:
          (fun s ->
             match Ptime.Span.of_float_s (float_of_string s) with
             | Some t -> Ok t
             | None -> Error (Format.asprintf "failed to parse timespan: %S" s)
             | exception _ -> Error (Format.asprintf "failed to parse timespan: %S" s))
        ~construct:(fun s -> string_of_float (Ptime.Span.to_float_s s))
        ()

  end

  let of_seconds_opt x =
    (* NOTE: on 32 bit machines, may behave incorrectly on times beyond the
       int31 end of time *)
    Ptime.of_span
      (Ptime.Span.of_int_s (Int64.to_int x))
  let of_seconds_exn x = match of_seconds_opt x with
    | Some t -> t
    | None -> invalid_arg "Time.of_seconds"
  let to_seconds x =
    match Ptime.(Span.to_int_s (to_span x)) with
    | Some s -> Int64.of_int s
    | None -> invalid_arg "Time.to_seconds"

  let of_protocol_exn = of_seconds_exn
  let of_protocol_opt = of_seconds_opt
  let to_protocol = to_seconds

  let of_notation_opt s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Some t
    | Error _ -> None
  let of_notation_exn s =
    match Ptime.(rfc3339_error_to_msg (of_rfc3339 s)) with
    | Ok (t, _, _) -> t
    | Error (`Msg msg) -> invalid_arg ("Time.of_notation: " ^ msg)
  let to_notation t = Ptime.to_rfc3339 t

  let rfc_encoding =
    let open Data_encoding in
    def
      "timestamp.rfc"
      ~title: "RFC 3339 formatted timestamp"
      ~description: "A date in RFC 3339 notation." @@
    conv
      to_notation
      (fun s -> match of_notation_opt s with
         | Some s -> s
         | None -> Data_encoding.Json.cannot_destruct "Time.of_notation")
      string

  let encoding =
    let open Data_encoding in
    let binary = conv to_seconds of_seconds_exn int64 in
    let json =
      union [
        case Json_only
          ~title:"RFC encoding"
          rfc_encoding
          (fun i -> Some i)
          (fun i -> i) ;
        case Json_only
          ~title:"Second since epoch"
          int64
          (fun _ -> None)
          (fun i -> of_seconds_exn i) ;
      ] in
    def "timestamp" @@
    splitted ~binary ~json

  let rpc_arg =
    RPC_arg.make
      ~name:(Format.asprintf "date")
      ~descr:(Format.asprintf "A date in seconds from epoch")
      ~destruct:
        (fun s ->
           if s = "none" || s = "epoch" then
             Ok Ptime.epoch
           else
             match of_notation_opt s with
             | Some t -> Ok t
             | None ->
                 match of_seconds_exn (Int64.of_string s) with
                 | t -> Ok t
                 | exception _ ->
                     Error (Format.asprintf "failed to parse time (epoch): %S" s))
      ~construct:to_notation
      ()

  let pp_hum ppf t = Ptime.pp_rfc3339 () ppf t

  type 'a stamped = {
    data: 'a ;
    stamp: Ptime.t ;
  }
  let stamped_encoding arg_encoding =
    let open Data_encoding in
    conv
      (fun {stamp; data} -> (stamp, data))
      (fun (stamp, data) -> {stamp; data})
      (tup2 encoding arg_encoding)
  let stamp ~time data =
    { data ; stamp = time }
  let recent a1 a2 =
    match a1, a2 with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some (_, t1), Some (_, t2)) ->
        if Ptime.compare t1 t2 < 0 then a2 else a1

  let hash t = Int64.to_int (to_seconds t)
  include Compare.Make (Ptime)
  module Set = Set.Make (Ptime)
  module Map = Map.Make (Ptime)
  module Table = Hashtbl.Make (struct include Ptime let hash = hash end)

end
