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

module Encoding =
struct
  include Encoding
  let splitted ~json ~binary = raw_splitted ~json:(Json.convert json) ~binary
  let assoc enc =
    let json = Json_encoding.assoc (Json.convert enc) in
    let binary = list (tup2 string enc) in
    raw_splitted ~json ~binary

  module Bounded = struct

    let string length =
      raw_splitted
        ~binary: begin
          let kind = Binary_size.unsigned_range_to_size length in
          check_size (length + Binary_size.integer_to_size kind) @@
          dynamic_size ~kind Variable.string
        end
        ~json: begin
          let open Json_encoding in
          conv
            (fun s ->
               if String.length s > length then invalid_arg "oversized string" ;
               s)
            (fun s ->
               if String.length s > length then
                 raise (Cannot_destruct ([], Invalid_argument "oversized string")) ;
               s)
            string
        end

    let bytes length =
      raw_splitted
        ~binary: begin
          let kind = Binary_size.unsigned_range_to_size length in
          check_size (length + Binary_size.integer_to_size kind) @@
          dynamic_size ~kind Variable.bytes
        end
        ~json: begin
          let open Json_encoding in
          conv
            (fun s ->
               if MBytes.length s > length then invalid_arg "oversized string" ;
               s)
            (fun s ->
               if MBytes.length s > length then
                 raise (Cannot_destruct ([], Invalid_argument "oversized string")) ;
               s)
            Json.bytes_jsont
        end

  end

  type 'a lazy_state =
    | Value of 'a
    | Bytes of MBytes.t
    | Both of MBytes.t * 'a
  type 'a lazy_t =
    { mutable state : 'a lazy_state ;
      encoding : 'a t }
  let force_decode le =
    match le.state with
    | Value value -> Some value
    | Both (_, value) -> Some value
    | Bytes bytes ->
        match Binary_reader.of_bytes le.encoding bytes with
        | Some expr -> le.state <- Both (bytes, expr) ; Some expr
        | None -> None
  let force_bytes le =
    match le.state with
    | Bytes bytes -> bytes
    | Both (bytes, _) -> bytes
    | Value value ->
        let bytes = Binary_writer.to_bytes_exn le.encoding value in
        le.state <- Both (bytes, value) ;
        bytes
  let lazy_encoding encoding =
    let binary =
      Encoding.conv
        force_bytes
        (fun bytes -> { state = Bytes bytes ; encoding })
        Encoding.bytes in
    let json =
      Encoding.conv
        (fun le ->
           match force_decode le with
           | Some r -> r
           | None -> raise Exit)
        (fun value -> { state = Value value ; encoding })
        encoding in
    splitted ~json ~binary
  let make_lazy encoding value =
    { encoding ; state = Value value }
  let apply_lazy ~fun_value ~fun_bytes ~fun_combine le =
    match le.state with
    | Value value -> fun_value value
    | Bytes bytes -> fun_bytes bytes
    | Both (bytes, value) -> fun_combine (fun_value value) (fun_bytes bytes)

end

include Encoding



module Json = Json
module Bson = Bson
module Binary_schema = Binary_schema
module Binary = struct
  include Binary_error
  include Binary_length
  include Binary_writer
  include Binary_reader
  include Binary_stream_reader
  let describe = Binary_description.describe
end

type json = Json.t
let json = Json.encoding
type json_schema = Json.schema
let json_schema = Json.schema_encoding
type bson = Bson.t
