(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

end

include Encoding



module Json = Json
module Bson = Bson
module Binary = struct
  include Binary_error
  include Binary_length
  include Binary_writer
  include Binary_reader
  include Binary_stream_reader
end

type json = Json.t
let json = Json.encoding
type json_schema = Json.schema
let json_schema = Json.schema_encoding
type bson = Bson.t
