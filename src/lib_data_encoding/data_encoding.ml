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
