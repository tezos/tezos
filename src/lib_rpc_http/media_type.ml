(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Resto_cohttp.Media_type.Make(RPC_encoding)

let json  = {
  name = Cohttp.Accept.MediaType ("application", "json") ;
  q = Some 1000 ;
  pp = begin fun _enc ppf raw ->
    match Data_encoding_ezjsonm.from_string raw with
    | Error err ->
        Format.fprintf ppf
          "@[Invalid JSON:@ \
          \ - @[<v 2>Error:@ %s@]\
          \ - @[<v 2>Raw data:@ %s@]@]"
          err raw
    | Ok json ->
        Data_encoding_ezjsonm.pp ppf json
  end ;
  construct = begin fun enc v ->
    Data_encoding_ezjsonm.to_string ~minify:true @@
    Data_encoding.Json.construct enc v
  end ;
  destruct = begin fun enc body ->
    match Data_encoding_ezjsonm.from_string body with
    | Error _ as err -> err
    | Ok json ->
        try Ok (Data_encoding.Json.destruct enc json)
        with Data_encoding.Json.Cannot_destruct (_, exn) ->
          Error (Format.asprintf "%a"
                   (fun fmt -> Data_encoding.Json.print_error fmt)
                   exn)
  end ;
}


let bson  = {
  name = Cohttp.Accept.MediaType ("application", "bson") ;
  q = Some 100 ;
  pp = begin fun _enc ppf raw ->
    match Json_repr_bson.bytes_to_bson ~laziness:false ~copy:false
            (Bytes.unsafe_of_string raw) with
    | exception Json_repr_bson.Bson_decoding_error (msg, _, _) ->
        Format.fprintf ppf
          "@[Invalid BSON:@ %s@]"
          msg
    | bson ->
        let json =
          Json_repr.convert
            (module Json_repr_bson.Repr)
            (module Json_repr.Ezjsonm)
            bson in
        Data_encoding_ezjsonm.pp ppf json
  end ;
  construct = begin fun enc v ->
    Bytes.unsafe_to_string @@
    Json_repr_bson.bson_to_bytes @@
    Data_encoding.Bson.construct enc v
  end ;
  destruct = begin fun enc body ->
    match Json_repr_bson.bytes_to_bson ~laziness:false ~copy:false
            (Bytes.unsafe_of_string body) with
    | exception Json_repr_bson.Bson_decoding_error (msg, _, pos) ->
        Error (Format.asprintf "(at offset: %d) %s" pos msg)
    | bson ->
        try Ok (Data_encoding.Bson.destruct enc bson)
        with Data_encoding.Json.Cannot_destruct (_, exn) ->
          Error (Format.asprintf "%a"
                   (fun fmt -> Data_encoding.Json.print_error fmt)
                   exn)
  end ;
}

let octet_stream = {
  name = Cohttp.Accept.MediaType ("application", "octet-stream") ;
  q = Some 200 ;
  pp = begin fun enc ppf raw ->
    match Data_encoding.Binary.of_bytes enc (MBytes.of_string raw) with
    | None -> Format.fprintf ppf "Invalid binary data."
    | Some v ->
        Format.fprintf ppf
          ";; binary equivalent of the following json@.%a"
          Data_encoding_ezjsonm.pp (Data_encoding.Json.construct enc v)
  end ;
  construct = begin fun enc v ->
    MBytes.to_string @@
    Data_encoding.Binary.to_bytes enc v
  end ;
  destruct = begin fun enc s ->
    match Data_encoding.Binary.of_bytes enc (MBytes.of_string s) with
    | None -> Error "Failed to parse binary data."
    | Some data -> Ok data
  end ;
}

let all_media_types = [ json ; bson ; octet_stream ]
