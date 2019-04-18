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

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {!Data_encoding.Json}. *)

type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]
type t = json
type schema = Json_schema.schema

val convert : 'a Encoding.t -> 'a Json_encoding.encoding
val schema : ?definitions_path:string -> 'a Encoding.t -> schema
val encoding: json Encoding.t
val schema_encoding: schema Encoding.t
val construct : 't Encoding.t -> 't -> json
val destruct : 't Encoding.t -> json -> 't

type path = path_item list
and path_item =
  [ `Field of string
  | `Index of int
  | `Star
  | `Next
  ]
exception Cannot_destruct of (path * exn)
exception Unexpected of string * string
exception No_case_matched of exn list
exception Bad_array_size of int * int
exception Missing_field of string
exception Unexpected_field of string

val print_error :
  ?print_unknown: (Format.formatter -> exn -> unit) ->
  Format.formatter -> exn -> unit

val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a
val wrap_error : ('a -> 'b) -> 'a -> 'b

val from_string : string -> (json, string) result
val from_stream : string Lwt_stream.t -> (json, string) result Lwt_stream.t
val to_string : ?newline:bool -> ?minify:bool -> json -> string
val pp : Format.formatter -> json -> unit

val bytes_jsont: MBytes.t Json_encoding.encoding
