(* Representations of JSON documents *)

(************************************************************************)
(*  ocplib-json-typed                                                   *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocplib-json-typed is distributed in the hope that it will be useful,*)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

open Json_repr

module Repr = struct
  type serialized =
    { buffer : bytes ;
      offset : int ;
      length : int ;
      array_field : bool }
  and deserialized =
    [ `O of (string * value) list
    | `A of value list
    | `Bool of bool
    | `Float of float
    | `String of string
    | `Null ]
  and node =
    | Deserialized of deserialized
    | Serialized of serialized
    | Both of deserialized * serialized
  and value =
    { mutable node : node ;
      conforming : bool ; (* when lazily deserializing the root *)
      cache : bool (* when lazily deserializing *) }

  module LEB = EndianBytes.LittleEndian_unsafe

  exception Bson_decoding_error of string * bytes * int

  let view root =
    match root.node with
    | Deserialized deserialized
    | Both (deserialized, _) -> deserialized
    | Serialized ({ buffer ; offset ; length ; array_field } as serialized) ->
      let offset = ref offset in
      let length = ref length in
      let error fmt =
        Format.ksprintf
          (fun msg -> raise (Bson_decoding_error (msg, buffer, !offset)))
          fmt in
      let box node =
        { node ; conforming = false ; cache = root.cache } in
      let skip n =
        offset := !offset + n ;
        length := !length - n in
      let read_float () =
        if !length < 8 then
          error "not enough data, double expected (8 bytes)" ;
        let res = LEB.get_double buffer !offset in
        skip 8 ;
        res in
      let read_string () =
        if !length < 4 then
          error "not enough data, string size tag expected (4 bytes)" ;
        let strlen = Int32.to_int (LEB.get_int32 buffer !offset) - 1 in
        skip 4 ;
        if !length < strlen then
          error "not enough data, string expected (%d bytes)" strlen ;
        let res = Bytes.sub_string buffer !offset strlen in
        skip strlen ;
        if !length < 1 then
          error "not enough data, string terminator expected (0x00)" ;
        if LEB.get_int8 buffer !offset <> 0x00 then
          error "string terminator expected (0x00)" ;
        skip 1 ;
        res in
      let read_bool () =
        if !length < 1 then
          error "not enough data, bool expected (1 byte)" ;
        let res = match LEB.get_int8 buffer !offset with
          | 0x00 -> false
          | 0x01 -> true
          | byte -> error "invalid bool value (0x%02X)" byte in
        skip 1 ;
        res in
      let read_field_name () =
        let rec find_terminator len =
          if !length = 0 then
            error "not enough data, field name terminator expected (0x00)" ;
          match LEB.get_int8 buffer !offset with
          | 0x00 ->
            skip (-len) ;
            len
          | _ ->
            skip 1 ;
            find_terminator (len + 1) in
        let fieldlen = find_terminator 0 in
        let res = Bytes.sub_string buffer !offset fieldlen in
        skip (fieldlen + 1) ;
        res in
      let deserialized =
        if !length < 5 then
          error "not enough data for size and terminator" ;
        let size = Int32.to_int (LEB.get_int32 buffer !offset) in
        if size <> !length then
          error "size tag inconsistent with actual data" ;
        skip 4 ;
        let tag = LEB.get_int8 buffer !offset in
        if tag = 0x00 then begin
          if !length = 1 then
            `O []
          else
            error "early terminator" ;
        end else if not root.conforming && tag land 0xF0 = 0x80 then begin
          skip 1 ;
          let res = match tag land 0x0F with
            | 0x01 -> `Float (read_float ())
            | 0x02 -> `String (read_string ())
            | 0x08 -> `Bool (read_bool ())
            | 0x0A -> `Null
            | tag ->
              error "unknown immediate tag (0x%02X)" tag in
          if !length <> 1 then
            error "not enough data, terminator expected (0x00)" ;
          if LEB.get_int8 buffer !offset <> 0x00 then
            error "terminator expected (0x00)" ;
          skip 1 ;
          res
        end else begin
          let rec loop acc =
            let tag = LEB.get_int8 buffer !offset in
            if tag = 0x00 then
              if !length = 1 then
                if array_field then
                  try
                    let rec to_array acc i = function
                      | [] -> `A (List.rev acc)
                      | (name, bson) :: rest ->
                        if name = string_of_int i then
                          to_array (bson :: acc) (i + 1) rest
                        else raise Exit in
                    to_array [] 0 (List.rev acc)
                  with Exit ->
                    error "invalid field names for array field"
                else
                  `O (List.rev acc)
              else
                error "early terminator"
            else begin
              skip 1 ;
              match tag with
              | 0x01 ->
                let name = read_field_name () in
                loop ((name, box (Deserialized (`Float (read_float ())))) :: acc)
              | 0x02 ->
                let name = read_field_name () in
                loop ((name, box (Deserialized (`String (read_string ())))) :: acc)
              | 0x08 ->
                let name = read_field_name () in
                loop ((name, box (Deserialized (`Bool (read_bool ())))) :: acc)
              | 0x0A ->
                let name = read_field_name () in
                loop ((name, box (Deserialized (`Null))) :: acc)
              | 0x03 | 0x04 ->
                let name = read_field_name () in
                if !length < 4 then
                  error "not enough data, subdocument size tag expected (4 bytes)" ;
                let doclen = Int32.to_int (LEB.get_int32 buffer !offset) in
                if !length < doclen then
                  error "not enough data, subdocument expected (%d bytes)" doclen ;
                let serialized =
                  { buffer ; length = doclen ; offset = !offset ;
                    array_field = (tag = 0x04) } in
                skip doclen ;
                loop ((name, box (Serialized serialized)) :: acc)
              | tag ->
                error "unknown tag (0x%02X)" tag
            end in
          loop []
        end in
      if root.cache then begin
        root.node <- Both (deserialized, serialized)
      end else begin
        root.node <- Deserialized deserialized
      end ;
      deserialized

  let repr deserialized =
    { node = (Deserialized deserialized) ;
      conforming = false ;
      cache = true }

  let to_bytes ~cache ~conforming root =
    match root.node with
    | Serialized serialized
    | Both (_, serialized) ->
      if serialized.offset = 0
      && serialized.length = Bytes.length serialized.buffer then
        serialized.buffer
      else
        Bytes.sub serialized.buffer serialized.offset serialized.length
    | Deserialized _ ->
      let rec compute_size bson =
        match bson.node with
        | Serialized { length }
        | Both (_, { length }) ->
          length
        | Deserialized deserialized ->
          match deserialized with
          | `Float _ -> 4 + 1 + 8 + 1
          | `String str -> 4 + 1 + 4 + String.length str + 1 + 1
          | `Bool _ -> 4 + 1 + 1 + 1
          | `Null -> 4 + 1 + 1
          | `O fields ->
            let acc = List.fold_left
                (fun acc (name, bson) ->
                   let self = match view bson with
                     | `Float _ -> 8
                     | `String str -> 4 + String.length str + 1
                     | `Bool _ -> 1
                     | `Null -> 0
                     | `O _ | `A _ -> compute_size bson in
                   acc +  1 + String.length name + 1 + self)
                0 fields in
            4 + acc + 1
          | `A cells ->
            let acc, _ = List.fold_left
                (fun (acc, i) bson ->
                   let self = match view bson with
                     | `Float _ -> 8
                     | `String str -> 4 + String.length str + 1
                     | `Bool _ -> 1
                     | `Null -> 0
                     | `O _ | `A _ -> compute_size bson in
                   let rec digits acc i =
                     if i <= 9 then (1 + acc)
                     else digits (1 + acc) (i / 10) in
                   (acc + 1 + digits 0 i + 1 + self, i + 1))
                (0, 0) cells in
            4 + acc + 1 in
      let computed_size = compute_size root in
      let result = Bytes.create computed_size in
      let pos = ref 0 in
      let (+=) r i = r := !r + i in
      let reserve_size_stamp () =
        let offset = !pos in
        pos += 4 ;
        fun () ->
          LEB.set_int8 result !pos 0x00 ;
          pos += 1 ;
          let size = Int32.of_int (!pos - offset) in
          LEB.set_int32 result offset size in
      let rec serialize_toplevel conforming = function
        | `Float _ | `String _ | `Bool _ | `Null | `A _ when conforming ->
          raise (Invalid_argument "Json_repr.bson_to_bytes")
        | `Float f ->
          let update_size_stamp = reserve_size_stamp () in
          LEB.set_int8 result !pos 0x81 ;
          pos += 1 ;
          LEB.set_double result !pos f ;
          pos += 8 ;
          update_size_stamp ()
        | `String str ->
          let update_size_stamp = reserve_size_stamp () in
          LEB.set_int8 result !pos 0x82 ;
          pos += 1 ;
          let strlen = String.length str in
          LEB.set_int32 result !pos Int32.(of_int (strlen + 1)) ;
          pos += 4 ;
          Bytes.blit_string str 0 result !pos strlen ;
          pos += strlen ;
          LEB.set_int8 result !pos 0x00 ;
          pos += 1 ;
          update_size_stamp ()
        | `Bool b ->
          let update_size_stamp = reserve_size_stamp () in
          LEB.set_int8 result !pos 0x88 ;
          pos += 1 ;
          LEB.set_int8 result !pos (if b then 0x01 else 0x00) ;
          pos += 1 ;
          update_size_stamp ()
        | `Null ->
          let update_size_stamp = reserve_size_stamp () in
          LEB.set_int8 result !pos 0x8A ;
          pos += 1 ;
          update_size_stamp ()
        | `O _ | `A _ as fields_or_cells ->
          let fields = match fields_or_cells with
            | `O fields -> fields
            | `A cells -> List.mapi (fun i v -> string_of_int i, v) cells in
          let update_size_stamp = reserve_size_stamp () in
          serialize_fields fields ;
          update_size_stamp ()
      and serialize_fields fields =
        List.iter
          (fun (name, bson) ->
             LEB.set_int8 result !pos
               (match view bson with
                | `Float _ -> 0x01
                | `String _ -> 0x02
                | `Bool _ -> 0x08
                | `Null -> 0x0A
                | `O _ -> 0x03 ;
                | `A _ -> 0x04) ;
             pos += 1 ;
             let strlen = String.length name in
             Bytes.blit_string name 0 result !pos strlen ;
             pos += strlen ;
             LEB.set_int8 result !pos 0x00 ;
             pos += 1 ;
             begin match view bson with
               | `Float f ->
                 LEB.set_double result !pos f ;
                 pos += 8 ;
               | `String str ->
                 let strlen = String.length str in
                 LEB.set_int32 result !pos Int32.(of_int (strlen + 1)) ;
                 pos += 4 ;
                 Bytes.blit_string str 0 result !pos strlen ;
                 pos += strlen ;
                 LEB.set_int8 result !pos 0x00 ;
                 pos += 1 ;
               | `Bool b ->
                 LEB.set_int8 result !pos (if b then 0x01 else 0x00) ;
                 pos += 1 ;
               | `Null -> ()
               | `O _ | `A _ -> serialize false bson
             end)
          fields
      and serialize conforming bson =
        match bson.node with
        | Serialized { buffer ; offset ; length }
        | Both (_, { buffer ; offset ; length }) ->
          Bytes.blit buffer offset result !pos length ;
          pos := !pos + length
        | Deserialized deserialized ->
          let offset = !pos in
          serialize_toplevel conforming deserialized ;
          let length = !pos - offset in
          if cache then begin
            let serialized =
              let array_field =
                match deserialized with `A _ -> true | _ -> false in
              { buffer = result ; offset ; length ; array_field  } in
            bson.node <- Both (deserialized, serialized)
          end in
      serialize conforming root ;
      result

  let from_bytes ~laziness ~cache ~conforming buffer =
    let serialized =
      { offset = 0 ; length = Bytes.length buffer ; buffer ;
        array_field = false } in
    let root =
      { node = Serialized serialized ; conforming ; cache } in
    let rec traverse bson = match view bson with
      | `O fields -> List.iter (fun (_, bson) -> traverse bson) fields
      | `A cells -> List.iter traverse cells
      | `Float _ | `String _ | `Bool _ | `Null -> () in
    if not laziness then begin
      (* a simple traversal will expand the structure as a side effect *)
      traverse root
    end ;
    root

  let repr_uid : value Json_repr.repr_uid = repr_uid ()

end

type bson = Repr.value

exception Bson_decoding_error = Repr.Bson_decoding_error

let bson_to_bytes ?(cache = true) ?(conforming = false) bson =
  Repr.to_bytes ~cache ~conforming bson

let bytes_to_bson ?(laziness = true) ?(cache = true) ?(conforming = false) ~copy buffer =
  let buffer = if copy then Bytes.copy buffer else buffer in
  Repr.from_bytes ~laziness ~cache ~conforming buffer

module Json_encoding = Json_encoding.Make (Repr)
module Json_query = Json_query.Make (Repr)
