(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let apply_and_get_first_some ?(error=Encoding.No_case_matched) fs v =
  let rec loop = function
    | [] -> raise error
    | f :: fs ->
        match f v with
        | Some l -> l
        | None -> loop fs in
  loop fs

type 'l writer = {
  write: 'a. 'a Encoding.t -> 'a -> MBytes.t -> int -> int ;
}

let rec length : type x. x Encoding.t -> x -> int = fun e ->
  let open Encoding in
  match e.encoding with
  (* Fixed *)
  | Null -> fun _ -> 0
  | Empty -> fun _ -> 0
  | Constant _ -> fun _ -> 0
  | Bool -> fun _ -> Binary_size.bool
  | Int8 -> fun _ -> Binary_size.int8
  | Uint8 -> fun _ -> Binary_size.uint8
  | Int16 -> fun _ -> Binary_size.int16
  | Uint16 -> fun _ -> Binary_size.uint16
  | Int31 -> fun _ -> Binary_size.int31
  | Int32 -> fun _ -> Binary_size.int32
  | Int64 -> fun _ -> Binary_size.int64
  | Z -> fun z -> (Z.numbits z + 1 + 6) / 7
  | RangedInt { minimum ; maximum } ->
      fun _ -> Binary_size.(integer_to_size @@ range_to_size ~minimum ~maximum)
  | Float -> fun _ -> Binary_size.float
  | RangedFloat _ -> fun _ -> Binary_size.float
  | Bytes `Fixed n -> fun _ -> n
  | String `Fixed n -> fun _ -> n
  | String_enum (_, arr) ->
      fun _ -> Binary_size.(integer_to_size @@ enum_size arr)
  | Objs (`Fixed n, _, _) -> fun _ -> n
  | Tups (`Fixed n, _, _) -> fun _ -> n
  | Union (`Fixed n, _, _) -> fun _ -> n
  (* Dynamic *)
  | Objs (`Dynamic, e1, e2) ->
      let length1 = length e1 in
      let length2 = length e2 in
      fun (v1, v2) -> length1 v1 + length2 v2
  | Tups (`Dynamic, e1, e2) ->
      let length1 = length e1 in
      let length2 = length e2 in
      fun (v1, v2) -> length1 v1 + length2 v2
  | Union (`Dynamic, sz, cases) ->
      let tag_size = Binary_size.tag_size sz in
      let case_length (Case { encoding = e ; proj }) =
        let length v = tag_size + length e v in
        fun v -> Option.map ~f:length (proj v) in
      apply_and_get_first_some (List.map case_length cases)
  | Mu (`Dynamic, _name, self) ->
      fun v -> length (self e) v
  | Obj (Opt (`Dynamic, _, e)) ->
      let length = length e in
      (function None -> 1 | Some x -> 1 + length x)
  (* Variable *)
  | Ignore -> fun _ -> 0
  | Bytes `Variable -> MBytes.length
  | String `Variable -> String.length
  | Array e ->
      let length = length e in
      fun v ->
        Array.fold_left
          (fun acc v -> length v + acc)
          0 v
  | List e ->
      let length = length e in
      fun v ->
        List.fold_left
          (fun acc v -> length v + acc)
          0 v
  | Objs (`Variable, e1, e2) ->
      let length1 = length e1 in
      let length2 = length e2 in
      fun (v1, v2) -> length1 v1 + length2 v2
  | Tups (`Variable, e1, e2) ->
      let length1 = length e1
      and length2 = length e2 in
      fun (v1, v2) -> length1 v1 + length2 v2
  | Obj (Opt (`Variable, _, e)) ->
      let length = length e in
      (function None -> 0 | Some x -> length x)
  | Union (`Variable, sz, cases) ->
      let rec case_lengths json_only_cases acc = function
        | [] -> (List.rev acc, json_only_cases)
        | Case { tag = Json_only } :: tl -> case_lengths true acc tl
        | Case { encoding = e ; proj ; tag = Tag _ } :: tl ->
            let length v = Binary_size.tag_size sz + length e v in
            case_lengths
              json_only_cases
              ((fun v ->
                  match proj v with
                  | None -> None
                  | Some v -> Some (length v)) :: acc)
              tl in
      let cases, json_only = case_lengths false [] cases in
      apply_and_get_first_some
        ~error:(if json_only
                then Failure "No case matched, but JSON only cases were present in union"
                else No_case_matched)
        cases
  | Mu (`Variable, _name, self) ->
      fun v -> length (self e) v
  (* Recursive*)
  | Obj (Req (_, e)) -> length e
  | Obj (Dft (_, e, _)) -> length e
  | Tup e -> length e
  | Conv  { encoding = e ; proj } ->
      let length = length e in
      fun v -> length (proj v)
  | Describe { encoding = e } -> length e
  | Def { encoding = e } -> length e
  | Splitted { encoding = e } -> length e
  | Dynamic_size e ->
      let length = length e in
      fun v -> Binary_size.int32 + length v
  | Delayed f -> length (f ())

(** Writer *)

module Writer = struct

  let int8 v buf ofs =
    if (v < - (1 lsl 7) || v >= 1 lsl 7) then
      invalid_arg "Data_encoding.Binary.Writer.int8" ;
    MBytes.set_int8 buf ofs v;
    ofs + Binary_size.int8

  let uint8 v buf ofs =
    if (v < 0 || v >= 1 lsl 8) then
      invalid_arg "Data_encoding.Binary.Writer.uint8" ;
    MBytes.set_int8 buf ofs v;
    ofs + Binary_size.uint8

  let char v buf ofs =
    MBytes.set_char buf ofs v;
    ofs + Binary_size.char

  let bool v buf ofs =
    uint8 (if v then 255 else 0) buf ofs

  let int16 v buf ofs =
    if (v < - (1 lsl 15) || v >= 1 lsl 15) then
      invalid_arg "Data_encoding.Binary.Writer.int16" ;
    MBytes.set_int16 buf ofs v;
    ofs + Binary_size.int16

  let uint16 v buf ofs =
    if (v < 0 || v >= 1 lsl 16) then
      invalid_arg "Data_encoding.Binary.Writer.uint16" ;
    MBytes.set_int16 buf ofs v;
    ofs + Binary_size.uint16

  let uint30 v buf ofs =
    if v < 0 || (Sys.int_size > 31 && v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.uint30" ;
    MBytes.set_int32 buf ofs (Int32.of_int v);
    ofs + Binary_size.uint30

  let int31 v buf ofs =
    if Sys.int_size > 31 && (v < ~- (1 lsl 30) || v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.int31" ;
    MBytes.set_int32 buf ofs (Int32.of_int v);
    ofs + Binary_size.int31

  let int32 v buf ofs =
    MBytes.set_int32 buf ofs v;
    ofs + Binary_size.int32

  let int64 v buf ofs =
    MBytes.set_int64 buf ofs v;
    ofs + Binary_size.int64

  let z v res ofs =
    let sign = Z.sign v < 0 in
    let bits = Z.numbits v in
    if Z.equal v Z.zero then begin
      MBytes.set_int8 res ofs 0x00 ;
      ofs + 1
    end else
      let v = Z.abs v in
      let get_chunk pos len = Z.to_int (Z.extract v pos len) in
      let length = (bits + 1 + 6) / 7 in
      MBytes.set_int8 res ofs
        ((if sign then 0x40 else 0x00)
         lor (if bits > 6 then 0x80 else 0x00)
         lor (get_chunk 0 6)) ;
      for i = 1 to length - 1 do
        let pos = 6 + (i - 1) * 7 in
        let chunk_len = if i = length - 1 then bits - pos else 7 in
        MBytes.set_int8 res (ofs + i)
          ((if i = bits / 7 then 0x00 else 0x80)
           lor (get_chunk pos chunk_len))
      done ;
      ofs + length

  (** write a float64 (double) **)
  let float v buf ofs =
    (*Here, float means float64, which is written using MBytes.set_double !!*)
    MBytes.set_double buf ofs v;
    ofs + Binary_size.float

  let fixed_kind_bytes length s buf ofs =
    if MBytes.length s <> length then invalid_arg "fixed_kind_bytes";
    MBytes.blit s 0 buf ofs length;
    ofs + length

  let variable_length_bytes s buf ofs =
    let length = MBytes.length s in
    MBytes.blit s 0 buf ofs length ;
    ofs + length

  let fixed_kind_string length s buf ofs =
    if String.length s <> length then invalid_arg "fixed_kind_string";
    MBytes.blit_of_string s 0 buf ofs length;
    ofs + length

  let variable_length_string s buf ofs =
    let length = String.length s in
    MBytes.blit_of_string s 0 buf ofs length ;
    ofs + length

  let objs w1 w2 (v1,v2) buf ofs =
    w1 v1 buf ofs |> w2 v2 buf

  let array w a buf ofs =
    Array.fold_left (fun ofs v -> w v buf ofs) ofs a

  let list w l buf ofs =
    List.fold_left (fun ofs v -> w v buf ofs) ofs l

  let conv proj w v buf ofs =
    w (proj v) buf ofs

  let write_tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16

  let union w sz cases =
    let open Encoding in
    let writes_case = function
      | Case { tag = Json_only } -> None
      | Case { encoding = e ; proj ; tag = Tag tag } ->
          let write = w.write e in
          let write v buf ofs =
            write_tag sz tag buf ofs |> write v buf in
          Some (fun v ->
              match proj v with
              | None -> None
              | Some v -> Some (write v)) in
    apply_and_get_first_some (TzList.filter_map writes_case cases)

end

module BufferedWriter = struct

  let int8 v buf =
    if (v < - (1 lsl 7) || v >= 1 lsl 7) then
      invalid_arg "Data_encoding.Binary.Writer.int8" ;
    MBytes_buffer.write_int8 buf v

  let uint8 v buf =
    if (v < 0 || v >= 1 lsl 8) then
      invalid_arg "Data_encoding.Binary.Writer.uint8" ;
    MBytes_buffer.write_int8 buf v

  let char v buf =
    MBytes_buffer.write_char buf v

  let bool v buf =
    uint8 (if v then 255 else 0) buf

  let int16 v buf =
    if (v < - (1 lsl 15) || v >= 1 lsl 15) then
      invalid_arg "Data_encoding.Binary.Writer.int16" ;
    MBytes_buffer.write_int16 buf v

  let uint16 v buf =
    if (v < 0 || v >= 1 lsl 16) then
      invalid_arg "Data_encoding.Binary.Writer.uint16" ;
    MBytes_buffer.write_int16 buf v

  let uint30 v buf =
    if v < 0 || (Sys.int_size > 31 && v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.uint30" ;
    MBytes_buffer.write_int32 buf (Int32.of_int v)

  let int31 v buf =
    if Sys.int_size > 31 && (v < ~- (1 lsl 30) || v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.int31" ;
    MBytes_buffer.write_int32 buf (Int32.of_int v)

  let int32 v buf =
    MBytes_buffer.write_int32 buf v

  let int64 v buf =
    MBytes_buffer.write_int64 buf v

  let z v buf =
    let bits = Z.numbits v in
    let length = (bits + 1 + 6) / 7 in
    let res = MBytes.create length in
    ignore (Writer.z v res 0) ;
    MBytes_buffer.write_mbytes buf res 0 length

  (** write a float64 (double) **)
  let float v buf =
    MBytes_buffer.write_double buf v

  let fixed_kind_bytes length s buf =
    if MBytes.length s <> length then invalid_arg "fixed_kind_bytes";
    MBytes_buffer.write_mbytes buf s 0 length

  let variable_length_bytes s buf =
    let length = MBytes.length s in
    MBytes_buffer.write_mbytes buf s 0 length

  let fixed_kind_string length s buf =
    if String.length s <> length then invalid_arg "fixed_kind_string";
    MBytes_buffer.write_string_data buf s

  let variable_length_string s buf =
    MBytes_buffer.write_string_data buf s

  let write_tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16

end

let rec assoc_snd target = function
  | [] -> raise Encoding.No_case_matched
  | (value, hd) :: tl ->
      if hd = target
      then value
      else assoc_snd target tl

let get_string_enum_case tbl v =
  try
    snd (Hashtbl.find tbl v)
  with _ ->
    raise Encoding.No_case_matched

let rec write_rec
  : type a. a Encoding.t -> a -> MBytes.t -> int -> int = fun e ->
  let open Encoding in
  let open Writer in
  match e.encoding with
  | Null -> (fun () _buf ofs -> ofs)
  | Empty -> (fun () _buf ofs -> ofs)
  | Constant _ -> (fun () _buf ofs -> ofs)
  | Ignore -> (fun () _buf ofs -> ofs)
  | Bool -> bool
  | Int8 -> int8
  | Uint8 -> uint8
  | Int16 -> int16
  | Uint16 -> uint16
  | Int31 -> int31
  | Int32 -> int32
  | Int64 -> int64
  | Z -> z
  | RangedInt { minimum ; maximum } ->
      fun v ->
        begin
          if v < minimum || v > maximum
          then invalid_arg (Printf.sprintf "Integer %d not in range [%d, %d]." v minimum maximum) ;
          let v = if minimum >= 0 then v - minimum else v in
          match Binary_size.range_to_size ~minimum ~maximum with
          | `Uint8 -> uint8 v
          | `Uint16 -> uint16 v
          | `Uint30 -> uint30 v
          | `Int8 -> int8 v
          | `Int16 -> int16 v
          | `Int31 -> int31 v
        end
  | Float -> float
  | RangedFloat { minimum ; maximum } ->
      fun v ->
        if v < minimum || v > maximum
        then invalid_arg (Printf.sprintf "Float %f not in range [%f, %f]." v minimum maximum) ;
        float v
  | Bytes (`Fixed n) -> fixed_kind_bytes n
  | String (`Fixed n) -> fixed_kind_string n
  | Bytes `Variable -> variable_length_bytes
  | String `Variable -> variable_length_string
  | Array t -> array (write_rec t)
  | List t -> list (write_rec t)
  | String_enum (tbl, arr) ->
      (fun v ->
         let value = get_string_enum_case tbl v in
         match Binary_size.enum_size arr with
         | `Uint30 -> uint30 value
         | `Uint16 -> uint16 value
         | `Uint8 -> uint8 value)
  | Obj (Req (_, e)) -> write_rec e
  | Obj (Opt (`Dynamic, _, e)) ->
      let write = write_rec e in
      (function None -> int8 0
              | Some x -> fun buf ofs -> int8 1 buf ofs |> write x buf)
  | Obj (Opt (`Variable, _, e)) ->
      let write = write_rec e in
      (function None -> fun _buf ofs -> ofs
              | Some x -> write x)
  | Obj (Dft (_, e, _)) -> write_rec e
  | Objs (_, e1, e2) ->
      objs (write_rec e1) (write_rec e2)
  | Tup e -> write_rec e
  | Tups (_, e1, e2) ->
      objs (write_rec e1) (write_rec e2)
  | Conv { encoding = e; proj } -> conv proj (write_rec e)
  | Describe { encoding = e } -> write_rec e
  | Def { encoding = e } -> write_rec e
  | Splitted { encoding = e } -> write_rec e
  | Union (_, sz, cases) -> union { write = write_rec } sz cases
  | Mu (_, _, self) -> fun v buf ofs -> write_rec (self e) v buf ofs
  | Dynamic_size e ->
      let length = length e
      and write = write_rec e in
      fun v buf ofs ->
        int32 (Int32.of_int @@ length v) buf ofs |> write v buf
  | Delayed f -> write_rec (f ())

let rec write_rec_buffer
  : type a. a Encoding.t -> a -> MBytes_buffer.t -> unit =
  fun encoding value buffer ->
    let open Encoding in
    let open BufferedWriter in
    match encoding.encoding with
    | Null -> ()
    | Empty -> ()
    | Constant _ -> ()
    | Ignore -> ()
    | Bool -> bool value buffer
    | Int8 -> int8 value buffer
    | Uint8 -> uint8 value buffer
    | Int16 -> int16 value buffer
    | Uint16 -> uint16 value buffer
    | Int31 -> int31 value buffer
    | Int32 -> int32 value buffer
    | Int64 -> int64 value buffer
    | Z -> z value buffer
    | Float -> float value buffer
    | Bytes (`Fixed n) -> fixed_kind_bytes n value buffer
    | String (`Fixed n) -> fixed_kind_string n value buffer
    | Bytes `Variable -> variable_length_bytes value buffer
    | String `Variable -> variable_length_string value buffer
    | Array t -> Array.iter (fun x -> write_rec_buffer t x buffer) value
    | List t -> List.iter (fun x -> write_rec_buffer t x buffer) value
    | RangedInt { minimum ; maximum } ->
        if value < minimum || value > maximum
        then invalid_arg (Printf.sprintf "Integer %d not in range [%d, %d]."
                            value minimum maximum) ;
        let value = if minimum >= 0 then value - minimum else value in
        begin
          match Binary_size.range_to_size ~minimum ~maximum with
          | `Uint30 -> uint30 value buffer
          | `Uint16 -> uint16 value buffer
          | `Uint8 -> uint8 value buffer
          | `Int8 -> int8 value buffer
          | `Int16 -> int16 value buffer
          | `Int31 -> int31 value buffer
        end
    | RangedFloat { minimum ; maximum } ->
        if value < minimum || value > maximum
        then invalid_arg (Printf.sprintf "Float %f not in range [%f, %f]."
                            value minimum maximum) ;
        float value buffer
    | String_enum (tbl, arr) ->
        (match Binary_size.enum_size arr with
         | `Uint30 -> BufferedWriter.uint30
         | `Uint16 -> BufferedWriter.uint16
         | `Uint8 -> BufferedWriter.uint8)
          (get_string_enum_case tbl value)
          buffer
    | Obj (Req (_, e)) -> write_rec_buffer e value buffer
    | Obj (Opt (`Dynamic, _, e)) ->
        (match value with
         | None -> int8 0 buffer
         | Some x ->
             begin
               int8 1 buffer ;
               write_rec_buffer e x buffer
             end)
    | Obj (Opt (`Variable, _, e)) ->
        (match value with
         | None -> ()
         | Some x -> write_rec_buffer e x buffer)
    | Obj (Dft (_, e, _)) -> write_rec_buffer e value buffer
    | Objs (_, e1, e2) ->
        let v1, v2 = value in
        write_rec_buffer e1 v1 buffer ;
        write_rec_buffer e2 v2 buffer
    | Tup e -> write_rec_buffer e value buffer
    | Tups (_, e1, e2) ->
        let v1, v2 = value in
        write_rec_buffer e1 v1 buffer ;
        write_rec_buffer e2 v2 buffer
    | Conv { encoding = e; proj } ->
        write_rec_buffer e (proj value) buffer
    | Describe { encoding = e } -> write_rec_buffer e value buffer
    | Def { encoding = e } -> write_rec_buffer e value buffer
    | Splitted { encoding = e } -> write_rec_buffer e value buffer
    | Union (_, sz, cases) ->
        let rec write_case = function
          | [] -> raise No_case_matched
          | Case { tag = Json_only } :: tl -> write_case tl
          | Case { encoding = e ; proj ; tag = Tag tag } :: tl ->
              begin
                match proj value with
                | None -> write_case tl
                | Some data ->
                    write_tag sz tag buffer ;
                    write_rec_buffer e data buffer
              end  in
        write_case cases
    | Mu (_, _, self) ->
        write_rec_buffer (self encoding) value buffer
    | Dynamic_size e ->
        MBytes_buffer.write_sized buffer (fun () -> write_rec_buffer e value buffer)
    | Delayed f -> write_rec_buffer (f ()) value buffer

let write t v buf ofs =
  try Some (write_rec t v buf ofs)
  with _ -> None

let to_bytes t v =
  let bytes = MBytes_buffer.create () in
  write_rec_buffer t v bytes ;
  MBytes_buffer.to_mbytes bytes

let to_bytes = to_bytes

let length = length

let fixed_length e =
  match Encoding.classify e with
  | `Fixed n -> Some n
  | `Dynamic | `Variable -> None
let fixed_length_exn e =
  match fixed_length e with
  | Some n -> n
  | None -> invalid_arg "Data_encoding.Binary.fixed_length_exn"
