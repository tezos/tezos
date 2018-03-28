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

type 'l reader = {
  read: 'a. 'a Encoding.t -> MBytes.t -> int -> int -> (int * 'a) ;
}

let rec length : type x. x Encoding.t -> x -> int = fun e ->
  let open Encoding in
  match e.encoding with
  (* Fixed *)
  | Null -> fun _ -> 0
  | Empty -> fun _ -> 0
  | Constant _ -> fun _ -> 0
  | Bool -> fun _ -> Size.bool
  | Int8 -> fun _ -> Size.int8
  | Uint8 -> fun _ -> Size.uint8
  | Int16 -> fun _ -> Size.int16
  | Uint16 -> fun _ -> Size.uint16
  | Int31 -> fun _ -> Size.int31
  | Int32 -> fun _ -> Size.int32
  | Int64 -> fun _ -> Size.int64
  | Z -> fun z -> (Z.numbits z + 1 + 6) / 7
  | RangedInt { minimum ; maximum } ->
      fun _ -> Size.(integer_to_size @@ range_to_size ~minimum ~maximum)
  | Float -> fun _ -> Size.float
  | RangedFloat _ -> fun _ -> Size.float
  | Bytes `Fixed n -> fun _ -> n
  | String `Fixed n -> fun _ -> n
  | String_enum (_, arr) ->
      fun _ -> Size.(integer_to_size @@ enum_size arr)
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
      let tag_size = Size.tag_size sz in
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
            let length v = Size.tag_size sz + length e v in
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
      fun v -> Size.int32 + length v
  | Delayed f -> length (f ())

(** Writer *)

module Writer = struct

  let int8 v buf ofs =
    if (v < - (1 lsl 7) || v >= 1 lsl 7) then
      invalid_arg "Data_encoding.Binary.Writer.int8" ;
    MBytes.set_int8 buf ofs v;
    ofs + Size.int8

  let uint8 v buf ofs =
    if (v < 0 || v >= 1 lsl 8) then
      invalid_arg "Data_encoding.Binary.Writer.uint8" ;
    MBytes.set_int8 buf ofs v;
    ofs + Size.uint8

  let char v buf ofs =
    MBytes.set_char buf ofs v;
    ofs + Size.char

  let bool v buf ofs =
    uint8 (if v then 255 else 0) buf ofs

  let int16 v buf ofs =
    if (v < - (1 lsl 15) || v >= 1 lsl 15) then
      invalid_arg "Data_encoding.Binary.Writer.int16" ;
    MBytes.set_int16 buf ofs v;
    ofs + Size.int16

  let uint16 v buf ofs =
    if (v < 0 || v >= 1 lsl 16) then
      invalid_arg "Data_encoding.Binary.Writer.uint16" ;
    MBytes.set_int16 buf ofs v;
    ofs + Size.uint16

  let uint30 v buf ofs =
    if v < 0 || (Sys.int_size > 31 && v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.uint30" ;
    MBytes.set_int32 buf ofs (Int32.of_int v);
    ofs + Size.uint30

  let int31 v buf ofs =
    if Sys.int_size > 31 && (v < ~- (1 lsl 30) || v >= 1 lsl 30) then
      invalid_arg "Data_encoding.Binary.Writer.int31" ;
    MBytes.set_int32 buf ofs (Int32.of_int v);
    ofs + Size.int31

  let int32 v buf ofs =
    MBytes.set_int32 buf ofs v;
    ofs + Size.int32

  let int64 v buf ofs =
    MBytes.set_int64 buf ofs v;
    ofs + Size.int64

  let z v res ofs =
    let sign = Z.sign v < 0 in
    let bits = Z.numbits v in
    if Z.equal v Z.zero then begin
      MBytes.set_int8 res ofs 0x00 ;
      ofs + 1
    end else
      let raw = Z.to_bits v in
      let get_chunk pos len (* < 8 *) =
        let byte = pos / 8 in
        let bit = pos mod 8 in
        if bit + len <= 8 then
          let mask = 0xFF lsr (8 - len) in
          (Char.code (String.get raw byte) lsr bit) land mask
        else
          let mask = 0xFF lsr (16 - len - bit) in
          (Char.code (String.get raw byte) lsr bit)
          lor ((Char.code (String.get raw (byte + 1))) land mask) lsl (8 - bit) in
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
    ofs + Size.float

  let fixed_kind_bytes length s buf ofs =
    MBytes.blit s 0 buf ofs length;
    ofs + length

  let variable_length_bytes s buf ofs =
    let length = MBytes.length s in
    MBytes.blit s 0 buf ofs length ;
    ofs + length

  let fixed_kind_string length s buf ofs =
    if String.length s <> length then invalid_arg "fixed_kind_string";
    MBytes.blit_from_string s 0 buf ofs length;
    ofs + length

  let variable_length_string s buf ofs =
    let length = String.length s in
    MBytes.blit_from_string s 0 buf ofs length ;
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
          match Size.range_to_size ~minimum ~maximum with
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
         match Size.enum_size arr with
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
          match Size.range_to_size ~minimum ~maximum with
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
        (match Size.enum_size arr with
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

let to_bytes_list ?(copy_blocks=false) block_sz t v =
  assert (block_sz > 0);
  let bytes = to_bytes t v in   (* call to generic function to_bytes *)
  let length = MBytes.length bytes in
  if length <= block_sz then
    [bytes] (* if the result fits in the given block_sz *)
  else
    let may_copy = if copy_blocks then MBytes.copy else fun t -> t in
    let nb_full = length / block_sz in (* nb of blocks of size block_sz *)
    let sz_full = nb_full * block_sz in (* size of the full part *)
    let acc = (* eventually init acc with a non-full block *)
      if sz_full = length then []
      else [may_copy (MBytes.sub bytes sz_full (length - sz_full))]
    in
    let rec split_full_blocks curr_upper_limit acc =
      let start = curr_upper_limit - block_sz in
      assert (start >= 0);
      (* copy the block [ start, curr_upper_limit [ of size block_sz *)
      let acc = (may_copy (MBytes.sub bytes start block_sz)) :: acc in
      if start = 0 then acc else split_full_blocks start acc
    in
    split_full_blocks sz_full acc

(** Reader *)

module Reader = struct

  let int8 buf ofs _len =
    ofs + Size.int8, MBytes.get_int8 buf ofs

  let uint8 buf ofs _len =
    ofs + Size.uint8, MBytes.get_uint8 buf ofs

  let char buf ofs _len =
    ofs + Size.char, MBytes.get_char buf ofs

  let bool buf ofs len =
    let ofs, v = int8 buf ofs len in
    ofs, v <> 0

  let int16 buf ofs _len =
    ofs + Size.int16, MBytes.get_int16 buf ofs

  let uint16 buf ofs _len =
    ofs + Size.uint16, MBytes.get_uint16 buf ofs

  let uint30 buf ofs _len =
    let v = Int32.to_int (MBytes.get_int32 buf ofs) in
    if v < 0 then
      failwith "Data_encoding.Binary.Reader.uint30: invalid data." ;
    ofs + Size.uint30, v

  let int31 buf ofs _len =
    ofs + Size.int31, Int32.to_int (MBytes.get_int32 buf ofs)

  let int32 buf ofs _len =
    ofs + Size.int32, MBytes.get_int32 buf ofs

  let int64 buf ofs _len =
    ofs + Size.int64, MBytes.get_int64 buf ofs

  let z buf ofs _len =
    let res = Buffer.create 100 in
    let rec read prev i value bit =
      if prev land 0x80 = 0x00 then begin
        if bit > 0 then Buffer.add_char res (Char.unsafe_chr value) ;
        if prev = 0x00 then failwith "trailing zeroes in Z encoding" ;
        i
      end else
        let byte = MBytes.get_uint8 buf (ofs + i) in
        let value = value lor ((byte land 0x7F) lsl bit) in
        let bit = bit + 7 in
        let bit, value = if bit >= 8 then begin
            Buffer.add_char res (Char.unsafe_chr (value land 0xFF)) ;
            bit - 8, value lsr 8
          end else bit, value in
        read byte (i + 1) value bit in
    let first = MBytes.get_uint8 buf ofs in
    if first = 0 then
      ofs + 1, Z.zero
    else
      let value = first land 0x3F in
      let sign = (first land 0x40) <> 0 in
      let length = read first 1 value 6 in
      let bits = Buffer.contents res in
      let res = Z.of_bits bits in
      let res = if sign then Z.neg res else res in
      ofs + length, res

  (** read a float64 (double) **)
  let float buf ofs _len =
    (*Here, float means float64, which is read using MBytes.get_double !!*)
    ofs + Size.float, MBytes.get_double buf ofs

  let int_of_int32 i =
    let i' = Int32.to_int i in
    let i'' = Int32.of_int i' in
    if i'' = i then
      i'
    else
      invalid_arg "int_of_int32 overflow"

  let fixed_length_bytes length buf ofs _len =
    let s = MBytes.sub buf ofs length in
    ofs + length, s

  let fixed_length_string length buf ofs _len =
    let s = MBytes.substring buf ofs length in
    ofs + length, s

  let seq r1 r2 buf ofs len =
    let ofs', v1 = r1 buf ofs len in
    let ofs'', v2 = r2 buf ofs' (len - (ofs' - ofs)) in
    ofs'', (v1, v2)

  let varseq r e1 e2 buf ofs len =
    let k1 = Encoding.classify e1
    and k2 = Encoding.classify e2 in
    match k1, k2 with
    | (`Dynamic | `Fixed _), `Variable ->
        let ofs', v1 = r.read e1 buf ofs len in
        let ofs'', v2 = r.read e2 buf ofs' (len - (ofs' - ofs)) in
        ofs'', (v1, v2)
    | `Variable, `Fixed n ->
        let ofs', v1 = r.read e1 buf ofs (len - n) in
        let ofs'', v2 = r.read e2 buf ofs' n in
        ofs'', (v1, v2)
    | _ -> assert false (* Should be rejected by Kind.combine *)

  let list read buf ofs len =
    let rec loop acc ofs len =
      assert (len >= 0);
      if len <= 0
      then ofs, List.rev acc
      else
        let ofs', v = read buf ofs len in
        assert (ofs' > ofs);
        loop (v :: acc) ofs'  (len - (ofs' - ofs))
    in
    loop [] ofs len

  let array read buf ofs len =
    let ofs, l = list read buf ofs len in
    ofs, Array.of_list l

  let conv inj r buf ofs len =
    let ofs, v = r buf ofs len in
    ofs, inj v

  let read_tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16

  let union r sz cases =
    let open Encoding in
    let read_cases =
      TzList.filter_map
        (function
          | (Case { tag = Json_only }) -> None
          | (Case { encoding = e ; inj ; tag = Tag tag }) ->
              let read = r.read e in
              Some (tag, fun len buf ofs ->
                  let ofs, v = read len buf ofs in
                  ofs, inj v))
        cases in
    fun buf ofs len ->
      let ofs, tag = read_tag sz buf ofs len in
      try List.assoc tag read_cases buf ofs (len - Size.tag_size sz)
      with Not_found -> raise (Unexpected_tag tag)

end

let rec read_rec : type a. a Encoding.t-> MBytes.t -> int -> int -> int * a = fun e ->
  let open Encoding in
  let open Reader in
  match e.encoding with
  | Null -> (fun _buf ofs _len -> ofs, ())
  | Empty -> (fun _buf ofs _len -> ofs, ())
  | Constant _ -> (fun _buf ofs _len -> ofs, ())
  | Ignore -> (fun _buf ofs len -> ofs + len, ())
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
      (fun buf ofs alpha ->
         let ofs, value =
           match Size.range_to_size ~minimum ~maximum with
           | `Int8 -> int8 buf ofs alpha
           | `Int16 -> int16 buf ofs alpha
           | `Int31 -> int31 buf ofs alpha
           | `Uint8 -> uint8 buf ofs alpha
           | `Uint16 -> uint16 buf ofs alpha
           | `Uint30 -> uint30 buf ofs alpha in
         let value = if minimum > 0 then value + minimum else value in
         if value < minimum || value > maximum
         then raise (Int_out_of_range (value, minimum, maximum)) ;
         (ofs, value))
  | Float -> float
  | RangedFloat { minimum ; maximum } ->
      (fun buf ofs len ->
         let offset, value = float buf ofs len in
         if value < minimum || value > maximum
         then raise (Float_out_of_range (value, minimum, maximum)) ;
         (offset, value))
  | Bytes (`Fixed n) -> fixed_length_bytes n
  | String (`Fixed n) -> fixed_length_string n
  | Bytes `Variable -> fun buf ofs len -> fixed_length_bytes len buf ofs len
  | String `Variable -> fun buf ofs len -> fixed_length_string len buf ofs len
  | String_enum (_, arr) -> begin
      fun buf ofs a ->
        let ofs, ind =
          match Size.enum_size arr with
          | `Uint8 -> uint8 buf ofs a
          | `Uint16 -> uint16 buf ofs a
          | `Uint30 -> uint30 buf ofs a in
        if ind >= Array.length arr
        then raise No_case_matched
        else (ofs, arr.(ind))
    end
  | Array e -> array (read_rec e)
  | List e -> list (read_rec e)
  | Obj (Req (_, e)) -> read_rec e
  | Obj (Opt (`Dynamic, _, t)) ->
      let read = read_rec t in
      (fun buf ofs len ->
         let ofs, v = int8 buf ofs len in
         if v = 0 then ofs, None
         else let ofs, v = read buf ofs (len - Size.int8) in ofs, Some v)
  | Obj (Opt (`Variable, _, t)) ->
      let read = read_rec t in
      (fun buf ofs len ->
         if len = 0 then ofs, None
         else
           let ofs', v = read buf ofs len in
           assert (ofs' = ofs + len) ;
           ofs + len, Some v)
  | Obj (Dft (_, e, _)) -> read_rec e
  | Objs ((`Fixed _ | `Dynamic), e1, e2) ->
      seq (read_rec e1) (read_rec e2)
  | Objs (`Variable, e1, e2) ->
      varseq { read = fun t -> read_rec t } e1 e2
  | Tup e -> read_rec e
  | Tups ((`Fixed _ | `Dynamic), e1, e2) ->
      seq (read_rec e1) (read_rec e2)
  | Tups (`Variable, e1, e2) ->
      varseq { read = fun t -> read_rec t } e1 e2
  | Conv { inj ; encoding = e } -> conv inj (read_rec e)
  | Describe { encoding = e } -> read_rec e
  | Def { encoding = e } -> read_rec e
  | Splitted { encoding = e } -> read_rec e
  | Union (_, sz, cases) ->
      union { read = fun t -> read_rec t } sz cases
  | Mu (_, _, self) -> fun buf ofs len -> read_rec (self e) buf ofs len
  | Dynamic_size e ->
      let read = read_rec e in
      fun buf ofs len ->
        let ofs, sz = int32 buf ofs len in
        let sz = Int32.to_int sz in
        if sz < 0 then raise (Invalid_size sz);
        read buf ofs sz
  | Delayed f -> read_rec (f ())

let read t buf ofs len =
  try Some (read_rec t buf ofs len)
  with _ -> None
let write = write
let of_bytes_exn ty buf =
  let len = MBytes.length buf in
  let read_len, r = read_rec ty buf 0 len in
  if read_len <> len then
    failwith "Data_encoding.Binary.of_bytes_exn: remainig data" ;
  r
let of_bytes ty buf =
  try Some (of_bytes_exn ty buf)
  with _ -> None
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
