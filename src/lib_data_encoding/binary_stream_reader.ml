(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Binary_error

let raise e = raise (Read_error e)

(** Persistent state of the binary reader. *)
type state = {

  stream : Binary_stream.t ;
  (** All the remaining data to be read. *)

  remaining_bytes : int option ;
  (** Total number of bytes that should be from 'stream' (None =
      illimited). Reading less bytes should raise [Extra_bytes] and
      trying to read more bytes should raise [Not_enough_data]. *)

  total_read : int ;
  (** Total number of bytes that has been read from [stream] since the
      beginning. *)

}

(** Return type for the function [read_rec]. See [Data_encoding] for its
    description. *)
type 'ret status =
  | Success of { result : 'ret ; size : int ; stream : Binary_stream.t }
  | Await of (MBytes.t -> 'ret status)
  | Error of read_error

let check_remaining_bytes state size =
  match state.remaining_bytes with
  | Some len when len < size -> raise Not_enough_data
  | Some len -> Some (len - size)
  | None -> None

(** [read_atom resume size conv state k] reads [size] bytes from [state],
    pass it to [conv] to be decoded, and finally call the continuation [k]
    with the decoded value and the updated state.

    The function [conv] is also allowed to raise [Read_error err].
    In that case the exception is catched and [Error err] is returned.

    If there is not enough [remaining_bytes] to be read in [state], the
    function returns [Error Not_enough_data] instead of calling
    the continuation.

    If there is not enough [allowed_bytes] to be read in [state], the
    function returns [Error Size_limit_exceeded] instead of calling
    the continuation.

    If there is not enough bytes to be read in [state], the function
    returns [Await resume] instead of calling the continuation. *)
let read_atom resume size conv state k =
  match
    let remaining_bytes = check_remaining_bytes state size in
    let res, stream = Binary_stream.read state.stream size in
    conv res.buffer res.ofs,
    { remaining_bytes ; stream ;
      total_read = state.total_read + size }
  with
  | exception (Read_error error) -> Error error
  | exception Binary_stream.Need_more_data -> Await resume
  | v -> k v (* tail call *)

(** Reader for all the atomic types. *)
module Atom = struct

  let uint8 r = read_atom r Binary_size.uint8 MBytes.get_uint8
  let uint16 r = read_atom r Binary_size.int16 MBytes.get_uint16

  let int8 r = read_atom r Binary_size.int8 MBytes.get_int8
  let int16 r = read_atom r Binary_size.int16 MBytes.get_int16
  let int32 r = read_atom r Binary_size.int32 MBytes.get_int32
  let int64 r = read_atom r Binary_size.int64 MBytes.get_int64

  let float r = read_atom r Binary_size.float MBytes.get_double

  let bool resume state k =
    int8 resume state @@ fun (v, state) ->
    k (v <> 0, state)

  let uint30 r =
    read_atom r Binary_size.uint30 @@ fun buffer ofs ->
    let v = Int32.to_int (MBytes.get_int32 buffer ofs) in
    if v < 0 then
      raise (Invalid_int { min = 0 ; v ; max = (1 lsl 30) - 1 }) ;
    v

  let int31 r =
    read_atom r Binary_size.int31 @@ fun buffer ofs ->
    Int32.to_int (MBytes.get_int32 buffer ofs)

  let ranged_int ~minimum ~maximum resume state k =
    let read_int =
      match Binary_size.range_to_size ~minimum ~maximum with
      | `Int8 -> int8
      | `Int16 -> int16
      | `Int31 -> int31
      | `Uint8 -> uint8
      | `Uint16 -> uint16
      | `Uint30 -> uint30 in
    read_int resume state @@ fun (ranged, state) ->
    let ranged = if minimum > 0 then ranged + minimum else ranged in
    if not (minimum <= ranged && ranged <= maximum) then
      Error (Invalid_int { min = minimum ; v =ranged ; max = maximum })
    else
      k (ranged, state)

  let ranged_float ~minimum ~maximum resume state k =
    float resume state @@ fun (ranged, state) ->
    if not (minimum <= ranged && ranged <= maximum) then
      Error (Invalid_float { min = minimum ; v = ranged ; max = maximum })
    else
      k (ranged, state)

  let z resume state k =
    let res = Buffer.create 100 in
    uint8 resume state @@ fun (first, state) ->
    if first = 0 then
      k (Z.zero, state)
    else
      let first_value = first land 0x3F in
      let sign = (first land 0x40) <> 0 in
      let rec read prev value bit state =
        if prev land 0x80 = 0x00 then begin
          if bit > 0 then Buffer.add_char res (Char.unsafe_chr value) ;
          if prev = 0x00 then raise Trailing_zero ;
          let bits = Buffer.contents res in
          let res = Z.of_bits bits in
          let res = if sign then Z.neg res else res in
          k (res, state)
        end else
          let resume buffer =
            let stream = Binary_stream.push buffer state.stream in
            uint8 resume { state with stream } (read_next value bit) in
          uint8 resume state (read_next value bit)
      and read_next value bit (byte, state) =
        let value = value lor ((byte land 0x7F) lsl bit) in
        let bit = bit + 7 in
        let bit, value =
          if bit >= 8 then begin
            Buffer.add_char res (Char.unsafe_chr (value land 0xFF)) ;
            bit - 8, value lsr 8
          end else
            bit, value in
        read byte value bit state in
      read first first_value 6 state

  let string_enum arr resume state k =
    let read_index =
      match Binary_size.enum_size arr with
      | `Uint8 -> uint8
      | `Uint16 -> uint16
      | `Uint30 -> uint30 in
    read_index resume state @@ fun (index, state) ->
    if index >= Array.length arr then
      Error No_case_matched
    else
      k (arr.(index), state)

  let fixed_length_bytes length r =
    read_atom r length @@ fun buf ofs ->
    MBytes.sub buf ofs length

  let fixed_length_string length r =
    read_atom r length @@ fun buf ofs ->
    MBytes.sub_string buf ofs length

  let tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16

end

(** Main recursive reading function, in continuation passing style. *)
let rec read_rec
  : type next ret.
    next Encoding.t -> state -> ((next * state) -> ret status) -> ret status
  = fun e state k ->
    let resume buffer =
      let stream = Binary_stream.push buffer state.stream in
      try read_rec e { state with stream }k
      with Read_error err -> Error err in
    let open Encoding in
    assert (Encoding.classify e <> `Variable || state.remaining_bytes <> None) ;
    match e.encoding with
    | Null -> k ((), state)
    | Empty -> k ((), state)
    | Constant _ -> k ((), state)
    | Ignore -> k ((), state)
    | Bool   -> Atom.bool resume state k
    | Int8   -> Atom.int8 resume state k
    | Uint8  -> Atom.uint8 resume state k
    | Int16  -> Atom.int16 resume state k
    | Uint16 -> Atom.uint16 resume state k
    | Int31  -> Atom.int31 resume state k
    | Int32  -> Atom.int32 resume state k
    | Int64  -> Atom.int64 resume state k
    | Z -> Atom.z resume state k
    | Float -> Atom.float resume state k
    | Bytes (`Fixed n) -> Atom.fixed_length_bytes n resume state k
    | Bytes `Variable ->
        let size = remaining_bytes state in
        Atom.fixed_length_bytes size resume state k
    | String (`Fixed n) -> Atom.fixed_length_string n resume state k
    | String `Variable ->
        let size = remaining_bytes state in
        Atom.fixed_length_string size resume state k
    | RangedInt { minimum ; maximum }  ->
        Atom.ranged_int ~minimum ~maximum resume state k
    | RangedFloat { minimum ; maximum } ->
        Atom.ranged_float ~minimum ~maximum resume state k
    | String_enum (_, arr) ->
        Atom.string_enum arr resume state k
    | Array e ->
        read_list e state @@ fun (l, state) ->
        k (Array.of_list l, state)
    | List e -> read_list e state k
    | (Obj (Req (_, e))) -> read_rec e state k
    | (Obj (Dft (_, e, _))) -> read_rec e state k
    | (Obj (Opt (`Dynamic, _, e))) ->
        Atom.bool resume state @@ fun (present, state) ->
        if not present then
          k (None, state)
        else
          read_rec e state @@ fun (v, state) ->
          k (Some v, state)
    | (Obj (Opt (`Variable, _, e))) ->
        let size = remaining_bytes state in
        if size = 0 then
          k (None, state)
        else
          read_rec e state @@ fun (v, state) ->
          k (Some v, state)
    | Objs (`Fixed sz, e1, e2) ->
        ignore (check_remaining_bytes state sz : int option) ;
        read_rec e1 state @@ fun (left, state) ->
        read_rec e2 state @@ fun (right, state) ->
        k ((left, right), state)
    | Objs (`Dynamic, e1, e2) ->
        read_rec e1 state @@ fun (left, state) ->
        read_rec e2 state @@ fun (right, state) ->
        k ((left, right), state)
    | (Objs (`Variable, e1, e2)) ->
        read_variable_pair e1 e2 state k
    | Tup e -> read_rec e state k
    | Tups (`Fixed sz, e1, e2) ->
        ignore (check_remaining_bytes state sz : int option) ;
        read_rec e1 state @@ fun (left, state) ->
        read_rec e2 state @@ fun (right, state) ->
        k ((left, right), state)
    | Tups (`Dynamic, e1, e2) ->
        read_rec e1 state @@ fun (left, state) ->
        read_rec e2 state @@ fun (right, state) ->
        k ((left, right), state)
    | (Tups (`Variable, e1, e2)) ->
        read_variable_pair e1 e2 state k
    | Conv { inj ; encoding } ->
        read_rec encoding state @@ fun (v, state) ->
        k (inj v, state)
    | Union (_, sz, cases) -> begin
        Atom.tag sz resume state @@ fun (ctag, state) ->
        match
          List.find
            (function
              | Case { tag = Tag tag } -> tag = ctag
              | Case { tag = Json_only } -> false)
            cases
        with
        | exception Not_found -> Error (Unexpected_tag ctag)
        | Case { encoding ; inj } ->
            read_rec encoding state @@ fun (v, state) ->
            k (inj v, state)
      end
    | Dynamic_size e ->
        Atom.int32 resume state @@ fun (sz, state) ->
        let sz = Int32.to_int sz in
        if sz < 0 then
          Error (Invalid_size sz)
        else
          let remaining = check_remaining_bytes state sz in
          let state = { state with remaining_bytes = Some sz } in
          read_rec e state @@ fun (v, state) ->
          if state.remaining_bytes <> Some 0 then
            Error Extra_bytes
          else
            k (v, { state with remaining_bytes = remaining })
    | Describe { encoding = e } -> read_rec e state k
    | Def { encoding = e } -> read_rec e state k
    | Splitted { encoding = e } -> read_rec e state k
    | Mu (_, _, self) -> read_rec (self e) state k
    | Delayed f -> read_rec (f ()) state k

and remaining_bytes { remaining_bytes } =
  match remaining_bytes with
  | None ->
      (* This function should only be called with a variable encoding,
         for which the `remaining_bytes` should never be `None`. *)
      assert false
  | Some len -> len

and read_variable_pair
  : type left right ret.
    left Encoding.t -> right Encoding.t -> state ->
    (((left * right) * state) -> ret status) -> ret status
  = fun e1 e2 state k ->
    let size = remaining_bytes state in
    match Encoding.classify e1, Encoding.classify e2 with
    | (`Dynamic | `Fixed _), `Variable ->
        read_rec e1 state @@ fun (left, state) ->
        read_rec e2 state @@ fun (right, state) ->
        k ((left, right), state)
    | `Variable, `Fixed n ->
        if n > size then
          Error Not_enough_data
        else
          let state = { state with remaining_bytes = Some (size - n) } in
          read_rec e1 state @@ fun (left, state) ->
          assert (state.remaining_bytes = Some 0) ;
          let state = { state with remaining_bytes = Some n } in
          read_rec e2 state @@ fun (right, state) ->
          assert (state.remaining_bytes = Some 0) ;
          k ((left, right), state)
    | _ -> assert false (* Should be rejected by [Encoding.Kind.combine] *)

and read_list
  : type a ret.
    a Encoding.t -> state -> ((a list * state) -> ret status) -> ret status
  = fun e state k ->
    let rec loop state acc =
      let size = remaining_bytes state in
      if size = 0 then
        k (List.rev acc, state)
      else
        read_rec e state @@ fun (v, state) ->
        loop state (v :: acc) in
    loop state []

let read_rec e state k =
  try read_rec e state k
  with Read_error err -> Error err



(** ******************** *)
(** Various entry points *)

let success (v, state) =
  Success { result = v ; size = state.total_read ; stream = state.stream }

let read_stream ?(init = Binary_stream.empty) encoding =
  match Encoding.classify encoding with
  | `Variable ->
      invalid_arg "Data_encoding.Binary.read_stream: variable encoding"
  | `Dynamic | `Fixed _ ->
      (* No hardcoded read limit in a stream. *)
      let state = { remaining_bytes = None ;
                    stream = init ; total_read = 0 } in
      read_rec encoding state success
