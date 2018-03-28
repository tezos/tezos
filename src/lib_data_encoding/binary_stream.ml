
(* Facilities to decode streams of binary data *)

type 'a status =
  | Success of { res : 'a ; res_len : int ; remaining : MBytes.t list }
  | Await of (MBytes.t -> 'a status)
  | Error

(* used as a zipper to code the function read_checker with the
   ability to stop and wait for more data. In 'P_seq' case, data
   length is parameterized by the current offset. Hence, it's a
   function 'fun_data_len'. For the 'P_list' case, we store the
   base offset (before starting reading the elements) and the
   number of elements that have been read so far. *)
type path =
  | P_top : path
  | P_await : { path : path ; encoding : 'a Encoding.t ; data_len : int } -> path
  | P_seq : { path : path ; encoding : 'a Encoding.t ;
              fun_data_len : int -> int } -> path
  | P_list : { path:path ; encoding:'a Encoding.t ; data_len : int ;
               base_ofs : int ; nb_elts_read : int } -> path

(* used to accumulate given mbytes when reading a list of blocks,
   as well as the current offset and the number of unread bytes *)
type mbytes_stream = {
  past : MBytes.t Queue.t ; (* data that have been entirely read *)
  future : (MBytes.t * int) Queue.t ; (* data that are not (fully) read *)
  mutable past_len : int ; (*length of concatenation of data in 'past'*)
  mutable unread : int ;  (*number of cells that are unread in 'future'*)
  ofs : int (*current absolute offset wrt to concatenation past @ future*)
}

(* exception raised when additional mbytes are needed to continue
   decoding *)
exception Need_more_data of mbytes_stream

(* read a data that is stored in may Mbytes *)
let read_from_many_blocks reader buf ofs d_ofs =
  let tmp = MBytes.create d_ofs in (*we will merge data in this mbyte*)
  let r = ref d_ofs in (*to count the cells to be read*)
  let rel_ofs = ref ofs in (*= ofs for first mbyte, 0 for others*)
  while !r > 0 do
    assert (not (Queue.is_empty buf.future)) ;
    let b, len_b = Queue.peek buf.future in (*take the next mbyte*)
    let len_chunk = len_b - !rel_ofs in (*the number of cells to read*)
    if !r >= len_chunk then
      begin (*copy b in 'past' if it is read entirely*)
        ignore (Queue.pop buf.future) ;
        Queue.push b buf.past ;
        buf.past_len <- buf.past_len + len_b ;
      end ;
    (* copy (min !r len_chunk) data from b to tmp *)
    MBytes.blit b !rel_ofs tmp (d_ofs - !r) (min !r len_chunk) ;
    r := !r - len_chunk ; (* len_chunk data read during this round*)
    rel_ofs := 0 ; (*next mbytes will be read starting from zero*)
  done ;
  reader tmp 0 d_ofs


(* generic function that reads data from an mbytes_stream. It is
   parameterized by a function "reader" that effectively reads the
   data *)
let generic_read_data delta_ofs reader buf =
  let absolute_ofs  = buf.ofs in
  if buf.unread < delta_ofs then (*not enough data*)
    raise (Need_more_data buf) ;
  if delta_ofs = 0 then (*we'll read nothing*)
    buf, reader (MBytes.create 0) 0 0
  else
    let new_ofs = absolute_ofs + delta_ofs in
    let ofs = absolute_ofs - buf.past_len in (*relative ofs wrt 'future'*)
    buf.unread <- buf.unread-delta_ofs ; (*'delta_ofs' cells will be read*)
    assert (not (Queue.is_empty buf.future)) ; (*we have some data to read*)
    let b, len_b = Queue.peek buf.future in
    let buf = { buf with ofs = new_ofs } in
    if ofs + delta_ofs > len_b then
      (*should read data from many mbytes*)
      buf, read_from_many_blocks reader buf ofs delta_ofs
    else
      begin
        if ofs + delta_ofs = len_b then
          begin (*the rest of b will be entirely read. Put it in 'past'*)
            ignore (Queue.pop buf.future) ;
            Queue.push b buf.past ;
            buf.past_len <- buf.past_len + len_b ;
          end ;
        buf, reader b ofs delta_ofs
      end

open Encoding (* open here, shadow below, use shadowed definitions later *)

(* functions that try to read data from a given mbytes_stream,
   or raise Need_more_data *)

let int8 buf =
  generic_read_data Size.int8 (fun x y _ -> MBytes.get_int8 x y) buf

let uint8 buf =
  generic_read_data Size.uint8 (fun x y _ -> MBytes.get_uint8 x y) buf

let char buf =
  let buf, v = int8 buf in
  buf, Char.chr v

let bool buf =
  let buf, v = int8 buf in
  buf, v <> 0

let int16 buf =
  generic_read_data Size.int16 (fun x y _ -> MBytes.get_int16 x y) buf

let uint16 buf =
  generic_read_data Size.uint16 (fun x y _ -> MBytes.get_uint16 x y) buf

let uint30 buf =
  generic_read_data Size.uint30
    (fun x y _ ->
       let v = Int32.to_int (MBytes.get_int32 x y) in
       if v < 0 then
         failwith "Data_encoding.Binary.Reader.uint30: invalid data." ;
       v) buf

let int31 buf =
  generic_read_data Size.int31
    (fun x y _ -> Int32.to_int (MBytes.get_int32 x y)) buf

let int32 buf =
  generic_read_data Size.int32 (fun x y _ -> MBytes.get_int32 x y) buf

let int64 buf =
  generic_read_data Size.int64 (fun x y _ -> MBytes.get_int64 x y) buf

(** read a float64 (double) **)
let float buf =
  (*Here, float means float64, which is read using MBytes.get_double !!*)
  generic_read_data Size.float (fun x y _ -> MBytes.get_double x y) buf

let fixed_length_bytes length buf =
  generic_read_data length MBytes.sub buf

let fixed_length_string length buf =
  generic_read_data length MBytes.substring buf

let read_tag = function
  | `Uint8 -> uint8
  | `Uint16 -> uint16

(* auxiliary function: computing size of data in branches
   Objs(`Variable) and Tups(`Variable) *)
let varseq_lengths e1 e2 ofs len = match Encoding.classify e1, Encoding.classify e2 with
  | (`Dynamic | `Fixed _), `Variable -> len, (fun ofs' -> len - ofs' + ofs)
  | `Variable, `Fixed n -> (len - n), (fun _ -> n)
  | _ -> assert false (* Should be rejected by Kind.combine *)


(* adaptation of function read_rec to check binary data
   incrementally.  The function takes (and returns) a 'path' (for
   incrementality), and 'mbytes_stream' *)
let rec data_checker
  : type a.
    path -> a Encoding.t -> mbytes_stream -> int ->
    path * mbytes_stream =
  fun path e buf len ->
    (*length of data with `Variable kind should be given by the caller*)
    assert (Encoding.classify e != `Variable || len >= 0) ;
    try match e.encoding with
      | Null   -> next_path path buf
      | Empty  -> next_path path buf
      | Constant _ -> next_path path buf
      | Ignore -> next_path path { buf with ofs = buf.ofs + len }
      | Bool   -> next_path path (fst (bool   buf))
      | Int8   -> next_path path (fst (int8   buf))
      | Uint8  -> next_path path (fst (uint8  buf))
      | Int16  -> next_path path (fst (int16  buf))
      | Uint16 -> next_path path (fst (uint16 buf))
      | Int31  -> next_path path (fst (int31  buf))
      | Int32  -> next_path path (fst (int32  buf))
      | Int64  -> next_path path (fst (int64  buf))
      | Z ->
          let rec while_not_terminator i buf =
            let buf, byte = uint8 buf in
            if (byte land 0x80) = 0x00 then
              if byte = 0x00 && i <> 0 then
                failwith "trailing zeroes in Z encoding"
              else
                next_path path buf
            else
              while_not_terminator (i + 1) buf in
          while_not_terminator 0 buf
      | RangedInt { minimum ; maximum }  ->
          let (stream, ranged) =
            match Size.range_to_size ~minimum ~maximum with
            | `Int8 -> int8 buf
            | `Int16 -> int16 buf
            | `Int31 -> int31 buf
            | `Uint8 -> uint8 buf
            | `Uint16 -> uint16 buf
            | `Uint30 -> uint30 buf in
          let ranged = if minimum > 0 then ranged + minimum else ranged in
          assert (minimum < ranged && ranged < maximum) ;
          next_path path stream
      | Float  -> next_path path (fst (float buf))
      | RangedFloat { minimum ; maximum } ->
          let stream, float = float buf in
          assert (minimum < float && maximum > float) ;
          next_path path stream
      | Bytes (`Fixed n) ->
          next_path path (fst (fixed_length_bytes n buf))

      | String (`Fixed n) ->
          next_path path (fst (fixed_length_string n buf))

      | Bytes `Variable ->
          next_path path (fst (fixed_length_bytes len buf))

      | String `Variable ->
          next_path path (fst (fixed_length_string len buf))

      | String_enum (_, arr) ->
          next_path path
            (match Size.enum_size arr with
             | `Uint8 -> fst @@ uint8 buf
             | `Uint16 -> fst @@ uint16 buf
             | `Uint30 -> fst @@ uint30 buf)

      | Array e ->
          let p = P_list { path ; encoding = e ; base_ofs = buf.ofs ;
                           data_len = len ; nb_elts_read = 0 } in
          next_path p buf

      | List e ->
          let p = P_list { path ; encoding = e ; base_ofs = buf.ofs ;
                           data_len = len ; nb_elts_read = 0 } in
          next_path p buf

      | Obj (Req (_, e)) -> data_checker path e buf len

      | Obj (Opt (`Dynamic, _, e)) ->
          let buf, v = int8 buf in
          if v = 0 then next_path path buf
          else data_checker path e buf (len - Size.int8)

      | Obj (Opt (`Variable, _, e)) ->
          if len = 0 then next_path path buf
          else data_checker path e buf len

      | Obj (Dft (_, e, _)) -> data_checker path e buf len

      | Objs ((`Fixed _ | `Dynamic), e1, e2) ->
          let f_len2 ofs' = len - (ofs' - buf.ofs) in
          let path =
            P_seq { path ; encoding = e2 ; fun_data_len = f_len2 } in
          data_checker path e1 buf len

      | Objs (`Variable, e1, e2) ->
          let len1, f_len2 = varseq_lengths e1 e2 buf.ofs len in
          let path =
            P_seq { path ; encoding = e2 ; fun_data_len = f_len2 } in
          data_checker path e1 buf len1

      | Tup e -> data_checker path e buf len

      | Tups ((`Fixed _ | `Dynamic), e1, e2) ->
          let f_len2 ofs' = len - (ofs' - buf.ofs) in
          let path =
            P_seq { path ; encoding = e2 ; fun_data_len = f_len2 } in
          data_checker path e1 buf len

      | Tups (`Variable, e1, e2) ->
          let len1, f_len2 = varseq_lengths e1 e2 buf.ofs len in
          let path =
            P_seq { path ; encoding = e2 ; fun_data_len = f_len2 } in
          data_checker path e1 buf len1

      | Conv { encoding = e } -> data_checker path e buf len

      | Describe { encoding = e } -> data_checker path e buf len

      | Def { encoding = e } -> data_checker path e buf len

      | Splitted { encoding = e } -> data_checker path e buf len

      | Mu (_, _, self) -> data_checker path (self e) buf len

      | Union (_, sz, cases) ->
          let buf, ctag = read_tag sz buf in
          let opt =
            List.fold_left
              (fun acc c -> match c with
                 | (Case { encoding ; tag = Tag tag })
                   when tag == ctag ->
                     assert (acc == None) ;
                     Some (data_checker path encoding buf)
                 | _ -> acc
              )None cases
          in
          begin match opt with
            | None -> raise (Encoding.Unexpected_tag ctag)
            | Some func -> func (len - (Size.tag_size sz))
          end

      | Dynamic_size e ->
          let buf, sz = int32 buf in
          let sz = Int32.to_int sz in
          if sz < 0 then raise (Encoding.Invalid_size sz) ;
          data_checker path e buf sz

      | Delayed f -> data_checker path (f ()) buf len

    with Need_more_data buf ->
      P_await { path ; encoding = e ; data_len = len }, buf

and next_path : path -> mbytes_stream -> path * mbytes_stream =
  fun path buf ->
    match path with
    | P_top ->
        P_top, buf (* success case *)

    | P_seq { path ; encoding ; fun_data_len } ->
        (* check the right branch of a sequence. fun_data_len ofs gives
           the length of the data to read *)
        data_checker path encoding buf (fun_data_len buf.ofs)

    | P_await { path ; encoding ; data_len } ->
        (* resume from an await *)
        data_checker path encoding buf data_len

    | P_list
        ({ path ; encoding ; base_ofs ; data_len ; nb_elts_read } as r) ->
        (* read/check an eventual element of a list *)
        if data_len = buf.ofs - base_ofs then
          (* we've read all the elements of the list *)
          next_path path buf
        else
          begin
            (*some more elements to read*)
            assert (data_len > buf.ofs - base_ofs) ;
            (*check: if we've already read some elements, then currrent ofs
              should be greater then initial ofs *)
            assert (nb_elts_read <= 0 || buf.ofs - base_ofs > 0) ;
            let path =
              P_list { r with nb_elts_read = nb_elts_read + 1} in
            data_checker path encoding buf data_len
          end

let data_checker = next_path

(* insert a given MBytes.t in a given mbytes_stream *)
let insert_mbytes mb_buf mb =
  let len = MBytes.length mb in
  if len > 0 then begin
    Queue.push (mb, len) mb_buf.future ;
    mb_buf.unread <- mb_buf.unread + len ;
  end

(* aux function called when data_checker succeeds: splits a given
   mbytes_stream into a 'read' and 'unread' queues. This may
   modify the content of the given mbytes_stream *)
let split_mbytes_stream { past_len ; past ; future ; unread ; ofs } =
  let rel_ofs = ofs - past_len in
  assert (rel_ofs >= 0) ;
  if rel_ofs = 0 then past, future (* already done *)
  else begin
    assert (not(Queue.is_empty future)) ; (*because data_checker succeeded*)
    let b, len = Queue.pop future in
    assert (rel_ofs < len) ; (*inv. maintained by read_from_many_blocks*)
    let b1 = MBytes.sub b 0 rel_ofs in (* read part of b *)
    let b2 = MBytes.sub b rel_ofs (len-rel_ofs) in (* unread part of b *)
    Queue.push b1 past ;

    (* push b2 at the beginning of 'future' using Queue.transfer*)
    let tmp = Queue.create() in
    Queue.push (b2, unread) tmp ;
    Queue.transfer future tmp ; (*tmp === b2 ::: future in constant time*)
    past, tmp
  end

(* given a state, this function returns a new status:
   - if data are successfully checked, accumulated mbytes are
     passed to 'success_result' that computes the final
     result. Unread mbytes are also returned
   - if some more data are needed, a function that waits for some
     additional mbytes is returned
   - eventual errors are reported/returned *)
let rec bytes_stream_reader_rec (path, mb_buf) success_result =
  let success =
    match path with
    | P_top -> true
    | P_await _ -> false
    | _ -> assert false
  in
  assert (mb_buf.ofs >= mb_buf.past_len) ;
  if success then
    let q_read, q_unread = split_mbytes_stream mb_buf in
    match success_result q_read mb_buf.ofs with
    | Some a ->
        let remaining =
          List.rev @@
          Queue.fold
            (fun acc (b, len) ->
               if len = 0 then acc else b:: acc) [] q_unread
        in
        Success { res = a ; res_len = mb_buf.ofs ; remaining }
    | None -> Error
    (* success_result may fail because data_checker is
       approximative in some situations *)
  else
    Await
      (fun mb ->
         insert_mbytes mb_buf mb ;
         try
           let state = data_checker path mb_buf in
           bytes_stream_reader_rec state success_result
         with _ -> Error)

(* This function checks reading a stream of 'MBytes.t' wrt. a given
   encoding:
   - the given data encoding should have a 'Fixed' or a 'Dynamic'
   size, otherwise an error is returned,
   - the function returns an 'Error', a function w
   ('Await w') that waits for more data (Mbytes.t), or
   'Success'. The function is parameterized by 'success_result'
   that computes the data to return in case of success.
   An exception 'Invalid_argument "streaming data with variable
   size"' is raised if the encoding has a variable size *)
let bytes_stream_reader :
  MBytes.t list -> 'a t ->
  (MBytes.t Queue.t -> int -> 'b option) -> 'b status
  = fun l e success_result ->
    match classify e with
    | `Variable -> invalid_arg "streaming data with variable size"
    | `Fixed _ | `Dynamic ->
        let mb_buf = {
          past   = Queue.create() ; past_len = 0 ;
          future = Queue.create() ; unread = 0; ofs = 0 }
        in
        List.iter (insert_mbytes mb_buf) l ;
        let path =
          P_await { path = P_top ; encoding = e ; data_len = - 1 } in
        try bytes_stream_reader_rec (data_checker path mb_buf) success_result
        with _ -> Error

(* concats a queue of mbytes into one MByte *)
let concat_mbyte_chunks queue tot_len =
  if Queue.length queue = 1 then Queue.pop queue (* no copy *)
  else (* copy smaller mbytes into one big mbyte *)
    let buf = MBytes.create tot_len in
    let cpt = ref 0 in
    let tot_len' = ref tot_len in
    while not (Queue.is_empty queue) do
      let mb = Queue.pop queue in
      let len = MBytes.length mb in
      tot_len' := !tot_len' - len ;
      assert (!tot_len' >= 0) ;
      MBytes.blit mb 0 buf !cpt len ;
      cpt := !cpt + len ;
    done ;
    assert (!tot_len' = 0) ;
    buf

(* Decode a stream of MBytes. see
   Stream_reader.bytes_stream_traversal for more details *)
let read_stream_of_bytes ?(init=[]) encoding =
  bytes_stream_reader init encoding
    (fun read_q ofs -> Binary.of_bytes encoding (concat_mbyte_chunks read_q ofs))

(* Check reading a stream of MBytes. see
   Stream_reader.bytes_stream_traversal for more details *)
let check_stream_of_bytes ?(init=[]) encoding =
  bytes_stream_reader init encoding (fun _ _ -> Some ())
