(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The type of a single datum in a network frame. The encoding of a
    datum is as follows: [[TYPE][CONTENTS]], where [[type]] is a
    single byte whose value is [1] for [S], [2] for [I], [3] for [L],
    [4] for B, [5] for [D], [6] for [F] and [7] for [C].
    For [S]. [I], [L] and [D]¸ the raw values are stored using big
    endianness. For [B], [F] and [C], the size is prefixed as a 16-bit,
    big endian, unsigned integer
    ([[SIZE][BYTES]]). *)
type chunk =
  | S of int (** A 16-bit integer *)
  | I of int32 (** A 32-bit integer *)
  | L of int64 (** A 64-bit integer *)
  | B of MBytes.t (** A series of bytes *)
  | D of float (** A 64-bits IEEE-754 floating point number *)
  | F of frame (** An encapsulated subframe *)
  | C of string (** A string *)

(** A network frame is a list of simple data. Its encoding on the
    network is as follows: [[SIZE][DATA]] where [[SIZE]] is the raw
    length of [[DATA]] in bytes as a big endian, 32-bit, unsigned
    integer. *)
and frame =
  chunk list

(** Pretty printing of frames for debugging *)
let rec print fmtr frame =
  Format.fprintf fmtr "[@[<hv 2>" ;
  let rec loop frame =
    let sep = match frame with [ _ ] -> "" | _ -> " ;" in
    match frame with
    | [] -> ()
    | e :: tl ->
        begin match e with
          | S i -> Format.fprintf fmtr "@ S %i%s@," i sep
          | I i -> Format.fprintf fmtr "@ I %li%s@," i sep
          | L i -> Format.fprintf fmtr "@ L %Li%s@," i sep
          | D i -> Format.fprintf fmtr "@ D %g%s@," i sep
          | B i -> Format.fprintf fmtr "@ B %S%s@," (MBytes.to_string i) sep
          | F f -> Format.fprintf fmtr "@ F %a%s@," print f sep
          | C s -> Format.fprintf fmtr "@ C %s%s@," s sep
        end ;
        loop tl
  in loop frame ;
  Format.fprintf fmtr "@ @]]%!"

(** Pretty prints of frames *)
let to_string frame =
  let buf = Buffer.create 100 in
  let fmtr = Format.formatter_of_buffer buf in
  print fmtr frame ;
  Buffer.contents buf

module BE = EndianBigstring.BigEndian

(** Encode a frame as raw bytes to send over the network *)
let to_raw frame =
  let rec raw_size frame =
    List.fold_left
      (fun sz item -> sz + 1 + match item with
         | S _ -> 2
         | I _ -> 4
         | L _ -> 8
         | D _ -> 8
         | F f -> raw_size f + 2
         | B str -> MBytes.length str + 2
         | C str -> String.length str + 2)
      0 frame
  in
  let sz = raw_size frame in
  let buf = MBytes.create (sz + 4) in
  let rec store items offset = match items with
    | S n :: tl ->
      BE.set_int8 buf offset 0x01 ;
      BE.set_int16 buf (offset + 1) n ;
      store tl (offset + 1 + 2)
    | I n :: tl ->
      BE.set_int8 buf offset 0x02 ;
      BE.set_int32 buf (offset + 1) n ;
      store tl (offset + 1 + 4)
    | L n :: tl ->
      BE.set_int8 buf offset 0x03 ;
      BE.set_int64 buf (offset + 1) n ;
      store tl (offset + 1 + 8)
    | B n :: tl ->
      BE.set_int8 buf offset 0x04 ;
      let len = MBytes.length n in
      BE.set_int16 buf (offset + 1) len ;
      MBytes.blit n 0 buf (offset + 1 + 2) len ;
      store tl (offset + 1 + 2 + len)
    | D n :: tl ->
      BE.set_int8 buf offset 0x05 ;
      BE.set_int64 buf (offset + 1) (Int64.bits_of_float n) ;
      store tl (offset + 1 + 8)
    | F f :: tl ->
      BE.set_int8 buf offset 0x06 ;
      let len = raw_size f in
      BE.set_int16 buf (offset + 1) len ;
      let offset = store f (offset + 1 + 2) in
      store tl offset
    | C n :: tl ->
      BE.set_int8 buf offset 0x07 ;
      let len = String.length n in
      BE.set_int16 buf (offset + 1) len ;
      MBytes.blit_from_string n 0 buf (offset + 1 + 2) len ;
      store tl (offset + 1 + 2 + len)
    | [] -> offset
  in
  BE.set_int32 buf 0 (Int32.of_int sz) ;
  ignore (store frame 4) ;
  buf

(** Decode a complete raw frame as read from the network *)
let of_raw buf =
  let rec decode items offset stop =
    let if_remains ofs sz cb = if ofs + sz <= stop then cb () else None in
    if offset = stop then Some (List.rev items)
    else if offset > stop then None
    else
      let tag = BE.get_int8 buf offset in
      let offset = offset + 1 in
      match tag with
      | 0x01 ->
          if_remains offset 2 @@ fun () ->
          let items = S (BE.get_int16 buf offset) :: items in
          decode items (offset + 2) stop
      | 0x02 ->
          if_remains offset 4 @@ fun () ->
          let items = I (BE.get_int32 buf offset) :: items in
          decode items (offset + 4) stop
      | 0x03 ->
          if_remains offset 8 @@ fun () ->
          let items = L (BE.get_int64 buf offset) :: items in
          decode items (offset + 8) stop
      | 0x04 ->
          if_remains offset 2 @@ fun () ->
          let len = BE.get_int16 buf offset in
          let offset = offset + 2 in
          if_remains offset len @@ fun () ->
          let items = B (MBytes.sub buf offset len) :: items in
          decode items (offset + len) stop
      | 0x05 ->
          if_remains offset 8 @@ fun () ->
          let items = D (Int64.float_of_bits (BE.get_int64 buf offset)) :: items in
          decode items (offset + 8) stop
      | 0x06 ->
          if_remains offset 2 @@ fun () ->
          let len = BE.get_int16 buf offset in
          let offset = offset + 2 in
          if_remains offset len @@ fun () ->
          begin match decode [] offset (offset + len) with
            | None -> None
            | Some fitems -> decode ((F fitems) :: items) (offset + len) stop
          end
      | 0x07 ->
          if_remains offset 2 @@ fun () ->
          let len = BE.get_int16 buf offset in
          let offset = offset + 2 in
          if_remains offset len @@ fun () ->
          let items = C (MBytes.substring buf offset len) :: items in
          decode items (offset + len) stop
      | _ -> None
  in
  decode [] 4 (MBytes.length buf)

open Lwt

(** Write a frame from to file descriptor. *)
let write descr frame =
  let buf = to_raw frame in
  catch
    (fun () ->
       Lwt_bytes.write descr buf 0 (MBytes.length buf) >>= fun _ ->
       return true)
    (function
      | Unix.Unix_error _ -> return false
      | e -> fail e)

(** Read a frame from a file descriptor. *)
let read descr limit =
  catch
    (fun () ->
       let szbuf = MBytes.create 4 in
       Lwt_bytes.recv descr szbuf 0 4 [ Lwt_unix.MSG_PEEK ] >>= fun wsz ->
       if wsz <> 4 then
         return None
       else
         let len = Int32.to_int (BE.get_int32 szbuf 0) + 4 in
         if len < 0 || len > limit then
           return None
         else
           let buf = MBytes.create len in
           Lwt_bytes.read descr buf 0 len >>= fun wsz ->
           if wsz <> len then
             return None
           else
             return (of_raw buf))
    (function
      | Unix.Unix_error (_err, _, _) -> return None
      | e -> fail e)
