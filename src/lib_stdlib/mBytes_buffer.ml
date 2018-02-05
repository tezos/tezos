(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = { mutable buffer : MBytes.t ;
           mutable offset : int }

let create ?(initial_size=4096) () =
  if initial_size <= 0
  then invalid_arg "MBytes_buffer size must be greater than zero" ;
  { buffer = MBytes.create initial_size ;
    offset = 0 }

let resize ?at_least_size buf =
  let new_buf =
    MBytes.create
      (2 * (MBytes.length buf.buffer + Option.unopt ~default:0 at_least_size)) in
  MBytes.blit buf.buffer 0 new_buf 0 buf.offset ;
  buf.buffer <- new_buf

let resize_if_necessary buf bytes =
  if buf.offset + bytes > MBytes.length buf.buffer
  then resize ~at_least_size:bytes buf
  else ()

let write_mbytes dst src srcoff len =
  resize_if_necessary dst len ;
  MBytes.blit src srcoff dst.buffer dst.offset len ;
  dst.offset <- dst.offset + len

let write_char buf char =
  resize_if_necessary buf 1 ;
  MBytes.set_char buf.buffer buf.offset char ;
  buf.offset <- buf.offset + 1

let write_int8 buf int =
  resize_if_necessary buf 1 ;
  MBytes.set_int8 buf.buffer buf.offset int ;
  buf.offset <- buf.offset + 1

let write_int16 buf int =
  resize_if_necessary buf 2 ;
  MBytes.set_int16 buf.buffer buf.offset int ;
  buf.offset <- buf.offset + 2

let write_int32 buf int =
  resize_if_necessary buf 4 ;
  MBytes.set_int32 buf.buffer buf.offset int ;
  buf.offset <- buf.offset + 4

let write_int64 buf int =
  resize_if_necessary buf 8 ;
  MBytes.set_int64 buf.buffer buf.offset int ;
  buf.offset <- buf.offset + 8

let write_float buf float =
  resize_if_necessary buf 4 ;
  MBytes.set_float buf.buffer buf.offset float ;
  buf.offset <- buf.offset + 4

let write_double buf float =
  resize_if_necessary buf 8 ;
  MBytes.set_double buf.buffer buf.offset float ;
  buf.offset <- buf.offset + 8

let write_string_data buf str =
  let len = String.length str in
  resize_if_necessary buf len ;
  MBytes.blit_from_string str 0 buf.buffer buf.offset len ;
  buf.offset <- buf.offset + len

let write_sized buffer (writer : unit -> unit) =
  let initial_offset = buffer.offset in
  write_int32 buffer Int32.zero ;
  writer () ;
  let ending_offset = buffer.offset in
  let size = ending_offset - initial_offset - 4 in
  let size32 = Int32.of_int size in
  if (Int32.to_int size32) <> size
  then failwith "Tried to write more than [Int32.max_int] bytes in sized location"
  else MBytes.set_int32 buffer.buffer initial_offset size32

let to_mbytes buf =
  MBytes.sub buf.buffer 0 buf.offset
