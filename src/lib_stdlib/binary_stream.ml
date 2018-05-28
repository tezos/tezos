(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Facilities to decode streams of binary data *)

type buffer = {
  buffer : MBytes.t ;
  ofs : int ;
  len : int ;
}

type t = {
  current : buffer ;
  (* buffer queue (classical double list implementation) *)
  pending : MBytes.t list ;
  pending_rev : MBytes.t list ;
  (* number unread bytes in 'current + pending + pending_rev' *)
  unread : int ;
}

let is_empty { unread ; _ } = unread = 0

let of_buffer current =
  { current ;
    pending = [] ;
    pending_rev = [] ;
    unread = current.len }

let of_bytes buffer =
  let len = MBytes.length buffer in
  of_buffer { buffer ; ofs = 0 ; len }

let empty = of_bytes (MBytes.create 0)

let push buffer stream =
  { stream with pending_rev = buffer :: stream.pending_rev ;
                unread = stream.unread + MBytes.length buffer }

exception Need_more_data

let split buffer len =
  assert (len <= buffer.len) ;
  { buffer with len },
  { buffer with ofs = buffer.ofs + len ; len = buffer.len - len }

let read stream len =
  if len > stream.unread then raise Need_more_data ;
  if len <= stream.current.len then
    let res, current = split stream.current len in
    res, { stream with current ; unread = stream.unread - len }
  else
    let res = { buffer = MBytes.create len ; ofs = 0 ; len } in
    MBytes.blit
      stream.current.buffer stream.current.ofs
      res.buffer 0
      stream.current.len ;
    let rec loop ofs pending_rev = function
      | [] -> loop ofs [] (List.rev pending_rev)
      | buffer :: pending ->
          let current = { buffer ; ofs = 0 ; len = MBytes.length buffer } in
          let to_read = len - ofs in
          if to_read <= current.len then begin
            MBytes.blit current.buffer 0 res.buffer ofs to_read ;
            res,
            { current = { current with ofs = to_read ;
                                       len = current.len - to_read } ;
              pending ;
              pending_rev ;
              unread = stream.unread - len ;
            }
          end else begin
            MBytes.blit current.buffer 0 res.buffer ofs current.len ;
            loop (ofs + current.len) pending_rev pending
          end in
    loop stream.current.len stream.pending_rev stream.pending
