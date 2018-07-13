(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type cmd = Apdu_command : {
    cmd : 'a ;
    cla_of_cmd : 'a -> int ;
    ins_of_cmd : 'a -> int ;
  } -> cmd

let create_cmd ~cmd ~cla_of_cmd ~ins_of_cmd =
  Apdu_command { cmd ; cla_of_cmd ; ins_of_cmd }

type t = {
  cmd : cmd ;
  p1 : int ;
  p2 : int ;
  lc : int ;
  le : int ;
  data : Cstruct.t ;
}

let max_data_length = 230

let create ?(p1=0) ?(p2=0) ?(lc=0) ?(le=0) ?(data=Cstruct.create 0) cmd =
  { cmd ; p1 ; p2 ; lc ; le ; data }

let create_string ?(p1=0) ?(p2=0) ?(lc=0) ?(le=0) ?(data="") cmd =
  let data = Cstruct.of_string data in
  { cmd ; p1 ; p2 ; lc ; le ; data }

let length { data ; _ } = 5 + Cstruct.len data

let write cs { cmd = Apdu_command { cmd ; cla_of_cmd ; ins_of_cmd } ;
               p1 ; p2 ; lc ; le ; data } =
  let len = match lc, le with | 0, _ -> le | _ -> lc in
  let datalen = Cstruct.len data in
  Cstruct.set_uint8 cs 0 (cla_of_cmd cmd) ;
  Cstruct.set_uint8 cs 1 (ins_of_cmd cmd) ;
  Cstruct.set_uint8 cs 2 p1 ;
  Cstruct.set_uint8 cs 3 p2 ;
  Cstruct.set_uint8 cs 4 len ;
  Cstruct.blit data 0 cs 5 datalen ;
  Cstruct.shift cs (5 + datalen)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
