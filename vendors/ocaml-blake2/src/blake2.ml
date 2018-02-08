(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Blake2b = struct
  type t = Cstruct.t

  external sizeof_state : unit -> int =
    "sizeof_blake2b_state" [@@noalloc]

  let bytes = sizeof_state ()

  external init : Cstruct.buffer -> int -> int =
    "ml_blake2b_init" [@@noalloc]

  external outlen : Cstruct.buffer -> int =
    "blake2b_state_outlen" [@@noalloc]

  let outlen t = outlen t.Cstruct.buffer

  external init_key : Cstruct.buffer -> int -> Cstruct.buffer -> int =
    "ml_blake2b_init_key" [@@noalloc]

  external update : Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_blake2b_update" [@@noalloc]

  external final : Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_blake2b_final" [@@noalloc]

  external direct :
    Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> int =
    "ml_blake2b" [@@noalloc]

  let or_fail ~msg f =
    match f () with
    | 0 -> ()
    | _ -> failwith msg

  let init ?key size =
    if size < 1 || size > 64 then
      invalid_arg "Blake2b.init: size must be between 1 and 64" ;
    let t = Cstruct.create_unsafe bytes in
    begin match key with
      | Some key ->
          or_fail ~msg:"Blake2b.init"
            (fun () -> init_key t.buffer size key.Cstruct.buffer)
      | None ->
          or_fail ~msg:"Blake2b.init"
            (fun () -> init t.buffer size)
    end ;
    t

  let update t buf =
    or_fail ~msg:"Blake2b.update"
      (fun () -> update t.Cstruct.buffer buf.Cstruct.buffer)

  type hash = Hash of Cstruct.t

  let final t =
    let len = outlen t in
    let buf = Cstruct.create_unsafe len in
    or_fail ~msg:"Blake2b.final"
      (fun () -> final t.Cstruct.buffer buf.Cstruct.buffer) ;
    Hash buf

  let direct ?(key=Cstruct.create_unsafe 0) inbuf len =
    if len < 1 || len > 64 then
      invalid_arg "Blake2b.direct: size must be between 1 and 64" ;
    let outbuf = Cstruct.create len in
    or_fail ~msg:"Blake2b.direct"
      (fun () -> direct outbuf.Cstruct.buffer
          inbuf.Cstruct.buffer key.buffer) ;
    Hash outbuf
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

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
