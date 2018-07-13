(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Blake2b = struct
  type t = Bigstring.t

  external sizeof_state : unit -> int =
    "sizeof_blake2b_state" [@@noalloc]

  let bytes = sizeof_state ()

  external init : Bigstring.t -> int -> int =
    "ml_blake2b_init" [@@noalloc]

  external outlen : Bigstring.t -> int =
    "blake2b_state_outlen" [@@noalloc]

  let outlen t = outlen t

  external init_key : Bigstring.t -> int -> Bigstring.t -> int =
    "ml_blake2b_init_key" [@@noalloc]

  external update : Bigstring.t -> Bigstring.t -> int =
    "ml_blake2b_update" [@@noalloc]

  external final : Bigstring.t -> Bigstring.t -> int =
    "ml_blake2b_final" [@@noalloc]

  external direct :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> int =
    "ml_blake2b" [@@noalloc]

  let or_fail ~msg f =
    match f () with
    | 0 -> ()
    | _ -> failwith msg

  let init ?key size =
    if size < 1 || size > 64 then
      invalid_arg "Blake2b.init: size must be between 1 and 64" ;
    let t = Bigstring.create bytes in
    begin match key with
      | Some key ->
          or_fail ~msg:"Blake2b.init"
            (fun () -> init_key t size key)
      | None ->
          or_fail ~msg:"Blake2b.init"
            (fun () -> init t size)
    end ;
    t

  let update t buf =
    or_fail ~msg:"Blake2b.update" (fun () -> update t buf)

  type hash = Hash of Bigstring.t

  let final t =
    let len = outlen t in
    let buf = Bigstring.create len in
    or_fail ~msg:"Blake2b.final" (fun () -> final t buf) ;
    Hash buf

  let direct ?(key=Bigstring.create 0) inbuf len =
    if len < 1 || len > 64 then
      invalid_arg "Blake2b.direct: size must be between 1 and 64" ;
    let outbuf = Bigstring.create len in
    or_fail ~msg:"Blake2b.direct" (fun () -> direct outbuf inbuf key) ;
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
