(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Blake2b : sig
  type t
  type hash = Hash of Bigstring.t

  val init : ?key:Bigstring.t -> int -> t
  (** [init ?key size] is a blake2b context for hashes of size [size],
      using [key] if present. *)

  val update : t -> Bigstring.t -> unit
  (** [update t buf] updates [t] with the data in [buf]. *)

  val final : t -> hash
  (** [final t] is the blake2b hash of all data updated in [t] so
      far. *)

  val direct : ?key:Bigstring.t -> Bigstring.t -> int -> hash
  (** [direct ?key inbuf len] is the blake2b hash of length [len],
      using [key] is present. *)
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
