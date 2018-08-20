(* Copyright 2018 Vincent Bernardoff, Marco Stronati.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open EndianBigstring

module Rand = struct
  external write : Bigstring.t -> unit =
    "ml_randombytes" [@@noalloc]

  let gen len =
    let buf = Bigstring.create len in
    write buf ;
    buf
end

module Hash = struct
  module type HASH = sig
    val init : Bigstring.t -> unit
    val update : Bigstring.t -> Bigstring.t -> unit
    val update_last : Bigstring.t -> Bigstring.t -> int -> unit
    val finish : Bigstring.t -> Bigstring.t -> unit

    val bytes : int
    val blockbytes : int
    val statebytes : int
  end

  module Make(S: HASH) = struct
    type state = {
      state : Bigstring.t ;
      buf : Bigstring.t ;
      mutable pos : int ;
    }

    let bytes = S.bytes
    let blockbytes = S.blockbytes
    let statebytes = S.statebytes

    let init () =
      let state = Bigstring.create S.statebytes in
      let buf = Bigstring.create S.blockbytes in
      S.init state ;
      { state ; buf ; pos = 0 }

    let rec update ({ state ; buf ; pos } as st) m p l =
      if pos + l < S.blockbytes then begin
        Bigstring.blit m p buf pos l ;
        st.pos <- st.pos + l
      end
      else begin
        let nb_consumed = S.blockbytes - pos in
        Bigstring.blit m p buf pos nb_consumed ;
        S.update state buf ;
        st.pos <- 0 ;
        update st m (p + nb_consumed) (l - nb_consumed)
      end

    let update st msg =
      update st msg 0 (Bigstring.length msg)

    let finish { state ; buf ; pos } =
      S.update_last state buf pos ;
      S.finish state buf ;
      Bigstring.sub buf 0 S.bytes

    let digest msg =
      let st = init () in
      update st msg ;
      finish st
  end

  module type S = sig
    type state

    val bytes : int
    val blockbytes : int
    val statebytes : int

    (** Incremental Interface *)

    val init : unit -> state
    val update : state -> Bigstring.t -> unit
    val finish : state -> Bigstring.t

    (** Direct Interface *)

    val digest : Bigstring.t -> Bigstring.t

    module HMAC : sig
      val write :
        key:Bigstring.t -> msg:Bigstring.t -> Bigstring.t -> unit
      (** @raise [Invalid_argument] if argument is less than 32 bytes long *)

      val digest :
        key:Bigstring.t -> msg:Bigstring.t -> Bigstring.t
    end
  end

  module SHA256 = struct
    module H = Make(struct
        (* state -> unit *)
        external init : Bigstring.t -> unit =
          "ml_Hacl_SHA2_256_init" [@@noalloc]

        (* state -> data -> unit *)
        external update : Bigstring.t -> Bigstring.t -> unit =
          "ml_Hacl_SHA2_256_update" [@@noalloc]

        (* state -> data -> datalen -> unit *)
        external update_last : Bigstring.t -> Bigstring.t -> int -> unit =
          "ml_Hacl_SHA2_256_update_last" [@@noalloc]

        (* state -> hash *)
        external finish : Bigstring.t -> Bigstring.t -> unit =
          "ml_Hacl_SHA2_256_finish" [@@noalloc]

        let bytes = 32
        let blockbytes = 64
        let statebytes = 137 * 4
      end)
    include H

    module HMAC = struct
      (* mac -> key -> data *)
      external hmac :
        Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
        "ml_Hacl_HMAC_SHA2_256_hmac" [@@noalloc]

      let write ~key ~msg buf =
        let buflen = Bigstring.length buf in
        if buflen < 32 then
          invalid_arg (Printf.sprintf "Hash.SHA256.HMAC.write: invalid \
                                       len (%d), expected %d" buflen bytes) ;
        hmac buf key msg

      let digest ~key ~msg =
        let buf = Bigstring.create 32 in
        write ~key ~msg buf ;
        buf
    end
  end

  module SHA512 = struct
    module H = Make(struct
        (* state -> unit *)
        external init : Bigstring.t -> unit =
          "ml_Hacl_SHA2_512_init" [@@noalloc]

        (* state -> data -> unit *)
        external update : Bigstring.t -> Bigstring.t -> unit =
          "ml_Hacl_SHA2_512_update" [@@noalloc]

        (* state -> data -> datalen -> unit *)
        external update_last : Bigstring.t -> Bigstring.t -> int -> unit =
          "ml_Hacl_SHA2_512_update_last" [@@noalloc]

        (* state -> hash *)
        external finish : Bigstring.t -> Bigstring.t -> unit =
          "ml_Hacl_SHA2_512_finish" [@@noalloc]

        let bytes = 64
        let blockbytes = 128
        let statebytes = 169 * 8
      end)
    include H

    module HMAC = struct
      let derive_key k =
        let buf = Bigstring.create blockbytes in
        Bigstring.fill buf '\x00' ;
        let keylen = Bigstring.length k in
        let k, keylen =
          if keylen > blockbytes then H.digest k, bytes else k, keylen in
        Bigstring.blit k 0 buf 0 keylen ;
        buf

      let xor_ipad =
        Bigstring.map ~f:(fun c -> Char.(chr ((code c) lxor 0x36)))
      let xor_opad =
        Bigstring.map ~f:(fun c -> Char.(chr ((code c) lxor 0x5c)))

      let digest ~key ~msg =
        let key = derive_key key in
        let preimage =
          Bigstring.concat "" [
            xor_opad key ;
            digest (Bigstring.concat "" [xor_ipad key ; msg])
          ] in
        digest preimage

      let write ~key ~msg buf =
        let buflen = Bigstring.length buf in
        if buflen < bytes then
          invalid_arg (Printf.sprintf "Hash.SHA512.HMAC.write: invalid \
                                       len (%d), expected %d" buflen bytes) ;
        let d = digest ~key ~msg in
        Bigstring.blit d 0 buf 0 bytes
    end
  end
end

module Nonce = struct
  type t = Bigstring.t
  let bytes = 24

  let gen () =
    Rand.gen bytes

  let rec incr_byte b step byteno =
    let res = BigEndian.get_uint16 b byteno + step in
    let lo = res land 0xffff in
    let hi = res asr 16 in
    BigEndian.set_int16 b byteno lo ;
    if hi = 0 || byteno = 0 then ()
    else incr_byte b hi (byteno - 2)

  let increment ?(step = 1) nonce =
    let new_nonce = Bigstring.create 24 in
    Bigstring.blit nonce 0 new_nonce 0 24 ;
    incr_byte new_nonce step 22 ;
    new_nonce

  let of_bytes buf =
    if Bigstring.length buf <> bytes then None else Some buf

  let of_bytes_exn buf =
    match of_bytes buf with
    | Some s -> s
    | None -> invalid_arg "Hacl.Nonce.of_bytes_exn: invalid length"

end

module Secretbox = struct
  type key = Bigstring.t

  let keybytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let unsafe_of_bytes buf =
    if Bigstring.length buf <> keybytes then
      invalid_arg (Printf.sprintf "Secretbox.unsafe_of_bytes: buffer \
                                   must be %d bytes long" keybytes) ;
    buf

  let blit_of_bytes buf pos =
    if Bigstring.length buf < keybytes then
      invalid_arg (Printf.sprintf "Secretbox.blit_of_bytes: buffer \
                                   must be at least %d bytes long" keybytes) ;
    let key = Bigstring.create keybytes in
    Bigstring.blit buf pos key 0 keybytes ;
    key

  let genkey () = Rand.gen 32

  (* c -> m -> n -> k -> unit *)
  external box :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_NaCl_crypto_secretbox_easy" [@@noalloc]

  (* m -> c -> mac -> n -> k -> int *)
  external box_open :
    Bigstring.t -> Bigstring.t -> Bigstring.t ->
    Bigstring.t -> Bigstring.t -> int =
    "ml_NaCl_crypto_secretbox_open_detached" [@@noalloc]

  let box ~key ~nonce ~msg ~cmsg =
    box cmsg msg nonce key

  let box_open ~key ~nonce ~cmsg ~msg =
    let mac = Bigstring.sub cmsg boxzerobytes boxzerobytes in
    match box_open msg cmsg mac nonce key with
    | 0 -> true
    | _ -> false
end

type secret
type public

module Box = struct
  type combined
  type _ key =
    | Sk : Bigstring.t -> secret key
    | Pk : Bigstring.t -> public key
    | Ck : Bigstring.t -> combined key

  let skbytes = 32
  let pkbytes = 32
  let ckbytes = 32
  let zerobytes = 32
  let boxzerobytes = 16

  let unsafe_to_bytes : type a. a key -> Bigstring.t = function
    | Pk buf -> buf
    | Sk buf -> buf
    | Ck buf -> buf

  let blit_to_bytes :
    type a. a key -> ?pos:int -> Bigstring.t -> unit = fun key ?(pos=0) buf ->
    match key with
    | Pk pk -> Bigstring.blit pk 0 buf pos pkbytes
    | Sk sk -> Bigstring.blit sk 0 buf pos skbytes
    | Ck ck -> Bigstring.blit ck 0 buf pos ckbytes

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Bigstring.equal a b
    | Sk a, Sk b -> Bigstring.equal a b
    | Ck a, Ck b -> Bigstring.equal a b

  let unsafe_sk_of_bytes buf =
    if Bigstring.length buf <> skbytes then
      invalid_arg (Printf.sprintf "Box.unsafe_sk_of_bytes: buffer must \
                                   be %d bytes long" skbytes) ;
    Sk buf

  let unsafe_pk_of_bytes buf =
    if Bigstring.length buf <> pkbytes then
      invalid_arg (Printf.sprintf "Box.unsafe_pk_of_bytes: buffer must \
                                   be %d bytes long" pkbytes) ;
    Pk buf

  let unsafe_ck_of_bytes buf =
    if Bigstring.length buf <> ckbytes then
      invalid_arg (Printf.sprintf "Box.unsafe_ck_of_bytes: buffer must \
                                   be %d bytes long" ckbytes) ;
    Ck buf

  let of_seed ?(pos=0) buf =
    let buflen = Bigstring.length buf in
    if pos < 0 || pos + skbytes > buflen then
      invalid_arg (Printf.sprintf "Box.of_seed: invalid pos (%d) or \
                                   buffer size (%d)" pos buflen) ;
    let sk = Bigstring.create skbytes in
    Bigstring.blit buf pos sk 0 skbytes ;
    Sk sk

  let basepoint =
    Bigstring.init 32 (function 0 -> '\x09' | _ -> '\x00')

  (* pk -> sk -> basepoint -> unit *)
  external scalarmult :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_Hacl_Curve25519_crypto_scalarmult" [@@noalloc]

  let neuterize (Sk sk) =
    let pk = Bigstring.create pkbytes in
    scalarmult pk sk basepoint ;
    Pk pk

  let keypair () =
    let sk = Sk (Rand.gen skbytes) in
    neuterize sk, sk

  (* ck -> pk -> sk -> unit *)
  external box_beforenm :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_NaCl_crypto_box_beforenm" [@@noalloc]

  let dh (Pk pk) (Sk sk) =
    let combined = Bigstring.create ckbytes in
    box_beforenm combined pk sk ;
    Ck combined

  (* cmsg -> msg -> nonce -> k -> unit *)
  external box_easy_afternm :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_NaCl_crypto_box_easy_afternm" [@@noalloc]

  let box ~k:(Ck k) ~nonce ~msg ~cmsg =
    box_easy_afternm cmsg msg nonce k

  (* msg -> cmsg -> n -> k -> int *)
  external box_open_easy_afternm :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> int =
    "ml_NaCl_crypto_box_open_easy_afternm" [@@noalloc]

  let box_open ~k:(Ck k) ~nonce ~cmsg ~msg =
    match box_open_easy_afternm msg cmsg nonce k with
    | 0 -> true
    | _ -> false
end

module Sign = struct
  type _ key =
    | Sk : Bigstring.t -> secret key
    | Pk : Bigstring.t -> public key

  let bytes = 64
  let pkbytes = 32
  let skbytes = 32

  let unsafe_to_bytes : type a. a key -> Bigstring.t = function
    | Pk buf -> buf
    | Sk buf -> buf

  let unsafe_sk_of_bytes seed =
    if Bigstring.length seed <> skbytes then
      invalid_arg (Printf.sprintf "Sign.unsafe_sk_of_bytes: sk must \
                                   be at least %d bytes long" skbytes) ;
    Sk seed

  let unsafe_pk_of_bytes pk =
    if Bigstring.length pk <> pkbytes then
      invalid_arg (Printf.sprintf "Sign.unsafe_pk_of_bytes: pk must be \
                                   at least %d bytes long" pkbytes) ;
    Pk pk

  let blit_to_bytes :
    type a. a key -> ?pos:int -> Bigstring.t -> unit = fun key ?(pos=0) buf ->
    match key with
    | Pk pk -> Bigstring.blit pk 0 buf pos pkbytes
    | Sk sk -> Bigstring.blit sk 0 buf pos skbytes

  let equal :
    type a. a key -> a key -> bool = fun a b -> match a, b with
    | Pk a, Pk b -> Bigstring.equal a b
    | Sk a, Sk b -> Bigstring.equal a b

  (* pk -> sk -> unit *)
  external neuterize :
    Bigstring.t -> Bigstring.t -> unit =
    "ml_Hacl_Ed25519_secret_to_public" [@@noalloc]

  let neuterize : type a. a key -> public key = function
    | Pk pk -> Pk pk
    | Sk sk ->
        let pk = Bigstring.create pkbytes in
        neuterize pk sk ;
        Pk pk

  let keypair () =
    let sk = Sk (Rand.gen skbytes) in
    neuterize sk, sk

  (* sig -> sk -> m -> unit *)
  external sign :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> unit =
    "ml_Hacl_Ed25519_sign" [@@noalloc]

  let sign ~sk:(Sk sk) ~msg ~signature =
    if Bigstring.length signature < bytes then
      invalid_arg (Printf.sprintf "Sign.write_sign: output buffer must \
                                   be at least %d bytes long" bytes) ;
    sign signature sk msg

  (* pk -> m -> sig -> bool *)
  external verify :
    Bigstring.t -> Bigstring.t -> Bigstring.t -> bool =
    "ml_Hacl_Ed25519_verify" [@@noalloc]

  let verify ~pk:(Pk pk) ~msg ~signature =
    verify pk msg signature
end
