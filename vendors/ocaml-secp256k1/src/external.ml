open StdLabels

module BA = struct
  include Bigarray.Array1

  let length = size_in_bytes

  let rec compare_rec a b i len_a len_b =
    if i=len_a && i=len_b then 0
    else if i=len_a then -1
    else if i=len_b then 1
    else
      match Char.compare (get a i) (get b i) with
      | 0 -> compare_rec a b (i+1) len_a len_b
      | n -> n

  let compare a b =
    compare_rec a b 0 (length a) (length b)

  let equal a b = compare a b = 0

  let create len =
    Bigarray.(create char c_layout len)
end

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Context = struct
  type flag =
    | Verify
    | Sign

  type t

  external flags : buffer -> int = "context_flags"
  external create : int -> t = "context_create"
  external clone : t -> t = "context_clone"
  external randomize : t -> buffer -> bool = "context_randomize" [@@noalloc]
  external get_16 : buffer -> int -> int = "%caml_bigstring_get16" [@@noalloc]

  let flags =
    let buf = BA.create (3 * 2) in
    let _ = flags buf in
    buf

  let int_of_flag = function
    | Verify -> get_16 flags 2
    | Sign -> get_16 flags 4

  let create a =
    List.fold_left a ~init:(get_16 flags 0) ~f:(fun a f -> a lor (int_of_flag f)) |>
    create

  let randomize ctx buf =
    if BA.length buf <> 32 then
      invalid_arg "Context.randomize: input must be 32 bytes long" ;
    randomize ctx buf
end

module Key = struct
  type secret
  type public
  type _ t =
    | Sk : buffer -> secret t
    | Pk : buffer -> public t

  let to_buffer : type a. a t -> buffer = function
    | Sk k -> k
    | Pk k -> k

  let secret_bytes = 32
  let public_bytes = 64

  let length : type a. a t -> int = function
    | Sk _ -> 32
    | Pk _ -> 64

  let equal : type a. a t -> a t -> bool = fun a b ->
    match a, b with
    | Sk a, Sk b -> BA.equal a b
    | Pk a, Pk b -> BA.equal a b

  let copy : type a. a t -> a t = function
    | Sk k ->
        let k' = BA.create secret_bytes in
        BA.blit k k' ;
        Sk k'
    | Pk k ->
        let k' = BA.create public_bytes in
        BA.blit k k' ;
        Pk k'

  external sk_negate_inplace : Context.t -> buffer -> unit =
    "ec_privkey_negate" [@@noalloc]
  external sk_add_tweak_inplace : Context.t -> buffer -> buffer -> bool =
    "ec_privkey_tweak_add" [@@noalloc]
  external sk_mul_tweak_inplace : Context.t -> buffer -> buffer -> bool =
    "ec_privkey_tweak_mul" [@@noalloc]
  external pk_negate_inplace : Context.t -> buffer -> unit =
    "ec_pubkey_negate" [@@noalloc]
  external pk_add_tweak_inplace : Context.t -> buffer -> buffer -> bool =
    "ec_pubkey_tweak_add" [@@noalloc]
  external pk_mul_tweak_inplace : Context.t -> buffer -> buffer -> bool =
    "ec_pubkey_tweak_mul" [@@noalloc]
  external pk_combine : Context.t -> buffer -> buffer list -> bool =
    "ec_pubkey_combine" [@@noalloc]

  let negate_inplace :
    type a. Context.t -> a t -> unit = fun ctx -> function
    | Sk k -> sk_negate_inplace ctx k
    | Pk k -> pk_negate_inplace ctx k

  let negate ctx k =
    let k' = copy k in
    negate_inplace ctx k' ;
    k'

  let op_tweak :
    type a. string -> (Context.t -> buffer -> buffer -> bool) ->
    Context.t -> a t -> ?pos:int -> buffer -> buffer =
    fun name f ctx k ?(pos=0) buf ->
      let buflen = BA.length buf in
      if pos < 0 || pos > buflen - 32 then
        invalid_arg (Printf.sprintf "Key.%s: pos < 0 or pos > buflen - 32" name) ;
      let buf = BA.sub buf pos 32 in
      let k' = copy k |> to_buffer in
      if not (f ctx k' buf) then
        failwith (Printf.sprintf "Key.%s: operation failed" name) ;
      k'

  let add_tweak :
    type a. Context.t -> a t -> ?pos:int -> buffer -> a t =
    fun ctx k ?pos buf ->
      match k with
      | Sk _ -> Sk (op_tweak "add_tweak" sk_add_tweak_inplace ctx k ?pos buf)
      | Pk _ -> Pk (op_tweak "add_tweak" pk_add_tweak_inplace ctx k ?pos buf)

  let mul_tweak :
    type a. Context.t -> a t -> ?pos:int -> buffer -> a t =
    fun ctx k ?pos buf ->
      match k with
      | Sk _ -> Sk (op_tweak "mul_tweak" sk_mul_tweak_inplace ctx k ?pos buf)
      | Pk _ -> Pk (op_tweak "mul_tweak" pk_mul_tweak_inplace ctx k ?pos buf)

  external pk_parse : Context.t -> buffer -> buffer -> bool =
    "ec_pubkey_parse" [@@noalloc]
  external pk_serialize : Context.t -> buffer -> buffer -> int =
    "ec_pubkey_serialize" [@@noalloc]
  external pk_create : Context.t -> buffer -> buffer -> bool =
    "ec_pubkey_create" [@@noalloc]

  let neuterize :
    type a. Context.t -> a t -> public t option = fun ctx -> function
    | Pk pk -> Some (Pk pk)
    | Sk sk ->
        let pk = BA.create public_bytes in
        if pk_create ctx pk sk then Some (Pk pk) else None

  let neuterize_exn ctx k =
    match neuterize ctx k with
    | None -> invalid_arg "Key.neuterize_exn: invalid secret key"
    | Some pk -> pk

  let list_map_filter_opt ~f l =
    List.fold_left ~init:[] ~f:begin fun a e ->
      match f e with
      | None -> a
      | Some r -> r :: a
    end l

  let combine ctx pks =
    let nb_pks = List.length pks in
    if nb_pks = 0 || nb_pks > 1024 then None
    else
      let pk = BA.create public_bytes in
      let pks = list_map_filter_opt ~f:begin fun k ->
          match neuterize ctx k with
          | None -> None
          | Some (Pk k) -> Some k
        end pks in
      if pk_combine ctx pk pks then Some (Pk pk)
      else None

  let combine_exn ctx pks =
    match combine ctx pks with
    | None -> invalid_arg "Key.combine_exn: sum of pks is invalid"
    | Some pk -> pk

  external verify_sk : Context.t -> buffer -> bool =
    "ec_seckey_verify" [@@noalloc]

  let read_sk_exn ctx ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - secret_bytes then
      invalid_arg "Key.read_sk: pos < 0 or pos + 32 > buflen" ;
    let buf = BA.sub buf pos secret_bytes in
    match verify_sk ctx buf with
    | true ->
        let t = BA.create secret_bytes in
        BA.blit buf t ;
        Sk buf
    | false -> invalid_arg "Key.read_sk_exn: secret key is invalid"

  let read_sk ctx ?pos buf =
    try Ok (read_sk_exn ctx ?pos buf) with
    | Invalid_argument msg -> Error msg

  let read_pk_exn ctx ?(pos=0) inbuf =
    let pklen = BA.length inbuf in
    if pos < 0 || pos > pklen - 33 then
      invalid_arg "Key.read_pk: pos < 0 or pos > buflen - 33" ;
    let inbuf = BA.(sub inbuf pos (length inbuf)) in
    if BA.(length inbuf < 33) then
      invalid_arg "Key.read_pk: input must be at least 33 bytes long" ;
    let outbuf = BA.create public_bytes in
    if (pk_parse ctx outbuf inbuf) then Pk outbuf
    else invalid_arg "Key.read_pk_exn: public key is invalid"

  let read_pk ctx ?pos buf =
    try Ok (read_pk_exn ctx ?pos buf) with
    | Invalid_argument msg -> Error msg

  let write :
    type a. ?compress:bool -> Context.t -> ?pos:int -> buffer -> a t -> int =
    fun ?(compress=true) ctx ?(pos=0) buf -> function
      | Sk sk ->
          let buflen = BA.length buf in
          if pos < 0 || pos > buflen - secret_bytes then
            invalid_arg "Key.write (secret): pos < 0 or pos + 32 > buflen" ;
          let buf = BA.sub buf pos secret_bytes in
          BA.blit sk buf ;
          secret_bytes
      | Pk pk ->
          let buflen = BA.length buf in
          if pos < 0
          || (compress && pos > buflen - 33)
          || (not compress && pos > buflen - 65) then
            invalid_arg (Printf.sprintf "Key.write (public): pos=%d, buflen=%d" pos buflen) ;
          let len = if compress then 33 else 65 in
          let buf = BA.sub buf pos len in
          pk_serialize ctx buf pk

  let to_bytes :
    type a. ?compress:bool -> Context.t -> a t -> buffer =
    fun ?(compress=true) ctx -> function
      | Sk _ as sk ->
          let buf = BA.create secret_bytes in
          let _ = write ~compress ctx buf sk in
          buf
      | Pk _ as pk ->
          let buf =
            BA.create (1 + (if compress then secret_bytes else public_bytes)) in
          let _ = write ~compress ctx buf pk in
          buf
end

module Sign = struct
  type plain
  type recoverable
  type _ t =
    | P : buffer -> plain t
    | R : buffer -> recoverable t

  let plain_bytes = 64
  let recoverable_bytes = 65
  let msg_bytes = 32

  type msg = buffer

  let msg_of_bytes ?(pos=0) buf =
    try Some (BA.sub buf pos msg_bytes) with _ -> None
  let msg_of_bytes_exn ?pos buf =
    match msg_of_bytes ?pos buf with
    | None -> invalid_arg "msg_of_bytes_exn"
    | Some msg -> msg
  let write_msg_exn ?(pos=0) buf msg =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - msg_bytes then
      invalid_arg "Sign.read_exn: pos < 0 or pos > buflen - 64" ;
    BA.blit (BA.sub msg 0 msg_bytes) (BA.sub buf pos msg_bytes) ;
    msg_bytes

  let write_msg ?pos buf msg =
    try Ok (write_msg_exn ?pos buf msg) with
    | Invalid_argument msg -> Error msg

  let msg_to_bytes msg = msg

  let equal : type a. a t -> a t -> bool = fun a b ->
    match a, b with
    | P a, P b -> BA.equal a b
    | R a, R b -> BA.equal a b

  external parse_compact : Context.t -> buffer -> buffer -> bool =
    "ecdsa_signature_parse_compact" [@@noalloc]
  external parse_der : Context.t -> buffer -> buffer -> bool =
    "ecdsa_signature_parse_der" [@@noalloc]
  external serialize_compact : Context.t -> buffer -> buffer -> unit =
    "ecdsa_signature_serialize_compact" [@@noalloc]
  external serialize_der : Context.t -> buffer -> buffer -> int =
    "ecdsa_signature_serialize_der" [@@noalloc]
  external parse_recoverable : Context.t -> buffer -> buffer -> int -> bool =
    "ecdsa_recoverable_signature_parse_compact" [@@noalloc]
  external serialize_recoverable : Context.t -> buffer -> buffer -> int =
    "ecdsa_recoverable_signature_serialize_compact" [@@noalloc]

  let read_exn ctx ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - plain_bytes then
      invalid_arg "Sign.read_exn: pos < 0 or pos > buflen - 64" ;
    let signature = BA.create plain_bytes in
    if parse_compact ctx signature (BA.sub buf pos plain_bytes) then
      P signature
    else invalid_arg "Sign.read_exn: signature could not be parsed"

  let read ctx ?pos buf =
    try Ok (read_exn ctx ?pos buf) with
    | Invalid_argument msg -> Error msg

  let read_der_exn ctx ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - plain_bytes then
      invalid_arg "Sign.read_der: pos < 0 or pos > buflen - 72" ;
    let signature = BA.create plain_bytes in
    if parse_der ctx signature BA.(sub buf pos (length buf)) then
      P signature
    else invalid_arg "Sign.read_der_exn: signature could not be parsed"

  let read_der ctx ?pos buf =
    try Ok (read_der_exn ctx ?pos buf) with
    | Invalid_argument msg -> Error msg

  let read_recoverable_exn ctx ~recid ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - plain_bytes then
      invalid_arg "Sign.read_recoverable_exn: pos < 0 or pos > buflen - 64" ;
    let signature = BA.create recoverable_bytes in
    if parse_recoverable ctx signature (BA.sub buf pos plain_bytes) recid then (R signature)
    else invalid_arg "Sign.read_recoverable_exn: signature could not be parsed"

  let read_recoverable ctx ~recid ?pos buf =
    try Ok (read_recoverable_exn ctx ~recid ?pos buf) with
    | Invalid_argument msg -> Error msg

  let write_exn :
    type a. ?der:bool -> Context.t -> ?pos:int -> buffer -> a t -> int =
    fun ?(der=false) ctx ?(pos=0) buf -> function
      | P signature ->
          let buf = BA.(sub buf pos (length buf)) in
          if der then serialize_der ctx buf signature
          else (serialize_compact ctx buf signature ; plain_bytes)
      | R signature ->
          let buflen = BA.length buf in
          if pos < 0 || pos > buflen - plain_bytes then
            invalid_arg "write: pos < 0 or pos > buflen - 64" ;
          ignore (serialize_recoverable ctx (BA.sub buf pos plain_bytes) signature) ;
          plain_bytes

  let write ?der ctx ?pos buf signature =
    try Ok (write_exn ?der ctx ?pos buf signature) with
    | Invalid_argument msg -> Error msg

  let to_bytes ?der ctx signature =
    let buf = BA.create 72 in
    let nb_written = write_exn ?der ctx buf signature in
    BA.sub buf 0 nb_written

  let to_bytes_recid ctx (R signature) =
    let buf = BA.create plain_bytes in
    let recid = serialize_recoverable ctx buf signature in
    buf, recid

  external sign : Context.t -> buffer -> buffer -> buffer -> bool =
    "ecdsa_sign" [@@noalloc]
  external verify : Context.t -> buffer -> buffer -> buffer -> bool =
    "ecdsa_verify" [@@noalloc]

  let write_sign_exn ctx ~sk ~msg ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - plain_bytes then
      invalid_arg "Sign.write_sign: outpos < 0 or outpos > outbuf - 64" ;
    if sign ctx (BA.sub buf pos plain_bytes) (Key.to_buffer sk) msg then plain_bytes
    else invalid_arg
        "Sign.write_sign: the nonce generation function failed, or the private key was invalid"

  let write_sign ctx ~sk ~msg ?pos buf =
    try Ok (write_sign_exn ctx ~sk ~msg ?pos buf) with
    | Invalid_argument msg -> Error msg

  let sign ctx ~sk ~msg =
    let signature = BA.create plain_bytes in
    match write_sign ctx ~sk ~msg signature with
    | Error msg -> Error msg
    | Ok _nb_written -> Ok (P signature)

  let sign_exn ctx ~sk ~msg =
    match sign ctx ~sk ~msg with
    | Error msg -> invalid_arg msg
    | Ok signature -> signature

  external sign_recoverable : Context.t -> buffer -> buffer -> buffer -> bool =
    "ecdsa_sign_recoverable" [@@noalloc]

  let write_sign_recoverable_exn ctx ~sk ~msg ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - recoverable_bytes then
      invalid_arg "Sign.write_sign_recoverable_exn: \
                   outpos < 0 or outpos > outbuflen - 65" ;
    if sign_recoverable ctx
        (BA.sub buf pos recoverable_bytes)
        (Key.to_buffer sk) msg then recoverable_bytes
    else invalid_arg
        "Sign.write_sign_recoverable_exn: \
         the nonce generation function failed, or the private key was invalid"

  let write_sign_recoverable ctx ~sk ~msg ?pos buf =
    try Ok (write_sign_recoverable_exn ctx ~sk ~msg ?pos buf) with
    | Invalid_argument msg -> Error msg

  let sign_recoverable ctx ~sk msg =
    let signature = BA.create recoverable_bytes in
    match write_sign_recoverable ctx ~sk ~msg signature with
    | Error error -> Error error
    | Ok _nb_written -> Ok (R signature)

  let sign_recoverable_exn ctx ~sk msg =
    match sign_recoverable ctx ~sk msg with
    | Error msg -> invalid_arg msg
    | Ok signature -> signature

  external to_plain : Context.t -> buffer -> buffer -> unit =
    "ecdsa_recoverable_signature_convert" [@@noalloc]

  let to_plain ctx (R recoverable) =
    let plain = BA.create plain_bytes in
    to_plain ctx plain recoverable ;
    P plain

  let verify_plain_exn ctx ~pk ?(pos=0) msg signature =
    let msglen = BA.length msg in
    if pos < 0 || pos > msglen - 32 then
      invalid_arg "Sign.verify: msg must be at least 32 bytes long" ;
    verify ctx (Key.to_buffer pk) (BA.sub msg pos 32) signature

  let verify_exn :
    type a. Context.t -> pk:Key.public Key.t -> msg:msg -> signature:a t -> bool =
    fun ctx ~pk ~msg ~signature -> match signature with
      | P signature -> verify_plain_exn ctx ~pk msg signature
      | R _ as r ->
          let P signature = to_plain ctx r in
          verify_plain_exn ctx ~pk msg signature

  let verify ctx ~pk ~msg ~signature =
    try Ok (verify_exn ctx ~pk ~msg ~signature) with
    | Invalid_argument msg -> Error msg

  external recover : Context.t -> buffer -> buffer -> buffer -> bool =
    "ecdsa_recover" [@@noalloc]

  let recover_exn ctx ~signature:(R signature) ~msg =
    let pk = BA.create Key.public_bytes in
    if recover ctx pk signature msg then Key.Pk pk
    else
      invalid_arg "Sign.recover: pk could not be recovered"

  let recover ctx ~signature ~msg =
    try Ok (recover_exn ctx ~signature ~msg) with
    | Invalid_argument msg -> Error msg
end
