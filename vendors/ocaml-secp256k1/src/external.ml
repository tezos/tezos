module Context = struct
  type t

  external create : int -> t = "caml_secp256k1_context_create"
  external clone : t -> t = "caml_secp256k1_context_clone"
  external randomize : t -> Bigstring.t -> bool = "caml_secp256k1_context_randomize" [@@noalloc]

  let create ?(sign=true) ?(verify=true) () =
    let flags = 1 lor
                (if sign then 0x100 else 0) lor
                (if verify then 0x200 else 0) in
    create flags

  let randomize ctx buf =
    if Bigstring.length buf < 32 then
      invalid_arg "Context.randomize: input must be at least 32 bytes long" ;
    randomize ctx buf
end

module Key = struct
  type secret
  type public
  type _ t =
    | Sk : Bigstring.t -> secret t
    | Pk : Bigstring.t -> public t

  let buffer : type a. a t -> Bigstring.t = function
    | Sk sk -> sk
    | Pk pk -> pk

  let secret_bytes = 32
  let public_bytes = 64
  let compressed_pk_bytes = 33
  let uncompressed_pk_bytes = 65

  let bytes : type a. a t -> int = function
    | Sk _ -> secret_bytes
    | Pk _ -> public_bytes

  let serialized_bytes :
    type a. ?compressed:bool -> a t -> int =
    fun ?(compressed=true) -> function
      | Sk _ -> secret_bytes
      | Pk _ -> if compressed then public_bytes + 1 else secret_bytes + 1

  let equal : type a. a t -> a t -> bool = fun a b ->
    match a, b with
    | Sk a, Sk b -> Bigstring.equal a b
    | Pk a, Pk b -> Bigstring.equal a b

  let copy : type a. a t -> a t = function
    | Sk sk -> Sk (Bigstring.copy sk)
    | Pk pk -> Pk (Bigstring.copy pk)

  external sk_negate_inplace : Context.t -> Bigstring.t -> unit =
    "caml_secp256k1_ec_privkey_negate" [@@noalloc]
  external sk_add_tweak_inplace : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_privkey_tweak_add" [@@noalloc]
  external sk_mul_tweak_inplace : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_privkey_tweak_mul" [@@noalloc]
  external pk_negate_inplace : Context.t -> Bigstring.t -> unit =
    "caml_secp256k1_ec_pubkey_negate" [@@noalloc]
  external pk_add_tweak_inplace : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_pubkey_tweak_add" [@@noalloc]
  external pk_mul_tweak_inplace : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_pubkey_tweak_mul" [@@noalloc]
  external pk_combine : Context.t -> Bigstring.t -> Bigstring.t list -> bool =
    "caml_secp256k1_ec_pubkey_combine" [@@noalloc]

  let negate_inplace :
    type a. Context.t -> a t -> unit = fun ctx -> function
    | Sk k -> sk_negate_inplace ctx k
    | Pk k -> pk_negate_inplace ctx k

  let negate ctx k =
    let k' = copy k in
    negate_inplace ctx k' ;
    k'

  let op_tweak :
    type a. string -> (Context.t -> Bigstring.t -> Bigstring.t -> bool) ->
    Context.t -> a t -> Bigstring.t -> Bigstring.t =
    fun name f ctx k buf ->
      let buflen = Bigstring.length buf in
      if buflen < 32 then
        invalid_arg (Printf.sprintf "Key.%s: " name) ;
      let k' = buffer (copy k) in
      if not (f ctx k' buf) then
        failwith (Printf.sprintf "Key.%s: operation failed" name) ;
      k'

  let add_tweak :
    type a. Context.t -> a t -> Bigstring.t -> a t =
    fun ctx k buf ->
      match k with
      | Sk _ -> Sk (op_tweak "add_tweak" sk_add_tweak_inplace ctx k buf)
      | Pk _ -> Pk (op_tweak "add_tweak" pk_add_tweak_inplace ctx k buf)

  let mul_tweak :
    type a. Context.t -> a t -> Bigstring.t -> a t =
    fun ctx k buf ->
      match k with
      | Sk _ -> Sk (op_tweak "mul_tweak" sk_mul_tweak_inplace ctx k buf)
      | Pk _ -> Pk (op_tweak "mul_tweak" pk_mul_tweak_inplace ctx k buf)

  external pk_parse : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_pubkey_parse" [@@noalloc]
  external pk_serialize : Context.t -> Bigstring.t -> Bigstring.t -> int =
    "caml_secp256k1_ec_pubkey_serialize" [@@noalloc]
  external pk_create : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_pubkey_create" [@@noalloc]

  let neuterize :
    type a. Context.t -> a t -> public t option = fun ctx -> function
    | Pk pk -> Some (Pk pk)
    | Sk sk ->
        let pk = Bigstring.create public_bytes in
        if pk_create ctx pk sk then Some (Pk pk) else None

  let neuterize_exn ctx k =
    match neuterize ctx k with
    | None -> invalid_arg "Key.neuterize_exn: invalid secret key"
    | Some pk -> pk

  let list_map_filter_opt ~f l =
    ListLabels.fold_left ~init:[] ~f:begin fun a e ->
      match f e with
      | None -> a
      | Some r -> r :: a
    end l

  let combine ctx pks =
    let nb_pks = List.length pks in
    if nb_pks = 0 || nb_pks > 1024 then None
    else
      let pk = Bigstring.create public_bytes in
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

  external verify_sk : Context.t -> Bigstring.t -> bool =
    "caml_secp256k1_ec_seckey_verify" [@@noalloc]

  let read_sk_exn ctx buf =
    let buflen = Bigstring.length buf in
    if buflen < secret_bytes then
      invalid_arg (Printf.sprintf "Key.read_sk: invalid buffer size %d" buflen) ;
    match verify_sk ctx buf with
    | true -> Sk Bigstring.(copy (sub buf 0 secret_bytes))
    | false -> invalid_arg "Key.read_sk: secret key is invalid"

  let read_sk ctx buf =
    try Ok (read_sk_exn ctx buf) with
    | Invalid_argument msg -> Error msg

  let read_pk_exn ctx buf =
    let buflen = Bigstring.length buf in
    if buflen < compressed_pk_bytes then
      invalid_arg (Printf.sprintf "Key.read_pk: invalid buffer size %d" buflen) ;
    let outbuf = Bigstring.create public_bytes in
    if pk_parse ctx outbuf buf then
      Pk outbuf
    else
      invalid_arg "Key.read_pk_exn: public key is invalid"

  let read_pk ctx buf =
    try Ok (read_pk_exn ctx buf) with
    | Invalid_argument msg -> Error msg

  let write :
    type a. ?compress:bool -> Context.t -> ?pos:int -> Bigstring.t -> a t -> int =
    fun ?(compress=true) ctx ?(pos=0) buf -> function
      | Sk sk ->
          let buflen = Bigstring.length buf in
          if pos < 0 || pos > buflen - secret_bytes then
            invalid_arg "Key.write (secret): pos < 0 or pos + 32 > buflen" ;
          Bigstring.blit sk 0 buf pos secret_bytes ;
          secret_bytes
      | Pk pk ->
          let buflen = Bigstring.length buf in
          if pos < 0
          || (compress && pos > buflen - compressed_pk_bytes)
          || (not compress && pos > buflen - uncompressed_pk_bytes) then
            invalid_arg (Printf.sprintf "Key.write (public): pos=%d, buflen=%d" pos buflen) ;
          let len = if compress then 33 else 65 in
          let buf = Bigstring.sub buf pos len in
          pk_serialize ctx buf pk

  let to_bytes :
    type a. ?compress:bool -> Context.t -> a t -> Bigstring.t =
    fun ?(compress=true) ctx -> function
      | Sk _ as sk ->
          let buf = Bigstring.create secret_bytes in
          let _ = write ~compress ctx buf sk in
          buf
      | Pk _ as pk ->
          let buf =
            Bigstring.create (1 + (if compress then secret_bytes else public_bytes)) in
          let _ = write ~compress ctx buf pk in
          buf
end

module Sign = struct
  type plain
  type recoverable
  type _ t =
    | P : Bigstring.t -> plain t
    | R : Bigstring.t -> recoverable t

  let buffer : type a. a t -> Bigstring.t = function
    | P plain -> plain
    | R recoverable -> recoverable

  let plain_bytes = 64
  let recoverable_bytes = 65
  let msg_bytes = 32

  let equal : type a. a t -> a t -> bool = fun a b ->
    match a, b with
    | P a, P b -> Bigstring.equal a b
    | R a, R b -> Bigstring.equal a b

  external parse_compact : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_signature_parse_compact" [@@noalloc]
  external parse_der : Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_signature_parse_der" [@@noalloc]
  external serialize_compact : Context.t -> Bigstring.t -> Bigstring.t -> unit =
    "caml_secp256k1_ecdsa_signature_serialize_compact" [@@noalloc]
  external serialize_der : Context.t -> Bigstring.t -> Bigstring.t -> int =
    "caml_secp256k1_ecdsa_signature_serialize_der" [@@noalloc]
  external parse_recoverable : Context.t -> Bigstring.t -> Bigstring.t -> int -> bool =
    "caml_secp256k1_ecdsa_recoverable_signature_parse_compact" [@@noalloc]
  external serialize_recoverable : Context.t -> Bigstring.t -> Bigstring.t -> int =
    "caml_secp256k1_ecdsa_recoverable_signature_serialize_compact" [@@noalloc]

  let read_exn ctx buf =
    let buflen = Bigstring.length buf in
    if buflen < plain_bytes then
      invalid_arg (Printf.sprintf "Sign.read: invalid buffer size %d" buflen) ;
    let signature = Bigstring.create plain_bytes in
    if parse_compact ctx signature buf then
      P signature
    else
      invalid_arg "Sign.read: signature could not be parsed"

  let read ctx buf =
    try Ok (read_exn ctx buf) with
      Invalid_argument msg -> Error msg

  let read_der_exn ctx buf =
    let signature = Bigstring.create plain_bytes in
    if parse_der ctx signature buf then
      P signature
    else
      invalid_arg "Sign.read_der: signature could not be parsed"

  let read_der ctx buf =
    try Ok (read_der_exn ctx buf) with
      Invalid_argument msg -> Error msg

  let read_recoverable_exn ctx buf =
    let buflen = Bigstring.length buf in
    if buflen < recoverable_bytes then
      invalid_arg (Printf.sprintf "Sign.read_recoverable: invalid buffer size %d" buflen) ;
    let signature = Bigstring.create recoverable_bytes in
    let recid = int_of_char (Bigstring.get buf 64) in
    if parse_recoverable ctx signature buf recid then
      R signature
    else
      invalid_arg "Sign.read_recoverable: signature could not be parsed"

  let read_recoverable ctx buf =
    try Ok (read_recoverable_exn ctx buf) with
    | Invalid_argument msg -> Error msg

  let write_exn :
    type a. ?der:bool -> Context.t -> Bigstring.t -> a t -> int =
    fun ?(der=false) ctx buf -> function
      | P signature ->
          let buflen = Bigstring.length buf in
          if not der then begin
            if buflen < plain_bytes then
              invalid_arg (Printf.sprintf "Sign.write: buffer length too small (%d)" buflen) ;
            serialize_compact ctx buf signature ;
            plain_bytes
          end
          else begin
            match serialize_der ctx buf signature with
            | 0 -> invalid_arg "Sign.write_exn: buffer too small to \
                                contain a DER signature"
            | len -> len
          end
      | R signature ->
          let buflen = Bigstring.length buf in
          if buflen < recoverable_bytes then
            invalid_arg (Printf.sprintf "Sign.write: buffer length too small (%d)" buflen) ;
          let recid = serialize_recoverable ctx buf signature in
          Bigstring.set buf 64 (char_of_int recid) ;
          recoverable_bytes

  let write ?der ctx buf signature =
    try Ok (write_exn ?der ctx buf signature) with
      Invalid_argument msg -> Error msg

  let to_bytes :
    type a. ?der:bool -> Context.t -> a t -> Bigstring.t =
    fun ?(der=false) ctx -> function
      | P _ as signature ->
          if der then begin
            let buf = Bigstring.create 72 in
            let nb_written = write_exn ~der ctx buf signature in
            Bigstring.sub buf 0 nb_written
          end
          else
            let buf = Bigstring.create plain_bytes in
            let _nb_written = write_exn ~der ctx buf signature in
            buf
      | R _ as signature ->
          let buf = Bigstring.create recoverable_bytes in
          let _nb_written = write_exn ctx buf signature in
          buf

  external normalize :
    Context.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_signature_normalize" [@@noalloc]

  let normalize ctx (P signature) =
    let normalized_sig = Bigstring.create plain_bytes in
    if normalize ctx normalized_sig signature then
      Some (P normalized_sig) else None

  (* [sign ctx signature msg sk] *)
  external sign :
    Context.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_sign" [@@noalloc]

  (* [verify ctx pk msg signature] *)
  external verify :
    Context.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_verify" [@@noalloc]

  let check_msglen msg =
    let msglen = Bigstring.length msg in
    if msglen < msg_bytes
    then invalid_arg
        (Printf.sprintf "message is too small (%d < %d)" msglen msg_bytes)

  let sign_exn ctx buf ~sk ~msg =
    check_msglen msg ;
    let buflen = Bigstring.length buf in
    if buflen < plain_bytes then
      invalid_arg (Printf.sprintf "Sign.write_sign: buffer length too \
                                   small (%d)" buflen) ;
    match sign ctx buf (Key.buffer sk) msg with
    | true -> ()
    | false -> invalid_arg "Sign.write_sign: the nonce generation \
                            function failed, or the private key was \
                            invalid"

  let write_sign_exn ctx buf ~sk ~msg =
    let signature = Bigstring.create plain_bytes in
    sign_exn ctx signature ~sk ~msg ;
    write_exn ctx buf (P signature)

  let write_sign ctx buf ~sk ~msg =
    try Ok (write_sign_exn ctx ~sk ~msg buf)
    with Invalid_argument msg -> Error msg

  let sign_exn ctx ~sk msg =
    let signature = Bigstring.create plain_bytes in
    sign_exn ctx signature ~sk ~msg ;
    P signature

  let sign ctx ~sk msg =
    try Ok (sign_exn ctx ~sk msg)
    with Invalid_argument msg -> Error msg

  external sign_recoverable :
    Context.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_sign_recoverable" [@@noalloc]

  let write_sign_recoverable_exn ctx ~sk ~msg buf =
    check_msglen msg ;
    let buflen = Bigstring.length buf in
    if buflen < recoverable_bytes then
      invalid_arg (Printf.sprintf "Sign.write_sign_recoverable: buffer \
                                   length too small (%d)" buflen) ;
    if sign_recoverable ctx buf (Key.buffer sk) msg then
      recoverable_bytes
    else invalid_arg "Sign.write_sign_recoverable_exn: the nonce \
                      generation function failed, or the private key \
                      was invalid"

  let write_sign_recoverable ctx ~sk ~msg buf =
    try Ok (write_sign_recoverable_exn ctx ~sk ~msg buf)
    with Invalid_argument msg -> Error msg

  let sign_recoverable ctx ~sk msg =
    let signature = Bigstring.create recoverable_bytes in
    match write_sign_recoverable ctx ~sk ~msg signature with
    | Error error -> Error error
    | Ok _nb_written -> Ok (R signature)

  let sign_recoverable_exn ctx ~sk msg =
    match sign_recoverable ctx ~sk msg with
    | Error msg -> invalid_arg msg
    | Ok signature -> signature

  external to_plain : Context.t -> Bigstring.t -> Bigstring.t -> unit =
    "caml_secp256k1_ecdsa_recoverable_signature_convert" [@@noalloc]

  let to_plain : type a. Context.t -> a t -> plain t = fun ctx -> function
    | P _ as signature -> signature
    | R recoverable ->
        let plain = Bigstring.create plain_bytes in
        to_plain ctx plain recoverable ;
        P plain

  let verify_plain_exn ctx ~pk msg signature =
    check_msglen msg ;
    let siglen = Bigstring.length signature in
    if siglen < plain_bytes then
      invalid_arg (Printf.sprintf "verify: signature too short (%d < %d)"
                     siglen plain_bytes) ;
    verify ctx (Key.buffer pk) msg signature

  let verify_exn ctx ~pk ~msg ~signature =
    let P signature = to_plain ctx signature in
    verify_plain_exn ctx ~pk msg signature

  let verify ctx ~pk ~msg ~signature =
    try Ok (verify_exn ctx ~pk ~msg ~signature) with
    | Invalid_argument msg -> Error msg

  external recover :
    Context.t -> Bigstring.t -> Bigstring.t -> Bigstring.t -> bool =
    "caml_secp256k1_ecdsa_recover" [@@noalloc]

  let recover_exn ctx ~signature:(R signature) msg =
    check_msglen msg ;
    let pk = Bigstring.create Key.public_bytes in
    if recover ctx pk signature msg then Key.Pk pk
    else
      invalid_arg "Sign.recover: pk could not be recovered"

  let recover ctx ~signature msg =
    try Ok (recover_exn ctx ~signature msg) with
      Invalid_argument msg -> Error msg
end
