
module Public_key_hash = Blake2B.Extend (Tezos_crypto.Ed25519.Public_key_hash)

module Public_key = struct
  include Tezos_crypto.Ed25519.Public_key
  let encoding =
    let open Data_encoding in
    splitted
      ~json:
        (describe
           ~title: "An Ed25519 public key (Tezos_crypto.Base58Check encoded)" @@
         conv
           (fun s -> to_b58check s)
           (fun s ->
              match of_b58check_opt s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
                          "Ed25519 public key: unexpected prefix.")
           string)
      ~binary:
        (conv
           to_bytes
           of_bytes_exn
           (Fixed.bytes size))
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        Error_monad.generic_error
          "Failed to read a base58-encoded Ed25519 public key"
  let param
      ?(name="ed25519-public")
      ?(desc="Ed25519 public key (b58check-encoded)") t =
    Cli_entries.(param ~name ~desc
                   (parameter (fun _ str -> Lwt.return (of_b58check str))) t)

end

module Secret_key = struct
  include Tezos_crypto.Ed25519.Secret_key
  let encoding =
    let open Data_encoding in
    splitted
      ~json:
        (describe
           ~title: "An Ed25519 secret key (Tezos_crypto.Base58Check encoded)" @@
         conv
           (fun s -> to_b58check s)
           (fun s ->
              match of_b58check_opt s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
                          "Ed25519 secret key: unexpected prefix.")
           string)
      ~binary:
        (conv
           to_bytes
           of_bytes_exn
           (Fixed.bytes size))
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        Error_monad.generic_error
          "Failed to read a base58-encoded Ed25519 secret key"
  let param
      ?(name="ed25519-secret")
      ?(desc="Ed25519 secret key (b58check-encoded)") t =
    Cli_entries.(param ~name ~desc
                   (parameter (fun _ str -> Lwt.return (of_b58check str))) t)
end

module Signature = struct
  include Tezos_crypto.Ed25519.Signature
  let encoding =
    let open Data_encoding in
    splitted
      ~json:
        (describe
           ~title: "An Ed25519 signature (Base58Check encoded)" @@
         conv
           (fun s -> to_b58check s)
           (fun s ->
              match of_b58check_opt s with
              | Some x -> x
              | None -> Data_encoding.Json.cannot_destruct
                          "Ed25519 signature: unexpected prefix.")
           string)
      ~binary:
        (conv
           to_bytes
           of_bytes_exn
           (Fixed.bytes size))
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        Error_monad.generic_error
          "Failed to read a base58-encoded Ed25519 signature"
  let param
      ?(name="ed25519-signature")
      ?(desc="Ed25519 signature (b58check-encoded)") t =
    Cli_entries.(param ~name ~desc
                   (parameter (fun _ str -> Lwt.return (of_b58check str))) t)
end

include (Tezos_crypto.Ed25519 : (module type of Tezos_crypto.Ed25519)
         with module Public_key_hash := Public_key_hash
          and module Public_key := Public_key
          and module Secret_key := Secret_key
          and module Signature := Signature)
