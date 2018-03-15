(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Raw = struct

  type t = string

  let name = "Chain_id"
  let title = "Network identifier"

  let extract bh =
    MBytes.substring (Block_hash.to_bytes bh) 0 4
  let hash_bytes ?key l = extract (Block_hash.hash_bytes ?key l)
  let hash_string ?key l = extract (Block_hash.hash_string ?key l)

  let size = 4

  let compare = String.compare
  let equal = String.equal

  let of_string s =
    if String.length s <> size then None else Some s
  let of_string_exn s =
    match of_string s with
    | None ->
        let msg =
          Printf.sprintf "%s.of_string: wrong string size (%d)"
            name (String.length s) in
        raise (Invalid_argument msg)
    | Some h -> h

  let to_string s = s
  let of_hex s = of_string (Hex.to_string (`Hex s))
  let of_hex_exn s = of_string_exn (Hex.to_string (`Hex s))
  let to_hex s =
    let `Hex s = Hex.of_string (to_string s) in
    s


  let of_bytes_opt b =
    if MBytes.length b <> size then
      None
    else
      Some (MBytes.to_string b)
  let of_bytes_exn b =
    match of_bytes_opt b with
    | None ->
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h -> h
  let to_bytes = MBytes.of_string

  let read src off = of_bytes_exn @@ MBytes.sub src off size
  let write dst off h = MBytes.blit (to_bytes h) 0 dst off size

  let path_length = 1
  let to_path key l = to_hex key :: l
  let of_path path =
    let path = String.concat "" path in
    of_hex path
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn path

  let prefix_path p =
    let `Hex p = Hex.of_string p in
    [ p ]

  let zero =
    match of_hex (String.make (size * 2) '0') with
    | Some c -> c
    | None -> assert false

  type Tezos_crypto.Base58.data += Hash of t

  let b58check_encoding =
    Tezos_crypto.Base58.register_encoding
      ~prefix: Tezos_crypto.Base58.Prefix.chain_id
      ~length: size
      ~wrap: (fun s -> Hash s)
      ~of_raw:of_string ~to_raw: (fun h -> h)

  let of_b58check_opt s =
    Tezos_crypto.Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    match Tezos_crypto.Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" name
  let to_b58check s = Tezos_crypto.Base58.simple_encode b58check_encoding s
  let to_short_b58check = to_b58check

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t)

end

include Blake2B.Extend(Raw)

let of_block_hash bh = hash_bytes [Block_hash.to_bytes bh]
