(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = string

let name = "Net_id"
let title = "Network identifier"

let size = 4

let extract bh =
  MBytes.substring (Block_hash.to_bytes bh) 0 4

let hash_bytes l = extract (Block_hash.hash_bytes l)
let hash_string l = extract (Block_hash.hash_string l)

let of_block_hash bh = hash_bytes [Block_hash.to_bytes bh]

type Base58.data += Hash of t

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

let compare = String.compare
let equal = String.equal

let of_bytes b =
  if MBytes.length b <> size then
    None
  else
    Some (MBytes.to_string b)
let of_bytes_exn b =
  match of_bytes b with
  | None ->
      let msg =
        Printf.sprintf "%s.of_bytes: wrong string size (%d)"
          name (MBytes.length b) in
      raise (Invalid_argument msg)
  | Some h -> h
let to_bytes = MBytes.of_string

let read src off = of_bytes_exn @@ MBytes.sub src off size
let write dst off h = MBytes.blit (to_bytes h) 0 dst off size

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.net_id
    ~length: size
    ~wrap: (fun s -> Hash s)
    ~of_raw:of_string ~to_raw: (fun h -> h)

let of_b58check_opt s =
  Base58.simple_decode b58check_encoding s
let of_b58check_exn s =
  match Base58.simple_decode b58check_encoding s with
  | Some x -> x
  | None -> Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" name
let of_b58check s =
  match Base58.simple_decode b58check_encoding s with
  | Some x -> Ok x
  | None -> generic_error "Unexpected hash (%s)" name
let to_b58check s = Base58.simple_encode b58check_encoding s
let to_short_b58check = to_b58check

let encoding =
  let open Data_encoding in
  splitted
    ~binary: (Fixed.string size)
    ~json:
      (describe ~title: (title ^ " (Base58Check-encoded Blake2B hash)") @@
       conv to_b58check (Data_encoding.Json.wrap_error of_b58check_exn) string)

let param ?(name=name) ?(desc=title) t =
  Cli_entries.(param ~name ~desc (parameter (fun _ str -> Lwt.return (of_b58check str))) t)

let pp ppf t =
  Format.pp_print_string ppf (to_b58check t)

let pp_short ppf t =
  Format.pp_print_string ppf (to_short_b58check t)

module Set = struct
  include Set.Make(struct type nonrec t = t let compare = compare end)
  exception Found of elt
  let random_elt s =
    let n = Random.int (cardinal s) in
    try
      ignore
        (fold (fun x i -> if i = n then raise (Found x) ; i+1) s 0 : int) ;
      assert false
    with Found x -> x
  let encoding =
    Data_encoding.conv
      elements
      (fun l -> List.fold_left (fun m x -> add x m) empty l)
      Data_encoding.(list encoding)
end
let random_set_elt = Set.random_elt

module Map = struct
  include Map.Make(struct type nonrec t = t let compare = compare end)
  let encoding arg_encoding =
    Data_encoding.conv
      bindings
      (fun l -> List.fold_left (fun m (k,v) -> add k v m) empty l)
      Data_encoding.(list (tup2 encoding arg_encoding))
end

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

module Table = struct
  include Hashtbl.Make(struct
      type nonrec t = t
      let hash = Hashtbl.hash
      let equal = equal
    end)
end

let () =
  Base58.check_encoded_prefix b58check_encoding "Net" 15

let zero =
  match of_hex (String.make (size * 2) '0') with
  | Some c -> c
  | None -> assert false


let rpc_arg =
  RPC_arg.make
    ~name:(Format.asprintf "hash.%s" name)
    ~descr:(Format.asprintf "A b58check-encoded hash (%s)" name)
    ~destruct:
      (fun s ->
         match of_b58check_opt s with
         | None -> Error ""
         | Some v -> Ok v)
    ~construct:to_b58check
    ()
