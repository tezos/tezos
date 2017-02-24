(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let (//) = Filename.concat
let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

open Utils

let () =
  let expected_primitive = "blake2b"
  and primitive = Sodium.Generichash.primitive in
  if primitive <> expected_primitive then begin
    Printf.eprintf
      "FATAL ERROR: \
       invalid value for Sodium.Generichash.primitive: %S (expected %S)@."
      primitive expected_primitive ;
    exit 1
  end

(*-- Signatures -------------------------------------------------------------*)

module type MINIMAL_HASH = sig

  type t

  val name: string
  val title: string

  val hash_bytes: MBytes.t list -> t
  val hash_string: string list -> t
  val size: int (* in bytes *)
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val to_hex: t -> string
  val of_hex: string -> t option
  val of_hex_exn: string -> t

  val to_string: t -> string
  val of_string: string -> t option
  val of_string_exn: string -> t

  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option
  val of_bytes_exn: MBytes.t -> t

  val read: MBytes.t -> int -> t
  val write: MBytes.t -> int -> t -> unit

  val to_path: t -> string list
  val of_path: string list -> t option
  val of_path_exn: string list -> t

  val prefix_path: string -> string list
  val path_length: int

end

module type INTERNAL_MINIMAL_HASH = sig
  include MINIMAL_HASH
  module Table : Hashtbl.S with type key = t
end

module type HASH = sig

  include MINIMAL_HASH

  val of_b58check: string -> t
  val to_b58check: t -> string
  val to_short_b58check: t -> string
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit
  type Base58.data += Hash of t
  val b58check_encoding: t Base58.encoding

  module Set : sig
    include Set.S with type elt = t
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

end

module type INTERNAL_HASH = sig
  include HASH
  module Table : Hashtbl.S with type key = t
end

module type Name = sig
  val name: string
  val title:  string
  val size: int option
end

module type PrefixedName = sig
  include Name
  val b58check_prefix: string
end

(*-- Type specific Hash builder ---------------------------------------------*)

module Make_minimal_Blake2B (K : Name) = struct

  type t = Sodium.Generichash.hash

  include K

  let size =
    match K.size with
    | None -> 32
    | Some x -> x

  let of_string s =
    if String.length s <> size then
      None
    else
      Some (Sodium.Generichash.Bytes.to_hash (Bytes.of_string s))
  let of_string_exn s =
    match of_string s with
    | None ->
        let msg =
          Printf.sprintf "%s.of_string: wrong string size (%d)"
            K.name (String.length s) in
        raise (Invalid_argument msg)
    | Some h -> h
  let to_string s = Bytes.to_string (Sodium.Generichash.Bytes.of_hash s)

  let of_hex s = of_string (Hex_encode.hex_decode s)
  let of_hex_exn s = of_string_exn (Hex_encode.hex_decode s)
  let to_hex s = Hex_encode.hex_encode (to_string s)

  let compare = Sodium.Generichash.compare
  let equal x y = compare x y = 0

  let of_bytes b =
    if MBytes.length b <> size then
      None
    else
      Some (Sodium.Generichash.Bigbytes.to_hash b)
  let of_bytes_exn b =
    match of_bytes b with
    | None ->
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            K.name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h -> h
  let to_bytes = Sodium.Generichash.Bigbytes.of_hash

  let read src off = of_bytes_exn @@ MBytes.sub src off size
  let write dst off h = MBytes.blit (to_bytes h) 0 dst off size

  let hash_bytes l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter (Bigbytes.update state) l ;
    final state

  let hash_string l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter
      (fun s -> Bytes.update state (BytesLabels.unsafe_of_string s))
      l ;
    final state

  let fold_read f buf off len init =
    let last = off + len * size in
    if last > MBytes.length buf then
      invalid_arg "Hash.read_set: invalid size.";
    let rec loop acc off =
      if off >= last then
        acc
      else
        let hash = read buf off in
        loop (f hash acc) (off + size)
    in
    loop init off

  let path_length = 6
  let to_path key =
    let key = to_hex key in
    [ String.sub key 0 2 ; String.sub key 2 2 ;
      String.sub key 4 2 ; String.sub key 6 2 ;
      String.sub key 8 2 ; String.sub key 10 (size * 2 - 10) ]
  let of_path path =
    let path = String.concat "" path in
    of_hex path
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn path

  let prefix_path p =
    let p = Hex_encode.hex_encode p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (len - 10) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

  module Table = struct
    include Hashtbl.Make(struct
      type nonrec t = t
      let hash s =
        Int64.to_int
          (EndianString.BigEndian.get_int64
             (Bytes.unsafe_to_string (Sodium.Generichash.Bytes.of_hash s))
             0)
      let equal = equal
    end)
  end

end

module Make_Blake2B (R : sig
    val register_encoding:
      prefix: string ->
      length:int ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base58.data) ->
      'a Base58.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal_Blake2B(K)

  (* Serializers *)

  type Base58.data += Hash of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length:size
      ~wrap: (fun s -> Hash s)
      ~of_raw:(fun h -> of_string h) ~to_raw:to_string

  let of_b58check s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf failwith "Unexpected hash (%s)" K.name
  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub (to_b58check s) 0 (10 + 2 * String.length K.b58check_prefix)

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (conv to_bytes of_bytes_exn (Fixed.bytes size))
      ~json:
        (describe ~title: (K.title ^ " (Base58Check-encoded Sha256)") @@
         conv to_b58check (Data_encoding.Json.wrap_error of_b58check) string)

  let param ?(name=K.name) ?(desc=K.title) t =
    Cli_entries.param ~name ~desc (fun _ str -> Lwt.return (of_b58check str)) t

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t)

  module Set = struct
    include Set.Make(struct type nonrec t = t let compare = compare end)
    let encoding =
      Data_encoding.conv
        elements
        (fun l -> List.fold_left (fun m x -> add x m) empty l)
        Data_encoding.(list encoding)
  end

  module Map = struct
    include Map.Make(struct type nonrec t = t let compare = compare end)
    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l -> List.fold_left (fun m (k,v) -> add k v m) empty l)
        Data_encoding.(list (tup2 encoding arg_encoding))
  end

end

(*-- Hash sets and maps -----------------------------------------------------*)

module Hash_set (Hash : HASH) = struct
  include Set.Make (Hash)
  let encoding =
    Data_encoding.conv
      elements
      (fun l -> List.fold_left (fun m x -> add x m) empty l)
      Data_encoding.(list Hash.encoding)
end

module Hash_map (Hash : HASH) = struct
  include Map.Make (Hash)
  let encoding arg_encoding =
    Data_encoding.conv
      bindings
      (fun l -> List.fold_left (fun m (k,v) -> add k v m) empty l)
      Data_encoding.(list (tup2 Hash.encoding arg_encoding))
end

module Hash_table (Hash : MINIMAL_HASH)
  : Hashtbl.S with type key = Hash.t
  = Hashtbl.Make (struct
    type t = Hash.t
    let equal = Hash.equal
    let hash v =
      let raw_hash = Hash.to_string v in
      let int64_hash = EndianString.BigEndian.get_int64 raw_hash 0 in
      Int64.to_int int64_hash
  end)

(*-- Pre-instanciated hashes ------------------------------------------------*)

module Block_hash =
  Make_Blake2B (Base58) (struct
    let name = "Block_hash"
    let title = "A Tezos block ID"
    let b58check_prefix = Base58.Prefix.block_hash
    let size = None
  end)

module Operation_hash =
  Make_Blake2B (Base58) (struct
    let name = "Operation_hash"
    let title = "A Tezos operation ID"
    let b58check_prefix = Base58.Prefix.operation_hash
    let size = None
   end)

module Protocol_hash =
  Make_Blake2B (Base58) (struct
    let name = "Protocol_hash"
    let title = "A Tezos protocol ID"
    let b58check_prefix = Base58.Prefix.protocol_hash
    let size = None
  end)

module Generic_hash =
  Make_minimal_Blake2B (struct
    let name = "Generic_hash"
    let title = ""
    let size = None
  end)

let () =
  Base58.check_encoded_prefix Block_hash.b58check_encoding "B" 51 ;
  Base58.check_encoded_prefix Operation_hash.b58check_encoding "o" 51 ;
  Base58.check_encoded_prefix Protocol_hash.b58check_encoding "P" 51
