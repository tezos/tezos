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
  val of_raw: string -> t
  val to_raw: t -> string
  val of_hex: string -> t
  val to_hex: t -> string
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t
  val read: MBytes.t -> int -> t
  val write: MBytes.t -> int -> t -> unit
  val to_path: t -> string list
  val of_path: string list -> t
  val prefix_path: string -> string list
  val path_len: int

end

module type HASH = sig

  include MINIMAL_HASH

  val of_b48check: string -> t
  val to_b48check: t -> string
  val to_short_b48check: t -> string
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit
  type Base48.data += Hash of t
  val b48check_encoding: t Base48.encoding

end

module type Name = sig
  val name: string
  val title:  string
end

module type PrefixedName = sig
  include Name
  val b48check_prefix: string
end

(*-- Type specific Hash builder ---------------------------------------------*)

module Make_minimal_SHA256 (K : Name) = struct

  type t = string

  include K

  let size = 32 (* SHA256 *)

  let of_raw s =
    if String.length s <> size then begin
      let msg =
        Printf.sprintf "%s.of_raw: wrong string size for %S (%d)"
          K.name s (String.length s) in
      raise (Invalid_argument msg)
    end;
    s
  let to_raw s = s

  let of_hex s = of_raw (Hex_encode.hex_decode s)
  let to_hex s = Hex_encode.hex_encode s

  let compare = String.compare
  let equal : t -> t -> bool = (=)

  let of_bytes b =
    let s = MBytes.to_string b in
    if String.length s <> size then begin
      let msg =
        Printf.sprintf "%s.of_bytes: wrong string size for %S (%d)"
          K.name s (String.length s) in
      raise (Invalid_argument msg)
    end;
    s
  let to_bytes = MBytes.of_string

  let read src off = MBytes.substring src off size
  let write dst off h = MBytes.blit_from_string h 0 dst off size

  let hash_bytes l =
    let hash = Cryptokit.Hash.sha256 () in
    (* FIXME... bigstring... *)
    List.iter (fun b -> hash#add_string (MBytes.to_string b)) l;
    let r = hash#result in hash#wipe; r

  let hash_string l =
    let hash = Cryptokit.Hash.sha256 () in
    List.iter (fun b -> hash#add_string b) l;
    let r = hash#result in hash#wipe; r

  module Set = Set.Make(struct type t = string let compare = compare end)

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

  module Map = Map.Make(struct type t = string let compare = compare end)
  module Table =
    (* TODO improve *)
    Hashtbl.Make(struct
      type t = string
      let hash s = Int64.to_int (EndianString.BigEndian.get_int64 s 0)
      let equal = equal
    end)

  let path_len = 6
  let to_path key =
    let key = to_hex key in
    [ String.sub key 0 2 ; String.sub key 2 2 ;
      String.sub key 4 2 ; String.sub key 6 2 ;
      String.sub key 8 2 ; String.sub key 10 (size * 2 - 10) ]
  let of_path path =
    let path = String.concat "" path in
    of_hex path

  let prefix_path p =
    let p = to_hex p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (len - 10) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

end

module Make_SHA256 (R : sig
    val register_encoding:
      prefix: string ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base48.data) ->
      'a Base48.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal_SHA256(K)

  (* Serializers *)

  type Base48.data += Hash of t

  let b48check_encoding =
    R.register_encoding
      ~prefix: K.b48check_prefix
      ~wrap: (fun x -> Hash x)
      ~of_raw:(fun s -> Some s) ~to_raw

  let of_b48check s =
    match Base48.simple_decode b48check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf failwith "Unexpected hash (%s)" K.name
  let to_b48check s = Base48.simple_encode b48check_encoding s

  let to_short_b48check s = String.sub (to_b48check s) 0 12

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (conv to_bytes of_bytes (Fixed.bytes size))
      ~json:
        (describe ~title: (K.title ^ " (Base48Check-encoded Sha256)") @@
         conv to_b48check (Data_encoding.Json.wrap_error of_b48check) string)

  let param ?(name=K.name) ?(desc=K.title) t =
    Cli_entries.param ~name ~desc (fun str -> Lwt.return (of_b48check str)) t

  let pp ppf t =
    Format.pp_print_string ppf (to_b48check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b48check t)

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

module Hash_table (Hash : HASH)
  : Hashtbl.S with type key = Hash.t
  = Hashtbl.Make (struct
    type t = Hash.t
    let equal = Hash.equal
    let hash v =
      let raw_hash = Hash.to_raw v in
      let int64_hash = EndianString.BigEndian.get_int64 raw_hash 0 in
      Int64.to_int int64_hash
  end)

(*-- Pre-instanciated hashes ------------------------------------------------*)

module Block_hash =
  Make_SHA256 (Base48) (struct
    let name = "Block_hash"
    let title = "A Tezos block ID"
    let b48check_prefix = Base48.Prefix.block_hash
  end)

module Block_hash_set = Hash_set (Block_hash)
module Block_hash_map = Hash_map (Block_hash)
module Block_hash_table = Hash_table (Block_hash)

module Operation_hash =
  Make_SHA256 (Base48) (struct
    let name = "Operation_hash"
    let title = "A Tezos operation ID"
    let b48check_prefix = Base48.Prefix.operation_hash
   end)

module Operation_hash_set = Hash_set (Operation_hash)
module Operation_hash_map = Hash_map (Operation_hash)
module Operation_hash_table = Hash_table (Operation_hash)

module Protocol_hash =
  Make_SHA256 (Base48) (struct
    let name = "Protocol_hash"
    let title = "A Tezos protocol ID"
    let b48check_prefix = Base48.Prefix.protocol_hash
  end)

module Protocol_hash_set = Hash_set (Protocol_hash)
module Protocol_hash_map = Hash_map (Protocol_hash)
module Protocol_hash_table = Hash_table (Protocol_hash)
