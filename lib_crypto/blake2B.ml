(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

(*-- Type specific Hash builder ---------------------------------------------*)

module Make_minimal (K : S.Name) = struct

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

  let path_length = 6
  let to_path key l =
    let key = to_hex key in
    String.sub key 0 2 :: String.sub key 2 2 ::
    String.sub key 4 2 :: String.sub key 6 2 ::
    String.sub key 8 2 :: String.sub key 10 (size * 2 - 10) :: l
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

module Make (R : sig
    val register_encoding:
      prefix: string ->
      length:int ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base58.data) ->
      'a Base58.encoding
  end) (K : S.PrefixedName) = struct

  include Make_minimal(K)

  (* Serializers *)

  type Base58.data += Hash of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length:size
      ~wrap: (fun s -> Hash s)
      ~of_raw:(fun h -> of_string h) ~to_raw:to_string

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" K.name
  let of_b58check s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> Ok x
    | None -> generic_error "Unexpected hash (%s)" K.name
  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub (to_b58check s) 0 (10 + 2 * String.length K.b58check_prefix)

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (conv to_bytes of_bytes_exn (Fixed.bytes size))
      ~json:
        (describe ~title: (K.title ^ " (Base58Check-encoded Blake2B hash)") @@
         conv to_b58check (Data_encoding.Json.wrap_error of_b58check_exn) string)

  let param ?(name=K.name) ?(desc=K.title) t =
    Cli_entries.param
      ~name
      ~desc (Cli_entries.parameter (fun _ str -> Lwt.return (of_b58check str))) t

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

  let zero =
    match of_hex (String.make (size * 2) '0') with
    | Some c -> c
    | None -> assert false

end

module Generic_Merkle_tree (H : sig
    type t
    type elt
    val encoding : t Data_encoding.t
    val empty : t
    val leaf : elt -> t
    val node : t -> t -> t
  end) = struct

  let rec step a n =
    let m = (n+1) / 2 in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      a.(0)
    else if m mod 2 = 0 then
      step a m
    else begin
      a.(m+1) <- a.(m) ;
      step a (m+1)
    end

  let empty = H.empty

  let compute xs =
    match xs with
    | [] -> H.empty
    | [x] -> H.leaf x
    | _ :: _ :: _ ->
        let last = TzList.last_exn xs in
        let n = List.length xs in
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x -> a.(i) <- H.leaf x) xs ;
        step a n

  type path =
    | Left of path * H.t
    | Right of H.t * path
    | Op

  let rec step_path a n p j =
    let m = (n+1) / 2 in
    let p = if j mod 2 = 0 then Left (p, a.(j+1)) else Right (a.(j-1), p) in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      p
    else if m mod 2 = 0 then
      step_path a m p (j/2)
    else begin
      a.(m+1) <- a.(m) ;
      step_path a (m+1) p (j/2)
    end

  let compute_path xs i =
    match xs with
    | [] -> invalid_arg "compute_path"
    | [_] -> Op
    | _ :: _ :: _ ->
        let last = TzList.last_exn xs in
        let n = List.length xs in
        if i < 0 || n <= i then invalid_arg "compute_path" ;
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x -> a.(i) <- H.leaf x) xs ;
        step_path a n Op i

  let rec check_path p h =
    match p with
    | Op ->
        H.leaf h, 1, 0
    | Left (p, r) ->
        let l, s, pos = check_path p h in
        H.node l r, s * 2, pos
    | Right (l, p) ->
        let r, s, pos = check_path p h in
        H.node l r, s * 2, pos + s

  let check_path p h =
    let h, _, pos = check_path p h in
    h, pos

  let path_encoding =
    let open Data_encoding in
    mu "path"
      (fun path_encoding ->
         union [
           case ~tag:240
             (obj2
                (req "path" path_encoding)
                (req "right" H.encoding))
             (function Left (p, r) -> Some (p, r) | _ -> None)
             (fun (p, r) -> Left (p, r)) ;
           case ~tag:15
             (obj2
                (req "left" H.encoding)
                (req "path" path_encoding))
             (function Right (r, p) -> Some (r, p) | _ -> None)
             (fun (r, p) -> Right (r, p)) ;
           case ~tag:0
             unit
             (function Op -> Some () | _ -> None)
             (fun () -> Op)
         ])

end

module Make_merkle_tree
    (R : sig
       val register_encoding:
         prefix: string ->
         length:int ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base58.data) ->
         'a Base58.encoding
     end)
    (K : S.PrefixedName)
    (Contents: sig
       type t
       val to_bytes: t -> MBytes.t
     end) = struct

  include Make (R) (K)

  type elt = Contents.t

  let empty = hash_bytes []

  include Generic_Merkle_tree(struct
      type nonrec t = t
      type nonrec elt = elt
      let encoding = encoding
      let empty = empty
      let leaf x = hash_bytes [Contents.to_bytes x]
      let node x y = hash_bytes [to_bytes x; to_bytes y]
    end)

end

include
  Make_minimal (struct
    let name = "Generic_hash"
    let title = ""
    let size = None
  end)
