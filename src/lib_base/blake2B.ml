(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_crypto
open Error_monad

module Extend(H : Tezos_crypto.S.HASH) = struct

  include H

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (conv H.to_bytes H.of_bytes_exn (Fixed.bytes H.size))
      ~json:
        (describe ~title: (H.title ^ " (Base58Check-encoded Blake2B hash)") @@
         conv H.to_b58check (Data_encoding.Json.wrap_error H.of_b58check_exn) string)

  let of_b58check s =
    match H.of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        generic_error "Failed to read a base58-encoded hash (%s)" H.name

  let of_bytes s =
    match H.of_bytes_opt s with
    | Some x -> Ok x
    | None ->
        generic_error "Failed to deserialize a hash (%s)" H.name

  let rpc_arg =
    RPC_arg.make
      ~name:(Format.asprintf "hash.%s" H.name)
      ~descr:(Format.asprintf "A b58check-encoded hash (%s)" H.name)
      ~destruct:
        (fun s ->
           match H.of_b58check_opt s with
           | None ->
               Error (Format.asprintf
                        "failed to decode b58check-encoded hash (%s): %S"
                        H.name s)
           | Some v -> Ok v)
      ~construct:H.to_b58check
      ()

  let param ?(name=H.name) ?(desc=H.title) t =
    Clic.param
      ~name
      ~desc (Clic.parameter (fun _ str -> Lwt.return (of_b58check str))) t

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

  module Table = struct
    include Hashtbl.Make(struct
        type t = H.t
        let hash =
          if H.size >= 64 then
            fun h -> Int64.to_int (MBytes.get_int64 (H.to_bytes h) 0)
          else if H.size >= 32 then
            fun h -> Int32.to_int (MBytes.get_int32 (H.to_bytes h) 0)
          else
            fun h ->
              let r = ref 0 in
              let h = H.to_bytes h in
              for i = 0 to H.size / 8 - 1 do
                r := MBytes.get_uint8 h i + 8 * !r
              done ;
              !r
        let equal = H.equal
      end)
    let encoding arg_encoding =
      Data_encoding.conv
        (fun h -> fold (fun k v l -> (k, v) :: l) h [])
        (fun l ->
           let h = create (List.length l) in
           List.iter (fun (k,v) -> add h k v) l ;
           h)
        Data_encoding.(list (tup2 encoding arg_encoding))
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

module Extend_merkle_tree(H : Tezos_crypto.S.MERKLE_TREE) = struct

  include Extend(H)

  type elt = H.elt
  let elt_bytes = H.elt_bytes

  let empty = hash_bytes []

  include Tezos_crypto.Blake2B.Generic_Merkle_tree(struct
      type nonrec t = t
      type nonrec elt = elt
      let empty = empty
      let leaf x = hash_bytes [H.elt_bytes x]
      let node x y = hash_bytes [to_bytes x; to_bytes y]
    end)

  let path_encoding =
    let open Data_encoding in
    mu "path"
      (fun path_encoding ->
         union [
           case (Tag 240)
             (obj2
                (req "path" path_encoding)
                (req "right" encoding))
             (function Left (p, r) -> Some (p, r) | _ -> None)
             (fun (p, r) -> Left (p, r)) ;
           case (Tag 15)
             (obj2
                (req "left" encoding)
                (req "path" path_encoding))
             (function Right (r, p) -> Some (r, p) | _ -> None)
             (fun (r, p) -> Right (r, p)) ;
           case (Tag 0)
             unit
             (function Op -> Some () | _ -> None)
             (fun () -> Op)
         ])

end

module type Name = Blake2B.Name
module type PrefixedName = Blake2B.PrefixedName
module Make_minimal = Blake2B.Make_minimal
module Make
    (R : sig
       val register_encoding:
         prefix: string ->
         length:int ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base58.data) ->
         'a Base58.encoding
     end)
    (K : Blake2B.PrefixedName) =
  Extend(Blake2B.Make(R)(K))
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
    (K : PrefixedName)
    (Contents: sig
       type t
       val to_bytes: t -> MBytes.t
     end) =
  Extend_merkle_tree(Blake2B.Make_merkle_tree(R)(K)(Contents))
module Generic_Merkle_tree = Blake2B.Generic_Merkle_tree
