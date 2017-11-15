(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(*  Tezos - Persistent structures on top of {!Store} or {!Context} *)

(*-- Signatures --------------------------------------------------------------*)

type key = string list
type value = MBytes.t

module type STORE = sig
  type t
  val mem: t -> key -> bool Lwt.t
  val dir_mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val fold:
    t -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
  val keys: t -> key -> key list Lwt.t
  val fold_keys:
    t -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

module type BYTES_STORE = sig
  type t
  type key
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val remove_rec: t -> key -> t Lwt.t
end

module type TYPED_STORE = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
end

module type KEY = sig
  type t
  val prefix: key
  val length: int
  val to_path: t -> key
  val of_path: key -> t
  val compare: t -> t -> int
end

module type VALUE = sig
  type t
  val of_bytes: value -> t option
  val to_bytes: t -> value
end

module type PERSISTENT_SET = sig
  type t and key
  val mem : t -> key -> bool Lwt.t
  val set : t -> key -> t Lwt.t
  val del : t -> key -> t Lwt.t
  val elements : t -> key list Lwt.t
  val clear : t -> t Lwt.t
  val iter : t -> f:(key -> unit Lwt.t) -> unit Lwt.t
  val fold : t -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

module type BUFFERED_PERSISTENT_SET = sig
  include PERSISTENT_SET
  module Set : Set.S with type elt = key
  val read : t -> Set.t Lwt.t
  val write : t -> Set.t -> t Lwt.t
end

module type PERSISTENT_MAP = sig
  type t and key and value
  val mem : t -> key -> bool Lwt.t
  val get : t -> key -> value option Lwt.t
  val set : t -> key -> value -> t Lwt.t
  val del : t -> key -> t Lwt.t
  val bindings : t -> (key * value) list Lwt.t
  val clear : t -> t Lwt.t
  val iter : t -> f:(key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold : t -> init:'a -> f:(key -> value -> 'a -> 'a Lwt.t) -> 'a Lwt.t
end

module type BUFFERED_PERSISTENT_MAP = sig
  include PERSISTENT_MAP
  module Map : Map.S with type key = key
  val read : t -> value Map.t Lwt.t
  val write : t -> value Map.t -> t Lwt.t
end

(*-- Utils -------------------------------------------------------------------*)

let prefix prf key =
  prf @ key

let unprefix prf key =
  let rec eat = function
    | k :: key, p :: prefix ->
        assert Compare.String.(k = p) ;
        eat (key, prefix)
    | key, [] -> key
    | _ -> assert false in
  eat (key, prf)

(*-- Typed Store Overlays ----------------------------------------------------*)

module MakeBytesStore
    (S : STORE) (K : KEY) = struct

  type t = S.t
  type key = K.t

  let to_path k =
    let suffix = K.to_path k in
    prefix K.prefix suffix

  let mem s k =
    S.mem s (to_path k)

  let get s k =
    S.get s (to_path k)

  let set s k v =
    S.set s (to_path k) v

  let del s k =
    S.del s (to_path k)

  let remove_rec s k =
    S.remove_rec s (to_path k)

end

module MakeTypedStore
    (S : STORE) (K : KEY) (C : VALUE) = struct

  type t = S.t
  type key = K.t
  type value = C.t

  module S = MakeBytesStore (S) (K)

  let mem = S.mem
  let get s k =
    S.get s k >>= function
    | None -> Lwt.return None
    | Some v -> Lwt.return (C.of_bytes v)
  let set s k v = S.set s k (C.to_bytes v)
  let del = S.del

end

module CompareStringList = Compare.List(Compare.String)

module RawKey = struct
  type t = key
  let prefix = []
  let length = 0
  let to_path p = p
  let of_path p = p
  let compare = CompareStringList.compare
end
module RawValue = struct
  type t = value
  let to_bytes b = b
  let of_bytes b = Some b
end

(*-- Set Builders ------------------------------------------------------------*)

module MakePersistentSet
    (S : STORE) (K : KEY) = struct

  let to_path k =
    let suffix = K.to_path k in
    assert Compare.Int.(List.length suffix = K.length) ;
    prefix K.prefix suffix

  let of_path p = K.of_path (unprefix K.prefix p)

  let empty =
    MBytes.of_string ""

  let inited_key =
    prefix K.prefix [ "inited" ]

  let mem c k =
    S.mem c (to_path k)

  let set c k =
    S.set c inited_key empty >>= fun c ->
    S.set c (to_path k) empty

  let del c k =
    S.del c (to_path k)

  let clear c =
    S.remove_rec c K.prefix

  let fold s ~init ~f =
    let rec dig i path acc =
      if Compare.Int.(i <= 1) then
        S.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir _ -> Lwt.return acc
          | `Key file -> f (of_path file) acc
        end
      else
        S.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir k ->
              dig (i-1) k acc
          | `Key _ ->
              Lwt.return acc
        end in
    dig K.length K.prefix init

  let iter c ~f = fold c ~init:() ~f:(fun x () -> f x)
  let elements c = fold c ~init:[] ~f:(fun p xs -> Lwt.return (p :: xs))

end

module MakeBufferedPersistentSet
    (S : STORE) (K : KEY) (Set : Set.S with type elt = K.t) = struct

  include MakePersistentSet(S)(K)

  let read c =
    fold c ~init:Set.empty ~f:(fun p set -> Lwt.return (Set.add p set))

  let write c set =
    S.set c inited_key empty >>= fun c ->
    read c >>= fun old_set ->
    Lwt_list.fold_left_s
      (fun c h -> S.del c (to_path h))
      c Set.(elements (diff old_set set)) >>= fun c ->
    Lwt_list.fold_left_s
      (fun c h -> S.set c (to_path h) empty)
      c Set.(elements (diff set old_set))

end

(*-- Map Builders ------------------------------------------------------------*)

module MakePersistentMap
    (S : STORE) (K : KEY) (C : VALUE) = struct

  let to_path k =
    let suffix = K.to_path k in
    assert Compare.Int.(List.length suffix = K.length) ;
    prefix K.prefix suffix

  let of_path p = K.of_path (unprefix K.prefix p)

  let empty =
    MBytes.of_string ""

  let inited_key =
    prefix K.prefix [ "inited" ]

  let mem c k =
    S.mem c (to_path k)

  let get c k =
    S.get c (to_path k) >|= function
    | None -> None
    | Some b -> C.of_bytes b

  let set c k b =
    S.set c inited_key empty >>= fun c ->
    S.set c (to_path k) (C.to_bytes b)

  let del c k =
    S.del c (to_path k)

  let clear c =
    S.remove_rec c K.prefix

  let fold s ~init ~f =
    let rec dig i path acc =
      if Compare.Int.(i <= 1) then
        S.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir _ -> Lwt.return acc
          | `Key file ->
              S.get s file >>= function
              | None -> Lwt.return acc
              | Some b ->
                  match C.of_bytes b with
                  | None ->
                      (* Silently ignore unparsable data *)
                      Lwt.return acc
                  | Some v -> f (of_path file) v acc
        end
      else
        S.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir k -> dig (i-1) k acc
          | `Key _ -> Lwt.return acc
        end in
    dig K.length K.prefix init

  let iter c ~f = fold c ~init:() ~f:(fun k v () -> f k v)
  let bindings c = fold c ~init:[] ~f:(fun k v acc -> Lwt.return ((k, v) :: acc))

end

module MakeBufferedPersistentMap
    (S : STORE) (K : KEY) (C : VALUE) (Map : Map.S with type key = K.t) = struct

  include MakePersistentMap(S)(K)(C)

  let read c = fold c ~init:Map.empty ~f:(fun k v m -> Lwt.return (Map.add k v m))

  let write c m =
    clear c >>= fun c ->
    S.set c inited_key empty >>= fun c ->
    Lwt_list.fold_left_s
      (fun c (k, b) -> S.set c (to_path k) (C.to_bytes b))
      c (Map.bindings m)

end

(*-- Predefined Instances ----------------------------------------------------*)

module MBytesValue = struct
  type t = MBytes.t
  let of_bytes x = Some x
  let to_bytes x = x
end

module MakePersistentBytesMap
    (S : STORE) (K : KEY) =
  MakePersistentMap(S)(K)(MBytesValue)

module MakeBufferedPersistentBytesMap
    (S : STORE) (K : KEY) (Map : Map.S with type key = K.t) =
  MakeBufferedPersistentMap(S)(K)(MBytesValue)(Map)

module type TYPED_VALUE_REPR = sig
  type value
  val encoding: value Data_encoding.t
end

module TypedValue (T : TYPED_VALUE_REPR) = struct
  type t = T.value
  let of_bytes x = Data_encoding.Binary.of_bytes T.encoding x
  let to_bytes x = Data_encoding.Binary.to_bytes T.encoding x
end

module MakePersistentTypedMap
    (S : STORE)    (K : KEY)
    (T : TYPED_VALUE_REPR) =
  MakePersistentMap(S)(K)(TypedValue(T))

module MakeBufferedPersistentTypedMap
    (S : STORE)
    (K : KEY)
    (T : TYPED_VALUE_REPR)
    (Map : Map.S with type key = K.t)
  =
  MakeBufferedPersistentMap(S)(K)(TypedValue(T))(Map)

module MakeHashResolver
    (Store : sig
       type t
       val dir_mem: t -> string list -> bool Lwt.t
       val fold:
         t -> key -> init:'a ->
         f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
         'a Lwt.t
       val prefix: string list
     end)
    (H: HASH) = struct
  let plen = List.length Store.prefix
  let build path =
    H.of_path_exn @@
    Misc.remove_elem_from_list plen path
  let list t k =
    Store.fold t k ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
  let resolve t p =
    let rec loop prefix = function
      | [] ->
          list t prefix >>= fun prefixes ->
          Lwt_list.map_p (function
              | `Key prefix | `Dir prefix -> loop prefix []) prefixes
          >|= List.flatten
      | "" :: ds ->
          list t prefix >>= fun prefixes ->
          Lwt_list.map_p (function
              | `Key prefix | `Dir prefix -> loop prefix ds) prefixes
          >|= List.flatten
      | [d] ->
          list t prefix >>= fun prefixes ->
          Lwt_list.filter_map_p (function
              | `Dir _ -> Lwt.return_none
              | `Key prefix ->
                  match Misc.remove_prefix ~prefix:d (List.hd (List.rev prefix)) with
                  | None -> Lwt.return_none
                  | Some _ -> Lwt.return (Some (build prefix))
            ) prefixes
      | d :: ds ->
          Store.dir_mem t (prefix @ [d]) >>= function
          | true -> loop (prefix @ [d]) ds
          | false -> Lwt.return_nil in
    loop Store.prefix (H.prefix_path p)
end
