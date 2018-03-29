(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Storage_sigs

module type ENCODED_VALUE = sig
  type t
  val encoding: t Data_encoding.t
end

module Make_value (V : ENCODED_VALUE) = struct
  type t = V.t
  let of_bytes b =
    match Data_encoding.Binary.of_bytes V.encoding b with
    | None -> Error [Raw_context.Storage_error (Corrupted_data [(* FIXME??*)])]
    | Some v -> Ok v
  let to_bytes v =
    match Data_encoding.Binary.to_bytes V.encoding v with
    | Some b -> b
    | None -> MBytes.create 0
end

module Raw_value = struct
  type t = MBytes.t
  let of_bytes b = ok b
  let to_bytes b = b
end

let map_key f = function
  | `Key k -> `Key (f k)
  | `Dir k -> `Dir (f k)

module Make_subcontext (C : Raw_context.T) (N : NAME)
  : Raw_context.T with type t = C.t = struct
  type t = C.t
  type context = t
  let name_length = List.length N.name
  let to_key k = N.name @ k
  let of_key k = Misc.remove_elem_from_list name_length k
  let mem t k = C.mem t (to_key k)
  let dir_mem t k = C.dir_mem t (to_key k)
  let get t k = C.get t (to_key k)
  let get_option t k = C.get_option t (to_key k)
  let init t k v = C.init t (to_key k) v
  let set t k v = C.set t (to_key k) v
  let init_set t k v = C.init_set t (to_key k) v
  let set_option t k v = C.set_option t (to_key k) v
  let delete t k = C.delete t (to_key k)
  let remove t k = C.remove t (to_key k)
  let remove_rec t k = C.remove_rec t (to_key k)
  let copy t ~from ~to_ = C.copy t ~from:(to_key from) ~to_:(to_key to_)
  let fold t k ~init ~f =
    C.fold t (to_key k) ~init
      ~f:(fun k acc -> f (map_key of_key k) acc)
  let keys t k = C.keys t (to_key k) >|= fun keys -> List.map of_key keys
  let fold_keys t k ~init ~f =
    C.fold_keys t (to_key k) ~init ~f:(fun k acc -> f (of_key k) acc)
  let project = C.project
end

module Make_single_data_storage (C : Raw_context.T) (N : NAME) (V : VALUE)
  : Single_data_storage with type t = C.t
                         and type value = V.t = struct
  type t = C.t
  type context = t
  type value = V.t
  let mem t =
    C.mem t N.name
  let get t =
    C.get t N.name >>=? fun b ->
    Lwt.return (V.of_bytes b)
  let get_option t =
    C.get_option t N.name >>= function
    | None -> return None
    | Some b ->
        match V.of_bytes b with
        | Ok v -> return (Some v)
        | Error _ as err -> Lwt.return err
  let init t v =
    C.init t N.name (V.to_bytes v) >>=? fun t ->
    return (C.project t)
  let set t v =
    C.set t N.name (V.to_bytes v) >>=? fun t ->
    return (C.project t)
  let init_set t v =
    C.init_set t N.name (V.to_bytes v) >>= fun t ->
    Lwt.return (C.project t)
  let set_option t v =
    C.set_option t N.name (Option.map ~f:V.to_bytes v) >>= fun t ->
    Lwt.return (C.project t)
  let remove t =
    C.remove t N.name >>= fun t ->
    Lwt.return (C.project t)
  let delete t =
    C.delete t N.name >>=? fun t ->
    return (C.project t)
end

module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end

module Pair(I1 : INDEX)(I2 : INDEX)
  : INDEX with type t = I1.t * I2.t = struct
  type t = I1.t * I2.t
  let path_length = I1.path_length + I2.path_length
  let to_path (x, y) l = I1.to_path x (I2.to_path y l)
  let of_path l =
    match Misc.take I1.path_length l with
    | None -> None
    | Some (l1, l2) ->
        match I1.of_path l1, I2.of_path l2 with
        | Some x, Some y -> Some (x, y)
        | _ -> None
end

module Make_data_set_storage (C : Raw_context.T) (I : INDEX)
  : Data_set_storage with type t = C.t and type elt = I.t = struct

  type t = C.t
  type context = t
  type elt = I.t

  let inited = MBytes.of_string "inited"

  let mem s i =
    C.mem s (I.to_path i [])
  let add s i =
    C.init_set s (I.to_path i []) inited >>= fun t ->
    Lwt.return (C.project t)
  let del s i =
    C.remove s (I.to_path i []) >>= fun t ->
    Lwt.return (C.project t)
  let set s i = function
    | true -> add s i
    | false -> del s i
  let clear s =
    C.remove_rec s [] >>= fun t ->
    Lwt.return (C.project t)

  let fold s ~init ~f =
    let rec dig i path acc =
      if Compare.Int.(i <= 1) then
        C.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir _ -> Lwt.return acc
          | `Key file ->
              match I.of_path file with
              | None -> assert false
              | Some p -> f p acc
        end
      else
        C.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir k ->
              dig (i-1) k acc
          | `Key _ ->
              Lwt.return acc
        end in
    dig I.path_length [] init

  let elements s =
    fold s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

end

module Make_indexed_data_storage
    (C : Raw_context.T) (I : INDEX) (V : VALUE)
  : Indexed_data_storage with type t = C.t
                          and type key = I.t
                          and type value = V.t = struct
  type t = C.t
  type context = t
  type key = I.t
  type value = V.t
  let mem s i =
    C.mem s (I.to_path i [])
  let get s i =
    C.get s (I.to_path i []) >>=? fun b ->
    Lwt.return (V.of_bytes b)
  let get_option s i =
    C.get_option s (I.to_path i []) >>= function
    | None -> return None
    | Some b ->
        match V.of_bytes b with
        | Ok v -> return (Some v)
        | Error _ as err -> Lwt.return err
  let set s i v =
    C.set s (I.to_path i []) (V.to_bytes v) >>=? fun t ->
    return (C.project t)
  let init s i v =
    C.init s (I.to_path i []) (V.to_bytes v) >>=? fun t ->
    return (C.project t)
  let init_set s i v =
    C.init_set s (I.to_path i []) (V.to_bytes v) >>= fun t ->
    Lwt.return (C.project t)
  let set_option s i v =
    C.set_option s (I.to_path i []) (Option.map ~f:V.to_bytes v) >>= fun t ->
    Lwt.return (C.project t)
  let remove s i =
    C.remove s (I.to_path i []) >>= fun t ->
    Lwt.return (C.project t)
  let delete s i =
    C.delete s (I.to_path i []) >>=? fun t ->
    return (C.project t)
  let clear s =
    C.remove_rec s [] >>= fun t ->
    Lwt.return (C.project t)

  let fold_keys s ~init ~f =
    let rec dig i path acc =
      if Compare.Int.(i <= 1) then
        C.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir _ -> Lwt.return acc
          | `Key file ->
              match I.of_path file with
              | None -> assert false
              | Some path -> f path acc
        end
      else
        C.fold s path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir k -> dig (i-1) k acc
          | `Key _ -> Lwt.return acc
        end in
    dig I.path_length [] init

  let fold s ~init ~f =
    let f path acc =
      get s path >>= function
      | Error _ ->
          (* FIXME: silently ignore unparsable data *)
          Lwt.return acc
      | Ok v ->
          f path v acc in
    fold_keys s ~init ~f
  let bindings s =
    fold s ~init:[] ~f:(fun p v acc -> Lwt.return ((p,v) :: acc))
  let keys s =
    fold_keys s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

end

module Make_indexed_data_snapshotable_storage (C : Raw_context.T)
    (Snapshot_index : INDEX) (I : INDEX) (V : VALUE)
  : Indexed_data_snapshotable_storage with type t = C.t
                                       and type snapshot = Snapshot_index.t
                                       and type key = I.t
                                       and type value = V.t = struct
  type snapshot = Snapshot_index.t

  let data_name = ["current"]
  let snapshot_name = ["snapshot"]

  module C_data = Make_subcontext(C)(struct let name = data_name end)
  module C_snapshot = Make_subcontext(C)(struct let name = snapshot_name end)

  include Make_indexed_data_storage(C_data)(I) (V)
  module Snapshot = Make_indexed_data_storage(C_snapshot)(Pair(Snapshot_index)(I))(V)

  let snapshot_path id = snapshot_name @ Snapshot_index.to_path id []

  let snapshot_exists s id =
    C.dir_mem s (snapshot_path id)

  let snapshot s id =
    C.copy s ~from:data_name ~to_:(snapshot_path id) >>=? fun t ->
    return (C.project t)

  let delete_snapshot s id =
    C.remove_rec s (Snapshot_index.to_path id snapshot_name) >>= fun t ->
    Lwt.return (C.project t)

end


module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX)
  : Indexed_raw_context with type t = C.t
                         and type key = I.t = struct

  type t = C.t
  type context = t
  type key = I.t

  let clear t =
    C.remove_rec t [] >>= fun t ->
    Lwt.return (C.project t)

  module Raw_context = struct
    type t = C.t * I.t
    type context = t
    let to_key i k = I.to_path i k
    let of_key k = Misc.remove_elem_from_list I.path_length k
    let mem (t, i) k = C.mem t (to_key i k)
    let dir_mem (t, i) k = C.dir_mem t (to_key i k)
    let get (t, i) k = C.get t (to_key i k)
    let get_option (t, i) k = C.get_option t (to_key i k)
    let init (t, i) k v =
      C.init t (to_key i k) v >>=? fun t -> return (t, i)
    let set (t, i) k v =
      C.set t (to_key i k) v >>=? fun t -> return (t, i)
    let init_set (t, i) k v =
      C.init_set t (to_key i k) v >>= fun t -> Lwt.return (t, i)
    let set_option (t, i) k v =
      C.set_option t (to_key i k) v >>= fun t -> Lwt.return (t, i)
    let delete (t, i) k =
      C.delete t (to_key i k) >>=? fun t -> return (t, i)
    let remove (t, i) k =
      C.remove t (to_key i k) >>= fun t -> Lwt.return (t, i)
    let remove_rec (t, i) k =
      C.remove_rec t (to_key i k) >>= fun t -> Lwt.return (t, i)
    let copy (t, i) ~from ~to_ =
      C.copy t ~from:(to_key i from) ~to_:(to_key i to_) >>=? fun t ->
      return (t, i)
    let fold (t, i) k ~init ~f =
      C.fold t (to_key i k) ~init
        ~f:(fun k acc -> f (map_key of_key k) acc)
    let keys (t, i) k = C.keys t (to_key i k) >|= fun keys -> List.map of_key keys
    let fold_keys (t, i) k ~init ~f =
      C.fold_keys t (to_key i k) ~init ~f:(fun k acc -> f (of_key k) acc)
    let project (t, _) = C.project t
  end

  let fold_keys t ~init ~f =
    let rec dig i path acc =
      if Compare.Int.(i <= 0) then
        match I.of_path path with
        | None -> assert false
        | Some path -> f path acc
      else
        C.fold t path ~init:acc ~f:begin fun k acc ->
          match k with
          | `Dir k -> dig (i-1) k acc
          | `Key _ -> Lwt.return acc
        end in
    dig I.path_length [] init

  let keys t =
    fold_keys t ~init:[] ~f:(fun i acc -> Lwt.return (i :: acc))

  let list t k = C.fold t k ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
  let resolve t prefix =
    let rec loop i prefix = function
      | [] when Compare.Int.(i = I.path_length) -> begin
          match I.of_path prefix with
          | None -> assert false
          | Some path -> Lwt.return [path]
        end
      | [] ->
          list t prefix >>= fun prefixes ->
          Lwt_list.map_p (function
              | `Key prefix | `Dir prefix -> loop (i+1) prefix []) prefixes
          >|= List.flatten
      | [d] when Compare.Int.(i = I.path_length - 1) ->
          if Compare.Int.(i >= I.path_length) then invalid_arg "IO.resolve" ;
          list t prefix >>= fun prefixes ->
          Lwt_list.map_p (function
              | `Key prefix | `Dir prefix ->
                  match Misc.remove_prefix ~prefix:d (List.hd (List.rev prefix)) with
                  | None -> Lwt.return_nil
                  | Some _ -> loop (i+1) prefix [])
            prefixes
          >|= List.flatten
      | "" :: ds ->
          list t prefix >>= fun prefixes ->
          Lwt_list.map_p (function
              | `Key prefix | `Dir prefix -> loop (i+1) prefix ds) prefixes
          >|= List.flatten
      | d :: ds ->
          if Compare.Int.(i >= I.path_length) then invalid_arg "IO.resolve" ;
          C.dir_mem t (prefix @ [d]) >>= function
          | true -> loop (i+1) (prefix @ [d]) ds
          | false -> Lwt.return_nil in
    loop 0 [] prefix

  module Make_set (N : NAME) = struct
    type t = C.t
    type context = t
    type elt = I.t
    let inited = MBytes.of_string "inited"
    let mem s i = Raw_context.mem (s, i) N.name
    let add s i =
      Raw_context.init_set (s, i) N.name inited >>= fun (s, _) ->
      Lwt.return (C.project s)
    let del s i =
      Raw_context.remove (s, i) N.name >>= fun (s, _) ->
      Lwt.return (C.project s)
    let set s i = function
      | true -> add s i
      | false -> del s i
    let clear s =
      fold_keys s
        ~init:s
        ~f:begin fun i s ->
          Raw_context.remove (s, i) N.name >>= fun (s, _) ->
          Lwt.return s
        end >>= fun t ->
      Lwt.return (C.project t)
    let fold s ~init ~f =
      fold_keys s ~init
        ~f:(fun i acc ->
            mem s i >>= function
            | true -> f i acc
            | false -> Lwt.return acc)
    let elements s =
      fold s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))
  end

  module Make_map (N : NAME) (V : VALUE) = struct
    type t = C.t
    type context = t
    type key = I.t
    type value = V.t
    let mem s i =
      Raw_context.mem (s,i) N.name
    let get s i =
      Raw_context.get (s,i) N.name >>=? fun b ->
      Lwt.return (V.of_bytes b)
    let get_option s i =
      Raw_context.get_option (s,i) N.name >>= function
      | None -> return None
      | Some b ->
          match V.of_bytes b with
          | Ok v -> return (Some v)
          | Error _ as err -> Lwt.return err
    let set s i v =
      Raw_context.set (s,i) N.name (V.to_bytes v) >>=? fun (s, _) ->
      return (C.project s)
    let init s i v =
      Raw_context.init (s,i) N.name (V.to_bytes v) >>=? fun (s, _) ->
      return (C.project s)
    let init_set s i v =
      Raw_context.init_set (s,i) N.name (V.to_bytes v) >>= fun (s, _) ->
      Lwt.return (C.project s)
    let set_option s i v =
      Raw_context.set_option (s,i)
        N.name (Option.map ~f:V.to_bytes v) >>= fun (s, _) ->
      Lwt.return (C.project s)
    let remove s i =
      Raw_context.remove (s,i) N.name >>= fun (s, _) ->
      Lwt.return (C.project s)
    let delete s i =
      Raw_context.delete (s,i) N.name >>=? fun (s, _) ->
      return (C.project s)
    let clear s =
      fold_keys s ~init:s
        ~f:begin fun i s ->
          Raw_context.remove (s,i) N.name >>= fun (s, _) ->
          Lwt.return s
        end >>= fun t ->
      Lwt.return (C.project t)
    let fold s ~init ~f =
      fold_keys s ~init
        ~f:(fun i acc ->
            get s i >>= function
            | Error _ -> Lwt.return acc
            | Ok v -> f i v acc)
    let bindings s =
      fold s ~init:[] ~f:(fun p v acc -> Lwt.return ((p,v) :: acc))
    let fold_keys s ~init ~f =
      fold_keys s ~init
        ~f:(fun i acc ->
            mem s i >>= function
            | false -> Lwt.return acc
            | true -> f i acc)
    let keys s =
      fold_keys s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))
  end

end

module Wrap_indexed_data_storage
    (C : Indexed_data_storage)
    (K : sig
       type t
       val wrap: t -> C.key
       val unwrap: C.key -> t option
     end) = struct
  type t = C.t
  type context = C.t
  type key = K.t
  type value = C.value
  let mem ctxt k = C.mem ctxt (K.wrap k)
  let get ctxt k = C.get ctxt (K.wrap k)
  let get_option ctxt k = C.get_option ctxt (K.wrap k)
  let set ctxt k v = C.set ctxt (K.wrap k) v
  let init ctxt k v = C.init ctxt (K.wrap k) v
  let init_set ctxt k v = C.init_set ctxt (K.wrap k) v
  let set_option ctxt k v = C.set_option ctxt (K.wrap k) v
  let delete ctxt k = C.delete ctxt (K.wrap k)
  let remove ctxt k = C.remove ctxt (K.wrap k)
  let clear ctxt = C.clear ctxt
  let fold ctxt ~init ~f =
    C.fold ctxt ~init ~f:(fun k v acc ->
        match K.unwrap k with
        | None -> Lwt.return acc
        | Some k -> f k v acc)
  let bindings s =
    fold s ~init:[] ~f:(fun p v acc -> Lwt.return ((p,v) :: acc))
  let fold_keys s ~init ~f =
    C.fold_keys s ~init
      ~f:(fun k acc ->
          match K.unwrap k with
          | None -> Lwt.return acc
          | Some k -> f k acc)
  let keys s =
    fold_keys s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

end
