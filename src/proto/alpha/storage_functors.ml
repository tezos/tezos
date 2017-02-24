(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Typed storage accessor builders *)

open Misc

type context = Context.t * Constants_repr.constants

(*-- Errors ------------------------------------------------------------------*)

type error += Storage_error of string

let () =
  let open Data_encoding in
  register_error_kind `Permanent
    ~id:"storageError"
    ~title: "Storage error (fatal internal error)"
    ~description:
      "An error that should never happen unless something \
       has been deleted or corrupted in the database"
    ~pp:(fun ppf msg ->
        Format.fprintf ppf "@[<v 2>Storage error:@ %a@]"
          pp_print_paragraph msg)
    (obj1 (req "msg" string))
    (function Storage_error msg -> Some msg | _ -> None)
    (fun msg -> Storage_error msg)

(*-- Generic data accessor ---------------------------------------------------*)

module type Raw_data_description = sig
  type key
  type value
  val name : string
  val key : key -> string list
  val of_bytes : MBytes.t -> value tzresult
  val to_bytes : value -> MBytes.t
end

module Make_raw_data_storage (P : Raw_data_description) = struct

  type key = P.key
  type value = P.value

  let key k = P.key k

  let key_to_string l = String.concat "/" (key l)

  let get (c, _) k =
    Context.get c (key k) >>= function
    | None ->
        let msg =
          "cannot get undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)
    | Some bytes ->
        Lwt.return (P.of_bytes bytes)

  let mem (c, _) k = Context.mem c (key k)

  let get_option (c, _) k =
    Context.get c (key k) >>= function
    | None -> return None
    | Some bytes ->
        Lwt.return (P.of_bytes bytes >|? fun v -> Some v)

  (* Verify that the key is present before modifying *)
  let set (c, x) k v =
    let key = key k in
    Context.get c key >>= function
    | None ->
        let msg =
          "cannot set undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)
    | Some old ->
        let bytes = P.to_bytes v in
        if MBytes.(old = bytes) then
          return (c, x)
        else
          Context.set c key (P.to_bytes v) >>= fun c ->
          return (c, x)

  (* Verify that the key is not present before inserting *)
  let init (c, x) k v =
    let key = key k in
    Context.get c key >>=
      function
      | Some _ ->
          let msg
            = "cannot init existing " ^ P.name ^ " key " ^ key_to_string k in
          fail (Storage_error msg)
      | None ->
          Context.set c key (P.to_bytes v) >>= fun c ->
          return (c, x)

  (* Does not verify that the key is present or not *)
  let init_set (c, x) k v =
    Context.set c (key k) (P.to_bytes v) >>= fun c -> return (c, x)

  (* Verify that the key is present before deleting *)
  let delete (c, x) k =
    let key = key k in
    Context.get c key >>= function
    | Some _ ->
        Context.del c key >>= fun c ->
        return (c, x)
    | None ->
        let msg =
          "cannot delete undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)

  (* Do not verify before deleting *)
  let remove (c, x) k =
    Context.del c (key k) >>= fun c -> Lwt.return (c, x)

end

(*-- Indexed data accessor ---------------------------------------------------*)

module type Data_description = sig
  type value
  val name : string
  val encoding : value Data_encoding.t
end

module type Indexed_data_description = sig
  type key
  val key : key -> string list
  include Data_description
end

module Make_indexed_data_storage (P : Indexed_data_description) =
  Make_raw_data_storage(struct
    include P

    let of_bytes b =
      match Data_encoding.Binary.of_bytes P.encoding b with
      | None ->
          let msg =
            "cannot deserialize " ^ P.name ^ " value" in
          error (Storage_error msg)
      | Some v -> Ok v
    let to_bytes v = Data_encoding.Binary.to_bytes P.encoding v
  end)

module Make_indexed_optional_data_storage (P : Indexed_data_description) = struct
  module Raw = Make_indexed_data_storage(P)
  type key = P.key
  type value = P.value
  let get = Raw.get_option
  let mem = Raw.mem
  let set c k r =
    match r with
    | None -> Raw.remove c k >>= fun c -> return c
    | Some r -> Raw.init_set c k r
end

(*-- Single data accessor ----------------------------------------------------*)

module type Single_data_description = sig
  val key : string list
  include Data_description
end

module Make_single_data_storage (P : Single_data_description) = struct
  module Single_desc = struct
    type value = P.value
    type key = unit
    let encoding = P.encoding
    let name = P.name
    let key () = P.key
  end
  include Make_indexed_data_storage(Single_desc)
  let get c = get c ()
  let mem c = mem c ()
  let get_option c = get_option c ()
  let set c r = set c () r
  let init c r = init c () r
  let init_set c r = init_set c () r
  let remove c = remove c ()
  let delete c = delete c ()
end

module Make_single_optional_data_storage (P : Single_data_description) = struct
  module Raw = Make_single_data_storage (P)
  type value = P.value
  let get = Raw.get_option
  let mem = Raw.mem
  let set c r =
    match r with
    | None -> Raw.remove c >>= fun c -> return c
    | Some r -> Raw.init_set c r
end

(*-- Data set (set of homogeneous data under a key prefix) -------------------*)

module Make_data_set_storage (P : Single_data_description) = struct

  module Key = struct
    include Hash.Make_minimal_Blake2B(struct
        let name = P.name
        let title = ("A " ^ P.name ^ "key")
        let size = None
      end)
    let of_path = of_path_exn
    let prefix = P.key
    let length = path_length
  end

  module HashTbl =
    Persist.MakePersistentMap(Context)(Key)(Persist.RawValue)

  type value = P.value

  let serial v =
    let data = Data_encoding.Binary.to_bytes P.encoding v in
    Key.hash_bytes [data], data

  let unserial b =
    match Data_encoding.Binary.of_bytes P.encoding b with
    | None ->
        let msg =
          "cannot deserialize " ^ P.name ^ " value" in
        error (Storage_error msg)
    | Some v -> Ok v

  let add (c, x) v =
    let hash, data = serial v in
    HashTbl.mem c hash >>= function
    | true -> return (c, x)
    | false -> HashTbl.set c hash data >>= fun c -> return (c, x)

  let del (c, x) v =
    let hash, _ = serial v in
    HashTbl.mem c hash >>= function
    | false -> return (c, x)
    | true -> HashTbl.del c hash >>= fun c -> return (c, x)

  let mem (c, _) v =
    let hash, _ = serial v in
    HashTbl.mem c hash >>= fun v ->
    return v

  let elements (c, _) =
    HashTbl.bindings c >>= fun elts ->
    map_s (fun (_, data) -> Lwt.return (unserial data)) elts

  let fold (c, _) init ~f =
    HashTbl.fold c (ok init)
      ~f:(fun _ data acc ->
          match acc with
          | Error _ -> Lwt.return acc
          | Ok acc ->
              match unserial data with
              | Error _ as err -> Lwt.return err
              | Ok data ->
                  f data acc >>= fun acc ->
                  return acc)

  let clear (c, x) =
    HashTbl.fold c c ~f:(fun hash _ c -> HashTbl.del c hash) >>= fun c ->
    return (c, x)

end

module Raw_make_iterable_data_storage
    (K: Persist.KEY)
    (P: Data_description) = struct

  type key = K.t
  type value = P.value

  module HashTbl =
    Persist.MakePersistentMap(Context)(K)(struct
    type t = P.value
    let of_bytes b = Data_encoding.Binary.of_bytes P.encoding b
    let to_bytes v = Data_encoding.Binary.to_bytes P.encoding v
  end)

  let key_to_string k = String.concat "/" (K.to_path k)

  let get (c, _) k =
    HashTbl.get c k >>= function
    | None ->
        let msg =
          "cannot get undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)
    | Some v ->
        return v

  let mem (c, _) k = HashTbl.mem c k

  let get_option (c, _) k =
    HashTbl.get c k >>= function
    | None -> return None
    | Some v -> return (Some v)

  (* Verify that the key is present before modifying *)
  let set (c, x) k v =
    HashTbl.get c k >>= function
    | None ->
        let msg =
          "cannot set undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)
    | Some _ ->
        HashTbl.set c k v >>= fun c ->
        return (c, x)

  (* Verify that the key is not present before inserting *)
  let init (c, x) k v =
    HashTbl.get c k >>=
      function
      | Some _ ->
          let msg
            = "cannot init existing " ^ P.name ^ " key " ^ key_to_string k in
          fail (Storage_error msg)
      | None ->
          HashTbl.set c k v >>= fun c ->
          return (c, x)

  (* Does not verify that the key is present or not *)
  let init_set (c, x) k v = HashTbl.set c k v >>= fun c -> return (c, x)

  (* Verify that the key is present before deleting *)
  let delete (c, x) k =
    HashTbl.get c k >>= function
    | Some _ ->
        HashTbl.del c k >>= fun c ->
        return (c, x)
    | None ->
        let msg =
          "cannot delete undefined " ^ P.name ^ " key " ^ key_to_string k in
        fail (Storage_error msg)

  (* Do not verify before deleting *)
  let remove (c, x) k =
    HashTbl.del c k >>= fun c -> Lwt.return (c, x)

  let clear (c, x) = HashTbl.clear c >>= fun c -> Lwt.return (c, x)
  let fold (c, _) x ~f = HashTbl.fold c x ~f:(fun k v acc -> f k v acc)
  let iter (c, _) ~f = HashTbl.fold c () ~f:(fun k v () -> f k v)

end

module Make_iterable_data_storage (H: HASH) (P: Single_data_description) =
  Raw_make_iterable_data_storage(struct
    include H
    let of_path = H.of_path_exn
    let prefix = P.key
    let length = path_length
  end)(P)

let register_resolvers (module H : Hash.HASH) prefixes =

  let module Set = H.Set in

  let resolvers =
    List.map
      (fun prefix ->
         let module R = Persist.MakeHashResolver(struct
             include Context
             let prefix = prefix
           end)(H) in
         R.resolve)
      prefixes in

  let resolve c m =
    match resolvers with
    | [resolve] -> resolve c m
    | resolvers ->
        Lwt_list.map_p (fun resolve -> resolve c m) resolvers >|= fun hs ->
        List.fold_left
          (fun acc hs -> List.fold_left (fun acc h -> Set.add h acc) acc hs)
          Set.empty hs |>
        Set.elements in

  Context.register_resolver H.b58check_encoding resolve


