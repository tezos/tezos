(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos - Versioned (key x value) store (over Irmin) *)

module IrminPath = Irmin.Path.String_list

module MBytesContent = struct
  type t = MBytes.t
  let t =
    Irmin.Type.(like cstruct)
      (fun x -> Cstruct.to_bigarray x)
      (fun x -> Cstruct.of_bigarray x)
  let merge = Irmin.Merge.default Irmin.Type.(option t)
  let pp ppf b = Format.pp_print_string ppf (MBytes.to_string b)
  let of_string s = Ok (MBytes.of_string s)
end

module Metadata = struct
  type t = unit
  let t = Irmin.Type.unit
  let default = ()
  let merge = Irmin.Merge.default t
end

module IrminBlake2B : Irmin.Hash.S with type t = Context_hash.t = struct

  type t = Context_hash.t

  let digest_size = Context_hash.size

  let to_raw t = Cstruct.of_bigarray (Context_hash.to_bytes t)
  let of_raw t =
    match Context_hash.of_bytes_opt (Cstruct.to_bigarray t) with
    | Some t -> t
    | None ->
        let str = Cstruct.to_string t in
        Format.kasprintf invalid_arg "%s (%d)" str (String.length str)

  let t = Irmin.Type.like Irmin.Type.cstruct of_raw to_raw

  let digest t x =
    Context_hash.hash_bytes
      [Cstruct.to_bigarray (Irmin.Type.encode_cstruct t x)]

  let pp = Context_hash.pp

  let of_string x =
    match Context_hash.of_b58check_exn x with
    | exception (Invalid_argument s) -> Error (`Msg s)
    | h -> Ok h

  let has_kind = function
    | `SHA1 -> true
    | _ -> false

  let to_raw_int c =
    Int64.to_int @@ MBytes.get_int64 (Context_hash.to_bytes c) 0

end

module GitStore =
  Irmin_lmdb.Make
    (Metadata)
    (MBytesContent)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (IrminBlake2B)

type index = {
  path: string ;
  repo: GitStore.Repo.t ;
  patch_context: context -> context Lwt.t ;
}

and context = {
  index: index ;
  parents: GitStore.Commit.t list ;
  tree: GitStore.tree ;
}
type t = context

(*-- Version Access and Update -----------------------------------------------*)

let current_protocol_key = ["protocol"]
let current_test_chain_key = ["test_chain"]

let exists index key =
  GitStore.Commit.of_hash index.repo key >>= function
  | None -> Lwt.return_false
  | Some _ -> Lwt.return_true

let checkout index key =
  GitStore.Commit.of_hash index.repo key >>= function
  | None -> Lwt.return_none
  | Some commit ->
      GitStore.Commit.tree commit >>= fun tree ->
      let ctxt = { index ; tree ; parents = [commit] } in
      Lwt.return_some ctxt

let checkout_exn index key =
  checkout index key >>= function
  | None -> Lwt.fail Not_found
  | Some p -> Lwt.return p

let raw_commit ~time ?(message = "") context =
  let info =
    Irmin.Info.v ~date:(Time.Protocol.to_seconds time) ~author:"Tezos" message in
  GitStore.Commit.v
    context.index.repo ~info ~parents:context.parents context.tree

module P = GitStore.Private

(* --- FIXME(samoht): I am so sorry --- *)
module Hack = struct

  module StepMap = struct
    module X = struct
      type t = GitStore.step
      let t = GitStore.step_t
      let compare = Irmin.Type.compare t
    end
    include Map.Make(X)
  end

  module Contents = struct

    type key = P.Contents.key
    type contents = P.Contents.value

    type t =
      | Key      of key
      | Contents of contents
      | Both     of key * contents

    let t =
      let open Irmin.Type in
      variant "Node.Contents" (fun key contents both -> function
          | Key x       -> key x
          | Contents x -> contents x
          | Both (x, y) -> both (x, y))
      |~ case1 "Key" P.Contents.Key.t (fun x -> Key x)
      |~ case1 "Contents" P.Contents.Val.t (fun x -> Contents x)
      |~ case1 "Both" (pair P.Contents.Key.t P.Contents.Val.t)
        (fun (x, y) -> Both (x, y))
      |> sealv

    let hash = function
      | Key k | Both (k, _) -> k
      | Contents c -> P.Contents.Key.digest P.Contents.Val.t c

  end

  type key = P.Node.key

  type value = [ `Node of node | `Contents of Contents.t * Metadata.t ]

  and map = value StepMap.t

  and node =
    | Map  of map
    | Key  of key
    | Both of key * map

  let value t =
    let open Irmin.Type in
    variant "Node.value" (fun node contents -> function
        | `Node x     -> node x
        | `Contents x -> contents x)
    |~ case1 "Node" t (fun x -> `Node x)
    |~ case1 "Contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
    |> sealv

  let map value =
    let open Irmin.Type in
    let to_map x =
      List.fold_left (fun acc (k, v) -> StepMap.add k v acc) StepMap.empty x
    in
    let of_map m = StepMap.fold (fun k v acc -> (k, v) :: acc) m [] in
    like (list (pair GitStore.step_t value)) to_map of_map

  let node map =
    let open Irmin.Type in
    variant "Node.node" (fun map key both -> function
        | Map x      -> map x
        | Key y      -> key y
        | Both (y,z) -> both (y, z))
    |~ case1 "Map" map (fun x -> Map x)
    |~ case1 "Key" P.Node.Key.t (fun x -> Key x)
    |~ case1 "Both" (pair P.Node.Key.t map) (fun (x, y) -> Both (x, y))
    |> sealv

  let node_t = Irmin.Type.mu (fun n ->
      let value = value n in
      node (map value)
    )

  (* Mimick irmin-lmdb ordering *)
  module Sort_key = struct

    exception Result of int

    let compare (x, vx) (y, vy) = match vx, vy with
      | `Contents _, `Contents _ -> String.compare x y
      | _  ->
          let lenx = String.length x in
          let leny = String.length y in
          let i = ref 0 in
          try
            while !i < lenx && !i < leny do
              match
                Char.compare
                  (String.unsafe_get x !i) (String.unsafe_get y !i)
              with
              | 0 -> incr i
              | i -> raise (Result i)
            done;
            let get len k v i =
              if i < len then String.unsafe_get k i
              else if i = len then match v with
                | `Node _     -> '/'
                | `Contents _ -> '\000'
              else '\000'
            in
            match Char.compare (get lenx x vx !i) (get leny y vy !i) with
            | 0 -> Char.compare (get lenx x vx (!i + 1)) (get leny y vy (!i + 1))
            | i -> i
          with Result i ->
            i

  end

  let sort_entries = List.fast_sort Sort_key.compare

  module Entry = struct
    type kind = [ `Node | `Contents of Metadata.t ]
    type entry = { kind : kind; name : string; node : IrminBlake2B.t; }

    let entry_t =
      let open Irmin.Type in
      record "Tree.entry"
        (fun kind name node ->
           let kind =
             match kind with
             | None -> `Node
             | Some m -> `Contents m in
           { kind ; name ; node } )
      |+ field "kind" (option Metadata.t) (function
          | { kind = `Node ; _ } -> None
          | { kind = `Contents m ; _ } -> Some m)
      |+ field "name" string (fun { name ; _ } -> name)
      |+ field "node" IrminBlake2B.t (fun { node ; _ } -> node)
      |> sealr

    let of_entry e = e.name, match e.kind with
      | `Node       -> `Node e.node
      | `Contents m -> `Contents (e.node, m)

    let to_entry (name, value) = match value with
      | `Node node -> { name; kind = `Node; node }
      | `Contents (node, m) -> { name; kind = `Contents m; node }

    let t = Irmin.Type.like entry_t of_entry to_entry

  end

  let rec export_map map =
    let alist =
      StepMap.fold (fun step v acc ->
          (step, hash_value v) :: acc
        ) map []
    in
    let l = sort_entries alist in
    P.Node.Val.v l

  and hash_value = function
    | `Contents (c, m) -> `Contents (Contents.hash c, m)
    | `Node n          -> `Node (hash_node n)

  and hash_node = function
    | Both (k, _) | Key k -> k
    | Map m ->
        let v = export_map m in
        let entries = P.Node.Val.list v in
        (* This needs to match what is done in the backend... *)
        let v = Irmin.Type.encode_cstruct (Irmin.Type.list Entry.t) entries in
        IrminBlake2B.digest Irmin.Type.cstruct v

  let cast: GitStore.node -> node = fun n ->
    let buf = Irmin.Type.encode_cstruct GitStore.node_t n in
    match Irmin.Type.decode_cstruct node_t buf with
    | Error (`Msg e) -> Fmt.failwith "invalid cast\n%s" e
    | Ok x -> x

end

let tree_hash: GitStore.tree -> GitStore.Tree.hash = function
  | `Contents (c, m) -> `Contents (P.Contents.Key.digest P.Contents.Val.t c, m)
  | `Node n          -> `Node (Hack.hash_node (Hack.cast n))

let hash ~time ?(message = "") context =
  let info =
    Irmin.Info.v ~date:(Time.Protocol.to_seconds time) ~author:"Tezos" message
  in
  let parents = List.map (fun c -> GitStore.Commit.hash c) context.parents in
  let node = match tree_hash context.tree with
    | `Contents _ -> assert false
    | `Node node  -> node
  in
  let commit = P.Commit.Val.v ~parents ~node ~info in
  let x = P.Commit.Key.digest P.Commit.Val.t commit in
  (* FIXME: this doesn't have to be lwt *)
  Lwt.return x

let commit ~time ?message context =
  raw_commit ~time ?message context >>= fun commit ->
  let h = GitStore.Commit.hash commit in
  Lwt.return h

(*-- Generic Store Primitives ------------------------------------------------*)

let data_key key = "data" :: key

type key = string list
type value = MBytes.t

let mem ctxt key =
  GitStore.Tree.mem ctxt.tree (data_key key) >>= fun v ->
  Lwt.return v

let dir_mem ctxt key =
  GitStore.Tree.mem_tree ctxt.tree (data_key key) >>= fun v ->
  Lwt.return v

let raw_get ctxt key =
  GitStore.Tree.find ctxt.tree key
let get t key = raw_get t (data_key key)

let raw_set ctxt key data =
  GitStore.Tree.add ctxt.tree key data >>= fun tree ->
  Lwt.return { ctxt with tree }
let set t key data = raw_set t (data_key key) data

let raw_del ctxt key =
  GitStore.Tree.remove ctxt.tree key >>= fun tree ->
  Lwt.return { ctxt with tree }
let del t key = raw_del t (data_key key)

let remove_rec ctxt key =
  GitStore.Tree.remove ctxt.tree (data_key key) >>= fun tree ->
  Lwt.return { ctxt with tree }

let copy ctxt ~from ~to_ =
  GitStore.Tree.find_tree ctxt.tree (data_key from) >>= function
  | None -> Lwt.return_none
  | Some sub_tree ->
      GitStore.Tree.add_tree ctxt.tree (data_key to_) sub_tree >>= fun tree ->
      Lwt.return_some { ctxt with tree }

let fold ctxt key ~init ~f =
  GitStore.Tree.list ctxt.tree (data_key key) >>= fun keys ->
  Lwt_list.fold_left_s
    begin fun acc (name, kind) ->
      let key =
        match kind with
        | `Contents -> `Key (key @ [name])
        | `Node -> `Dir (key @ [name]) in
      f key acc
    end
    init keys

(*-- Predefined Fields -------------------------------------------------------*)

let get_protocol v =
  raw_get v current_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)
let set_protocol v key =
  raw_set v current_protocol_key (Protocol_hash.to_bytes key)

let get_test_chain v =
  raw_get v current_test_chain_key >>= function
  | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_chain)")
  | Some data ->
      match Data_encoding.Binary.of_bytes Test_chain_status.encoding data with
      | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_chain)")
      | Some r -> Lwt.return r

let set_test_chain v id =
  raw_set v current_test_chain_key
    (Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding id)
let del_test_chain v  = raw_del v current_test_chain_key

let fork_test_chain v ~protocol ~expiration =
  set_test_chain v (Forking { protocol ; expiration })

(*-- Initialisation ----------------------------------------------------------*)

let init ?patch_context ?mapsize ?readonly root =
  GitStore.Repo.v
    (Irmin_lmdb.config ?mapsize ?readonly root) >>= fun repo ->
  Lwt.return {
    path = root ;
    repo ;
    patch_context =
      match patch_context with
      | None -> (fun ctxt -> Lwt.return ctxt)
      | Some patch_context -> patch_context
  }

let get_branch chain_id = Format.asprintf "%a" Chain_id.pp chain_id


let commit_genesis index ~chain_id ~time ~protocol =
  let tree = GitStore.Tree.empty in
  let ctxt = { index ; tree ; parents = [] } in
  index.patch_context ctxt >>= fun ctxt ->
  set_protocol ctxt protocol >>= fun ctxt ->
  set_test_chain ctxt Not_running >>= fun ctxt ->
  raw_commit ~time ~message:"Genesis" ctxt >>= fun commit ->
  GitStore.Branch.set index.repo (get_branch chain_id) commit >>= fun () ->
  Lwt.return (GitStore.Commit.hash commit)

let compute_testchain_chain_id genesis =
  let genesis_hash = Block_hash.hash_bytes [Block_hash.to_bytes genesis] in
  Chain_id.of_block_hash genesis_hash

let compute_testchain_genesis forked_block =
  let genesis = Block_hash.hash_bytes [Block_hash.to_bytes forked_block] in
  genesis

let commit_test_chain_genesis ctxt (forked_header : Block_header.t) =
  let message =
    Format.asprintf "Forking testchain at level %ld." forked_header.shell.level in
  raw_commit ~time:forked_header.shell.timestamp ~message ctxt >>= fun commit ->
  let faked_shell_header : Block_header.shell_header = {
    forked_header.shell with
    proto_level = succ forked_header.shell.proto_level ;
    predecessor = Block_hash.zero ;
    validation_passes = 0 ;
    operations_hash = Operation_list_list_hash.empty ;
    context = GitStore.Commit.hash commit ;
  } in
  let forked_block = Block_header.hash forked_header in
  let genesis_hash = compute_testchain_genesis forked_block in
  let chain_id = compute_testchain_chain_id genesis_hash in
  let genesis_header : Block_header.t =
    { shell = { faked_shell_header with predecessor = genesis_hash } ;
      protocol_data = MBytes.create 0 } in
  let branch = get_branch chain_id in
  GitStore.Branch.set ctxt.index.repo branch commit >>= fun () ->
  Lwt.return genesis_header


let clear_test_chain index chain_id =
  (* TODO remove commits... ??? *)
  let branch = get_branch chain_id in
  GitStore.Branch.remove index.repo branch

let set_head index chain_id commit =
  let branch = get_branch chain_id in
  GitStore.Commit.of_hash index.repo commit >>= function
  | None -> assert false
  | Some commit ->
      GitStore.Branch.set index.repo branch commit

let set_master index commit =
  GitStore.Commit.of_hash index.repo commit >>= function
  | None -> assert false
  | Some commit ->
      GitStore.Branch.set index.repo GitStore.Branch.master commit
