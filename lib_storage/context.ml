(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
    match Context_hash.of_bytes (Cstruct.to_bigarray t) with
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
  Irmin_leveldb.Make
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
let current_test_network_key = ["test_network"]

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
      index.patch_context ctxt >>= fun ctxt ->
      Lwt.return (Some ctxt)

let checkout_exn index key =
  checkout index key >>= function
  | None -> Lwt.fail Not_found
  | Some p -> Lwt.return p


let raw_commit ~time ~message context =
  let info =
    Irmin.Info.v ~date:(Time.to_seconds time) ~author:"Tezos" message in
  GitStore.Commit.v
    context.index.repo ~info ~parents:context.parents context.tree

let commit ~time ~message context =
  raw_commit ~time ~message context >>= fun commit ->
  Lwt.return (GitStore.Commit.hash commit)

(*-- Generic Store Primitives ------------------------------------------------*)

let data_key key = "data" :: key
let undata_key = function
  | "data" :: key -> key
  | _ -> assert false

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

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

(*-- Predefined Fields -------------------------------------------------------*)

let get_protocol v =
  raw_get v current_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)
let set_protocol v key =
  raw_set v current_protocol_key (Protocol_hash.to_bytes key)

let get_test_network v =
  raw_get v current_test_network_key >>= function
  | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_network)")
  | Some data ->
      match Data_encoding.Binary.of_bytes Test_network_status.encoding data with
      | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_network)")
      | Some r -> Lwt.return r

let set_test_network v id =
  raw_set v current_test_network_key
    (Data_encoding.Binary.to_bytes Test_network_status.encoding id)
let del_test_network v  = raw_del v current_test_network_key

let fork_test_network v ~protocol ~expiration =
  set_test_network v (Forking { protocol ; expiration })

(*-- Initialisation ----------------------------------------------------------*)

let init ?patch_context ~root =
  GitStore.Repo.v
    (Irmin_leveldb.config root) >>= fun repo ->
  Lwt.return {
    path = root ;
    repo ;
    patch_context =
      match patch_context with
      | None -> (fun ctxt -> Lwt.return ctxt)
      | Some patch_context -> patch_context
  }

let get_branch net_id = Format.asprintf "%a" Net_id.pp net_id


let commit_genesis index ~net_id ~time ~protocol =
  let tree = GitStore.Tree.empty in
  let ctxt = { index ; tree ; parents = [] } in
  index.patch_context ctxt >>= fun ctxt ->
  set_protocol ctxt protocol >>= fun ctxt ->
  set_test_network ctxt Not_running >>= fun ctxt ->
  raw_commit ~time ~message:"Genesis" ctxt >>= fun commit ->
  GitStore.Branch.set index.repo (get_branch net_id) commit >>= fun () ->
  Lwt.return (GitStore.Commit.hash commit)

let compute_testnet_genesis forked_block =
  let genesis = Block_hash.hash_bytes [Block_hash.to_bytes forked_block] in
  let net_id = Net_id.of_block_hash genesis in
  net_id, genesis

let commit_test_network_genesis index forked_block time ctxt =
  let net_id, genesis = compute_testnet_genesis forked_block in
  let branch = get_branch net_id in
  let message = Format.asprintf "Forking testnet: %s." branch in
  raw_commit ~time ~message ctxt >>= fun commit ->
  GitStore.Branch.set index.repo branch commit >>= fun () ->
  return (net_id, genesis, GitStore.Commit.hash commit)

let reset_test_network ctxt forked_block timestamp =
  get_test_network ctxt >>= function
  | Not_running -> Lwt.return ctxt
  | Running { expiration } ->
      if Time.(expiration <= timestamp) then
        set_test_network ctxt Not_running
      else
        Lwt.return ctxt
  | Forking { protocol ; expiration } ->
      let net_id, genesis = compute_testnet_genesis forked_block in
      set_test_network ctxt
        (Running { net_id ; genesis ;
                   protocol ; expiration })

let clear_test_network index net_id =
  (* TODO remove commits... ??? *)
  let branch = get_branch net_id in
  GitStore.Branch.remove index.repo branch

let set_head index net_id commit =
  let branch = get_branch net_id in
  GitStore.Commit.of_hash index.repo commit >>= function
  | None -> assert false
  | Some commit ->
      GitStore.Branch.set index.repo branch commit

let set_master index commit =
  GitStore.Commit.of_hash index.repo commit >>= function
  | None -> assert false
  | Some commit ->
      GitStore.Branch.set index.repo GitStore.Branch.master commit
