(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Versioned (key x value) store (over Irmin) *)

open Hash

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

module GitStore =
  Irmin_leveldb.Make
    (Metadata)
    (MBytesContent)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)

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

type commit = GitStore.Commit.Hash.t

let dummy_commit =
  match
    GitStore.Commit.Hash.of_string "0000000000000000000000000000000000000000"
  with
  | Ok c -> c
  | Error _ -> assert false

let commit_encoding : commit Data_encoding.t =
  let open Data_encoding in
  conv
    (fun c -> Cstruct.to_bigarray (Irmin.Type.encode_cstruct GitStore.Commit.Hash.t c))
    (fun c ->
       match
         Irmin.Type.decode_cstruct
           GitStore.Commit.Hash.t
           (Cstruct.of_bigarray c)
       with
       | Ok x -> x
       | _ -> assert false
    )
    bytes

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

type test_network =
  | Not_running
  | Forking of {
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }
  | Running of {
      net_id: Net_id.t ;
      genesis: Block_hash.t ;
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }

let pp_test_network ppf = function
  | Not_running -> Format.fprintf ppf "@[<v 2>Not running@]"
  | Forking { protocol ; expiration } ->
      Format.fprintf ppf
        "@[<v 2>Forking %a (expires %a)@]"
        Hash.Protocol_hash.pp
        protocol
        Time.pp_hum
        expiration
  | Running { net_id ; genesis ; protocol ; expiration } ->
      Format.fprintf ppf
        "@[<v 2>Running %a\
         @ Genesis: %a\
         @ Net id: %a\
         @ Expiration: %a@]"
        Hash.Protocol_hash.pp protocol
        Hash.Block_hash.pp genesis
        Hash.Net_id.pp net_id
        Time.pp_hum expiration

let test_network_encoding =
  let open Data_encoding in
  union [
    case ~tag:0
      (obj1 (req "status" (constant "not_running")))
      (function Not_running -> Some () | _ -> None)
      (fun () -> Not_running) ;
    case ~tag:1
      (obj3
         (req "status" (constant "forking"))
         (req "protocol" Protocol_hash.encoding)
         (req "expiration" Time.encoding))
      (function
        | Forking { protocol ; expiration } ->
            Some ((), protocol, expiration)
        | _ -> None)
      (fun ((), protocol, expiration) ->
         Forking { protocol ; expiration }) ;
    case ~tag:2
      (obj5
         (req "status" (constant "running"))
         (req "net_id" Net_id.encoding)
         (req "genesis" Block_hash.encoding)
         (req "protocol" Protocol_hash.encoding)
         (req "expiration" Time.encoding))
      (function
        | Running { net_id ; genesis ; protocol ; expiration } ->
            Some ((), net_id, genesis, protocol, expiration)
        | _ -> None)
      (fun ((), net_id, genesis, protocol, expiration) ->
         Running { net_id ; genesis ; protocol ; expiration }) ;
  ]

let get_test_network v =
  raw_get v current_test_network_key >>= function
  | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_network)")
  | Some data ->
      match Data_encoding.Binary.of_bytes test_network_encoding data with
      | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_network)")
      | Some r -> Lwt.return r

let set_test_network v id =
  raw_set v current_test_network_key
    (Data_encoding.Binary.to_bytes test_network_encoding id)
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
