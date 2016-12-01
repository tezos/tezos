(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Versioned (key x value) store (over Irmin) *)

open Logging.Db

module IrminPath = Irmin.Path.String_list

module rec S : sig

  module type STORE = sig

    include Irmin.S with type commit_id = Irmin.Hash.SHA1.t
                     and type key = IrminPath.t
                     and type value = MBytes.t
                     and type branch_id = string

    module FunView : sig

      type v

      val of_path: t -> IrminPath.t -> v Lwt.t
      val update_path: t -> IrminPath.t -> v -> unit Lwt.t

      val mem: v -> IrminPath.t -> bool Lwt.t
      val dir_mem: v -> IrminPath.t -> bool Lwt.t
      val get: v -> IrminPath.t -> MBytes.t option Lwt.t
      val set: v -> IrminPath.t -> MBytes.t-> v Lwt.t
      val del: v -> IrminPath.t -> v Lwt.t
      val list: v -> IrminPath.t list -> IrminPath.t list Lwt.t
      val remove_rec: v -> IrminPath.t -> v Lwt.t

    end
    val path : string
    val local_repo : Repo.t
    val patch_context : (module S.VIEW) -> (module S.VIEW) Lwt.t
  end

  module type VIEW = sig
    module Store : STORE
    val s : Store.t
    val v : Store.FunView.v
  end

end = struct
  module type STORE = S.STORE
  module type VIEW = S.VIEW
end

include S

let pack (type s) (type v)
    (module S : STORE with type t = s and type FunView.v = v) (s : s) (v : v) =
  (module struct
     module Store = S
     let s = s
     let v = v
   end : VIEW)

type index = (module STORE)

type store = (module VIEW)

(*-- Version Access and Update -----------------------------------------------*)

let genesis_block_key = ["genesis";"block"]
let genesis_protocol_key = ["genesis";"protocol"]
let genesis_time_key = ["genesis";"time"]
let current_protocol_key = ["protocol"]
let current_test_protocol_key = ["test_protocol"]
let current_test_network_key = ["test_network"]
let current_test_network_expiration_key = ["test_network_expiration"]
let current_fork_test_network_key = ["fork_test_network"]
let invalid_context_key = ["invalid_context"]

let exists (module GitStore : STORE) key =
  GitStore.of_branch_id
    Irmin.Task.none (Block_hash.to_b48check key) GitStore.local_repo >>= fun t ->
  let store = t () in
  GitStore.read store genesis_block_key >>= function
  | Some _ ->
      Lwt.return true
  | None ->
      GitStore.read store invalid_context_key >>= function
      | Some _ ->
          Lwt.return true
      | None ->
          Lwt.return false

let checkout ((module GitStore : STORE) as index) key =
  lwt_debug "-> Context.checkout %a"
    Block_hash.pp_short key >>= fun () ->
  exists index key >>= fun exists ->
  if not exists then
    Lwt.return None
  else
    GitStore.of_branch_id
      Irmin.Task.none (Block_hash.to_b48check key) GitStore.local_repo >>= fun t ->
    let store = t () in
    GitStore.FunView.of_path store [] >>= fun v ->
    lwt_debug "<- Context.checkout %a OK"
      Block_hash.pp_short key >>= fun () ->
    GitStore.FunView.get v invalid_context_key >>= function
    | None ->
        GitStore.patch_context (pack (module GitStore) store v) >>= fun ctxt ->
        Lwt.return (Some (Ok ctxt))
    | Some bytes ->
        match Data_encoding_ezjsonm.from_string (MBytes.to_string bytes) with
        | Ok (`A errors) ->
            Lwt.return (Some (Error (List.map error_of_json errors)))
        | Error _ | Ok _->
            Lwt.return (Some (generic_error (MBytes.to_string bytes)))

exception Invalid_context of error list

let checkout_exn index key =
  checkout index key >>= function
  | None -> Lwt.fail Not_found
  | Some (Error error) -> Lwt.fail (Invalid_context error)
  | Some (Ok p) -> Lwt.return p

let exists ((module GitStore : STORE) as index) key =
  lwt_debug "-> Context.exists %a"
    Block_hash.pp_short key >>= fun () ->
  exists index key >>= fun exists ->
  lwt_debug "<- Context.exists %a %B"
    Block_hash.pp_short key exists >>= fun () ->
  Lwt.return exists

exception Preexistent_context of string * Block_hash.t
exception Empty_head of string * Block_hash.t

let commit (module GitStore : STORE) block key (module View : VIEW) =
  let module GitStore = View.Store in
  let task =
    Irmin.Task.create
      ~date:(Time.to_seconds block.Store.shell.timestamp) ~owner:"tezos" in
  GitStore.clone task View.s (Block_hash.to_b48check key) >>= function
  | `Empty_head -> Lwt.fail (Empty_head (GitStore.path, key))
  | `Duplicated_branch -> Lwt.fail (Preexistent_context (GitStore.path, key))
  | `Ok store ->
      let msg =
        Format.asprintf "%a %a"
          Fitness.pp block.shell.fitness
          Block_hash.pp_short key in
      GitStore.FunView.update_path (store msg) [] View.v

let commit_invalid (module GitStore : STORE) block key exns =
  let task =
    Irmin.Task.create
      ~date:(Time.to_seconds block.Store.shell.timestamp) ~owner:"tezos" in
  GitStore.of_branch_id
    task (Block_hash.to_b48check key) GitStore.local_repo >>= fun t ->
      let msg =
        Format.asprintf "%a %a"
          Fitness.pp block.shell.fitness
          Block_hash.pp_short key in
  let store = t msg in
  GitStore.clone Irmin.Task.none store (Block_hash.to_b48check key) >>= function
  | `Empty_head ->
      GitStore.update store invalid_context_key
        (MBytes.of_string @@ Data_encoding_ezjsonm.to_string @@
         `A (List.map json_of_error exns))
  | `Duplicated_branch | `Ok _ ->
      Lwt.fail (Preexistent_context (GitStore.path, key))


(*-- Generic Store Primitives ------------------------------------------------*)

type t = store

type key = string list

let data_key key = "data" :: key
let undata_key = function
  | "data" :: key -> key
  | _ -> assert false

let mem (module View : VIEW) key =
  let module GitStore = View.Store in
  GitStore.FunView.mem View.v (data_key key) >>= fun v ->
  Lwt.return v

let dir_mem (module View : VIEW) key =
  let module GitStore = View.Store in
  GitStore.FunView.dir_mem View.v (data_key key) >>= fun v ->
  Lwt.return v

let raw_get (module View : VIEW) key =
  let module GitStore = View.Store in
  GitStore.FunView.get View.v key >>= function
  | None -> Lwt.return_none
  | Some bytes -> Lwt.return (Some bytes)
let get t key = raw_get t (data_key key)

let raw_set (module View : VIEW) key data =
  let module GitStore = View.Store in
  GitStore.FunView.set View.v key data >>= fun v ->
  Lwt.return (pack (module GitStore) View.s v)
let set t key data = raw_set t (data_key key) data

let raw_del (module View : VIEW) key =
  let module GitStore = View.Store in
  GitStore.FunView.del View.v key >>= fun v ->
  Lwt.return (pack (module GitStore) View.s v)
let del t key = raw_del t (data_key key)

let list (module View : VIEW) keys =
  let module GitStore = View.Store in
  GitStore.FunView.list View.v (List.map data_key keys) >>= fun v ->
  Lwt.return (List.map undata_key v)

let remove_rec (module View : VIEW) key =
  let module GitStore = View.Store in
  GitStore.FunView.remove_rec View.v (data_key key) >>= fun v ->
  Lwt.return (pack (module GitStore) View.s v)

let keys (module View : VIEW) = Store.undefined_key_fn

(*-- Initialisation ----------------------------------------------------------*)

let init ?patch_context ~root =
  let module GitStore =
    Irmin_unix.Irmin_git.FS
      (Store.MBytesContent) (Irmin.Ref.String) (Irmin.Hash.SHA1) in
  GitStore.Repo.create
    (Irmin_unix.Irmin_git.config ~root ~bare:true ()) >>= fun local_repo ->
  let module GitStoreView = Irmin.View (GitStore) in
  let module ViewStore = struct

    let path = root
    let local_repo = local_repo
    let patch_context =
      match patch_context with
      | None -> (fun ctxt -> Lwt.return ctxt)
      | Some patch_context -> patch_context

    include GitStore

    module FunView = struct
      include Ir_funview.Make (GitStore)
      type v = t
      let get = read
      let del = remove
      let set = update
      let list v k = Lwt_list.map_p (list v) k >|= List.flatten
    end
  end in
  Lwt.return (module ViewStore : STORE)

let create_genesis_context (module GitStore : STORE) genesis test_protocol =
  GitStore.of_branch_id
    Irmin.Task.none (Block_hash.to_b48check genesis.Store.block)
    GitStore.local_repo >>= fun t ->
  let store = t () in
  GitStore.FunView.of_path store [] >>= fun v ->
  GitStore.FunView.set v genesis_block_key
    (Block_hash.to_bytes genesis.block) >>= fun v ->
  GitStore.FunView.set v genesis_protocol_key
    (Protocol_hash.to_bytes genesis.protocol) >>= fun v ->
  GitStore.FunView.set v genesis_time_key
    (MBytes.of_string (Time.to_notation genesis.time)) >>= fun v ->
  GitStore.FunView.set v current_protocol_key
    (Protocol_hash.to_bytes genesis.protocol) >>= fun v ->
  GitStore.FunView.set v current_test_protocol_key
    (Protocol_hash.to_bytes test_protocol) >>= fun v ->
  let ctxt = pack (module GitStore) store v in
  GitStore.patch_context ctxt >>= fun ctxt ->
  let (module View : VIEW) = ctxt in
  View.Store.FunView.update_path View.s [] View.v >>= fun () ->
  Lwt.return ctxt

(*-- Predefined Fields -------------------------------------------------------*)

let get_protocol v =
  raw_get v current_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes data)
let set_protocol v key =
  raw_set v current_protocol_key (Protocol_hash.to_bytes key)

let get_test_protocol v =
  raw_get v current_test_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes data)
let set_test_protocol v data =
  raw_set v current_test_protocol_key (Protocol_hash.to_bytes data)

let get_test_network v =
  raw_get v current_test_network_key >>= function
  | None -> Lwt.return_none
  | Some data -> Lwt.return (Some (Store.Net (Block_hash.of_bytes data)))
let set_test_network v (Store.Net data) =
  raw_set v current_test_network_key (Block_hash.to_bytes data)
let del_test_network v  = raw_del v current_test_network_key

let get_test_network_expiration v =
  raw_get v current_test_network_expiration_key >>= function
  | None -> Lwt.return_none
  | Some data -> Lwt.return (Time.of_notation @@ MBytes.to_string data)
let set_test_network_expiration v data =
  raw_set v current_test_network_expiration_key
    (MBytes.of_string @@ Time.to_notation data)
let del_test_network_expiration v  =
  raw_del v current_test_network_expiration_key

let read_and_reset_fork_test_network v =
  raw_get v current_fork_test_network_key >>= function
  | None -> Lwt.return (false, v)
  | Some _ ->
      raw_del v current_fork_test_network_key >>= fun v ->
      Lwt.return (true, v)

let fork_test_network v =
  raw_set v current_fork_test_network_key (MBytes.of_string "fork")

let get_genesis_block v =
  raw_get v genesis_block_key >>= function
  | None -> assert false
  | Some block -> Lwt.return (Block_hash.of_bytes block)

let get_genesis_time v =
  raw_get v genesis_time_key >>= function
  | None -> assert false
  | Some time -> Lwt.return (Time.of_notation_exn (MBytes.to_string time))

