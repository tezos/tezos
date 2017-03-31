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

module MBytesContent = struct
  module Tc_S0 =
    (val Tc.biject Tc.cstruct Cstruct.to_bigarray Cstruct.of_bigarray)
  include Tc_S0
  module Path = Irmin.Path.String_list
  let merge =
    let fn = Irmin.Merge.(option (module Tc_S0) (default (module Tc_S0))) in
    fun _path -> fn
end

module GitStore = struct

  module Store =
    Irmin_unix.Irmin_git.FS
      (MBytesContent) (Irmin.Ref.String) (Irmin.Hash.SHA1)

  include Store

  module View = Irmin.View (Store)

  module FunView = struct
    include Ir_funview.Make (Store)
    type v = t * Lwt_utils.Idle_waiter.t
    let get (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      read t k
    let mem (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      mem t k
    let dir_mem (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      dir_mem t k
    let del (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      remove t k >>= fun t ->
      Lwt.return (t, w)
    let remove_rec (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      remove_rec t k >>= fun t ->
      Lwt.return (t, w)
    let set (t, w) k v =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      update t k v >>= fun t ->
      Lwt.return (t, w)
    let update_path db k (t, w) =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      update_path db k t
    let list (t, w) k =
      Lwt_utils.Idle_waiter.task w @@ fun () ->
      Lwt_list.map_p (list t) k >|= List.flatten
  end

end

type index = {
  path: string ;
  repo: GitStore.Repo.t ;
  patch_context: context -> context Lwt.t ;
  mutable commits: int ;
  repack_scheduler : Lwt_utils.Idle_waiter.t ;
}
and context = {
  index: index ;
  store: GitStore.t ;
  view: GitStore.FunView.v ;
}
type t = context

(*-- Version Access and Update -----------------------------------------------*)

let current_protocol_key = ["protocol"]
let current_fitness_key = ["fitness"]
let current_timestamp_key = ["timestamp"]
let current_test_protocol_key = ["test_protocol"]
let current_test_network_key = ["test_network"]
let current_test_network_expiration_key = ["test_network_expiration"]
let current_fork_test_network_key = ["fork_test_network"]

let transient_commit_message_key = ["message"]

let exists { repo } key =
  GitStore.of_branch_id
    Irmin.Task.none (Block_hash.to_b58check key) repo >>= fun t ->
  let store = t () in
  GitStore.read store current_protocol_key >>= function
  | Some _ ->
      Lwt.return true
  | None ->
      Lwt.return false

let checkout index key =
  lwt_debug "-> Context.checkout %a"
    Block_hash.pp_short key >>= fun () ->
  exists index key >>= fun exists ->
  if not exists then
    Lwt.return None
  else
    GitStore.of_branch_id
      Irmin.Task.none (Block_hash.to_b58check key) index.repo >>= fun t ->
    let store = t () in
    GitStore.FunView.of_path store [] >>= fun view ->
    let view = (view, index.repack_scheduler) in
    let ctxt = { index ; store ; view } in
    index.patch_context ctxt >>= fun ctxt ->
    lwt_debug "<- Context.checkout %a OK"
      Block_hash.pp_short key >>= fun () ->
    Lwt.return (Some ctxt)

let checkout_exn index key =
  checkout index key >>= function
  | None -> Lwt.fail Not_found
  | Some p -> Lwt.return p

let exists index key =
  lwt_debug "-> Context.exists %a"
    Block_hash.pp_short key >>= fun () ->
  exists index key >>= fun exists ->
  lwt_debug "<- Context.exists %a %B"
    Block_hash.pp_short key exists >>= fun () ->
  Lwt.return exists

let get_and_erase_commit_message ctxt =
  GitStore.FunView.get ctxt.view transient_commit_message_key >>= function
  | None -> Lwt.return (None, ctxt)
  | Some bytes ->
     GitStore.FunView.del ctxt.view transient_commit_message_key >>= fun view ->
     Lwt.return (Some (MBytes.to_string bytes), { ctxt with view })
let set_commit_message ctxt msg =
  GitStore.FunView.set ctxt.view
    transient_commit_message_key
    (MBytes.of_string msg) >>= fun view ->
  Lwt.return { ctxt with view }

let get_fitness { view } =
  GitStore.FunView.get view current_fitness_key >>= function
  | None -> assert false
  | Some data ->
      match Data_encoding.Binary.of_bytes Fitness.encoding data with
      | None -> assert false
      | Some data -> Lwt.return data
let set_fitness ctxt data =
  GitStore.FunView.set ctxt.view current_fitness_key
    (Data_encoding.Binary.to_bytes Fitness.encoding data) >>= fun view ->
  Lwt.return { ctxt with view }

let get_timestamp { view } =
  GitStore.FunView.get view current_timestamp_key >>= function
  | None -> assert false
  | Some time ->
     Lwt.return (Time.of_notation_exn (MBytes.to_string time))
let set_timestamp ctxt time =
  GitStore.FunView.set ctxt.view current_timestamp_key
    (MBytes.of_string (Time.to_notation time)) >>= fun view ->
  Lwt.return { ctxt with view }

exception Preexistent_context of Block_hash.t
exception Empty_head of Block_hash.t

let commit key context =
  get_timestamp context >>= fun timestamp ->
  get_fitness context >>= fun fitness ->
  let task =
    Irmin.Task.create ~date:(Time.to_seconds timestamp) ~owner:"Tezos" in
  GitStore.clone task context.store (Block_hash.to_b58check key) >>= function
  | `Empty_head -> Lwt.fail (Empty_head key)
  | `Duplicated_branch -> Lwt.fail (Preexistent_context key)
  | `Ok store ->
     get_and_erase_commit_message context >>= fun (msg, context) ->
     let msg = match msg with
       | None ->
          Format.asprintf "%a %a"
            Fitness.pp fitness Block_hash.pp_short key
       | Some msg -> msg in
     GitStore.FunView.update_path (store msg) [] context.view >>= fun () ->
     context.index.commits <- context.index.commits + 1 ;
     if context.index.commits mod 200 = 0 then
       Lwt_utils.Idle_waiter.force_idle
         context.index.repack_scheduler
         (fun () ->
            lwt_debug "begin git repack" >>= fun () ->
            let command =
              "git",
              [| "git" ; "-C" ; context.index.path ;
                 "repack" ; "-a" ; "-d" |] in
            let t0 = Unix.gettimeofday () in
            Lwt_process.exec
              ~stdout: `Dev_null ~stderr: `Dev_null
              command >>= fun res ->
            let dt = Unix.gettimeofday () -. t0 in
            match res with
            | WEXITED 0 ->
                lwt_log_notice "git repack complete in %0.2f sec" dt
            | WEXITED code | WSTOPPED code | WSIGNALED code ->
                lwt_log_error "git repack failed with code %d after  %0.2f sec"
                  code dt) >>= fun () ->
       Lwt.return ()
     else
       Lwt.return ()

(*-- Generic Store Primitives ------------------------------------------------*)

type key = string list

let data_key key = "data" :: key
let undata_key = function
  | "data" :: key -> key
  | _ -> assert false

let mem ctxt key =
  GitStore.FunView.mem ctxt.view (data_key key) >>= fun v ->
  Lwt.return v

let dir_mem ctxt key =
  GitStore.FunView.dir_mem ctxt.view (data_key key) >>= fun v ->
  Lwt.return v

let raw_get ctxt key = GitStore.FunView.get ctxt.view key
let get t key = raw_get t (data_key key)

let raw_set ctxt key data =
  GitStore.FunView.set ctxt.view key data >>= fun view ->
  Lwt.return { ctxt with view }
let set t key data = raw_set t (data_key key) data

let raw_del ctxt key =
  GitStore.FunView.del ctxt.view key >>= fun view ->
  Lwt.return { ctxt with view }
let del t key = raw_del t (data_key key)

let list ctxt keys =
  GitStore.FunView.list ctxt.view (List.map data_key keys) >>= fun keys ->
  Lwt.return (List.map undata_key keys)

let remove_rec ctxt key =
  GitStore.FunView.remove_rec ctxt.view (data_key key) >>= fun view ->
  Lwt.return { ctxt with view }

(*-- Initialisation ----------------------------------------------------------*)

let init ?patch_context ~root =
  GitStore.Repo.create
    (Irmin_unix.Irmin_git.config ~root ~bare:true ()) >>= fun repo ->
  Lwt.return {
    commits = 0 ;
    repack_scheduler = Lwt_utils.Idle_waiter.create () ;
    path = root ;
    repo ;
    patch_context =
      match patch_context with
      | None -> (fun ctxt -> Lwt.return ctxt)
      | Some patch_context -> patch_context
  }

let commit_genesis index ~id:block ~time ~protocol ~test_protocol =
  GitStore.of_branch_id
    Irmin.Task.none (Block_hash.to_b58check block)
    index.repo >>= fun t ->
  let store = t () in
  GitStore.FunView.of_path store [] >>= fun view ->
  let view = (view, index.repack_scheduler) in
  GitStore.FunView.set view current_timestamp_key
    (MBytes.of_string (Time.to_notation time)) >>= fun view ->
  GitStore.FunView.set view current_protocol_key
    (Protocol_hash.to_bytes protocol) >>= fun view ->
  GitStore.FunView.set view current_fitness_key
    (Data_encoding.Binary.to_bytes Fitness.encoding []) >>= fun view ->
  GitStore.FunView.set view current_test_protocol_key
    (Protocol_hash.to_bytes test_protocol) >>= fun view ->
  let ctxt = { index ; store ; view } in
  index.patch_context ctxt >>= fun ctxt ->
  GitStore.FunView.update_path ctxt.store [] ctxt.view >>= fun () ->
  Lwt.return ctxt

(*-- Predefined Fields -------------------------------------------------------*)

let get_protocol v =
  raw_get v current_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)
let set_protocol v key =
  raw_set v current_protocol_key (Protocol_hash.to_bytes key)

let get_test_protocol v =
  raw_get v current_test_protocol_key >>= function
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)
let set_test_protocol v data =
  raw_set v current_test_protocol_key (Protocol_hash.to_bytes data)

let get_test_network v =
  raw_get v current_test_network_key >>= function
  | None -> Lwt.return_none
  | Some data -> Lwt.return (Some (Net_id.of_bytes_exn data))
let set_test_network v id =
  raw_set v current_test_network_key (Net_id.to_bytes id)
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

let init_test_network v ~time ~genesis =
  get_test_protocol v >>= fun test_protocol ->
  del_test_network_expiration v >>= fun v ->
  set_protocol v test_protocol >>= fun v ->
  set_timestamp v time >>= fun v ->
  let task =
    Irmin.Task.create
      ~date:(Time.to_seconds time)
      ~owner:"tezos" in
  GitStore.clone task v.store (Block_hash.to_b58check genesis) >>= function
  | `Empty_head -> Lwt.return (Error [Exn (Empty_head genesis)])
  | `Duplicated_branch -> Lwt.return (Error [Exn (Preexistent_context genesis)])
  | `Ok store ->
      let msg =
        Format.asprintf "Fake block. Forking testnet: %a."
          Block_hash.pp_short genesis in
      GitStore.FunView.update_path (store msg) [] v.view >>= fun () ->
      return v

