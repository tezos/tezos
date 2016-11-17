(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos - Simple (key x value) store *)

open Logging.Db

let (//) = Filename.concat

(*-- Generic static storage in a Unix directory ------------------------------*)

type key = string list

module IrminPath = Irmin.Path.String_list

type value = MBytes.t

module MBytesContent = struct
  module Tc_S0 =
    (val Tc.biject Tc.cstruct Cstruct.to_bigarray Cstruct.of_bigarray)
  include Tc_S0
  module Path = Irmin.Path.String_list
  let merge =
    let fn = Irmin.Merge.(option (module Tc_S0) (default (module Tc_S0))) in
    fun _path -> fn
end

module FS = struct

  type t = string

  let init dir =
    IO.check_dir dir >>= fun () ->
    Lwt.return dir

  let file_of_key root key =
    String.concat Filename.dir_sep (root :: key)

  let key_of_file root file =
    let len = String.length root + 1 in
    String.sub file len (String.length file - len)

  let mem root key =
    let file = file_of_key root key in
    Lwt.return (Sys.file_exists file && not (Sys.is_directory file))

  let dir_mem root key =
    let file = file_of_key root key in
    Lwt.return (Sys.file_exists file && Sys.is_directory file)

  let exists root key =
    let file = file_of_key root key in
    Sys.file_exists file

  let get root key =
    mem root key >>= function
    | true  ->
        Lwt.catch
          (fun () ->
             IO.with_file_in (file_of_key root key)
               (fun ba -> Lwt.return (Some ba)))
          (fun e ->
             warn "warn: can't read %s: %s"
               (file_of_key root key) (Printexc.to_string e);
             Lwt.return_none)
    | false -> Lwt.return_none

  let del root key =
    IO.remove_file (file_of_key root key)

  let set root key value =
    del root key >>= fun () ->
    IO.with_file_out (file_of_key root key) value

  let list root keys =
    let dirs = List.map (file_of_key root) keys in
    Lwt_list.map_p
      (fun dir ->
         Lwt.catch
           (fun () ->
              IO.list_files dir >|= fun files ->
              List.map (fun file ->
                  Utils.split_path (key_of_file root (dir // file))) files)
           (fun _ -> Lwt.return []))
      dirs >>= fun files ->
    Lwt.return (List.concat files)

  let remove_rec root key =
    IO.remove_rec (file_of_key root key)

end

type generic_store = FS.t
type block_store = FS.t
type blockchain_store = FS.t
type operation_store = FS.t
type protocol_store = FS.t

type store = {
  block: block_store Persist.shared_ref ;
  blockchain: blockchain_store Persist.shared_ref ;
  operation: operation_store Persist.shared_ref ;
  protocol: protocol_store Persist.shared_ref ;
  global_store: generic_store Persist.shared_ref ;
  net_init: ?expiration:Time.t -> genesis -> net_store Lwt.t ;
  net_read: net_id -> net_store tzresult Lwt.t ;
  net_destroy: net_store -> unit Lwt.t ;
}

and net_store = {
  net_genesis: genesis ;
  net_expiration: Time.t option ;
  net_store: generic_store Persist.shared_ref ;
}

and genesis = {
  time: Time.t ;
  block: Block_hash.t ;
  protocol: Protocol_hash.t ;
}

and net_id = Net of Block_hash.t

module type TYPED_IMPERATIVE_STORE = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val get_exn: t -> key -> value Lwt.t
  val set: t -> key -> value -> unit Lwt.t
  val del: t -> key -> unit Lwt.t

  val keys: t -> key list Lwt.t
end

module type IMPERATIVE_STORE = sig
  type t
  val mem: t -> key -> bool Lwt.t
  val dir_mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val get_exn: t -> key -> value Lwt.t
  val set: t -> key -> value -> unit Lwt.t
  val del: t -> key -> unit Lwt.t
  val list: t -> key list -> key list Lwt.t
  val remove_rec: t -> key -> unit Lwt.t
end

(*-- Generic data store under "data/" ----------------------------------------*)

module type KEY = sig
  type t
  val to_path: t -> string list
end

module type HASHKEY = sig
  type t
  val to_path: t -> string list
  val of_path: string list -> t
  val prefix : string list
  val length : int
end

module Raw_key = struct
  type t = string list
  let to_path x = x
end

module type VALUE = sig
  type t
  val of_bytes: MBytes.t -> t option
  val to_bytes: t -> MBytes.t
end

module Raw_value = struct
  type t = MBytes.t
  let to_bytes b = b
  let of_bytes b = Some b
end

module Block_hash_value = struct
  type t = Block_hash.t
  let to_bytes = Block_hash.to_bytes
  let of_bytes v = try Some (Block_hash.of_bytes v) with _ -> None
end

module Block_hash_set_value = struct
  type t = Block_hash_set.t
  let to_bytes = Data_encoding.Binary.to_bytes Block_hash_set.encoding
  let of_bytes = Data_encoding.Binary.of_bytes Block_hash_set.encoding
end

module Time_value = struct
  type t = Time.t
  let to_bytes v = MBytes.of_string @@ Time.to_notation v
  let of_bytes b = Time.of_notation (MBytes.to_string b)
end

module Errors_value = struct
  type t = error list
  let to_bytes v = Data_encoding.(Binary.to_bytes (list (error_encoding ()))) v
  let of_bytes b = Data_encoding.(Binary.of_bytes (list (error_encoding ()))) b
end

let undefined_key_fn = Lwt.fail_invalid_arg "function keys cannot be implemented in this module"

module Make (K : KEY) (V : Persist.VALUE) = struct
  type t = FS.t
  type key = K.t
  type value = V.t
  let mem t k = FS.mem t (K.to_path k)
  let dir_mem t k = FS.dir_mem t (K.to_path k)
  let get t k =
    FS.get t (K.to_path k) >|= function
    | None -> None
    | Some v -> V.of_bytes v
  let get_exn t key =
    get t key >>= function
    | None -> Lwt.fail Not_found
    | Some v -> Lwt.return v
  let set t k v = FS.set t (K.to_path k) (V.to_bytes v)
  let del t k = FS.del t (K.to_path k)
  let list t ks = FS.list t (List.map K.to_path ks)
  let remove_rec t k = FS.remove_rec t (K.to_path k)

  let keys _t = undefined_key_fn
end

module Data_store : IMPERATIVE_STORE with type t = FS.t =
  Make (Raw_key) (Raw_value)

include Data_store


(*-- Typed block store under "blocks/" ---------------------------------------*)

type shell_block = {
  net_id: net_id ;
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  fitness: MBytes.t list ;
  operations: Operation_hash.t list ;
}
type block = {
  shell: shell_block ;
  proto: MBytes.t ;
}

let net_id_encoding =
  let open Data_encoding in
  conv
    (fun (Net net_id) -> net_id)
    (fun net_id -> Net net_id)
    Block_hash.encoding

let pp_net_id ppf (Net id) = Block_hash.pp_short ppf id

let shell_block_encoding =
  let open Data_encoding in
  conv
    (fun { net_id ; predecessor ; timestamp ; fitness ; operations } ->
       (net_id, predecessor, timestamp, fitness, operations))
    (fun (net_id, predecessor, timestamp, fitness, operations) ->
       { net_id ; predecessor ; timestamp ; fitness ; operations })
    (obj5
       (req "net_id" net_id_encoding)
       (req "predecessor" Block_hash.encoding)
       (req "timestamp" Time.encoding)
       (req "fitness" Fitness.encoding)
       (req "operations" (list Operation_hash.encoding)))

let block_encoding =
  let open Data_encoding in
  conv
    (fun { shell ; proto } -> (shell, proto))
    (fun (shell, proto) -> { shell ; proto })
    (merge_objs
       shell_block_encoding
       (obj1 (req "data" Variable.bytes)))

module Raw_block_value = struct
  type t = block
  let to_bytes v =
    Data_encoding.Binary.to_bytes block_encoding v
  let of_bytes b =
    Data_encoding.Binary.of_bytes block_encoding b
end

module Block_key = struct
  type t = Block_hash.t
  let to_path p = "blocks" :: Block_hash.to_path p @ [ "contents" ]
end
module Parsed_block = Make (Block_key) (Raw_block_value)
module Raw_block = Make (Block_key) (Raw_value)

module Block_pred_key = struct
  type t = Block_hash.t
  let to_path p = "blocks" :: Block_hash.to_path p @ [ "pred" ]
end
module Block_pred = Make (Block_pred_key) (Block_hash_value)

module Block_time_key = struct
  type t = Block_hash.t
  let to_path p = "blocks" :: Block_hash.to_path p @ [ "discovery_time" ]
end
module Block_time = Make (Block_time_key) (Time_value)

module Block_errors_key = struct
  type t = Block_hash.t
  let to_path p = "blocks" :: Block_hash.to_path p @ [ "errors" ]
end
module Block_errors = Make (Block_errors_key) (Errors_value)

module Block_resolver =
  Persist.MakeHashResolver
    (struct
      include FS
      let prefix = ["blocks"]
    end)
    (Block_hash)

module Block = struct
  type t = FS.t
  type key = Block_hash.t
  type value = Block_hash.t *
               block Time.timed_data option Lwt.t Lazy.t
  let mem = Block_pred.mem
  let full_get s k =
    Block_time.get s k >>= function
    | None -> Lwt.return_none
    | Some time ->
        Parsed_block.get s k >>= function
        | None -> Lwt.return_none
        | Some data -> Lwt.return (Some { Time.data ; time })
  let get s k =
    Block_pred.get s k >>= function
    | None -> Lwt.return_none
    | Some pred ->
        Lwt.return (Some (pred, lazy (full_get s k)))
  let get_exn s k =
    get s k >>= function
    | None -> Lwt.fail Not_found
    | Some x -> Lwt.return x
  let set s k (p, lazy r) =
    Block_pred.set s k p >>= fun () ->
    r >>= function
    | None -> Lwt.return_unit
    | Some { Time.data ; time } ->
        Parsed_block.set s k data >>= fun () ->
        Block_time.set s k time
  let full_set s k r =
    set s k (r.Time.data.shell.predecessor, Lazy.from_val (Lwt.return (Some r)))
  let del s k =
    Block_pred.del s k >>= fun () ->
    Block_time.del s k >>= fun () ->
    Parsed_block.del s k

  let compare b1 b2 =
    let (>>) x y = if x = 0 then y () else x in
    let rec list compare xs ys =
      match xs, ys with
      | [], [] -> 0
      | _ :: _, [] -> -1
      | [], _ :: _ -> 1
      | x :: xs, y :: ys ->
          compare x y >> fun () -> list compare xs ys in
    Block_hash.compare b1.shell.predecessor b2.shell.predecessor >> fun () ->
    compare b1.proto b2.proto >> fun () ->
    list Operation_hash.compare
      b1.shell.operations b2.shell.operations >> fun () ->
    Time.compare b1.shell.timestamp b2.shell.timestamp >> fun () ->
    list compare b1.shell.fitness b2.shell.fitness

  let equal b1 b2 = compare b1 b2 = 0
  let of_bytes = Raw_block_value.of_bytes
  let to_bytes = Raw_block_value.to_bytes
  let hash block = Block_hash.hash_bytes [to_bytes block]

  let raw_get t k = Raw_block.get t k

  let keys _t = undefined_key_fn (** We never list keys here *)
end

module Blockchain_succ_key = struct
  type t = Block_hash.t
  let to_path p =
    "blocks" :: Block_hash.to_path p @ ["blockchain_successor"]
end
module Blockchain_succ = Make (Blockchain_succ_key) (Block_hash_value)

module Blockchain_test_succ_key = struct
  type t = Block_hash.t
  let to_path p =
    "blocks" :: Block_hash.to_path p @ ["test_blockchain_successor"]
end
module Blockchain_test_succ = Make (Blockchain_test_succ_key) (Block_hash_value)

module Block_valid_succs_key = struct
  type t = Block_hash.t
  let to_path p =
    "blocks" :: Block_hash.to_path p @ ["valid_successors"]
end
module Block_valid_succs =
  Make (Block_valid_succs_key) (Block_hash_set_value)

module Block_invalid_succs_key = struct
  type t = Block_hash.t
  let to_path p =
    "blocks" :: Block_hash.to_path p @ ["invalid_successors"]
end
module Block_invalid_succs =
  Make (Block_invalid_succs_key) (Block_hash_set_value)

module Blockchain_key = struct
  type t = Block_hash.t
  let to_path p =
    "blocks" :: Block_hash.to_path p @ ["time"]
end
module Blockchain = Make (Blockchain_key) (Time_value)


(*-- Typed operation store under "operations/" -------------------------------*)

type shell_operation = {
  net_id: net_id ;
}
type operation = {
  shell: shell_operation ;
  proto: MBytes.t ;
}

let shell_operation_encoding =
  let open Data_encoding in
  conv
    (fun { net_id } -> net_id)
    (fun net_id -> { net_id })
    (obj1 (req "net_id" net_id_encoding))

let operation_encoding =
  let open Data_encoding in
  conv
    (fun { shell ; proto } -> (shell, proto))
    (fun (shell, proto) -> { shell ; proto })
    (merge_objs
       shell_operation_encoding
       (obj1 (req "data" Variable.bytes)))

module Raw_operation_value = struct
  type t = operation
  let to_bytes v = Data_encoding.Binary.to_bytes operation_encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes operation_encoding b
end

module Raw_operation_key = struct
  type t = Operation_hash.t
  let to_path p = "operations" :: Operation_hash.to_path p @ [ "contents" ]
end
module Operation_data = Make (Raw_operation_key) (Raw_operation_value)
module Raw_operation_data = Make (Raw_operation_key) (Raw_value)

module Operation_time_key = struct
  type t = Operation_hash.t
  let to_path p = "operations" :: Operation_hash.to_path p @ [ "discovery_time" ]
end
module Operation_time = Make (Operation_time_key) (Time_value)

module Operation_errors_key = struct
  type t = Operation_hash.t
  let to_path p = "operations" :: Operation_hash.to_path p @ [ "errors" ]
end
module Operation_errors = Make (Operation_errors_key) (Errors_value)

module Operation_resolver =
  Persist.MakeHashResolver
    (struct
      include FS
      let mem t k = Lwt.return (exists t k)
      let prefix = ["operations"]
    end)
    (Operation_hash)

module Operation = struct
  type t = FS.t
  type key = Operation_hash.t
  type value = operation tzresult Time.timed_data
  let mem = Operation_data.mem
  let get s k =
    Operation_time.get s k >>= function
    | None -> Lwt.return_none
    | Some time ->
        Operation_errors.get s k >>= function
        | Some exns -> Lwt.return (Some { Time.data = Error exns ; time })
        | None ->
            Operation_data.get s k >>= function
            | None -> Lwt.return_none
            | Some bytes -> Lwt.return (Some { Time.data = Ok bytes ; time })
  let get_exn s k =
    get s k >>= function
    | None -> Lwt.fail Not_found
    | Some x -> Lwt.return x
  let set s k { Time.data ; time } =
    Operation_time.set s k time >>= fun () ->
    match data with
    | Ok bytes ->
        Operation_data.set s k bytes >>= fun () ->
        Operation_errors.del s k
    | Error exns ->
        Operation_errors.set s k exns >>= fun () ->
        Operation_data.del s k
  let del s k =
    Operation_time.del s k >>= fun () ->
    Operation_data.del s k >>= fun () ->
    Operation_errors.del s k
  let compare o1 o2 =
    let (>>) x y = if x = 0 then y () else x in
    let Net net_id1 = o1.shell.net_id
    and Net net_id2 = o2.shell.net_id in
    Block_hash.compare net_id1 net_id2 >> fun () ->
    MBytes.compare o1.proto o2.proto
  let equal b1 b2 = compare b1 b2 = 0
  let of_bytes = Raw_operation_value.of_bytes
  let to_bytes = Raw_operation_value.to_bytes
  let hash op = Operation_hash.hash_bytes [to_bytes op]
  let raw_get t k = Raw_operation_data.get t k

  let keys _t = undefined_key_fn (** We never list keys here *)
end


(*-- Typed operation store under "protocols/" -------------------------------*)

type protocol = Tezos_compiler.Protocol.t
let protocol_encoding = Tezos_compiler.Protocol.encoding

module Raw_protocol_value = Tezos_compiler.Protocol

module Raw_protocol_key = struct
  type t = Protocol_hash.t
  let to_path p = "protocols" :: Protocol_hash.to_path p @ [ "contents" ]
end

module Protocol_data = Make (Raw_protocol_key) (Raw_protocol_value)
module Raw_protocol_data = Make (Raw_protocol_key) (Raw_value)

module Protocol_time_key = struct
  type t = Protocol_hash.t
  let to_path p = "protocols" :: Protocol_hash.to_path p @ [ "discovery_time" ]
end
module Protocol_time = Make (Protocol_time_key) (Time_value)

module Protocol_errors_key = struct
  type t = Protocol_hash.t
  let to_path p = "protocols" :: Protocol_hash.to_path p @ [ "errors" ]
end
module Protocol_errors = Make (Protocol_errors_key) (Errors_value)

module Protocol = struct
  type t = FS.t
  type key = Protocol_hash.t
  type value = Tezos_compiler.Protocol.t tzresult Time.timed_data
  let mem = Protocol_data.mem
  let get s k =
    Protocol_time.get s k >>= function
    | None -> Lwt.return_none
    | Some time ->
        Protocol_errors.get s k >>= function
        | Some exns -> Lwt.return (Some { Time.data = Error exns ; time })
        | None ->
            Protocol_data.get s k >>= function
            | None -> Lwt.return_none
            | Some bytes -> Lwt.return (Some { Time.data = Ok bytes ; time })
  let get_exn s k =
    get s k >>= function
    | None -> Lwt.fail Not_found
    | Some x -> Lwt.return x
  let set s k { Time.data ; time } =
    Protocol_time.set s k time >>= fun () ->
    match data with
    | Ok bytes ->
        Protocol_data.set s k bytes >>= fun () ->
        Protocol_errors.del s k
    | Error exns ->
        Protocol_errors.set s k exns >>= fun () ->
        Protocol_data.del s k
  let del s k =
    Protocol_time.del s k >>= fun () ->
    Protocol_data.del s k >>= fun () ->
    Protocol_errors.del s k
  let of_bytes = Raw_protocol_value.of_bytes
  let to_bytes = Raw_protocol_value.to_bytes
  let hash = Raw_protocol_value.hash
  let compare p1 p2 =
    Protocol_hash.(compare (hash_bytes [to_bytes p1]) (hash_bytes [to_bytes p2]))
  let equal b1 b2 = compare b1 b2 = 0
  let raw_get t k = Raw_protocol_data.get t k

  let fold s x ~f =
    let rec dig i root acc =
      if i <= 0 then
        f (Protocol_hash.of_path @@ List.tl root) acc
      else
        FS.list s [root] >>= fun roots ->
        Lwt_list.fold_right_s (dig (i - 1)) roots acc
    in
    dig Protocol_hash.path_len ["protocols"] x

  let keys s = fold s [] ~f:(fun k a -> Lwt.return @@ k :: a)
end

(*- Genesis and initialization -----------------------------------------------*)

let genesis_encoding =
  let open Data_encoding in
  conv
    (fun {time;block;protocol} -> (time,block,protocol))
    (fun (time,block,protocol) -> {time;block;protocol})
    (obj3
       (req "timestamp" Time.encoding)
       (req "block" Block_hash.encoding)
       (req "protocol" Protocol_hash.encoding))

let read_genesis, store_genesis =
  let key = ["genesis"] in
  let read t =
    get t key >>= function
    | None -> Lwt.return None
    | Some v ->
        match Data_encoding.Json.from_string (MBytes.to_string v) with
        | Error _ ->
            fatal_error
              "Store.read_genesis: invalid json object."
        | Ok json ->
            try Lwt.return
                  (Some (Data_encoding.Json.destruct genesis_encoding json))
            with _ ->
              fatal_error
                "Store.read_genesis: cannot parse json object."  in
  let store t h =
    set t key ( MBytes.of_string @@
                Data_encoding.Json.to_string @@
                Data_encoding.Json.construct genesis_encoding h ) in
  (read, store)

let read_expiration, store_expiration =
  let key = ["expiration"] in
  let read t =
    get t key >>= function
    | None -> Lwt.return None
    | Some v -> Lwt.return (Time.of_notation (MBytes.to_string v)) in
  let store t h =
    set t key ( MBytes.of_string @@ Time.to_notation h ) in
  (read, store)

let current_store_version = MBytes.of_string "1"
let raw_init ~root () =
  FS.init root >>= fun t ->
  get t ["version"] >>= function
  | None ->
      set t ["version"] (MBytes.of_string "1") >>= fun () ->
      Lwt.return t
  | Some version ->
      if MBytes.(version = current_store_version) then
        Lwt.return t
      else
        fatal_error "Store.init: unknown database version"

let net_read ~root (Net net_id) =
  let root = root // "net" // Block_hash.to_hex net_id in
  raw_init ~root () >>= fun t ->
  read_genesis t >>= function
  | None ->
      failwith "Store.net_read: missing genesis information."
  | Some net_genesis ->
      if not (Block_hash.equal net_genesis.block net_id) then
        failwith "Store.net_read: inconsistent genesis block."
      else
        read_expiration t >>= fun net_expiration ->
        begin
          match net_expiration with
          | None -> return ()
          | Some expiration ->
              fail_unless
                Time.(expiration < now ())
                (Unclassified "Store.net_read expired network")
        end >>=? fun () ->

        return {
          net_genesis ;
          net_expiration ;
          net_store = Persist.share t ;
        }

let raw_net_init ~root ?expiration genesis =
  raw_init ~root () >>= fun t ->
  read_genesis t >>= function
  | None ->
      store_genesis t genesis >>= fun () ->
      begin
        match expiration with
        | None -> Lwt.return_unit
        | Some expiration -> store_expiration t expiration
      end >>= fun () ->
      Lwt.return t
  | Some stored_genesis ->
      if not (Block_hash.equal stored_genesis.block genesis.block) then
        fatal_error "Store.net_init: inconsistent genesis block."
      else if
        not (Protocol_hash.equal stored_genesis.protocol genesis.protocol)
      then
        fatal_error "Store.net_init: inconsistent genesis protocol."
      else if
        not (Time.equal stored_genesis.time genesis.time)
      then
        fatal_error "Store.net_init: inconsistent genesis time."
      else
        read_expiration t >>= fun stored_expiration ->
        match stored_expiration, expiration with
        | None, None -> Lwt.return t
        | Some t1, Some t2 when Time.equal t1 t2 -> Lwt.return t
        | _ ->
            fatal_error "Store.net_init: incoherent end of life."

let net_init ~root ?expiration (net_genesis : genesis) =
  let root = root // "net" // Block_hash.to_hex net_genesis.block in
  raw_net_init ~root ?expiration net_genesis >|= fun t ->
  {
    net_genesis ;
    net_expiration = expiration ;
    net_store = Persist.share t ;
  }

let net_destroy ~root { net_genesis } =
  let root = root // "net" // Block_hash.to_hex net_genesis.block in
  IO.remove_rec root >>= fun () ->
  Lwt.return_unit

let init root =
  raw_init ~root:(Filename.concat root "global") () >>= fun t ->
  Base48.register_resolver
    Block_hash.b48check_encoding
    (fun s -> Block_resolver.resolve t s);
  Base48.register_resolver
    Operation_hash.b48check_encoding
    (fun s -> Operation_resolver.resolve t s);
  Lwt.return
    { block = Persist.share t ;
      blockchain = Persist.share t ;
      operation = Persist.share t ;
      protocol = Persist.share t ;
      global_store = Persist.share t ;
      net_init = net_init ~root ;
      net_read = net_read ~root ;
      net_destroy = net_destroy ~root ;
    }

module Faked_functional_typed_store (S: TYPED_IMPERATIVE_STORE)
  : Persist.TYPED_STORE with type key = S.key
                         and type value = S.value
                         and type t = S.t
= struct
  include S
  let set s k v = S.set s k v >>= fun () -> Lwt.return s
  let del s k = S.del s k >>= fun () -> Lwt.return s
end

module Faked_functional_operation = Faked_functional_typed_store (Operation)
module Faked_functional_block = Faked_functional_typed_store (Block)
module Faked_functional_protocol = Faked_functional_typed_store (Protocol)

module Faked_functional_store : Persist.STORE with type t = t
= struct
  include Data_store
  let set s k v = Data_store.set s k v >>= fun () -> Lwt.return s
  let del s k = Data_store.del s k >>= fun () -> Lwt.return s
  let remove_rec s k = Data_store.remove_rec s k >>= fun () -> Lwt.return s

  let keys _s = invalid_arg "function keys not implementable here" (** We never use list here. *)
end
