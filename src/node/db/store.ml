(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Store_sigs

type t = Raw_store.t
type global_store = t

let init = Raw_store.init

(**************************************************************************
 * Net store under "net/"
 **************************************************************************)

module Net_id = struct

  module T = struct
    type t = Id of Block_hash.t
    type net_id = t

    let encoding =
      let open Data_encoding in
      conv
        (fun (Id net_id) -> net_id)
        (fun net_id -> Id net_id)
        Block_hash.encoding

    let pp ppf (Id id) = Block_hash.pp_short ppf id
    let compare (Id id1) (Id id2) = Block_hash.compare id1 id2
    let equal (Id id1) (Id id2) = Block_hash.equal id1 id2
    let hash (Id id) =
      let raw_hash = Block_hash.to_string id in
      let int64_hash = EndianString.BigEndian.get_int64 raw_hash 0 in
      Int64.to_int int64_hash

    let to_path (Id id) = Block_hash.to_path id
    let of_path p =
      match Block_hash.of_path p with
      | None -> None
      | Some id -> Some (Id id)
    let path_length = Block_hash.path_length
    let of_bytes_exn data = Id (Block_hash.of_bytes_exn data)
    let to_bytes (Id id) = Block_hash.to_bytes id

  end

  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  module Table = Hashtbl.Make(T)

end

module Net = struct

  type store = global_store * Net_id.t
  let get s id = (s, id)

  module Indexed_store =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore(Raw_store)(struct let name = ["net"] end))
      (Net_id)

  let destroy = Indexed_store.remove_all
  let list t =
    Indexed_store.fold_indexes t ~init:[]
      ~f:(fun h acc -> Lwt.return (h :: acc))

  module Genesis_time =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "time"] end)
      (Store_helpers.Make_value(Time))

  module Genesis_protocol =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "protocol"] end)
      (Store_helpers.Make_value(Protocol_hash))

  module Genesis_test_protocol =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "test_protocol"] end)
      (Store_helpers.Make_value(Protocol_hash))

  module Expiration =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["expiration"] end)
      (Store_helpers.Make_value(Time))

  module Forked_network_ttl =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["forked_network_ttl"] end)
      (Store_helpers.Make_value(struct
         type t = Int64.t
         let encoding = Data_encoding.int64
       end))

end


(**************************************************************************
 * Generic store for "tracked" data: discovery_time, invalidity,
 * incoming peers,... (for operations, block_headers, and protocols).
 **************************************************************************)

module type DATA_STORE = sig

  type store
  type key
  type key_set
  type value

  val encoding: value Data_encoding.t

  val compare: value -> value -> int
  val equal: value -> value -> bool

  val hash: value -> key
  val hash_raw: MBytes.t -> key

  module Discovery_time : MAP_STORE
    with type t := store
     and type key := key
     and type value := Time.t

  module Contents : SINGLE_STORE
    with type t = store * key
     and type value := value

  module RawContents : SINGLE_STORE
    with type t = store * key
     and type value := MBytes.t

  module Validation_time : SINGLE_STORE
    with type t = store * key
     and type value := Time.t

  module Errors : MAP_STORE
    with type t := store
     and type key := key
     and type value = error list

  module Pending : BUFFERED_SET_STORE
    with type t = store
     and type elt := key
     and type Set.t = key_set

end

module Errors_value =
  Store_helpers.Make_value(struct
    type t = error list
    let encoding = (Data_encoding.list (Error_monad.error_encoding ()))
  end)

module Raw_value = struct
  type t = MBytes.t
  let of_bytes b = ok b
  let to_bytes b = b
end

module Make_data_store
    (S : STORE) (I : INDEX) (V : VALUE)
    (Set : Set.S with type elt = I.t) = struct

  type key = I.t
  type value = V.t
  type key_set = Set.t

  let of_bytes = V.of_bytes
  let to_bytes = V.to_bytes

  module Indexed_store =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore (S) (struct let name = ["data"] end))
      (I)
  module Discovery_time =
    Indexed_store.Make_map
      (struct let name = ["discovery_time"] end)
      (Store_helpers.Make_value(Time))
  module Contents =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["contents"] end)
      (V)
  module RawContents =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["contents"] end)
      (Raw_value)
  module Errors =
    Store_helpers.Make_map
      (Store_helpers.Make_substore (S) (struct let name = ["invalids"] end))
      (I)
      (Errors_value)
  module Pending =
    Store_helpers.Make_buffered_set
      (Store_helpers.Make_substore (S) (struct let name = ["pending"] end))
      (I)
      (Set)
  module Validation_time =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["validation_time"] end)
      (Store_helpers.Make_value(Time))
end


(**************************************************************************
 * Operation store under "net/<id>/operations/"
 **************************************************************************)

module Operation = struct

  type shell_header = {
    net_id: Net_id.t ;
  }

  let shell_header_encoding =
    let open Data_encoding in
    conv
      (fun { net_id } -> net_id)
      (fun net_id -> { net_id })
      (obj1 (req "net_id" Net_id.encoding))

  module Encoding = struct
    type t = {
      shell: shell_header ;
      proto: MBytes.t ;
    }
    let encoding =
      let open Data_encoding in
      conv
        (fun { shell ; proto } -> (shell, proto))
        (fun (shell, proto) -> { shell ; proto })
        (merge_objs
           shell_header_encoding
           (obj1 (req "data" Variable.bytes)))
  end
  module Value = Store_helpers.Make_value(Encoding)
  include Encoding

  let compare o1 o2 =
    let (>>) x y = if x = 0 then y () else x in
    Net_id.compare o1.shell.net_id o1.shell.net_id >> fun () ->
    MBytes.compare o1.proto o2.proto
  let equal b1 b2 = compare b1 b2 = 0
  let hash op = Operation_hash.hash_bytes [Value.to_bytes op]
  let hash_raw bytes = Operation_hash.hash_bytes [bytes]

  type store = Net.store
  let get x = x

  include
    Make_data_store
      (Store_helpers.Make_substore
         (Net.Indexed_store.Store)
         (struct let name = ["operations"] end))
      (Operation_hash)
      (Value)
      (Operation_hash.Set)

end


(**************************************************************************
 * Block_header store under "net/<id>/blocks/"
 **************************************************************************)

module Block_header = struct

  type shell_header = {
    net_id: Net_id.t ;
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    fitness: MBytes.t list ;
    operations: Operation_hash.t list ;
  }

  let shell_header_encoding =
    let open Data_encoding in
    conv
      (fun { net_id ; predecessor ; timestamp ; fitness ; operations } ->
         (net_id, predecessor, timestamp, fitness, operations))
      (fun (net_id, predecessor, timestamp, fitness, operations) ->
         { net_id ; predecessor ; timestamp ; fitness ; operations })
      (obj5
         (req "net_id" Net_id.encoding)
         (req "predecessor" Block_hash.encoding)
         (req "timestamp" Time.encoding)
         (req "fitness" Fitness.encoding)
         (req "operations" (list Operation_hash.encoding)))

  module Encoding = struct
    type t = {
      shell: shell_header ;
      proto: MBytes.t ;
    }
    let encoding =
      let open Data_encoding in
      conv
        (fun { shell ; proto } -> (shell, proto))
        (fun (shell, proto) -> { shell ; proto })
        (merge_objs
           shell_header_encoding
           (obj1 (req "data" Variable.bytes)))
  end
  module Value = Store_helpers.Make_value(Encoding)
  include Encoding

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
  let hash block = Block_hash.hash_bytes [Value.to_bytes block]
  let hash_raw bytes = Block_hash.hash_bytes [bytes]

  type store = Net.store
  let get x = x

  include Make_data_store
      (Store_helpers.Make_substore
         (Net.Indexed_store.Store)
         (struct let name = ["blocks"] end))
      (Block_hash)
      (Value)
      (Block_hash.Set)

end


(**************************************************************************
 * Blockchain data
 **************************************************************************)

module Chain = struct

  type store = Net.store
  let get s = s

  module Known_heads =
    Store_helpers.Make_buffered_set
      (Store_helpers.Make_substore
         (Net.Indexed_store.Store)
         (struct let name = ["known_heads"] end))
      (Block_hash)
      (Block_hash.Set)

  module Current_head =
    Store_helpers.Make_single_store
      (Net.Indexed_store.Store)
      (struct let name = ["current_head"] end)
      (Store_helpers.Make_value(Block_hash))

  module Valid_successors =
    Store_helpers.Make_buffered_set
      (Store_helpers.Make_substore
         (Block_header.Indexed_store.Store)
         (struct let name = ["known_successors" ; "valid" ] end))
      (Block_hash)
      (Block_hash.Set)

  module Invalid_successors =
    Store_helpers.Make_buffered_set
      (Store_helpers.Make_substore
         (Block_header.Indexed_store.Store)
         (struct let name = ["known_successors" ; "invalid"] end))
      (Block_hash)
      (Block_hash.Set)

  module Successor_in_chain =
    Store_helpers.Make_single_store
      (Block_header.Indexed_store.Store)
      (struct let name = ["successor_in_chain"] end)
      (Store_helpers.Make_value(Block_hash))

  module In_chain_insertion_time =
    Store_helpers.Make_single_store
      (Block_header.Indexed_store.Store)
      (struct let name = ["in_chain_insertion_time"] end)
      (Store_helpers.Make_value(Time))

end


(**************************************************************************
 * Protocol store under "protocols/"
 **************************************************************************)

module Protocol = struct

  include Tezos_compiler.Protocol
  let hash_raw bytes = Protocol_hash.hash_bytes [bytes]

  type store = global_store
  let get x = x

  include Make_data_store
      (Store_helpers.Make_substore
         (Raw_store)
         (struct let name = ["protocols"] end))
      (Protocol_hash)
      (Store_helpers.Make_value(Tezos_compiler.Protocol))
      (Protocol_hash.Set)

end

