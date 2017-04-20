(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash

module type DATA = sig

  type t

  val compare: t -> t -> int
  val equal: t -> t -> bool

  val pp: Format.formatter -> t -> unit

  val encoding: t Data_encoding.t
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option

end

module type HASHABLE_DATA = sig

  include DATA

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end

module Fitness = struct

  type t = MBytes.t list

  (* Fitness comparison:
     - shortest lists are smaller ;
     - lexicographical order for lists of the same length. *)
  let compare_bytes b1 b2 =
    let len1 = MBytes.length b1 in
    let len2 = MBytes.length b2 in
    let c = compare len1 len2 in
    if c <> 0
    then c
    else
      let rec compare_byte b1 b2 pos len =
        if pos = len
        then 0
        else
          let c = compare (MBytes.get_char b1 pos) (MBytes.get_char b2 pos) in
          if c <> 0
          then c
          else compare_byte b1 b2 (pos+1) len
      in
      compare_byte b1 b2 0 len1

  let compare f1 f2 =
    let rec compare_rec f1 f2 = match f1, f2 with
      | [], [] -> 0
      | i1 :: f1, i2 :: f2 ->
          let i = compare_bytes i1 i2 in
          if i = 0 then compare_rec f1 f2 else i
      | _, _ -> assert false in
    let len = compare (List.length f1) (List.length f2) in
    if len = 0 then compare_rec f1 f2 else len

  let equal f1 f2 = compare f1 f2 = 0

  let rec pp fmt = function
    | [] -> ()
    | [f] -> Format.fprintf fmt "%s" (Hex_encode.hex_of_bytes f)
    | f1 :: f -> Format.fprintf fmt "%s::%a" (Hex_encode.hex_of_bytes f1) pp f

  let to_string x = Format.asprintf "%a" pp x

  let encoding =
    let open Data_encoding in
    describe ~title: "Tezos block fitness"
      (list bytes)

  let to_bytes v = Data_encoding.Binary.to_bytes encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes encoding b
  let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

end

module Operation = struct

  type shell_header = {
    net_id: Net_id.t ;
    branch: Block_hash.t ;
  }

  let shell_header_encoding =
    let open Data_encoding in
    conv
      (fun { net_id ; branch } -> net_id, branch)
      (fun (net_id, branch) -> { net_id ; branch })
      (obj2
         (req "net_id" Net_id.encoding)
         (req "branch" Block_hash.encoding))

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

  let pp fmt op =
    Format.pp_print_string fmt @@
    Data_encoding_ezjsonm.to_string (Data_encoding.Json.construct encoding op)

  let compare o1 o2 =
    let (>>) x y = if x = 0 then y () else x in
    Net_id.compare o1.shell.net_id o1.shell.net_id >> fun () ->
    MBytes.compare o1.proto o2.proto
  let equal b1 b2 = compare b1 b2 = 0

  let to_bytes v = Data_encoding.Binary.to_bytes encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes encoding b
  let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

  let hash op = Operation_hash.hash_bytes [to_bytes op]
  let hash_raw bytes = Operation_hash.hash_bytes [bytes]

end

module Block_header = struct

  type shell_header = {
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

  let shell_header_encoding =
    let open Data_encoding in
    conv
      (fun { net_id ; level ; proto_level ; predecessor ;
             timestamp ; operations_hash ; fitness } ->
        (net_id, level, proto_level, predecessor,
         timestamp, operations_hash, fitness))
      (fun (net_id, level, proto_level, predecessor,
            timestamp, operations_hash, fitness) ->
        { net_id ; level ; proto_level ; predecessor ;
          timestamp ; operations_hash ; fitness })
      (obj7
         (req "net_id" Net_id.encoding)
         (req "level" int32)
         (req "proto" uint8)
         (req "predecessor" Block_hash.encoding)
         (req "timestamp" Time.encoding)
         (req "operations_hash" Operation_list_list_hash.encoding)
         (req "fitness" Fitness.encoding))

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

  let encoding =
    let open Data_encoding in
    conv
      (fun { shell ; proto } -> (shell, proto))
      (fun (shell, proto) -> { shell ; proto })
      (merge_objs
         shell_header_encoding
         (obj1 (req "data" Variable.bytes)))

  let pp fmt op =
    Format.pp_print_string fmt @@
    Data_encoding_ezjsonm.to_string (Data_encoding.Json.construct encoding op)

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
    Operation_list_list_hash.compare
      b1.shell.operations_hash b2.shell.operations_hash >> fun () ->
    Time.compare b1.shell.timestamp b2.shell.timestamp >> fun () ->
    list compare b1.shell.fitness b2.shell.fitness

  let equal b1 b2 = compare b1 b2 = 0

  let to_bytes v = Data_encoding.Binary.to_bytes encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes encoding b
  let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

  let hash block = Block_hash.hash_bytes [to_bytes block]
  let hash_raw bytes = Block_hash.hash_bytes [bytes]

end

module Protocol = struct

  type t = component list

  and component = {
    name: string ;
    interface: string option ;
    implementation: string ;
  }

  let component_encoding =
    let open Data_encoding in
    conv
      (fun { name ; interface; implementation } ->
         (name, interface, implementation))
      (fun (name, interface, implementation) ->
         { name ; interface ; implementation })
      (obj3
         (req "name" string)
         (opt "interface" string)
         (req "implementation" string))

  let encoding = Data_encoding.list component_encoding

  let pp fmt op =
    Format.pp_print_string fmt @@
    Data_encoding_ezjsonm.to_string (Data_encoding.Json.construct encoding op)

  let compare = Pervasives.compare
  let equal = (=)

  let to_bytes v = Data_encoding.Binary.to_bytes encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes encoding b
  let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b
  let hash proto = Protocol_hash.hash_bytes [to_bytes proto]
  let hash_raw proto = Protocol_hash.hash_bytes [proto]

end
