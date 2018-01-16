(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type shell_header = {
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
  context: Context_hash.t ;
}

let shell_header_encoding =
  let open Data_encoding in
  conv
    (fun { level ; proto_level ; predecessor ;
           timestamp ; validation_passes ; operations_hash ; fitness ;
           context } ->
      (level, proto_level, predecessor,
       timestamp, validation_passes, operations_hash, fitness,
       context))
    (fun (level, proto_level, predecessor,
          timestamp, validation_passes, operations_hash, fitness,
          context) ->
      { level ; proto_level ; predecessor ;
        timestamp ; validation_passes ; operations_hash ; fitness ;
        context })
    (obj8
       (req "level" int32)
       (req "proto" uint8)
       (req "predecessor" Block_hash.encoding)
       (req "timestamp" Time.encoding)
       (req "validation_pass" uint8)
       (req "operations_hash" Operation_list_list_hash.encoding)
       (req "fitness" Fitness.encoding)
       (req "context" Context_hash.encoding))

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

let (=) = equal
let (<>) x y = compare x y <> 0
let (<) x y = compare x y < 0
let (<=) x y = compare x y <= 0
let (>=) x y = compare x y >= 0
let (>) x y = compare x y > 0
let min x y = if x <= y then x else y
let max x y = if x <= y then y else x

let to_bytes v = Data_encoding.Binary.to_bytes encoding v
let of_bytes b = Data_encoding.Binary.of_bytes encoding b
let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

let hash block = Block_hash.hash_bytes [to_bytes block]
let hash_raw bytes = Block_hash.hash_bytes [bytes]
