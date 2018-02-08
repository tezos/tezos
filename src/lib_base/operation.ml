(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type shell_header = {
  branch: Block_hash.t ;
}

let shell_header_encoding =
  let open Data_encoding in
  conv
    (fun { branch } -> branch)
    (fun branch -> { branch })
    (obj1 (req "branch" Block_hash.encoding))

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
  Data_encoding.Json.pp fmt
    (Data_encoding.Json.construct encoding op)

let compare o1 o2 =
  let (>>) x y = if x = 0 then y () else x in
  Block_hash.compare o1.shell.branch o1.shell.branch >> fun () ->
  MBytes.compare o1.proto o2.proto
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

let hash op = Operation_hash.hash_bytes [to_bytes op]
let hash_raw bytes = Operation_hash.hash_bytes [bytes]

