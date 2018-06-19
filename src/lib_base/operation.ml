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

include Compare.Make(struct
    type nonrec t = t
    let compare o1 o2 =
      let (>>) x y = if x = 0 then y () else x in
      Block_hash.compare o1.shell.branch o1.shell.branch >> fun () ->
      MBytes.compare o1.proto o2.proto
  end)

let encoding =
  let open Data_encoding in
  conv
    (fun { shell ; proto } -> (shell, proto))
    (fun (shell, proto) -> { shell ; proto })
    (merge_objs
       shell_header_encoding
       (obj1 (req "data" Variable.bytes)))

let bounded_encoding ?max_size () =
  match max_size with
  | None -> encoding
  | Some max_size -> Data_encoding.check_size max_size encoding

let bounded_list_encoding
    ?max_length ?max_size ?max_operation_size ?max_pass () =
  let open Data_encoding in
  let op_encoding = bounded_encoding ?max_size:max_operation_size () in
  let op_list_encoding =
    match max_size with
    | None ->
        Variable.list ?max_length (dynamic_size op_encoding)
    | Some max_size ->
        check_size max_size
          (Variable.list ?max_length (dynamic_size op_encoding)) in
  obj2
    (req "operation_hashes_path"
       (Operation_list_list_hash.bounded_path_encoding ?max_length:max_pass ()))
    (req "operations" op_list_encoding)

let bounded_hash_list_encoding ?max_length ?max_pass () =
  let open Data_encoding in
  obj2
    (req "operation_hashes_path"
       (Operation_list_list_hash.bounded_path_encoding ?max_length:max_pass ()))
    (req "operation_hashes" (Variable.list ?max_length Operation_hash.encoding))

let pp fmt op =
  Data_encoding.Json.pp fmt
    (Data_encoding.Json.construct encoding op)

let to_bytes v = Data_encoding.Binary.to_bytes_exn encoding v
let of_bytes b = Data_encoding.Binary.of_bytes encoding b
let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

let hash op = Operation_hash.hash_bytes [to_bytes op]
let hash_raw bytes = Operation_hash.hash_bytes [bytes]

