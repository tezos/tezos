(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Bounded_encoding = struct

  open Data_encoding

  let block_header_max_size = ref (Some (8 * 1024 * 1024)) (* FIXME: arbitrary *)
  let block_header_cache =
    ref (Block_header.bounded_encoding ?max_size:!block_header_max_size ())
  let block_locator_cache =
    ref (Block_locator.bounded_encoding
           ?max_header_size:!block_header_max_size ())

  let update_block_header_encoding () =
    block_header_cache :=
      Block_header.bounded_encoding ?max_size:!block_header_max_size () ;
    block_locator_cache :=
      Block_locator.bounded_encoding ?max_header_size:!block_header_max_size ()

  let set_block_header_max_size max =
    block_header_max_size := max ;
    update_block_header_encoding ()
  let block_header = delayed (fun () -> !block_header_cache)
  let block_locator = delayed (fun () -> !block_locator_cache)

  (* FIXME: all constants below are arbitrary high bounds until we
     have the mechanism to update them properly *)
  let operation_max_size = ref (Some (128 * 1024)) (* FIXME: arbitrary *)
  let operation_list_max_size = ref (Some (1024 * 1024)) (* FIXME: arbitrary *)
  let operation_list_max_length = ref None (* FIXME: arbitrary *)
  let operation_max_pass = ref (Some 8) (* FIXME: arbitrary *)

  let operation_cache =
    ref (Operation.bounded_encoding ?max_size:!operation_max_size ())
  let operation_list_cache =
    ref (Operation.bounded_list_encoding
           ?max_length:!operation_list_max_length
           ?max_size:!operation_list_max_size
           ?max_operation_size:!operation_max_size
           ?max_pass:!operation_max_pass ())
  let operation_hash_list_cache =
    ref (Operation.bounded_hash_list_encoding
           ?max_length:!operation_list_max_length
           ?max_pass:!operation_max_pass ())

  let update_operation_list_encoding () =
    operation_list_cache :=
      Operation.bounded_list_encoding
        ?max_length:!operation_list_max_length
        ?max_size:!operation_list_max_size
        ?max_operation_size:!operation_max_size
        ?max_pass:!operation_max_pass
        ()
  let update_operation_hash_list_encoding () =
    operation_list_cache :=
      Operation.bounded_list_encoding
        ?max_length:!operation_list_max_length
        ?max_pass:!operation_max_pass
        ()
  let update_operation_encoding () =
    operation_cache :=
      Operation.bounded_encoding ?max_size:!operation_max_size ()

  let set_operation_max_size max =
    operation_max_size := max ;
    update_operation_encoding () ;
    update_operation_list_encoding ()
  let set_operation_list_max_size max =
    operation_list_max_size := max ;
    update_operation_list_encoding ()
  let set_operation_list_max_length max =
    operation_list_max_length := max ;
    update_operation_list_encoding () ;
    update_operation_hash_list_encoding ()
  let set_operation_max_pass max =
    operation_max_pass := max ;
    update_operation_list_encoding () ;
    update_operation_hash_list_encoding ()

  let operation = delayed (fun () -> !operation_cache)
  let operation_list = delayed (fun () -> !operation_list_cache)
  let operation_hash_list = delayed (fun () -> !operation_hash_list_cache)

  let protocol_max_size = ref (Some (2 * 1024 * 1024)) (* FIXME: arbitrary *)
  let protocol_cache =
    ref (Protocol.bounded_encoding ?max_size:!protocol_max_size ())
  let set_protocol_max_size max =
    protocol_max_size := max
  let protocol = delayed (fun () -> !protocol_cache)

  let mempool_max_operations = ref None
  let mempool_cache =
    ref (Mempool.bounded_encoding ?max_operations:!mempool_max_operations ())
  let set_mempool_max_operations max =
    mempool_max_operations := max
  let mempool = delayed (fun () -> !mempool_cache)

end

type t =

  | Get_current_branch of Chain_id.t
  | Current_branch of Chain_id.t * Block_locator.t
  | Deactivate of Chain_id.t

  | Get_current_head of Chain_id.t
  | Current_head of Chain_id.t * Block_header.t * Mempool.t

  | Get_block_headers of Block_hash.t list
  | Block_header of Block_header.t

  | Get_operations of Operation_hash.t list
  | Operation of Operation.t

  | Get_protocols of Protocol_hash.t list
  | Protocol of Protocol.t

  | Get_operation_hashes_for_blocks of (Block_hash.t * int) list
  | Operation_hashes_for_block of
      Block_hash.t * int *
      Operation_hash.t list * Operation_list_list_hash.path

  | Get_operations_for_blocks of (Block_hash.t * int) list
  | Operations_for_block of
      Block_hash.t * int *
      Operation.t list * Operation_list_list_hash.path

let encoding =
  let open Data_encoding in
  let case ?max_length ~tag ~title encoding unwrap wrap =
    P2p.Encoding { tag ; title ; encoding ; wrap ; unwrap ; max_length } in
  [
    case ~tag:0x10
      ~title:"Get_current_branch"
      (obj1
         (req "get_current_branch" Chain_id.encoding))
      (function
        | Get_current_branch chain_id -> Some chain_id
        | _ -> None)
      (fun chain_id -> Get_current_branch chain_id) ;

    case ~tag:0x11
      ~title:"Current_branch"
      (obj2
         (req "chain_id" Chain_id.encoding)
         (req "current_branch" Bounded_encoding.block_locator))
      (function
        | Current_branch (chain_id, locator) -> Some (chain_id, locator)
        | _ -> None)
      (fun (chain_id, locator) -> Current_branch (chain_id, locator)) ;

    case ~tag:0x12
      ~title:"Deactivate"
      (obj1
         (req "deactivate" Chain_id.encoding))
      (function
        | Deactivate chain_id -> Some chain_id
        | _ -> None)
      (fun chain_id -> Deactivate chain_id) ;

    case ~tag:0x13
      ~title:"Get_current_head"
      (obj1
         (req "get_current_head" Chain_id.encoding))
      (function
        | Get_current_head chain_id -> Some chain_id
        | _ -> None)
      (fun chain_id -> Get_current_head chain_id) ;

    case ~tag:0x14
      ~title:"Current_head"
      (obj3
         (req "chain_id" Chain_id.encoding)
         (req "current_block_header" (dynamic_size Bounded_encoding.block_header))
         (req "current_mempool" Bounded_encoding.mempool))
      (function
        | Current_head (chain_id, bh, mempool) -> Some (chain_id, bh, mempool)
        | _ -> None)
      (fun (chain_id, bh, mempool) -> Current_head (chain_id, bh, mempool)) ;

    case ~tag:0x20
      ~title:"Get_block_headers"
      (obj1 (req "get_block_headers" (list ~max_length:10 Block_hash.encoding)))
      (function
        | Get_block_headers bhs -> Some bhs
        | _ -> None)
      (fun bhs -> Get_block_headers bhs) ;

    case ~tag:0x21
      ~title:"Block_header"
      (obj1 (req "block_header" Bounded_encoding.block_header))
      (function
        | Block_header bh -> Some bh
        | _ -> None)
      (fun bh -> Block_header bh) ;

    case ~tag:0x30
      ~title:"Get_operations"
      (obj1 (req "get_operations" (list ~max_length:10 Operation_hash.encoding)))
      (function
        | Get_operations bhs -> Some bhs
        | _ -> None)
      (fun bhs -> Get_operations bhs) ;

    case ~tag:0x31
      ~title:"Operation"
      (obj1 (req "operation" Bounded_encoding.operation))
      (function Operation o -> Some o | _ -> None)
      (fun o -> Operation o);

    case ~tag:0x40
      ~title:"Get_protocols"
      (obj1
         (req "get_protocols" (list ~max_length:10 Protocol_hash.encoding)))
      (function
        | Get_protocols protos -> Some protos
        | _ -> None)
      (fun protos -> Get_protocols protos);

    case ~tag:0x41
      ~title:"Protocol"
      (obj1 (req "protocol" Bounded_encoding.protocol))
      (function Protocol proto -> Some proto  | _ -> None)
      (fun proto -> Protocol proto);

    case ~tag:0x50
      ~title:"Get_operation_hashes_for_blocks"
      (obj1 (req "get_operation_hashes_for_blocks"
               (list ~max_length:10 (tup2 Block_hash.encoding int8))))
      (function
        | Get_operation_hashes_for_blocks keys -> Some keys
        | _ -> None)
      (fun keys -> Get_operation_hashes_for_blocks keys);

    case ~tag:0x51
      ~title:"Operation_hashes_for_blocks"
      (merge_objs
         (obj1
            (req "operation_hashes_for_block"
               (obj2
                  (req "hash" Block_hash.encoding)
                  (req "validation_pass" int8))))
         Bounded_encoding.operation_hash_list)
      (function Operation_hashes_for_block (block, ofs, ops, path) ->
         Some ((block, ofs), (path, ops)) | _ -> None)
      (fun ((block, ofs), (path, ops)) ->
         Operation_hashes_for_block (block, ofs, ops, path)) ;

    case ~tag:0x60
      ~title:"Get_operations_for_blocks"
      (obj1 (req "get_operations_for_blocks"
               (list ~max_length:10
                  (obj2
                     (req "hash" Block_hash.encoding)
                     (req "validation_pass" int8)))))
      (function
        | Get_operations_for_blocks keys -> Some keys
        | _ -> None)
      (fun keys -> Get_operations_for_blocks keys);

    case ~tag:0x61
      ~title:"Operations_for_blocks"
      (merge_objs
         (obj1
            (req "operations_for_block"
               (obj2
                  (req "hash" Block_hash.encoding)
                  (req "validation_pass" int8))))
         Bounded_encoding.operation_list)
      (function Operations_for_block (block, ofs, ops, path) ->
         Some ((block, ofs), (path, ops)) | _ -> None)
      (fun ((block, ofs), (path, ops)) ->
         Operations_for_block (block, ofs, ops, path)) ;

  ]

let cfg : _ P2p.message_config = {
  encoding ;
  chain_name = Distributed_db_version.chain_name ;
  distributed_db_versions = [
    Distributed_db_version.zero ;
  ] ;
}

let raw_encoding = P2p.Raw.encoding encoding

let pp_json ppf msg =
  Data_encoding.Json.pp ppf
    (Data_encoding.Json.construct raw_encoding (Message msg))

module Logging = struct
  let tag = Tag.def ~doc:"Message" "message" pp_json
end
