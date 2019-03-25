(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Data_encoding

type chain = [
  | `Main
  | `Test
  | `Hash of Chain_id.t
]

let chain_arg = Block_services.chain_arg
let to_string = Block_services.chain_to_string
let parse_chain = Block_services.parse_chain

type invalid_block = {
  hash: Block_hash.t ;
  level: Int32.t ;
  errors: error list ;
}

type prefix = Block_services.chain_prefix
let path = Block_services.chain_path

let invalid_block_encoding =
  conv
    (fun { hash ; level ; errors } -> (hash, level, errors))
    (fun (hash, level, errors) -> { hash ; level ; errors })
    (obj3
       (req "block" Block_hash.encoding)
       (req "level" int32)
       (req "errors" RPC_error.encoding))

module S = struct

  let path : prefix RPC_path.context = RPC_path.open_root

  let chain_id =
    RPC_service.get_service
      ~description:"The chain unique identifier."
      ~query: RPC_query.empty
      ~output: Chain_id.encoding
      RPC_path.(path / "chain_id")

  module Blocks = struct

    let list_query =
      let open RPC_query in
      query (fun length heads min_date ->
          object
            method length = length
            method heads = heads
            method min_date = min_date
          end)
      |+ opt_field "length"
        ~descr:
          "The requested number of predecessors to returns (per \
           requested head)."
        RPC_arg.int (fun x -> x#length)
      |+ multi_field "head"
        ~descr:
          "An empty argument requests blocks from the current heads. \
           A non empty list allow to request specific fragment \
           of the chain."
        Block_hash.rpc_arg (fun x -> x#heads)
      |+ opt_field "min_date"
        ~descr: "When `min_date` is provided, heads with a \
                 timestamp before `min_date` are filtered out"
        Time.Protocol.rpc_arg (fun x -> x#min_date)
      |> seal

    let path = RPC_path.(path / "blocks")

    let list =
      let open Data_encoding in
      RPC_service.get_service
        ~description:
          "Lists known heads of the blockchain sorted with decreasing fitness. \
           Optional arguments allows to returns the list of predecessors for \
           known heads or the list of predecessors for a given list of blocks."
        ~query: list_query
        ~output: (list (list Block_hash.encoding))
        path

  end

  module Invalid_blocks = struct

    let path = RPC_path.(path / "invalid_blocks")

    let list =
      RPC_service.get_service
        ~description:
          "Lists blocks that have been declared invalid along with the errors \
           that led to them being declared invalid."
        ~query: RPC_query.empty
        ~output: (list invalid_block_encoding)
        path

    let get =
      RPC_service.get_service
        ~description: "The errors that appears during the block (in)validation."
        ~query: RPC_query.empty
        ~output: invalid_block_encoding
        RPC_path.(path /: Block_hash.rpc_arg)

    let delete =
      RPC_service.delete_service
        ~description: "Remove an invalid block for the tezos storage"
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        RPC_path.(path /: Block_hash.rpc_arg)

  end

end

let make_call0 s ctxt chain q p =
  let s = RPC_service.prefix path s in
  RPC_context.make_call1 s ctxt chain q p

let make_call1 s ctxt chain a q p =
  let s = RPC_service.prefix path s in
  RPC_context.make_call2 s ctxt chain a q p

let chain_id ctxt =
  let f = make_call0 S.chain_id ctxt in
  fun ?(chain = `Main) () ->
    match chain with
    | `Hash h -> return h
    | _ -> f chain () ()

module Blocks = struct

  let list ctxt =
    let f = make_call0 S.Blocks.list ctxt in
    fun ?(chain = `Main) ?(heads = []) ?length ?min_date () ->
      f chain
        (object
          method heads = heads
          method length = length
          method min_date = min_date
        end)
        ()

  include Block_services.Empty

  type protocols = Block_services.protocols = {
    current_protocol: Protocol_hash.t ;
    next_protocol: Protocol_hash.t ;
  }

  let protocols = Block_services.protocols

end

module Mempool = Block_services.Empty.Mempool

module Invalid_blocks = struct

  let list ctxt =
    let f = make_call0 S.Invalid_blocks.list ctxt in
    fun ?(chain = `Main) () ->
      f chain () ()

  let get ctxt =
    let f = make_call1 S.Invalid_blocks.get ctxt in
    fun ?(chain = `Main) block  ->
      f chain block () ()

  let delete ctxt =
    let f = make_call1 S.Invalid_blocks.delete ctxt in
    fun ?(chain = `Main) block ->
      f chain block () ()

end
