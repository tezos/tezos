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

type chain = [
  | `Main
  | `Test
  | `Hash of Chain_id.t
]

type chain_prefix = unit * chain
val chain_path: (unit, chain_prefix) RPC_path.t

val parse_chain: string -> (chain, string) result
val chain_to_string: chain -> string

val chain_arg: chain RPC_arg.t

type block = [
  | `Genesis
  | `Head of int
  | `Hash of Block_hash.t * int
  | `Level of Int32.t
]
val parse_block: string -> (block, string) result
val to_string: block -> string

type prefix = (unit * chain) * block
val dir_path: (chain_prefix, chain_prefix) RPC_path.t
val path: (chain_prefix, chain_prefix * block) RPC_path.t
val mempool_path : ('a, 'b) RPC_path.t -> ('a, 'b) RPC_path.t
val live_blocks_path : ('a, 'b) RPC_path.t -> ('a, 'b) RPC_path.t

type operation_list_quota = {
  max_size: int ;
  max_op: int option ;
}

type raw_context =
  | Key of MBytes.t
  | Dir of (string * raw_context) list
  | Cut

val pp_raw_context: Format.formatter -> raw_context -> unit

type error +=
  | Invalid_depth_arg of int

module type PROTO = sig
  val hash: Protocol_hash.t
  type block_header_data
  val block_header_data_encoding: block_header_data Data_encoding.t
  type block_header_metadata
  val block_header_metadata_encoding:
    block_header_metadata Data_encoding.t
  type operation_data
  type operation_receipt
  type operation = {
    shell: Operation.shell_header ;
    protocol_data: operation_data ;
  }

  val operation_data_encoding: operation_data Data_encoding.t
  val operation_receipt_encoding: operation_receipt Data_encoding.t
  val operation_data_and_receipt_encoding:
    (operation_data * operation_receipt) Data_encoding.t
end

module Make(Proto : PROTO)(Next_proto : PROTO) : sig

  val path: (unit, chain_prefix * block) RPC_path.t

  type raw_block_header = {
    shell: Block_header.shell_header ;
    protocol_data: Proto.block_header_data ;
  }

  type block_header = {
    chain_id: Chain_id.t ;
    hash: Block_hash.t ;
    shell: Block_header.shell_header ;
    protocol_data: Proto.block_header_data ;
  }

  type block_metadata = {
    protocol_data: Proto.block_header_metadata ;
    test_chain_status: Test_chain_status.t ;
    max_operations_ttl: int ;
    max_operation_data_length: int ;
    max_block_header_length: int ;
    operation_list_quota: operation_list_quota list ;
  }

  type operation = {
    chain_id: Chain_id.t ;
    hash: Operation_hash.t ;
    shell: Operation.shell_header ;
    protocol_data: Proto.operation_data ;
    receipt: Proto.operation_receipt ;
  }

  type block_info = {
    chain_id: Chain_id.t ;
    hash: Block_hash.t ;
    header: raw_block_header ;
    metadata: block_metadata ;
    operations: operation list list ;
  }

  open RPC_context

  val info:
    #simple -> ?chain:chain -> ?block:block ->
    unit -> block_info tzresult Lwt.t

  val hash:
    #simple -> ?chain:chain -> ?block:block ->
    unit -> Block_hash.t tzresult Lwt.t

  val raw_header:
    #simple -> ?chain:chain -> ?block:block ->
    unit -> MBytes.t tzresult Lwt.t

  val header:
    #simple -> ?chain:chain -> ?block:block ->
    unit -> block_header tzresult Lwt.t

  val metadata:
    #simple -> ?chain:chain -> ?block:block ->
    unit -> block_metadata tzresult Lwt.t

  module Header : sig

    val shell_header:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Block_header.shell_header tzresult Lwt.t
    val protocol_data:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Proto.block_header_data tzresult Lwt.t
    val raw_protocol_data:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> MBytes.t tzresult Lwt.t

  end

  module Operations : sig

    val operations:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> operation list list tzresult Lwt.t
    val operations_in_pass:
      #simple -> ?chain:chain -> ?block:block ->
      int -> operation list tzresult Lwt.t
    val operation:
      #simple -> ?chain:chain -> ?block:block ->
      int -> int -> operation tzresult Lwt.t

  end

  module Operation_hashes : sig

    val operation_hashes:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Operation_hash.t list list tzresult Lwt.t
    val operation_hashes_in_pass:
      #simple -> ?chain:chain -> ?block:block ->
      int -> Operation_hash.t list tzresult Lwt.t
    val operation_hash:
      #simple -> ?chain:chain -> ?block:block ->
      int -> int -> Operation_hash.t tzresult Lwt.t

  end

  module Context : sig

    val read:
      #simple -> ?chain:chain -> ?block:block ->
      ?depth: int ->
      string list -> raw_context tzresult Lwt.t

  end

  module Helpers : sig

    module Forge : sig

      val block_header:
        #RPC_context.simple ->
        ?chain:chain ->
        ?block:block ->
        Block_header.t ->
        MBytes.t tzresult Lwt.t

    end

    module Preapply : sig

      val block:
        #simple -> ?chain:chain -> ?block:block ->
        ?sort:bool ->
        ?timestamp:Time.Protocol.t ->
        protocol_data:Next_proto.block_header_data ->
        Next_proto.operation list list ->
        (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

      val operations:
        #simple -> ?chain:chain -> ?block:block ->
        Next_proto.operation list ->
        (Next_proto.operation_data * Next_proto.operation_receipt) list tzresult Lwt.t

    end

    val complete:
      #simple -> ?chain:chain -> ?block:block ->
      string -> string list tzresult Lwt.t

  end

  module Mempool : sig

    type t = {
      applied: (Operation_hash.t * Next_proto.operation) list ;
      refused: (Next_proto.operation * error list) Operation_hash.Map.t ;
      branch_refused: (Next_proto.operation * error list) Operation_hash.Map.t ;
      branch_delayed: (Next_proto.operation * error list) Operation_hash.Map.t ;
      unprocessed: Next_proto.operation Operation_hash.Map.t ;
    }

    val pending_operations:
      #simple ->
      ?chain:chain ->
      unit -> t tzresult Lwt.t

    val monitor_operations:
      #streamed ->
      ?chain:chain ->
      ?applied:bool ->
      ?branch_delayed:bool ->
      ?branch_refused:bool ->
      ?refused:bool ->
      unit -> (Next_proto.operation list Lwt_stream.t * stopper) tzresult Lwt.t

    val request_operations:
      #simple ->
      ?chain:chain ->
      unit -> unit tzresult Lwt.t

  end

  val live_blocks:
    #simple ->
    ?chain:chain ->
    ?block:block ->
    unit -> Block_hash.Set.t tzresult Lwt.t

  module S : sig

    val hash:
      ([ `GET ], prefix,
       prefix, unit, unit,
       Block_hash.t) RPC_service.t

    val info:
      ([ `GET ], prefix,
       prefix, unit, unit,
       block_info)   RPC_service.t

    val header:
      ([ `GET ], prefix,
       prefix, unit, unit,
       block_header) RPC_service.t

    val raw_header:
      ([ `GET ], prefix,
       prefix, unit, unit,
       MBytes.t) RPC_service.t

    val metadata:
      ([ `GET ], prefix,
       prefix, unit, unit,
       block_metadata) RPC_service.t

    module Header : sig

      val shell_header:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Block_header.shell_header) RPC_service.t

      val protocol_data:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Proto.block_header_data) RPC_service.t

      val raw_protocol_data:
        ([ `GET ], prefix,
         prefix, unit, unit,
         MBytes.t) RPC_service.t

    end

    module Operations : sig

      val operations:
        ([ `GET ], prefix,
         prefix, unit, unit,
         operation list list) RPC_service.t

      val operations_in_pass:
        ([ `GET ], prefix,
         prefix * int, unit, unit,
         operation list) RPC_service.t

      val operation:
        ([ `GET ], prefix,
         (prefix * int) * int, unit, unit,
         operation) RPC_service.t

    end

    module Operation_hashes : sig

      val operation_hashes:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Tezos_crypto.Operation_hash.t list list) RPC_service.t

      val operation_hashes_in_pass:
        ([ `GET ], prefix,
         prefix * int, unit, unit,
         Tezos_crypto.Operation_hash.t list) RPC_service.t

      val operation_hash:
        ([ `GET ], prefix,
         (prefix * int) * int, unit, unit,
         Tezos_crypto.Operation_hash.t) RPC_service.t

    end

    module Context : sig

      val read:
        ([ `GET ], prefix,
         prefix * string list, < depth : int option >, unit,
         raw_context) RPC_service.t

    end

    module Helpers : sig

      module Forge : sig

        val block_header:
          ([ `POST ], prefix,
           prefix, unit, Block_header.t, MBytes.t) RPC_service.service

      end

      module Preapply : sig

        type block_param = {
          protocol_data: Next_proto.block_header_data ;
          operations: Next_proto.operation list list ;
        }

        val block:
          ([ `POST ], prefix,
           prefix, < sort_operations : bool;
                     timestamp : Time.Protocol.t option >, block_param,
           Block_header.shell_header * error Preapply_result.t list) RPC_service.t

        val operations:
          ([ `POST ], prefix,
           prefix, unit, Next_proto.operation list,
           (Next_proto.operation_data * Next_proto.operation_receipt) list) RPC_service.t

      end

      val complete:
        ([ `GET ], prefix,
         prefix * string, unit, unit,
         string list) RPC_service.t

    end

    module Mempool : sig

      val encoding: Mempool.t Data_encoding.t

      val pending_operations:
        ('a, 'b) RPC_path.t ->
        ([ `GET ], 'a,
         'b , unit, unit,
         Mempool.t) RPC_service.t

      val monitor_operations:
        ('a, 'b) RPC_path.t ->
        ([ `GET ], 'a, 'b,
         < applied : bool ; branch_delayed : bool ;
           branch_refused : bool ; refused : bool ; >,
         unit,
         Next_proto.operation list) RPC_service.t

      val request_operations :
        ('a, 'b) RPC_path.t ->
        ([ `POST ], 'a,
         'b , unit, unit, unit) RPC_service.t

    end

    val live_blocks:
      ([ `GET ], prefix,
       prefix, unit, unit,
       Block_hash.Set.t) RPC_service.t

  end

end

module Fake_protocol : PROTO
module Empty : (module type of Make(Fake_protocol)(Fake_protocol))

type protocols = {
  current_protocol: Protocol_hash.t ;
  next_protocol: Protocol_hash.t ;
}

val protocols:
  #RPC_context.simple -> ?chain:chain -> ?block:block ->
  unit -> protocols tzresult Lwt.t
