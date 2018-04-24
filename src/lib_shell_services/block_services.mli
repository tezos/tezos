(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
]
val parse_block: string -> (block, string) result
val to_string: block -> string

type prefix = (unit * chain) * block
val dir_path: (chain_prefix, chain_prefix) RPC_path.t
val path: (chain_prefix, chain_prefix * block) RPC_path.t

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
  | Invalid_depth_arg of (string list * int)
  | Missing_key of string list

module type PROTO = sig
  val hash: Protocol_hash.t
  type block_header_data
  val block_header_data_encoding: block_header_data Data_encoding.t
  type block_header_metadata
  val block_header_metadata_encoding:
    block_header_metadata Data_encoding.t
  type operation_data
  val operation_data_encoding: operation_data Data_encoding.t
  type operation_metadata
  val operation_metadata_encoding: operation_metadata Data_encoding.t
  type operation = {
    shell: Operation.shell_header ;
    protocol_data: operation_data ;
  }
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
    metadata: Proto.operation_metadata ;
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

  module Header : sig

    val header:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> block_header tzresult Lwt.t
    val shell_header:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Block_header.shell_header tzresult Lwt.t
    val protocol_data:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Proto.block_header_data tzresult Lwt.t

    module Shell : sig

      val level:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Int32.t tzresult Lwt.t
      val protocol_level:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> int tzresult Lwt.t
      val predecessor:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Block_hash.t tzresult Lwt.t
      val timestamp:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Time.t tzresult Lwt.t
      val validation_passes:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> int tzresult Lwt.t
      val operations_hash:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Operation_list_list_hash.t tzresult Lwt.t
      val fitness:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Fitness.t tzresult Lwt.t
      val context_hash:
        #simple -> ?chain:chain -> ?block:block ->
        unit -> Context_hash.t tzresult Lwt.t

    end

  end

  module Metadata : sig

    val metadata:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> block_metadata tzresult Lwt.t
    val protocol_data:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Proto.block_header_metadata tzresult Lwt.t
    val protocol_hash:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Protocol_hash.t tzresult Lwt.t
    val next_protocol_hash:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Protocol_hash.t tzresult Lwt.t
    val test_chain_status:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> Test_chain_status.t tzresult Lwt.t
    val max_operations_ttl:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> int tzresult Lwt.t
    val max_operation_data_length:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> int tzresult Lwt.t
    val max_block_header_length:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> int tzresult Lwt.t
    val max_operation_list_length:
      #simple -> ?chain:chain -> ?block:block ->
      unit -> operation_list_quota list tzresult Lwt.t

  end

  module Operation : sig

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

  module Operation_hash : sig

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
        ?timestamp:Time.t ->
        protocol_data:Next_proto.block_header_data ->
        Next_proto.operation list list ->
        (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

      val operations:
        #simple -> ?chain:chain -> ?block:block ->
        Next_proto.operation list ->
        Next_proto.operation_metadata list tzresult Lwt.t

    end

    val complete:
      #simple -> ?chain:chain -> ?block:block ->
      string -> string list tzresult Lwt.t

  end

  module S : sig

    val hash:
      ([ `GET ], prefix,
       prefix, unit, unit,
       Block_hash.t) RPC_service.t

    val info:
      ([ `GET ], prefix,
       prefix, unit, unit,
       block_info)   RPC_service.t

    module Header : sig

      val header:
        ([ `GET ], prefix,
         prefix, unit, unit,
         block_header) RPC_service.t

      val shell_header:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Block_header.shell_header) RPC_service.t

      val protocol_data:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Proto.block_header_data) RPC_service.t

      module Shell : sig

        val level:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Int32.t) RPC_service.t

        val protocol_level:
          ([ `GET ], prefix,
           prefix, unit, unit,
           int) RPC_service.t

        val predecessor:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Block_hash.t) RPC_service.t

        val timestamp:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Time.t) RPC_service.t

        val validation_passes:
          ([ `GET ], prefix,
           prefix, unit, unit,
           int) RPC_service.t

        val operations_hash:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Operation_list_list_hash.t) RPC_service.t

        val fitness:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Fitness.t) RPC_service.t

        val context_hash:
          ([ `GET ], prefix,
           prefix, unit, unit,
           Context_hash.t) RPC_service.t

      end

    end

    module Metadata : sig

      val metadata:
        ([ `GET ], prefix,
         prefix, unit, unit,
         block_metadata) RPC_service.t

      val protocol_data:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Proto.block_header_metadata) RPC_service.t

      val protocol_hash:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Protocol_hash.t) RPC_service.t

      val next_protocol_hash:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Protocol_hash.t) RPC_service.t

      val test_chain_status:
        ([ `GET ], prefix,
         prefix, unit, unit,
         Test_chain_status.t) RPC_service.t

      val max_operations_ttl:
        ([ `GET ], prefix,
         prefix, unit, unit,
         int) RPC_service.t

      val max_operation_data_length:
        ([ `GET ], prefix,
         prefix, unit, unit,
         int) RPC_service.t

      val max_block_header_length:
        ([ `GET ], prefix,
         prefix, unit, unit,
         int) RPC_service.t

      val operation_list_quota:
        ([ `GET ], prefix,
         prefix, unit, unit,
         operation_list_quota list) RPC_service.t

    end

    module Operation : sig

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

    module Operation_hash : sig

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
                     timestamp : Time.t option >, block_param,
           Block_header.shell_header * error Preapply_result.t list) RPC_service.t

        val operations:
          ([ `POST ], prefix,
           prefix, unit, Next_proto.operation list,
           Next_proto.operation_metadata list) RPC_service.t

      end

      val complete:
        ([ `GET ], prefix,
         prefix * string, unit, unit,
         string list) RPC_service.t

    end

  end

end

module Fake_protocol : PROTO
module Empty : (module type of Make(Fake_protocol)(Fake_protocol))
