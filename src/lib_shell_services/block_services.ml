(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

type chain = [
  | `Main
  | `Test
  | `Hash of Chain_id.t
]

let parse_chain s =
  try
    match s with
    | "main" -> Ok `Main
    | "test" -> Ok `Test
    | h -> Ok (`Hash (Chain_id.of_b58check_exn h))
  with _ -> Error "Cannot parse block identifier."

let chain_to_string = function
  | `Main -> "main"
  | `Test -> "test"
  | `Hash h -> Chain_id.to_b58check h

let chain_arg =
  let name = "chain_id" in
  let descr =
    "A chain identifier. This is either a chain hash in Base58Check notation \
     or a one the predefined aliases: 'main', 'test'." in
  let construct = chain_to_string in
  let destruct = parse_chain in
  RPC_arg.make ~name ~descr ~construct ~destruct ()

type block = [
  | `Genesis
  | `Head of int
  | `Hash of Block_hash.t * int
]

let parse_block s =
  try
    match String.split '~' s with
    | ["genesis"] -> Ok `Genesis
    | ["head"] -> Ok (`Head 0)
    | ["head"; n] -> Ok (`Head (int_of_string n))
    | [h] -> Ok (`Hash (Block_hash.of_b58check_exn h, 0))
    | [h ; n] -> Ok (`Hash (Block_hash.of_b58check_exn h, int_of_string n))
    | _ -> raise Exit
  with _ -> Error "Cannot parse block identifier."

let to_string = function
  | `Genesis -> "genesis"
  | `Head 0 -> "head"
  | `Head n -> Printf.sprintf "head~%d" n
  | `Hash (h, 0) -> Block_hash.to_b58check h
  | `Hash (h, n) -> Printf.sprintf "%s~%d" (Block_hash.to_b58check h) n

let blocks_arg =
  let name = "block_id" in
  let descr =
    "A block identifier. This is either a block hash in Base58Check notation \
     or a one the predefined aliases: 'genesis', 'head'. \
     One might alse use 'head~N' or '<hash>~N' where N is an integer to \
     denotes the Nth predecessors of the designated block." in
  let construct = to_string in
  let destruct = parse_block in
  RPC_arg.make ~name ~descr ~construct ~destruct ()

type chain_prefix = unit * chain
type prefix = chain_prefix * block
let chain_path = RPC_path.(root / "chains" /: chain_arg)
let dir_path : (chain_prefix, chain_prefix) RPC_path.t =
  RPC_path.(open_root / "blocks")
let path = RPC_path.(dir_path /: blocks_arg)

type operation_list_quota = {
  max_size: int ;
  max_op: int option ;
}

let operation_list_quota_encoding =
  conv
    (fun { max_size ; max_op } -> (max_size, max_op))
    (fun (max_size, max_op) -> { max_size ; max_op })
    (obj2
       (req "max_size" int31)
       (opt "max_op" int31))

type raw_context =
  | Key of MBytes.t
  | Dir of (string * raw_context) list
  | Cut

let rec pp_raw_context ppf = function
  | Cut -> Format.fprintf ppf "..."
  | Key v -> Hex.pp ppf (MBytes.to_hex v)
  | Dir l ->
      Format.fprintf ppf "{@[<v 1>@,%a@]@,}"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_cut
           (fun ppf (s, t) -> Format.fprintf ppf "%s : %a" s pp_raw_context t))
        l

let raw_context_encoding =
  mu "raw_context"
    (fun encoding ->
       union [
         case (Tag 0) bytes
           (function Key k -> Some k | _ -> None)
           (fun k -> Key k) ;
         case (Tag 1) (assoc encoding)
           (function Dir k -> Some k | _ -> None)
           (fun k -> Dir k) ;
         case (Tag 2) null
           (function Cut -> Some () | _ -> None)
           (fun () -> Cut) ;
       ])

type error +=
  | Invalid_depth_arg of (string list * int)
  | Missing_key of string list

let () =
  register_error_kind
    `Permanent
    ~id:"raw_context.missing_key"
    ~title:"...FIXME..."
    ~description:"...FIXME..."
    ~pp:(fun ppf path ->
        Format.fprintf ppf "Missing key: %s" (String.concat "/" path))
    Data_encoding.(obj1 (req "path" (list string)))
    (function Missing_key path -> Some path | _ -> None)
    (fun path -> Missing_key path)

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

module Make(Proto : PROTO)(Next_proto : PROTO) = struct

  let protocol_hash = Protocol_hash.to_b58check Proto.hash
  let next_protocol_hash = Protocol_hash.to_b58check Next_proto.hash

  type raw_block_header = {
    shell: Block_header.shell_header ;
    protocol_data: Proto.block_header_data ;
  }

  let raw_block_header_encoding =
    conv
      (fun { shell ; protocol_data } -> (shell, protocol_data))
      (fun (shell, protocol_data) -> { shell ; protocol_data } )
      (merge_objs
         Block_header.shell_header_encoding
         Proto.block_header_data_encoding)

  type block_header = {
    chain_id: Chain_id.t ;
    hash: Block_hash.t ;
    shell: Block_header.shell_header ;
    protocol_data: Proto.block_header_data ;
  }

  let block_header_encoding =
    conv
      (fun { chain_id ; hash ; shell ; protocol_data } ->
         (((), chain_id, hash), { shell ; protocol_data }))
      (fun (((), chain_id, hash), { shell ; protocol_data }) ->
         { chain_id ; hash ; shell ; protocol_data } )
      (merge_objs
         (obj3
            (req "protocol" (constant protocol_hash))
            (req "chain_id" Chain_id.encoding)
            (req "hash" Block_hash.encoding))
         raw_block_header_encoding)

  type block_metadata = {
    protocol_data: Proto.block_header_metadata ;
    test_chain_status: Test_chain_status.t ;
    (* for the next block: *)
    max_operations_ttl: int ;
    max_operation_data_length: int ;
    max_block_header_length: int ;
    operation_list_quota: operation_list_quota list ;
  }

  let block_metadata_encoding =
    conv
      (fun { protocol_data ; test_chain_status ; max_operations_ttl ;
             max_operation_data_length ; max_block_header_length ;
             operation_list_quota } ->
        (((), (), test_chain_status,
          max_operations_ttl, max_operation_data_length,
          max_block_header_length, operation_list_quota),
         protocol_data))
      (fun (((), (), test_chain_status,
             max_operations_ttl, max_operation_data_length,
             max_block_header_length, operation_list_quota),
            protocol_data) ->
        { protocol_data ; test_chain_status ; max_operations_ttl ;
          max_operation_data_length ; max_block_header_length ;
          operation_list_quota })
      (merge_objs
         (obj7
            (req "protocol" (constant protocol_hash))
            (req "next_protocol" (constant next_protocol_hash))
            (req "test_chain_status" Test_chain_status.encoding)
            (req "max_operations_ttl" int31)
            (req "max_operation_data_length" int31)
            (req "max_block_header_length" int31)
            (req "max_operation_list_length"
               (dynamic_size (list operation_list_quota_encoding))))
         Proto.block_header_metadata_encoding)

  let next_operation_encoding =
    let open Data_encoding in
    def "next_operation" @@
    conv
      (fun Next_proto.{ shell ; protocol_data } -> ((), (shell, protocol_data)))
      (fun ((), (shell, protocol_data)) -> { shell ; protocol_data } )
      (merge_objs
         (obj1 (req "protocol" (constant next_protocol_hash)))
         (merge_objs
            (dynamic_size Operation.shell_header_encoding)
            (dynamic_size Next_proto.operation_data_encoding)))

  type operation = {
    chain_id: Chain_id.t ;
    hash: Operation_hash.t ;
    shell: Operation.shell_header ;
    protocol_data: Proto.operation_data ;
    metadata: Proto.operation_metadata ;
  }

  let operation_encoding =
    let open Data_encoding in
    conv
      (fun { chain_id ; hash ; shell ; protocol_data ; metadata } ->
         (((), chain_id, hash), ((shell, protocol_data), metadata)))
      (fun (((), chain_id, hash), ((shell, protocol_data), metadata)) ->
         { chain_id ; hash ; shell ; protocol_data ; metadata } )
      (merge_objs
         (obj3
            (req "protocol" (constant protocol_hash))
            (req "chain_id" Chain_id.encoding)
            (req "hash" Operation_hash.encoding))
         (merge_objs
            (dynamic_size
               (merge_objs
                  Operation.shell_header_encoding
                  Proto.operation_data_encoding))
            (dynamic_size Proto.operation_metadata_encoding)))

  type block_info = {
    chain_id: Chain_id.t ;
    hash: Block_hash.t ;
    header: raw_block_header ;
    metadata: block_metadata ;
    operations: operation list list ;
  }

  let block_info_encoding =
    conv
      (fun { chain_id ; hash ; header ; metadata ; operations } ->
         ((((), chain_id, hash), (header, metadata)), operations))
      (fun ((((), chain_id, hash), (header, metadata)), operations) ->
         { chain_id ; hash ; header ; metadata ; operations })
      (merge_objs
         (merge_objs
            (obj3
               (req "protocol" (constant protocol_hash))
               (req "chain_id" Chain_id.encoding)
               (req "hash" Block_hash.encoding))
            (merge_objs
               (dynamic_size raw_block_header_encoding)
               (dynamic_size block_metadata_encoding)))
         (obj1 (req "operations"
                  (list (dynamic_size (list operation_encoding))))))

  module S = struct

    let path : prefix RPC_path.context = RPC_path.open_root

    let hash =
      RPC_service.get_service
        ~description:"The block's hash, its unique identifier."
        ~query: RPC_query.empty
        ~output: Block_hash.encoding
        RPC_path.(path / "hash")

    module Header = struct

      let path = RPC_path.(path / "header")

      let header =
        RPC_service.get_service
          ~description:"The whole block header."
          ~query: RPC_query.empty
          ~output: block_header_encoding
          path

      let shell_header =
        RPC_service.get_service
          ~description:"The shell-specific fragment of the block header."
          ~query: RPC_query.empty
          ~output: Block_header.shell_header_encoding
          RPC_path.(path / "shell")

      let protocol_data =
        RPC_service.get_service
          ~description:"The version-specific fragment of the block header."
          ~query: RPC_query.empty
          ~output:
            (conv
               (fun h -> ((), h)) (fun ((), h) -> h)
               (merge_objs
                  (obj1 (req "protocol" (constant protocol_hash)))
                  Proto.block_header_data_encoding))
          RPC_path.(path / "protocol_data")

      module Shell = struct

        let path = RPC_path.(path / "shell")

        let level =
          RPC_service.get_service
            ~description:"The block's level."
            ~query: RPC_query.empty
            ~output: int32
            RPC_path.(path / "level")

        let protocol_level =
          RPC_service.get_service
            ~description:"The block's protocol level (modulo 256)."
            ~query: RPC_query.empty
            ~output: uint8
            RPC_path.(path / "proto_level")

        let predecessor =
          RPC_service.get_service
            ~description:"The previous block's id."
            ~query: RPC_query.empty
            ~output: Block_hash.encoding
            RPC_path.(path / "predecessor")

        let timestamp =
          RPC_service.get_service
            ~description:"The block's timestamp."
            ~query: RPC_query.empty
            ~output: Time.encoding
            RPC_path.(path / "timestamp")

        let validation_passes =
          RPC_service.get_service
            ~description:"The number of validation passes for the block."
            ~query: RPC_query.empty
            ~output: uint8
            RPC_path.(path / "validation_passes")

        let operations_hash =
          RPC_service.get_service
            ~description:"The hash of merkle tree of the operations included in the block."
            ~query: RPC_query.empty
            ~output: Operation_list_list_hash.encoding
            RPC_path.(path / "operations_hash")

        let fitness =
          RPC_service.get_service
            ~description:"The block's fitness."
            ~query: RPC_query.empty
            ~output: Fitness.encoding
            RPC_path.(path / "fitness")

        let context_hash =
          RPC_service.get_service
            ~description:"The hash of the resulting validation context."
            ~query: RPC_query.empty
            ~output: Context_hash.encoding
            RPC_path.(path / "context_hash")

      end

    end

    module Metadata = struct

      let path = RPC_path.(path / "metadata")

      let metadata =
        RPC_service.get_service
          ~description:"All the metadata associated to the block."
          ~query: RPC_query.empty
          ~output: block_metadata_encoding
          path

      let protocol_data =
        RPC_service.get_service
          ~description:"The protocol-specific metadata associated to the block."
          ~query: RPC_query.empty
          ~output:
            (conv
               (fun h -> ((), h)) (fun ((), h) -> h)
               (merge_objs
                  (obj1 (req "protocol" (constant protocol_hash)))
                  Proto.block_header_metadata_encoding))
          RPC_path.(path / "protocol_data")

      let protocol_hash =
        RPC_service.get_service
          ~description:"The protocol used to bake this block."
          ~query: RPC_query.empty
          ~output: Protocol_hash.encoding
          RPC_path.(path / "protocol_hash")

      let next_protocol_hash =
        RPC_service.get_service
          ~description:"The protocol required to bake the next block."
          ~query: RPC_query.empty
          ~output: Protocol_hash.encoding
          RPC_path.(path / "next_protocol_hash")

      let test_chain_status =
        RPC_service.get_service
          ~description:"The status of the associated test chain."
          ~query: RPC_query.empty
          ~output: Test_chain_status.encoding
          RPC_path.(path / "test_chain_status")

      let max_operations_ttl =
        RPC_service.get_service
          ~description:"... FIXME ..."
          ~query: RPC_query.empty
          ~output: int31
          RPC_path.(path / "max_operations_ttl")

      let max_operation_data_length =
        RPC_service.get_service
          ~description:"... FIXME ..."
          ~query: RPC_query.empty
          ~output: int31
          RPC_path.(path / "max_operation_data_length")

      let max_block_header_length =
        RPC_service.get_service
          ~description:"... FIXME ..."
          ~query: RPC_query.empty
          ~output: int31
          RPC_path.(path / "max_block_header_length")

      let operation_list_quota =
        RPC_service.get_service
          ~description:"... FIXME ..."
          ~query: RPC_query.empty
          ~output: (list operation_list_quota_encoding)
          RPC_path.(path / "operation_list_quota")

    end

    module Operation = struct

      let path = RPC_path.(path / "operations")

      let operations =
        RPC_service.get_service
          ~description:"All the operations included in the block."
          ~query: RPC_query.empty
          ~output: (list (dynamic_size (list operation_encoding)))
          path

      let list_arg =
        let name = "list_offset" in
        let descr =
          "..." in
        let construct = string_of_int in
        let destruct s =
          try Ok (int_of_string s)
          with _ -> Error (Format.sprintf "Invalid list offset (%s)" s) in
        RPC_arg.make ~name ~descr ~construct ~destruct ()

      let offset_arg =
        let name = "operation_offset" in
        let descr =
          "..." in
        let construct = string_of_int in
        let destruct s =
          try Ok (int_of_string s)
          with _ -> Error (Format.sprintf "Invalid operation offset (%s)" s) in
        RPC_arg.make ~name ~descr ~construct ~destruct ()

      let operations_in_pass =
        RPC_service.get_service
          ~description:
            "All the operations included in `n-th` validation pass of the block."
          ~query: RPC_query.empty
          ~output: (list operation_encoding)
          RPC_path.(path /: list_arg)

      let operation =
        RPC_service.get_service
          ~description:
            "The `m-th` operation in the `n-th` validation pass of the block."
          ~query: RPC_query.empty
          ~output: operation_encoding
          RPC_path.(path /: list_arg /: offset_arg)

    end

    module Operation_hash = struct

      let path = RPC_path.(path / "operation_hashes")

      let operation_hashes =
        RPC_service.get_service
          ~description:"The hashes of all the operations included in the block."
          ~query: RPC_query.empty
          ~output: (list (list Operation_hash.encoding))
          path

      let operation_hashes_in_pass =
        RPC_service.get_service
          ~description:
            "All the operations included in `n-th` validation pass of the block."
          ~query: RPC_query.empty
          ~output: (list Operation_hash.encoding)
          RPC_path.(path /: Operation.list_arg)

      let operation_hash =
        RPC_service.get_service
          ~description:
            "The hash of then `m-th` operation in the `n-th` validation pass of the block."
          ~query: RPC_query.empty
          ~output: Operation_hash.encoding
          RPC_path.(path /: Operation.list_arg /: Operation.offset_arg)
    end

    module Helpers = struct

      let path = RPC_path.(path / "context" / "helpers")

      module Forge = struct

        let block_header =
          RPC_service.post_service
            ~description: "Forge a block header"
            ~query: RPC_query.empty
            ~input: Block_header.encoding
            ~output: (obj1 (req "block" bytes))
            RPC_path.(path / "forge_block_header")

      end

      module Preapply = struct

        let path = RPC_path.(path / "preapply")

        let block_result_encoding =
          obj2
            (req "shell_header" Block_header.shell_header_encoding)
            (req "operations"
               (list (Preapply_result.encoding RPC_error.encoding)))

        type block_param = {
          protocol_data: Next_proto.block_header_data ;
          operations: Next_proto.operation list list ;
        }

        let block_param_encoding =
          (conv
             (fun { protocol_data ; operations } ->
                (protocol_data, operations))
             (fun (protocol_data, operations) ->
                { protocol_data ; operations })
             (obj2
                (req "protocol_data"
                   (conv
                      (fun h -> ((), h)) (fun ((), h) -> h)
                      (merge_objs
                         (obj1 (req "protocol" (constant next_protocol_hash)))
                         (dynamic_size Next_proto.block_header_data_encoding))))
                (req "operations"
                   (list (dynamic_size (list next_operation_encoding))))))

        let block_query =
          let open RPC_query in
          query (fun sort timestamp -> object
                  method sort_operations = sort
                  method timestamp = timestamp
                end)
          |+ flag "sort" (fun t -> t#sort_operations)
          |+ opt_field "timestamp" Time.rpc_arg (fun t -> t#timestamp)
          |> seal

        let block =
          RPC_service.post_service
            ~description:
              "Simulate the validation of a block that would contain \
               the given operations and return the resulting fitness \
               and context hash."
            ~query: block_query
            ~input: block_param_encoding
            ~output: block_result_encoding
            RPC_path.(path / "block")

        let operations =
          RPC_service.post_service
            ~description:
              "Simulate the validation of an operation."
            ~query: RPC_query.empty
            ~input: (list next_operation_encoding)
            ~output: (list (dynamic_size Next_proto.operation_metadata_encoding))
            RPC_path.(path / "operations")

      end

      let complete =
        let prefix_arg =
          let destruct s = Ok s
          and construct s = s in
          RPC_arg.make ~name:"prefix" ~destruct ~construct () in
        RPC_service.get_service
          ~description: "Try to complete a prefix of a Base58Check-encoded data. \
                         This RPC is actually able to complete hashes of \
                         block, operations, public_keys and contracts."
          ~query: RPC_query.empty
          ~output: (list string)
          RPC_path.(path / "complete" /: prefix_arg )

    end

    module Context = struct

      let path = RPC_path.(path / "context")

      module Raw = struct

        let path = RPC_path.(path / "raw")

        let context_path_arg : string RPC_arg.t =
          let name = "context_path" in
          let descr = "A path inside the context" in
          let construct = fun s -> s in
          let destruct = fun s -> Ok s in
          RPC_arg.make ~name ~descr ~construct ~destruct ()

        let raw_context_query : < depth: int option > RPC_query.t =
          let open RPC_query in
          query (fun depth -> object
                  method depth = depth
                end)
          |+ opt_field "depth" RPC_arg.int (fun t -> t#depth)
          |> seal

        let read =
          RPC_service.get_service
            ~description:"Returns the raw context."
            ~query: raw_context_query
            ~output: raw_context_encoding
            RPC_path.(path /:* context_path_arg)

      end

    end

    let info =
      RPC_service.get_service
        ~description:"All the information about a block."
        ~query: RPC_query.empty
        ~output: block_info_encoding
        path

  end

  let path = RPC_path.prefix chain_path path

  let make_call0 s ctxt a b q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call2 s ctxt a b q p

  let make_call1 s ctxt a b c q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call3 s ctxt a b c q p

  let make_call2 s ctxt a b c d q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call s ctxt (((((), a), b), c), d) q p

  let hash ctxt =
    let f = make_call0 S.hash ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () ->
      match block with
      | `Hash (h, 0) -> return h
      | _ -> f chain block () ()

  module Header = struct

    module S = S.Header

    let header ctxt =
      let f = make_call0 S.header ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()
    let shell_header ctxt =
      let f = make_call0 S.shell_header ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()
    let protocol_data ctxt =
      let f = make_call0 S.protocol_data ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    module Shell = struct

      module S = S.Shell

      let level ctxt =
        let f = make_call0 S.level ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let protocol_level ctxt =
        let f = make_call0 S.protocol_level ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let predecessor ctxt =
        let f = make_call0 S.predecessor ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let timestamp ctxt =
        let f = make_call0 S.timestamp ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let validation_passes ctxt =
        let f = make_call0 S.validation_passes ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let operations_hash ctxt =
        let f = make_call0 S.operations_hash ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let fitness ctxt =
        let f = make_call0 S.fitness ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

      let context_hash ctxt =
        let f = make_call0 S.context_hash ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) () ->
          f chain block () ()

    end

  end

  module Metadata = struct

    module S = S.Metadata

    let metadata ctxt =
      let f = make_call0 S.metadata ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let protocol_data ctxt =
      let f = make_call0 S.protocol_data ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let protocol_hash ctxt =
      let f = make_call0 S.protocol_hash ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let next_protocol_hash ctxt =
      let f = make_call0 S.next_protocol_hash ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let test_chain_status ctxt =
      let f = make_call0 S.test_chain_status ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let max_operations_ttl ctxt =
      let f = make_call0 S.max_operations_ttl ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let max_operation_data_length ctxt =
      let f = make_call0 S.max_operation_data_length ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let max_block_header_length ctxt =
      let f = make_call0 S.max_block_header_length ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let max_operation_list_length ctxt =
      let f = make_call0 S.operation_list_quota ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

  end

  module Operation = struct

    module S = S.Operation

    let operations ctxt =
      let f = make_call0 S.operations ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let operations_in_pass ctxt =
      let f = make_call1 S.operations_in_pass ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n ->
        f chain block n () ()

    let operation ctxt =
      let f = make_call2 S.operation ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n m ->
        f chain block n m () ()

  end

  module Operation_hash = struct

    module S = S.Operation_hash

    let operation_hashes ctxt =
      let f = make_call0 S.operation_hashes ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () ->
        f chain block () ()

    let operation_hashes_in_pass ctxt =
      let f = make_call1 S.operation_hashes_in_pass ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n ->
        f chain block n () ()

    let operation_hash ctxt =
      let f = make_call2 S.operation_hash ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n m ->
        f chain block n m () ()

  end

  module Context = struct

    module S = S.Context

    module Raw = struct

      module S = S.Raw

      let read ctxt =
        let f = make_call1 S.read ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) ?depth path ->
          f chain block path
            (object method depth = depth end) ()

    end

  end

  module Helpers = struct

    module S = S.Helpers

    module Forge = struct

      module S = S.Forge

      let block_header ctxt =
        let f = make_call0 S.block_header ctxt in
        fun
          ?(chain = `Main) ?(block = `Head 0)
          header ->
          f chain block () header

    end

    module Preapply = struct

      module S = S.Preapply

      let block ctxt =
        let f = make_call0 S.block ctxt in
        fun
          ?(chain = `Main) ?(block = `Head 0)
          ?(sort = false) ?timestamp ~protocol_data operations ->
          f chain block
            (object method sort_operations = sort method timestamp = timestamp end)
            { protocol_data ; operations  }

      let operations ctxt =
        let f = make_call0 S.operations ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) operations ->
          f chain block () operations

    end

    let complete ctxt =
      let f = make_call1 S.complete ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) s ->
        f chain block s () ()

  end

  let info ctxt =
    let f = make_call0 S.info ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () ->
      f chain block () ()

end

module Fake_protocol = struct
  let hash = Protocol_hash.zero
  type block_header_data = unit
  let block_header_data_encoding = Data_encoding.empty
  type block_header_metadata = unit
  let block_header_metadata_encoding = Data_encoding.empty
  type operation_data = unit
  let operation_data_encoding = Data_encoding.empty
  type operation_metadata = unit
  let operation_metadata_encoding = Data_encoding.empty
  type operation = {
    shell: Operation.shell_header ;
    protocol_data: operation_data ;
  }
end

module Empty = Make(Fake_protocol)(Fake_protocol)
