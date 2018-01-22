(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

module Blocks = struct

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
  ]

  type block_info = {
    hash: Block_hash.t ;
    net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ; (* uint8 *)
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
    context: Context_hash.t ;
    data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_network: Test_network_status.t ;
  }

  let block_info_encoding =
    let operation_encoding =
      merge_objs
        (obj1 (req "hash" Operation_hash.encoding))
        Operation.encoding in
    conv
      (fun { hash ; net_id ; level ; proto_level ; predecessor ;
             fitness ; timestamp ; protocol ;
             validation_passes ; operations_hash ; context ; data ;
             operations ; test_network } ->
        ((hash, net_id, operations, protocol, test_network),
         { Block_header.shell =
             { level ; proto_level ; predecessor ;
               timestamp ; validation_passes ; operations_hash ; fitness ;
               context } ;
           proto = data }))
      (fun ((hash, net_id, operations, protocol, test_network),
            { Block_header.shell =
                { level ; proto_level ; predecessor ;
                  timestamp ; validation_passes ; operations_hash ; fitness ;
                  context } ;
              proto = data }) ->
        { hash ; net_id ; level ; proto_level ; predecessor ;
          fitness ; timestamp ; protocol ;
          validation_passes ; operations_hash ; context ; data ;
          operations ; test_network })
      (dynamic_size
         (merge_objs
            (obj5
               (req "hash" Block_hash.encoding)
               (req "net_id" Net_id.encoding)
               (opt "operations" (dynamic_size (list (dynamic_size (list (dynamic_size operation_encoding))))))
               (req "protocol" Protocol_hash.encoding)
               (dft "test_network"
                  Test_network_status.encoding Not_running))
            Block_header.encoding))

  let parse_block s =
    try
      match String.split '~' s with
      | ["genesis"] -> Ok `Genesis
      | ["head"] -> Ok (`Head 0)
      | ["prevalidation"] -> Ok `Prevalidation
      | ["test_head"] -> Ok (`Test_head 0)
      | ["test_prevalidation"] -> Ok `Test_prevalidation
      | ["head"; n] -> Ok (`Head (int_of_string n))
      | ["test_head"; n] -> Ok (`Test_head (int_of_string n))
      | [h] -> Ok (`Hash (Block_hash.of_b58check_exn h))
      | _ -> raise Exit
    with _ -> Error "Cannot parse block identifier."

  let to_string = function
    | `Genesis -> "genesis"
    | `Head 0 -> "head"
    | `Head n -> Printf.sprintf "head~%d" n
    | `Prevalidation -> "prevalidation"
    | `Test_head 0 -> "test_head"
    | `Test_head n -> Printf.sprintf "test_head~%d" n
    | `Test_prevalidation -> "test_prevalidation"
    | `Hash h -> Block_hash.to_b58check h

  let blocks_arg =
    let name = "block_id" in
    let descr =
      "A block identifier. This is either a block hash in hexadecimal \
       notation or a one the predefined aliases: \
       'genesis', 'head', 'prevalidation', \
       'test_head' or 'test_prevalidation'. One might alse use 'head~N'
       to 'test_head~N', where N is an integer to denotes the Nth predecessors
       of 'head' or 'test_head'." in
    let construct = to_string in
    let destruct = parse_block in
    RPC_arg.make ~name ~descr ~construct ~destruct ()

  let block_path : (unit, unit * block) RPC_path.path =
    RPC_path.(root / "blocks" /: blocks_arg )

  let info =
    RPC_service.post_service
      ~description:"All the information about a block."
      ~query: RPC_query.empty
      ~input: (obj1 (dft "operations" bool true))
      ~output: block_info_encoding
      ~error: Data_encoding.empty
      block_path

  let net_id =
    RPC_service.post_service
      ~description:"Returns the net of the chain in which the block belongs."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "net_id" Net_id.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "net_id")

  let level =
    RPC_service.post_service
      ~description:"Returns the block's level."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "level" int32))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "level")

  let predecessor =
    RPC_service.post_service
      ~description:"Returns the previous block's id."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "predecessor" Block_hash.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "predecessor")

  let predecessors =
    RPC_service.post_service
      ~description:
        "...."
      ~query: RPC_query.empty
      ~input: (obj1 (req "length" Data_encoding.uint16))
      ~output: (obj1
                  (req "blocks" (Data_encoding.list Block_hash.encoding)))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "predecessors")

  let hash =
    RPC_service.post_service
      ~description:"Returns the block's id."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "hash" Block_hash.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "hash")

  let fitness =
    RPC_service.post_service
      ~description:"Returns the block's fitness."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "fitness" Fitness.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "fitness")

  let context =
    RPC_service.post_service
      ~description:"Returns the hash of the resulting context."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "context" Context_hash.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "context")

  let timestamp =
    RPC_service.post_service
      ~description:"Returns the block's timestamp."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "timestamp" Time.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "timestamp")

  type operations_param = {
    contents: bool ;
    monitor: bool ;
  }

  let operations_param_encoding =
    let open Data_encoding in
    conv
      (fun { contents ; monitor } -> (contents, monitor))
      (fun (contents, monitor) -> { contents ; monitor })
      (obj2
         (dft "contents" bool false)
         (dft "monitor" bool false))

  let operations =
    RPC_service.post_service
      ~description:"List the block operations."
      ~query: RPC_query.empty
      ~input: operations_param_encoding
      ~output: (obj1
                  (req "operations"
                     (list (list
                              (obj2
                                 (req "hash" Operation_hash.encoding)
                                 (opt "contents"
                                    (dynamic_size Operation.encoding)))))))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "operations")

  let protocol =
    RPC_service.post_service
      ~description:"List the block protocol."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "protocol" Protocol_hash.encoding))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "protocol")

  let test_network =
    RPC_service.post_service
      ~description:"Returns the status of the associated test network."
      ~query: RPC_query.empty
      ~input: empty
      ~output: Test_network_status.encoding
      ~error: Data_encoding.empty
      RPC_path.(block_path / "test_network")

  let pending_operations =
    let operation_encoding =
      merge_objs
        (obj1 (req "hash" Operation_hash.encoding))
        Operation.encoding in
    (* TODO: branch_delayed/... *)
    RPC_service.post_service
      ~description:
        "List the not-yet-prevalidated operations."
      ~query: RPC_query.empty
      ~input: empty
      ~output:
        (conv
           (fun (preapplied, unprocessed) ->
              ({ preapplied with
                 Preapply_result.refused = Operation_hash.Map.empty },
               Operation_hash.Map.bindings unprocessed))
           (fun (preapplied, unprocessed) ->
              (preapplied,
               List.fold_right
                 (fun (h, op) m -> Operation_hash.Map.add h op m)
                 unprocessed Operation_hash.Map.empty))
           (merge_objs
              (dynamic_size
                 (Preapply_result.encoding RPC_error.encoding))
              (obj1 (req "unprocessed" (list (dynamic_size operation_encoding))))))
      ~error: Data_encoding.empty
      RPC_path.(block_path / "pending_operations")

  let proto_path =
    RPC_path.(block_path / "proto")

  type preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: Operation.t list list ;
    sort_operations: bool ;
  }

  let preapply_param_encoding =
    (conv
       (fun { timestamp ; proto_header ; operations ; sort_operations } ->
          (timestamp, proto_header, operations, sort_operations))
       (fun (timestamp, proto_header, operations, sort_operations) ->
          { timestamp ; proto_header ; operations ; sort_operations })
       (obj4
          (req "timestamp" Time.encoding)
          (req "proto_header" bytes)
          (req "operations" (list (dynamic_size (list (dynamic_size Operation.encoding)))))
          (dft "sort_operations" bool false)))

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Preapply_result.t list ;
  }

  let preapply_result_encoding =
    (conv
       (fun { shell_header ; operations } ->
          (shell_header, operations))
       (fun (shell_header, operations) ->
          { shell_header ; operations })
       (obj2
          (req "shell_header" Block_header.shell_header_encoding)
          (req "operations"
             (list (Preapply_result.encoding RPC_error.encoding)))))

  let preapply =
    RPC_service.post_service
      ~description:
        "Simulate the validation of a block that would contain \
         the given operations and return the resulting fitness."
      ~query: RPC_query.empty
      ~input: preapply_param_encoding
      ~output: (RPC_error.wrap preapply_result_encoding)
      ~error: Data_encoding.empty
      RPC_path.(block_path / "preapply")

  let complete =
    let prefix_arg =
      let destruct s = Ok s
      and construct s = s in
      RPC_arg.make ~name:"prefix" ~destruct ~construct () in
    RPC_service.post_service
      ~description: "Try to complete a prefix of a Base58Check-encoded data. \
                     This RPC is actually able to complete hashes of \
                     block, operations, public_keys and contracts."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (list string)
      ~error: Data_encoding.empty
      RPC_path.(block_path / "complete" /: prefix_arg )

  type list_param = {
    include_ops: bool ;
    length: int option ;
    heads: Block_hash.t list option ;
    monitor: bool option ;
    delay: int option ;
    min_date: Time.t option;
    min_heads: int option;
  }
  let list_param_encoding =
    conv
      (fun { include_ops ; length ; heads ; monitor ;
             delay ; min_date ; min_heads } ->
        (include_ops, length, heads, monitor, delay, min_date, min_heads))
      (fun (include_ops, length, heads, monitor,
            delay, min_date, min_heads) ->
        { include_ops ; length ; heads ; monitor ;
          delay ; min_date ; min_heads })
      (obj7
         (dft "include_ops"
            (Data_encoding.describe
               ~description:
                 "Whether the resulting block informations should include the \
                  list of operations' hashes. Default false."
               bool) false)
         (opt "length"
            (Data_encoding.describe
               ~description:
                 "The requested number of predecessors to returns (per \
                  requested head)."
               int31))
         (opt "heads"
            (Data_encoding.describe
               ~description:
                 "An empty argument requests blocks from the current heads. \
                  A non empty list allow to request specific fragment \
                  of the chain."
               (list Block_hash.encoding)))
         (opt "monitor"
            (Data_encoding.describe
               ~description:
                 "When true, the socket is \"kept alive\" after the first \
                  answer and new heads are streamed when discovered."
               bool))
         (opt "delay"
            (Data_encoding.describe
               ~description:
                 "By default only the blocks that were validated by the node \
                  are considered. \
                  When this optional argument is 0, only blocks with a \
                  timestamp in the past are considered. Other values allows to \
                  adjust the current time."
               int31))
         (opt "min_date"
            (Data_encoding.describe
               ~description: "When `min_date` is provided, heads with a \
                              timestamp before `min_date` are filtered ouf"
               Time.encoding))
         (opt "min_heads"
            (Data_encoding.describe
               ~description:"When `min_date` is provided, returns at least \
                             `min_heads` even when their timestamp is before \
                             `min_date`."
               int31)))

  let list =
    RPC_service.post_service
      ~description:
        "Lists known heads of the blockchain sorted with decreasing fitness. \
         Optional arguments allows to returns the list of predecessors for \
         known heads or the list of predecessors for a given list of blocks."
      ~query: RPC_query.empty
      ~input: list_param_encoding
      ~output: (obj1 (req "blocks" (list (list block_info_encoding))))
      ~error: Data_encoding.empty
      RPC_path.(root / "blocks")

  let list_invalid =
    RPC_service.post_service
      ~description:
        "Lists blocks that have been declared invalid along with the errors\
         that led to them being declared invalid"
      ~query: RPC_query.empty
      ~input:empty
      ~output:(Data_encoding.list
                 (obj3
                    (req "block" Block_hash.encoding)
                    (req "level" int32)
                    (req "errors" RPC_error.encoding)))
      ~error: Data_encoding.empty
      RPC_path.(root / "invalid_blocks")

  let unmark_invalid =
    RPC_service.post_service
      ~description:
        "Unmark an invalid block"
      ~query: RPC_query.empty
      ~input:Data_encoding.(obj1 (req "block" Block_hash.encoding))
      ~output:(Error.wrap Data_encoding.empty)
      ~error: Data_encoding.empty
      RPC_path.(root / "unmark_invalid")

end

module Protocols = struct

  let protocols_arg = Protocol_hash.rpc_arg

  let contents =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: empty
      ~output:
        (obj1 (req "data"
                 (describe ~title: "Tezos protocol"
                    (Protocol.encoding))))
      ~error: Data_encoding.empty
      RPC_path.(root / "protocols" /: protocols_arg)

  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }

  let list_param_encoding =
    conv
      (fun {contents; monitor} -> (contents, monitor))
      (fun (contents, monitor) -> {contents; monitor})
      (obj2
         (opt "contents" bool)
         (opt "monitor" bool))

  let list =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: list_param_encoding
      ~output:
        (obj1
           (req "protocols"
              (list
                 (obj2
                    (req "hash" Protocol_hash.encoding)
                    (opt "contents"
                       (dynamic_size Protocol.encoding)))
              )))
      ~error: Data_encoding.empty
      RPC_path.(root / "protocols")

end

module Workers = struct

  module Prevalidators = struct

    let (net_id_arg : Net_id.t RPC_arg.t) =
      RPC_arg.make
        ~name:"net_id"
        ~descr:"The network identifier of whom the prevalidator is responsible."
        ~destruct:(fun s -> try
                      Ok (Net_id.of_b58check_exn s)
                    with Failure msg -> Error msg)
        ~construct:Net_id.to_b58check
        ()

    let list =
      RPC_service.post_service
        ~description:"Lists the Prevalidator workers and their status."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (list
             (obj2
                (req "net_id" Net_id.encoding)
                (req "status" (Worker_types.worker_status_encoding  RPC_error.encoding))))
        RPC_path.(root / "workers" / "prevalidators")

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a prevalidator worker."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Prevalidator_worker_state.Request.encoding
             (Prevalidator_worker_state.Event.encoding RPC_error.encoding)
             RPC_error.encoding)
        RPC_path.(root / "workers" / "prevalidators" /: net_id_arg )

  end

  module Block_validator = struct

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of the block_validator worker."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Block_validator_worker_state.Request.encoding
             (Block_validator_worker_state.Event.encoding RPC_error.encoding)
             RPC_error.encoding)
        RPC_path.(root / "workers" / "block_validator")

  end

  module Peer_validators = struct

    let (net_id_arg : Net_id.t RPC_arg.t) =
      RPC_arg.make
        ~name:"net_id"
        ~descr:"The network identifier the peer validator is associated to."
        ~destruct:(fun s -> try
                      Ok (Net_id.of_b58check_exn s)
                    with Failure msg -> Error msg)
        ~construct:Net_id.to_b58check
        ()

    let (peer_id_arg : P2p_types.Peer_id.t RPC_arg.t) =
      RPC_arg.make
        ~name:"peer_id"
        ~descr:"The peer identifier of whom the prevalidator is responsible."
        ~destruct:(fun s -> try
                      Ok (P2p_types.Peer_id.of_b58check_exn s)
                    with Failure msg -> Error msg)
        ~construct:P2p_types.Peer_id.to_b58check
        ()

    let list =
      RPC_service.post_service
        ~description:"Lists the peer validator workers and their status."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (list
             (obj2
                (req "peer_id" P2p_types.Peer_id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "peer_validators" /: net_id_arg)

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a peer validator worker."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Peer_validator_worker_state.Request.encoding
             (Peer_validator_worker_state.Event.encoding RPC_error.encoding)
             RPC_error.encoding)
        RPC_path.(root / "workers" / "peer_validators" /: net_id_arg /: peer_id_arg)

  end

  module Net_validators = struct

    let (net_id_arg : Net_id.t RPC_arg.t) =
      RPC_arg.make
        ~name:"net_id"
        ~descr:"The network identifier of whom the net validator is responsible."
        ~destruct:(fun s -> try
                      Ok (Net_id.of_b58check_exn s)
                    with Failure msg -> Error msg)
        ~construct:Net_id.to_b58check
        ()

    let list =
      RPC_service.post_service
        ~description:"Lists the net validator workers and their status."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (list
             (obj2
                (req "net_id" Net_id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "net_validators")

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a net validator worker."
        ~query: RPC_query.empty
        ~error: Data_encoding.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Net_validator_worker_state.Request.encoding
             (Net_validator_worker_state.Event.encoding RPC_error.encoding)
             RPC_error.encoding)
        RPC_path.(root / "workers" / "net_validators" /: net_id_arg )

  end

end

let forge_block_header =
  RPC_service.post_service
    ~description: "Forge a block header"
    ~query: RPC_query.empty
    ~input: Block_header.encoding
    ~output: (obj1 (req "block" bytes))
    ~error: Data_encoding.empty
    RPC_path.(root / "forge_block_header")

type inject_block_param = {
  raw: MBytes.t ;
  blocking: bool ;
  force: bool ;
  net_id: Net_id.t option ;
  operations: Operation.t list list ;
}

let inject_block_param =
  conv
    (fun { raw ; blocking ; force ; net_id ; operations } ->
       (raw, blocking, force, net_id, operations))
    (fun (raw, blocking, force, net_id, operations) ->
       { raw ; blocking ; force ; net_id ; operations })
    (obj5
       (req "data" bytes)
       (dft "blocking"
          (describe
             ~description:
               "Should the RPC wait for the block to be \
                validated before answering. (default: true)"
             bool)
          true)
       (dft "force"
          (describe
             ~description:
               "Should we inject the block when its fitness is below \
                the current head. (default: false)"
             bool)
          false)
       (opt "net_id" Net_id.encoding)
       (req "operations"
          (describe
             ~description:"..."
             (list (list (dynamic_size Operation.encoding))))))

let inject_block =
  RPC_service.post_service
    ~description:
      "Inject a block in the node and broadcast it. The `operations` \
       embedded in `blockHeader` might be pre-validated using a \
       contextual RPCs from the latest block \
       (e.g. '/blocks/head/context/preapply'). Returns the ID of the \
       block. By default, the RPC will wait for the block to be \
       validated before answering."
    ~query: RPC_query.empty
    ~input: inject_block_param
    ~output:
      (RPC_error.wrap @@
       (obj1 (req "block_hash" Block_hash.encoding)))
    ~error: Data_encoding.empty
    RPC_path.(root / "inject_block")

let inject_operation =
  RPC_service.post_service
    ~description:
      "Inject an operation in node and broadcast it. Returns the \
       ID of the operation. The `signedOperationContents` should be \
       constructed using a contextual RPCs from the latest block \
       and signed by the client. By default, the RPC will wait for \
       the operation to be (pre-)validated before answering. See \
       RPCs under /blocks/prevalidation for more details on the \
       prevalidation context."
    ~query: RPC_query.empty
    ~input:
      (obj3
         (req "signedOperationContents"
            (describe ~title: "Tezos signed operation (hex encoded)"
               bytes))
         (dft "blocking"
            (describe
               ~description:
                 "Should the RPC wait for the operation to be \
                  (pre-)validated before answering. (default: true)"
               bool)
            true)
         (opt "net_id" Net_id.encoding))
    ~output:
      (RPC_error.wrap @@
       describe
         ~title: "Hash of the injected operation" @@
       (obj1 (req "injectedOperation" Operation_hash.encoding)))
    ~error: Data_encoding.empty
    RPC_path.(root / "inject_operation")

let inject_protocol =
  RPC_service.post_service
    ~description:
      "Inject a protocol in node. Returns the ID of the protocol."
    ~query: RPC_query.empty
    ~input:
      (obj3
         (req "protocol"
            (describe ~title: "Tezos protocol" Protocol.encoding))
         (dft "blocking"
            (describe
               ~description:
                 "Should the RPC wait for the protocol to be \
                  validated before answering. (default: true)"
               bool)
            true)
         (opt "force"
            (describe
               ~description:
                 "Should we inject protocol that is invalid. (default: false)"
               bool)))
    ~output:
      (RPC_error.wrap @@
       describe
         ~title: "Hash of the injected protocol" @@
       (obj1 (req "injectedProtocol" Protocol_hash.encoding)))
    ~error: Data_encoding.empty
    RPC_path.(root / "inject_protocol")

let bootstrapped =
  RPC_service.post_service
    ~description:""
    ~query: RPC_query.empty
    ~input: empty
    ~output: (obj2
                (req "block" Block_hash.encoding)
                (req "timestamp" Time.encoding))
    ~error: Data_encoding.empty
    RPC_path.(root / "bootstrapped")

let complete =
  let prefix_arg =
    let destruct s = Ok s
    and construct s = s in
    RPC_arg.make ~name:"prefix" ~destruct ~construct () in
  RPC_service.post_service
    ~description: "Try to complete a prefix of a Base58Check-encoded data. \
                   This RPC is actually able to complete hashes of \
                   block and hashes of operations."
    ~query: RPC_query.empty
    ~input: empty
    ~output: (list string)
    ~error: Data_encoding.empty
    RPC_path.(root / "complete" /: prefix_arg )

let describe =
  RPC_service.description_service
    ~description: "RPCs documentation and input/output schema"
    RPC_path.(root / "describe")
