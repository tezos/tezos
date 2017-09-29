(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

module Error = struct

  let service =
    RPC.service
      ~description: "Schema for all the RPC errors from the shell"
      ~input: Data_encoding.empty
      ~output: Data_encoding.json_schema
      RPC.Path.(root / "errors")

  let encoding =
    let meth, path, _ = RPC.forge_request service () () in
    describe
      ~description:
        (Printf.sprintf
           "The full list of error is available with \
            the global RPC `%s /%s`"
            (RPC.string_of_method meth) (String.concat "/" path))
      (conv
         ~schema:Json_schema.any
         (fun exn -> `A (List.map json_of_error exn))
         (function `A exns -> List.map error_of_json exns | _ -> [])
         json)

  let wrap param_encoding =
    union [
      case
        (obj1 (req "ok" param_encoding))
        (function Ok x -> Some x | _ -> None)
        (fun x -> Ok x) ;
      case
        (obj1 (req "error" encoding))
        (function Error x -> Some x | _ -> None)
        (fun x -> Error x) ;
    ]

end

type operation = Distributed_db.operation =
  | Blob of Operation.t
  | Hash of Operation_hash.t

let operation_encoding =
  let open Data_encoding in
  union [
    case Operation.encoding
      (function Blob op -> Some op | Hash _ -> None)
      (fun op -> Blob op) ;
    case Operation_hash.encoding
      (function Hash oph -> Some oph | Blob _ -> None)
      (fun oph -> Hash oph) ;
  ]

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
    data: MBytes.t ;
    operations: Operation_hash.t list list option ;
    protocol: Protocol_hash.t ;
    test_network: Context.test_network;
  }

  let block_info_encoding =
    conv
      (fun { hash ; net_id ; level ; proto_level ; predecessor ;
             fitness ; timestamp ; protocol ;
             validation_passes ; operations_hash ; data ;
             operations ; test_network } ->
        ((hash, operations, protocol, test_network),
         { Block_header.shell =
             { net_id ; level ; proto_level ; predecessor ;
               timestamp ; validation_passes ; operations_hash ; fitness } ;
           proto = data }))
      (fun ((hash, operations, protocol, test_network),
            { Block_header.shell =
                { net_id ; level ; proto_level ; predecessor ;
                  timestamp ; validation_passes ; operations_hash ; fitness } ;
              proto = data }) ->
        { hash ; net_id ; level ; proto_level ; predecessor ;
          fitness ; timestamp ; protocol ;
          validation_passes ; operations_hash ; data ;
          operations ; test_network })
      (dynamic_size
         (merge_objs
            (obj4
               (req "hash" Block_hash.encoding)
               (opt "operations" (list (list Operation_hash.encoding)))
               (req "protocol" Protocol_hash.encoding)
               (dft "test_network"
                  Context.test_network_encoding Context.Not_running))
            Block_header.encoding))

  let parse_block s =
    try
      match Utils.split '~' s with
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
    RPC.Arg.make ~name ~descr ~construct ~destruct ()

  let block_path : (unit, unit * block) RPC.Path.path =
    RPC.Path.(root / "blocks" /: blocks_arg )

  let info =
    RPC.service
      ~description:"All the information about a block."
      ~input: (obj1 (dft "operations" bool true))
      ~output: block_info_encoding
      block_path

  let net_id =
    RPC.service
      ~description:"Returns the net of the chain in which the block belongs."
      ~input: empty
      ~output: (obj1 (req "net_id" Net_id.encoding))
      RPC.Path.(block_path / "net_id")

  let level =
    RPC.service
      ~description:"Returns the block's level."
      ~input: empty
      ~output: (obj1 (req "level" int32))
      RPC.Path.(block_path / "level")

  let predecessor =
    RPC.service
      ~description:"Returns the previous block's id."
      ~input: empty
      ~output: (obj1 (req "predecessor" Block_hash.encoding))
      RPC.Path.(block_path / "predecessor")

  let predecessors =
    RPC.service
      ~description:
        "...."
      ~input: (obj1 (req "length" Data_encoding.uint16))
      ~output: (obj1
                  (req "blocks" (Data_encoding.list Block_hash.encoding)))
      RPC.Path.(block_path / "predecessors")

  let hash =
    RPC.service
      ~description:"Returns the block's id."
      ~input: empty
      ~output: (obj1 (req "hash" Block_hash.encoding))
      RPC.Path.(block_path / "hash")

  let fitness =
    RPC.service
      ~description:"Returns the block's fitness."
      ~input: empty
      ~output: (obj1 (req "fitness" Fitness.encoding))
      RPC.Path.(block_path / "fitness")

  let timestamp =
    RPC.service
      ~description:"Returns the block's timestamp."
      ~input: empty
      ~output: (obj1 (req "timestamp" Time.encoding))
      RPC.Path.(block_path / "timestamp")

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
    RPC.service
      ~description:"List the block operations."
      ~input: operations_param_encoding
      ~output: (obj1
                  (req "operations"
                     (list (list
                              (obj2
                                 (req "hash" Operation_hash.encoding)
                                 (opt "contents"
                                    (dynamic_size Operation.encoding)))))))
      RPC.Path.(block_path / "operations")

  let protocol =
    RPC.service
      ~description:"List the block protocol."
      ~input: empty
      ~output: (obj1 (req "protocol" Protocol_hash.encoding))
      RPC.Path.(block_path / "protocol")

  let test_network =
    RPC.service
      ~description:"Returns the status of the associated test network."
      ~input: empty
      ~output: Context.test_network_encoding
      RPC.Path.(block_path / "test_network")

  let pending_operations =
    (* TODO: branch_delayed/... *)
    RPC.service
      ~description:
        "List the not-yet-prevalidated operations."
      ~input: empty
      ~output:
        (conv
           (fun ({ Prevalidation.applied; branch_delayed ; branch_refused },
                 unprocessed) ->
             (applied,
              Operation_hash.Map.bindings branch_delayed,
              Operation_hash.Map.bindings branch_refused,
              Operation_hash.Set.elements unprocessed))
           (fun (applied, branch_delayed, branch_refused, unprocessed) ->
              ({ Prevalidation.applied ; refused = Operation_hash.Map.empty ;
                 branch_refused =
                   List.fold_right
                     (fun (k, o) -> Operation_hash.Map.add k o)
                     branch_refused  Operation_hash.Map.empty ;
                 branch_delayed =
                   List.fold_right
                     (fun (k, o) -> Operation_hash.Map.add k o)
                     branch_delayed  Operation_hash.Map.empty ;
               },
               List.fold_right Operation_hash.Set.add
                 unprocessed Operation_hash.Set.empty))
           (obj4
              (req "applied" (list Operation_hash.encoding))
              (req "branch_delayed"
                 (list (tup2 Operation_hash.encoding Error.encoding)))
              (req "branch_refused"
                 (list (tup2 Operation_hash.encoding Error.encoding)))
              (req "unprocessed" (list Operation_hash.encoding))))
      RPC.Path.(block_path / "pending_operations")

  let proto_path =
    RPC.Path.(block_path / "proto")

  type preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: operation list ;
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
          (req "operations" (list (dynamic_size operation_encoding)))
          (dft "sort_operations" bool false)))

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Prevalidation.preapply_result ;
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
             (Prevalidation.preapply_result_encoding Error.encoding))))

  let preapply =
    RPC.service
      ~description:
        "Simulate the validation of a block that would contain \
         the given operations and return the resulting fitness."
      ~input: preapply_param_encoding
      ~output: (Error.wrap preapply_result_encoding)
      RPC.Path.(block_path / "preapply")

  let complete =
    let prefix_arg =
      let destruct s = Ok s
      and construct s = s in
      RPC.Arg.make ~name:"prefix" ~destruct ~construct () in
    RPC.service
      ~description: "Try to complete a prefix of a Base58Check-encoded data. \
                     This RPC is actually able to complete hashes of \
                     block, operations, public_keys and contracts."
      ~input: empty
      ~output: (list string)
      RPC.Path.(block_path / "complete" /: prefix_arg )

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
    RPC.service
      ~description:
        "Lists known heads of the blockchain sorted with decreasing fitness. \
         Optional arguments allows to returns the list of predecessors for \
         known heads or the list of predecessors for a given list of blocks."
      ~input: list_param_encoding
      ~output: (obj1 (req "blocks" (list (list block_info_encoding))))
      RPC.Path.(root / "blocks")

end

module Protocols = struct

  let protocols_arg =
    let name = "protocol_id" in
    let descr =
      "A protocol identifier in hexadecimal." in
    let construct = Protocol_hash.to_b58check in
    let destruct h =
      try Ok (Protocol_hash.of_b58check_exn h)
      with _ -> Error "Can't parse hash" in
    RPC.Arg.make ~name ~descr ~construct ~destruct ()

  let contents =
    RPC.service
      ~input: empty
      ~output:
        (obj1 (req "data"
                 (describe ~title: "Tezos protocol"
                    (Protocol.encoding))))
      RPC.Path.(root / "protocols" /: protocols_arg)

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
    RPC.service
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
      RPC.Path.(root / "protocols")

end

module Network = struct

  open P2p_types

  let (peer_id_arg : P2p_types.Peer_id.t RPC.Arg.arg) =
    RPC.Arg.make
      ~name:"peer_id"
      ~descr:"A network global identifier, also known as an identity."
      ~destruct:(fun s -> try
                    Ok (Crypto_box.Public_key_hash.of_b58check_exn s)
                  with Failure msg -> Error msg)
      ~construct:Crypto_box.Public_key_hash.to_b58check
      ()

  let point_arg =
    RPC.Arg.make
      ~name:"point"
      ~descr:"A network point (ipv4:port or [ipv6]:port)."
      ~destruct:Point.of_string
      ~construct:Point.to_string
      ()

  let versions =
    RPC.service
      ~description:"Supported network layer versions."
      ~input: empty
      ~output: (list P2p.Version.encoding)
      RPC.Path.(root / "network" / "versions")

  let stat =
    RPC.service
      ~description:"Global network bandwidth statistics in B/s."
      ~input: empty
      ~output: P2p.Stat.encoding
      RPC.Path.(root / "network" / "stat")

  let events =
    RPC.service
      ~description:"Stream of all network events"
      ~input: empty
      ~output: P2p.RPC.Event.encoding
      RPC.Path.(root / "network" / "log")

  let connect =
    RPC.service
      ~description:"Connect to a peer"
      ~input: (obj1 (dft "timeout" float 5.))
      ~output: (Error.wrap @@ empty)
      RPC.Path.(root / "network" / "connect" /: point_arg)

  let monitor_encoding = obj1 (dft "monitor" bool false)

  module Connection = struct

    let list =
      RPC.service
        ~description:"List the running P2P connection."
        ~input: empty
        ~output: (list P2p.Connection_info.encoding)
        RPC.Path.(root / "network" / "connection")

    let info =
      RPC.service
        ~input: empty
        ~output: (option P2p.Connection_info.encoding)
        ~description:"Details about the current P2P connection to the given peer."
        RPC.Path.(root / "network" / "connection" /: peer_id_arg)

    let kick =
      RPC.service
        ~input: (obj1 (req "wait" bool))
        ~output: empty
        ~description:"Forced close of the current P2P connection to the given peer."
        RPC.Path.(root / "network" / "connection" /: peer_id_arg / "kick")

  end

  module Point = struct

    let info =
      RPC.service
        ~input: empty
        ~output: (option P2p.RPC.Point.info_encoding)
        ~description: "Details about a given `IP:addr`."
        RPC.Path.(root / "network" / "point" /: point_arg)

    let events =
      RPC.service
        ~input: monitor_encoding
        ~output: (list P2p.RPC.Point.Event.encoding)
        ~description: "Monitor network events related to an `IP:addr`."
        RPC.Path.(root / "network" / "point" /: point_arg / "log")

    let list =
      let filter =
        obj1 (dft "filter" (list P2p.RPC.Point.state_encoding) []) in
      RPC.service
        ~input: filter
        ~output: (list (tup2 P2p.Point.encoding P2p.RPC.Point.info_encoding))
        ~description:"List the pool of known `IP:port` \
                      used for establishing P2P connections ."
        RPC.Path.(root / "network" / "point")

  end

  module Peer_id = struct

    let info =
      RPC.service
        ~input: empty
        ~output: (option P2p.RPC.Peer_id.info_encoding)
        ~description:"Details about a given peer."
        RPC.Path.(root / "network" / "peer_id" /: peer_id_arg)

    let events =
      RPC.service
        ~input: monitor_encoding
        ~output: (list P2p.RPC.Peer_id.Event.encoding)
        ~description:"Monitor network events related to a given peer."
        RPC.Path.(root / "network" / "peer_id" /: peer_id_arg / "log")

    let list =
      let filter =
        obj1 (dft "filter" (list P2p.RPC.Peer_id.state_encoding) []) in
      RPC.service
        ~input: filter
        ~output: (list (tup2 P2p.Peer_id.encoding P2p.RPC.Peer_id.info_encoding))
        ~description:"List the peers the node ever met."
        RPC.Path.(root / "network" / "peer_id")

  end

end

let forge_block_header =
  RPC.service
    ~description: "Forge a block header"
    ~input: Block_header.encoding
    ~output: (obj1 (req "block" bytes))
    RPC.Path.(root / "forge_block_header")

let validate_block =
  RPC.service
    ~description:
      "Force the node to fetch and validate the given block hash."
    ~input:
      (obj2
         (req "net" Net_id.encoding)
         (req "hash" Block_hash.encoding))
    ~output:
      (Error.wrap @@ empty)
    RPC.Path.(root / "validate_block")

type inject_block_param = {
  raw: MBytes.t ;
  blocking: bool ;
  force: bool ;
  operations: operation list list ;
}

let inject_block_param =
  conv
    (fun { raw ; blocking ; force ; operations } ->
       (raw, blocking, force, operations))
    (fun (raw, blocking, force, operations) ->
       { raw ; blocking ; force ; operations })
    (obj4
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
       (req "operations"
          (describe
             ~description:"..."
             (list (list (dynamic_size operation_encoding))))))

let inject_block =
  RPC.service
    ~description:
      "Inject a block in the node and broadcast it. The `operations` \
       embedded in `blockHeader` might be pre-validated using a \
       contextual RPCs from the latest block \
       (e.g. '/blocks/head/context/preapply'). Returns the ID of the \
       block. By default, the RPC will wait for the block to be \
       validated before answering."
    ~input: inject_block_param
    ~output:
      (Error.wrap @@
       (obj1 (req "block_hash" Block_hash.encoding)))
    RPC.Path.(root / "inject_block")

let inject_operation =
  RPC.service
    ~description:
      "Inject an operation in node and broadcast it. Returns the \
       ID of the operation. The `signedOperationContents` should be \
       constructed using a contextual RPCs from the latest block \
       and signed by the client. By default, the RPC will wait for \
       the operation to be (pre-)validated before answering. See \
       RPCs ubder /blocks/prevalidation for more details on the \
       prevalidation context."
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
         (opt "force"
            (describe
               ~description:
                 "Should we inject operation that are \"branch_refused\" \
                  or \"branch_delayed\". (default: false)"
               bool)))
    ~output:
      (Error.wrap @@
       describe
         ~title: "Hash of the injected operation" @@
       (obj1 (req "injectedOperation" Operation_hash.encoding)))
    RPC.Path.(root / "inject_operation")

let inject_protocol =
  RPC.service
    ~description:
      "Inject a protocol in node. Returns the ID of the protocol."
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
      (Error.wrap @@
       describe
         ~title: "Hash of the injected protocol" @@
       (obj1 (req "injectedProtocol" Protocol_hash.encoding)))
    RPC.Path.(root / "inject_protocol")

let bootstrapped =
  RPC.service
    ~description:""
    ~input: empty
    ~output: (obj2
                (req "block" Block_hash.encoding)
                (req "timestamp" Time.encoding))
    RPC.Path.(root / "bootstrapped")

let complete =
  let prefix_arg =
    let destruct s = Ok s
    and construct s = s in
    RPC.Arg.make ~name:"prefix" ~destruct ~construct () in
  RPC.service
    ~description: "Try to complete a prefix of a Base58Check-encoded data. \
                   This RPC is actually able to complete hashes of \
                   block and hashes of operations."
    ~input: empty
    ~output: (list string)
    RPC.Path.(root / "complete" /: prefix_arg )

let describe =
  RPC.Description.service
    ~description: "RPCs documentation and input/output schema"
    RPC.Path.(root / "describe")
