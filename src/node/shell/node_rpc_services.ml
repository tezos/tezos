(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

  type net = Store.net_id = Net of Block_hash.t

  let net_encoding =
    conv (fun (Net id) -> id) (fun id -> Net id) Block_hash.encoding

  type block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: net ;
    test_protocol: Protocol_hash.t option ;
    test_network: (net * Time.t) option ;
  }

  let block_info_encoding =
    conv
      (fun { hash ; predecessor ; fitness ; timestamp ; protocol ; operations ;
             net ; test_protocol ; test_network } ->
        (hash, predecessor, fitness, timestamp, protocol, operations,
         net, test_protocol, test_network))
      (fun (hash, predecessor, fitness, timestamp, protocol, operations,
            net, test_protocol, test_network) ->
        { hash ; predecessor ; fitness ; timestamp ; protocol ; operations ;
          net ; test_protocol ; test_network })
      (obj9
         (req "hash" Block_hash.encoding)
         (req "predecessor" Block_hash.encoding)
         (req "fitness" Fitness.encoding)
         (req "timestamp" Time.encoding)
         (opt "protocol" Protocol_hash.encoding)
         (opt "operations" (list Operation_hash.encoding))
         (req "net" net_encoding)
         (opt "test_protocol" Protocol_hash.encoding)
         (opt "test_network" (tup2 net_encoding Time.encoding)))

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
      | [h] -> Ok (`Hash (Block_hash.of_b48check h))
      | _ -> raise Exit
    with _ -> Error "Cannot parse block identifier."


  let blocks_arg =
    let name = "block_id" in
    let descr =
      "A block identifier. This is either a block hash in hexadecimal \
       notation or a one the predefined aliases: \
       'genesis', 'head', 'prevalidation', \
       'test_head' or 'test_prevalidation'. One might alse use 'head~N'
       to 'test_head~N', where N is an integer to denotes the Nth predecessors
       of 'head' or 'test_head'." in
    let construct = function
      | `Genesis -> "genesis"
      | `Head 0 -> "head"
      | `Head n -> Printf.sprintf "head~%d" n
      | `Prevalidation -> "prevalidation"
      | `Test_head 0 -> "test_head"
      | `Test_head n -> Printf.sprintf "test_head~%d" n
      | `Test_prevalidation -> "test_prevalidation"
      | `Hash h -> Block_hash.to_b48check h in
    let destruct = parse_block in
    RPC.Arg.make ~name ~descr ~construct ~destruct

  type preapply_param = {
    operations: Operation_hash.t list ;
    sort: bool ;
    timestamp: Time.t option ;
  }

  let preapply_param_encoding =
    (conv
       (fun { operations ; sort ; timestamp } ->
          (operations, Some sort, timestamp))
       (fun (operations, sort, timestamp) ->
          let sort =
            match sort with
            | None -> true
            | Some x -> x in
          { operations ; sort ; timestamp })
       (obj3
          (req "operations" (list Operation_hash.encoding))
          (opt "sort" bool)
          (opt "timestamp" Time.encoding)))

  type preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }

  let preapply_result_encoding =
    (conv
       (fun { operations ; timestamp ; fitness } ->
          (timestamp, fitness, operations))
       (fun (timestamp, fitness, operations) ->
          { operations ; timestamp ; fitness })
       (obj3
          (req "timestamp" Time.encoding)
          (req "fitness" Fitness.encoding)
          (req "operations" (Updater.preapply_result_encoding RPC.Error.encoding))))

  let block_path : (unit, unit * block) RPC.Path.path =
    RPC.Path.(root / "blocks" /: blocks_arg )

  let info =
    RPC.service
      ~description:"All the block informations."
      ~input:
        (conv
           (fun x -> Some x)
           (function None -> false | Some x -> x)
           (obj1 (opt "operations" bool)))
      ~output: block_info_encoding
      block_path

  let net =
    RPC.service
      ~description:"Returns the net of the chain in which the block belongs."
      ~input: empty
      ~output: (obj1 (req "net" net_encoding))
      RPC.Path.(block_path / "net")

  let predecessor =
    RPC.service
      ~description:"Returns the previous block's id."
      ~input: empty
      ~output: (obj1 (req "predecessor" Block_hash.encoding))
      RPC.Path.(block_path / "predecessor")

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

  let operations =
    RPC.service
      ~description:"List the block operations."
      ~input: empty
      ~output: (obj1 (req "operations" (list Operation_hash.encoding)))
      RPC.Path.(block_path / "operations")

  let protocol =
    RPC.service
      ~description:"List the block protocol."
      ~input: empty
      ~output: (obj1 (req "protocol" Protocol_hash.encoding))
      RPC.Path.(block_path / "protocol")

  let test_protocol =
    RPC.service
      ~description:"List the block test protocol."
      ~input: empty
      ~output: (obj1 (opt "protocol" Protocol_hash.encoding))
      RPC.Path.(block_path / "test_protocol")

  let test_network =
    RPC.service
      ~description:"Returns the associated test network."
      ~input: empty
      ~output: (obj1 (opt "net" (tup2 net_encoding Time.encoding)))
      RPC.Path.(block_path / "test_network")

  let pending_operations =
    (* TODO: branch_delayed/... *)
    RPC.service
      ~description:
        "List the not-yet-prevalidated operations."
      ~input: empty
      ~output:
        (conv
           (fun ({ Updater.applied; branch_delayed ; branch_refused },
                 unprocessed) ->
             (applied,
              Operation_hash_map.bindings branch_delayed,
              Operation_hash_map.bindings branch_refused,
              Operation_hash_set.elements unprocessed))
           (fun (applied, branch_delayed, branch_refused, unprocessed) ->
              ({ Updater.applied ; refused = Operation_hash_map.empty ;
                 branch_refused =
                   List.fold_right
                     (fun (k, o) -> Operation_hash_map.add k o)
                     branch_refused  Operation_hash_map.empty ;
                 branch_delayed =
                   List.fold_right
                     (fun (k, o) -> Operation_hash_map.add k o)
                     branch_delayed  Operation_hash_map.empty ;
               },
               List.fold_right Operation_hash_set.add
                 unprocessed Operation_hash_set.empty))
           (obj4
              (req "applied" (list Operation_hash.encoding))
              (req "branch_delayed"
                 (list (tup2 Operation_hash.encoding RPC.Error.encoding)))
              (req "branch_refused"
                 (list (tup2 Operation_hash.encoding RPC.Error.encoding)))
              (req "unprocessed" (list Operation_hash.encoding))))
      RPC.Path.(block_path / "pending_operations")

  let proto_path =
    RPC.Path.(block_path / "proto")

  let preapply =
    RPC.service
      ~description:
        "Simulate the validation of a block that would contain \
         the given operations and return the resulting fitness."
      ~input: preapply_param_encoding
      ~output: (RPC.Error.wrap preapply_result_encoding)
      RPC.Path.(block_path / "preapply")

  type list_param = {
    operations: bool option ;
    length: int option ;
    heads: Block_hash.t list option ;
    monitor: bool option ;
    delay: int option ;
    min_date: Time.t option;
    min_heads: int option;
  }
  let list_param_encoding =
    conv
      (fun { operations ; length ; heads ; monitor ;
             delay ; min_date ; min_heads } ->
         (operations, length, heads, monitor, delay, min_date, min_heads))
      (fun (operations, length, heads, monitor, delay, min_date, min_heads) ->
         { operations ; length ; heads ; monitor ;
           delay ; min_date ; min_heads })
      (obj7
         (opt "operations"
            (Data_encoding.describe
               ~description:
                 "Whether the resulting block informations should include the \
                  list of operations' hashes. Default false."
               bool))
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

module Operations = struct

  let operations_arg =
    let name = "operation_id" in
    let descr =
      "A operation identifier in hexadecimal." in
    let construct = Operation_hash.to_b48check in
    let destruct h =
      try Ok (Operation_hash.of_b48check h)
      with _ -> Error "Can't parse hash" in
    RPC.Arg.make ~name ~descr ~construct ~destruct

  let bytes =
    RPC.service
      ~input: empty
      ~output:
        (obj1 (req "data"
                 (describe ~title: "Tezos signed operation (hex encoded)"
                    (Time.timed_encoding @@
                     RPC.Error.wrap @@
                     Updater.raw_operation_encoding))))
      RPC.Path.(root / "operations" /: operations_arg)

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
           (req "operations"
              (list
                 (obj2
                    (req "hash" Operation_hash.encoding)
                    (opt "contents"
                       (dynamic_size Updater.raw_operation_encoding)))
              )))
      RPC.Path.(root / "operations")

end

let forge_block =
  RPC.service
    ~description: "Forge a block header"
    ~input:
      (obj6
         (opt "net_id" Updater.net_id_encoding)
         (opt "predecessor" Block_hash.encoding)
         (opt "timestamp" Time.encoding)
         (req "fitness" Fitness.encoding)
         (req "operations" (list Operation_hash.encoding))
         (req "header" bytes))
    ~output: (obj1 (req "block" bytes))
    RPC.Path.(root / "forge_block")

let validate_block =
  RPC.service
    ~description:
      "Force the node to fetch and validate the given block hash."
    ~input:
      (obj2
         (req "net" Blocks.net_encoding)
         (req "hash" Block_hash.encoding))
    ~output:
      (RPC.Error.wrap @@ empty)
    RPC.Path.(root / "validate_block")

let inject_block =
  RPC.service
    ~description:
      "Inject a block in the node and broadcast it. The `operations` \
       embedded in `blockHeader` might pre-validated using a \
       contextual RPCs from the latest block \
       (e.g. '/blocks/head/context/preapply'). Returns the ID of the \
       block. By default, the RPC will wait for the block to be \
       validated before to answer."
    ~input:
      (conv
         (fun (block, blocking, force) ->
            (block, Some blocking, force))
         (fun (block, blocking, force) ->
            (block, Utils.unopt true blocking, force))
         (obj3
            (req "data" bytes)
            (opt "blocking"
               (describe
                  ~description:
                    "Should the RPC wait for the block to be \
                  validated before to answer. (default: true)"
                  bool))
            (opt "force"
               (describe
                  ~description:
                    "Should we inject the block when its fitness is below \
                     the current head. (default: false)"
                  bool))))
    ~output:
      (RPC.Error.wrap @@
       (obj1 (req "block_hash" Block_hash.encoding)))
    RPC.Path.(root / "inject_block")

let inject_operation =
  RPC.service
    ~description:
      "Inject an operation in node and broadcast it. Returns the \
       ID of the operation. The `signedOperationContents` should be \
       constructed using a contextual RPCs from the latest block \
       and signed by the client. By default, the RPC will wait for \
       the operation to be (pre-)validated before to answer. See \
       RPCs ubder /blocks/prevalidation for more details on the \
       prevalidation context."
    ~input:
      (conv
         (fun (block, blocking, force) -> (block, Some blocking, force))
         (fun (block, blocking, force) -> (block, unopt true blocking, force))
         (obj3
            (req "signedOperationContents"
               (describe ~title: "Tezos signed operation (hex encoded)"
                  bytes))
            (opt "blocking"
               (describe
                  ~description:
                    "Should the RPC wait for the operation to be \
                     (pre-)validated before to answer. (default: true)"
                  bool))
            (opt "force"
               (describe
                  ~description:
                    "Should we inject operation that are \"branch_refused\" \
                     or \"branch_delayed\". (default: false)"
                  bool))))
    ~output:
      (RPC.Error.wrap @@
       describe
         ~title: "Hash of the injected operation" @@
       (obj1 (req "injectedOperation" Operation_hash.encoding)))
    RPC.Path.(root / "inject_operation")

let describe =
  RPC.Description.service
    ~description: "RPCs documentation and input/output schema"
    RPC.Path.(root / "describe")
