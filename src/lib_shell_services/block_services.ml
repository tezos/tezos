(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

type block = [
  | `Genesis
  | `Head of int
  | `Test_head of int
  | `Hash of Block_hash.t
]

let parse_block s =
  try
    match String.split '~' s with
    | ["genesis"] -> Ok `Genesis
    | ["head"] -> Ok (`Head 0)
    | ["test_head"] -> Ok (`Test_head 0)
    | ["head"; n] -> Ok (`Head (int_of_string n))
    | ["test_head"; n] -> Ok (`Test_head (int_of_string n))
    | [h] -> Ok (`Hash (Block_hash.of_b58check_exn h))
    | _ -> raise Exit
  with _ -> Error "Cannot parse block identifier."

let to_string = function
  | `Genesis -> "genesis"
  | `Head 0 -> "head"
  | `Head n -> Printf.sprintf "head~%d" n
  | `Test_head 0 -> "test_head"
  | `Test_head n -> Printf.sprintf "test_head~%d" n
  | `Hash h -> Block_hash.to_b58check h

type block_info = {
  hash: Block_hash.t ;
  chain_id: Chain_id.t ;
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
  context: Context_hash.t ;
  protocol_data: MBytes.t ;
  operations: (Operation_hash.t * Operation.t) list list option ;
  protocol: Protocol_hash.t ;
  test_chain: Test_chain_status.t ;
}

let pp_block_info ppf
    { hash ; chain_id ; level ;
      proto_level ; predecessor ; timestamp ;
      operations_hash ; fitness ; protocol_data ;
      operations ; protocol ; test_chain } =
  Format.fprintf ppf
    "@[<v 2>Hash: %a\
     @ Test chain: %a\
     @ Level: %ld\
     @ Proto_level: %d\
     @ Predecessor: %a\
     @ Protocol: %a\
     @ Net id: %a\
     @ Timestamp: %a\
     @ @[<hov 2>Fitness: %a@]\
     @ Operations hash: %a\
     @ @[<hov 2>Operations:@ %a@]\
     @ @[<hov 2>Protocol data:@ %a@]@]"
    Block_hash.pp hash
    Test_chain_status.pp test_chain
    level
    proto_level
    Block_hash.pp predecessor
    Protocol_hash.pp protocol
    Chain_id.pp chain_id
    Time.pp_hum timestamp
    Fitness.pp fitness
    Operation_list_list_hash.pp operations_hash
    (fun ppf -> function
       | None -> Format.fprintf ppf "None"
       | Some operations ->
           Format.pp_print_list ~pp_sep:Format.pp_print_newline
             (Format.pp_print_list ~pp_sep:Format.pp_print_space
                (fun ppf (oph, _) -> Operation_hash.pp ppf oph))
             ppf operations)
    operations
    Hex.pp (MBytes.to_hex protocol_data)

let block_info_encoding =
  let operation_encoding =
    merge_objs
      (obj1 (req "hash" Operation_hash.encoding))
      Operation.encoding in
  conv
    (fun { hash ; chain_id ; level ; proto_level ; predecessor ;
           fitness ; timestamp ; protocol ;
           validation_passes ; operations_hash ; context ; protocol_data ;
           operations ; test_chain } ->
      ((hash, chain_id, operations, protocol, test_chain),
       { Block_header.shell =
           { level ; proto_level ; predecessor ;
             timestamp ; validation_passes ; operations_hash ; fitness ;
             context } ;
         protocol_data }))
    (fun ((hash, chain_id, operations, protocol, test_chain),
          { Block_header.shell =
              { level ; proto_level ; predecessor ;
                timestamp ; validation_passes ; operations_hash ; fitness ;
                context } ;
            protocol_data }) ->
      { hash ; chain_id ; level ; proto_level ; predecessor ;
        fitness ; timestamp ; protocol ;
        validation_passes ; operations_hash ; context ; protocol_data ;
        operations ; test_chain })
    (dynamic_size
       (merge_objs
          (obj5
             (req "hash" Block_hash.encoding)
             (req "chain_id" Chain_id.encoding)
             (opt "operations" (dynamic_size (list (dynamic_size (list (dynamic_size operation_encoding))))))
             (req "protocol" Protocol_hash.encoding)
             (dft "test_chain"
                Test_chain_status.encoding Not_running))
          Block_header.encoding))

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

type raw_context_result =
  | Key of MBytes.t
  | Dir of (string * raw_context_result) list
  | Cut

let raw_context_result_pp t =
  let open Format in
  let rec loop ppf = function
    | Cut -> fprintf ppf "..."
    | Key v -> let `Hex s = MBytes.to_hex v in fprintf ppf "%S" s
    | Dir l ->
        fprintf ppf "{@[<v 1>@,%a@]@,}"
          (pp_print_list ~pp_sep:Format.pp_print_cut
             (fun ppf (s,t) -> fprintf ppf "%s : %a" s loop t)) l
  in
  asprintf "%a" loop t

module S = struct

  let blocks_arg =
    let name = "block_id" in
    let descr =
      "A block identifier. This is either a block hash in hexadecimal \
       notation or a one the predefined aliases: \
       'genesis', 'head', \
       or 'test_head'. One might alse use 'head~N'
       to 'test_head~N', where N is an integer to denotes the Nth predecessors
       of 'head' or 'test_head'." in
    let construct = to_string in
    let destruct = parse_block in
    RPC_arg.make ~name ~descr ~construct ~destruct ()

  let block_path : (unit, unit * block) RPC_path.path =
    RPC_path.(root / "blocks" /: blocks_arg)
  let proto_path () =
    RPC_path.(open_root / "blocks" /: blocks_arg / "proto")


  let info =
    RPC_service.post_service
      ~description:"All the information about a block."
      ~query: RPC_query.empty
      ~input: (obj1 (dft "operations" bool true))
      ~output: block_info_encoding
      block_path

  let chain_id =
    RPC_service.post_service
      ~description:"Returns the chain in which the block belongs."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "chain_id" Chain_id.encoding))
      RPC_path.(block_path / "chain_id")

  let level =
    RPC_service.post_service
      ~description:"Returns the block's level."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "level" int32))
      RPC_path.(block_path / "level")

  let predecessor =
    RPC_service.post_service
      ~description:"Returns the previous block's id."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "predecessor" Block_hash.encoding))
      RPC_path.(block_path / "predecessor")

  let predecessors =
    RPC_service.post_service
      ~description:
        "...."
      ~query: RPC_query.empty
      ~input: (obj1 (req "length" Data_encoding.uint16))
      ~output: (obj1
                  (req "blocks" (Data_encoding.list Block_hash.encoding)))
      RPC_path.(block_path / "predecessors")

  let hash =
    RPC_service.post_service
      ~description:"Returns the block's id."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "hash" Block_hash.encoding))
      RPC_path.(block_path / "hash")

  let fitness =
    RPC_service.post_service
      ~description:"Returns the block's fitness."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "fitness" Fitness.encoding))
      RPC_path.(block_path / "fitness")

  let context =
    RPC_service.post_service
      ~description:"Returns the hash of the resulting context."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "context" Context_hash.encoding))
      RPC_path.(block_path / "context")

  let raw_context_args : string RPC_arg.t =
    let name = "context_path" in
    let descr = "A path inside the context" in
    let construct = fun s -> s in
    let destruct = fun s -> Ok s in
    RPC_arg.make ~name ~descr ~construct ~destruct ()

  let raw_context_result_encoding : raw_context_result Data_encoding.t =
    let open Data_encoding in
    obj1 (req "content"
            (mu "context_tree" (fun raw_context_result_encoding ->
                 union [
                   case (Tag 0) bytes
                     (function Key k -> Some k | _ -> None)
                     (fun k -> Key k) ;
                   case (Tag 1) (assoc raw_context_result_encoding)
                     (function Dir k -> Some k | _ -> None)
                     (fun k -> Dir k) ;
                   case (Tag 2) null
                     (function Cut -> Some () | _ -> None)
                     (fun () -> Cut) ;
                 ])))

  (* The depth query argument for the [raw_context] service,
     default value is 1. *)
  let depth_query : < depth: int > RPC_query.t =
    let open RPC_query in
    query (fun depth -> object
            method depth = depth
          end)
    |+ field "depth" RPC_arg.int 1 (fun t -> t#depth)
    |> seal

  let raw_context =
    RPC_service.post_service
      ~description:"Returns the raw context."
      ~query: depth_query
      ~input: empty
      ~output: raw_context_result_encoding
      RPC_path.(block_path / "raw_context" /:* raw_context_args)

  let timestamp =
    RPC_service.post_service
      ~description:"Returns the block's timestamp."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "timestamp" Time.encoding))
      RPC_path.(block_path / "timestamp")

  type operations_param = {
    contents: bool ;
  }

  let operations_param_encoding =
    let open Data_encoding in
    conv
      (fun { contents } -> (contents))
      (fun (contents) -> { contents })
      (obj1 (dft "contents" bool false))

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
      RPC_path.(block_path / "operations")

  let protocol =
    RPC_service.post_service
      ~description:"List the block protocol."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "protocol" Protocol_hash.encoding))
      RPC_path.(block_path / "protocol")

  let test_chain =
    RPC_service.post_service
      ~description:"Returns the status of the associated test chain."
      ~query: RPC_query.empty
      ~input: empty
      ~output: Test_chain_status.encoding
      RPC_path.(block_path / "test_chain")

  type preapply_param = {
    timestamp: Time.t ;
    protocol_data: MBytes.t ;
    operations: Operation.t list list ;
    sort_operations: bool ;
  }

  let preapply_param_encoding =
    (conv
       (fun { timestamp ; protocol_data ; operations ; sort_operations } ->
          (timestamp, protocol_data, operations, sort_operations))
       (fun (timestamp, protocol_data, operations, sort_operations) ->
          { timestamp ; protocol_data ; operations ; sort_operations })
       (obj4
          (req "timestamp" Time.encoding)
          (req "protocol_data" bytes)
          (req "operations" (list (dynamic_size (list (dynamic_size Operation.encoding)))))
          (dft "sort_operations" bool false)))

  let preapply =
    RPC_service.post_service
      ~description:
        "Simulate the validation of a block that would contain \
         the given operations and return the resulting fitness."
      ~query: RPC_query.empty
      ~input: preapply_param_encoding
      ~output: preapply_result_encoding
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
      RPC_path.(root / "invalid_blocks")

  let unmark_invalid =
    RPC_service.post_service
      ~description:
        "Unmark an invalid block"
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: Data_encoding.empty
      RPC_path.(root / "invalid_blocks" /: Block_hash.rpc_arg / "unmark" )

end

open RPC_context

let chain_id ctxt b = make_call1 S.chain_id ctxt b () ()
let level ctxt b = make_call1 S.level ctxt b () ()
let predecessor ctxt b = make_call1 S.predecessor ctxt b () ()
let predecessors ctxt b n = make_call1 S.predecessors ctxt b () n
let hash ctxt b = make_call1 S.hash ctxt b () ()
let timestamp ctxt b = make_call1 S.timestamp ctxt b () ()
let fitness ctxt b = make_call1 S.fitness ctxt b () ()
let operations ctxt ?(contents = false) h =
  make_call1 S.operations ctxt h () { contents }
let protocol ctxt b = make_call1 S.protocol ctxt b () ()
let test_chain ctxt b = make_call1 S.test_chain ctxt b () ()
let info ctxt ?(include_ops = true) h =
  make_call1 S.info ctxt h () include_ops
let monitor ?(include_ops = false)
    ?length ?heads ?delay ?min_date ?min_heads ctxt =
  make_streamed_call S.list ctxt () ()
    { include_ops ; length ; heads ;
      monitor = Some true ; delay ;
      min_date ; min_heads }
let list ?(include_ops = false)
    ?length ?heads ?delay ?min_date ?min_heads ctxt =
  make_call S.list ctxt () ()
    { include_ops ; length ; heads ;
      monitor = Some false ; delay ;
      min_date ; min_heads }
let complete ctxt b s =
  make_call2 S.complete ctxt b s () ()
let preapply ctxt h
    ?(timestamp = Time.now ()) ?(sort = false) ~protocol_data operations =
  make_call1 S.preapply ctxt h ()
    { timestamp ; protocol_data ; sort_operations = sort ; operations }

let unmark_invalid ctxt h =
  make_call1 S.unmark_invalid ctxt h () ()

let list_invalid ctxt =
  make_call S.list_invalid ctxt () () ()

let raw_context ctxt b key depth =
  let depth = object
    method depth = depth
  end
  in
  make_call2 S.raw_context ctxt b key depth ()
