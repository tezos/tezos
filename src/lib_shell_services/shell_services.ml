(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module S = struct

  open Data_encoding

  let forge_block_header =
    RPC_service.post_service
      ~description: "Forge a block header"
      ~query: RPC_query.empty
      ~input: Block_header.encoding
      ~output: (obj1 (req "block" bytes))
      RPC_path.(root / "forge_block_header")

  type inject_block_param = {
    raw: MBytes.t ;
    blocking: bool ;
    force: bool ;
    chain_id: Chain_id.t option ;
    operations: Operation.t list list ;
  }

  let inject_block_param =
    conv
      (fun { raw ; blocking ; force ; chain_id ; operations } ->
         (raw, blocking, force, chain_id, operations))
      (fun (raw, blocking, force, chain_id, operations) ->
         { raw ; blocking ; force ; chain_id ; operations })
      (obj5
         (req "data" bytes)
         (dft "blocking"
            ~description:
              "Should the RPC wait for the block to be \
               validated before answering. (default: true)"
            bool
            true)
         (dft "force"
            ~description:
              "Should we inject the block when its fitness is below \
               the current head. (default: false)"
            bool
            false)
         (opt "chain_id" Chain_id.encoding)
         (req "operations"
            (list (list (dynamic_size Operation.encoding)))))

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
      ~output: (obj1 (req "block_hash" Block_hash.encoding))
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
              ~title: "Tezos signed operation (hex encoded)"
              bytes)
           (dft "blocking"
              ~description:
                "Should the RPC wait for the operation to be \
                 (pre-)validated before answering. (default: true)"
              bool
              true)
           (opt "chain_id" Chain_id.encoding))
      ~output:
        (obj1 (req "injectedOperation" Operation_hash.encoding))
      RPC_path.(root / "inject_operation")

  let inject_protocol =
    RPC_service.post_service
      ~description:
        "Inject a protocol in node. Returns the ID of the protocol."
      ~query: RPC_query.empty
      ~input:
        (obj3
           (req "protocol" Protocol.encoding)
           (dft "blocking"
              ~description:
                "Should the RPC wait for the protocol to be \
                 validated before answering. (default: true)"
              bool
              true)
           (opt "force"
              ~description:
                "Should we inject protocol that is invalid. (default: false)"
              bool))
      ~output:
        (obj1 (req "injectedProtocol" Protocol_hash.encoding))
      RPC_path.(root / "inject_protocol")

  let bootstrapped =
    RPC_service.post_service
      ~description:""
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj2
                  (req "block" Block_hash.encoding)
                  (req "timestamp" Time.encoding))
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
      RPC_path.(root / "complete" /: prefix_arg )

end

open RPC_context

let forge_block_header ctxt header =
  make_call S.forge_block_header ctxt () () header

let inject_block ctxt
    ?(async = false) ?(force = false) ?chain_id
    raw operations =
  make_call S.inject_block ctxt () ()
    { raw ; blocking = not async ; force ; chain_id ; operations }

let inject_operation ctxt ?(async = false) ?chain_id operation =
  make_call S.inject_operation ctxt () ()
    (operation, not async, chain_id)

let inject_protocol ctxt ?(async = false) ?force protocol =
  make_call S.inject_protocol ctxt () ()
    (protocol, not async, force)

let bootstrapped ctxt =
  make_streamed_call S.bootstrapped ctxt () () ()

let complete ctxt ?block prefix =
  match block with
  | None ->
      make_call1 S.complete ctxt prefix () ()
  | Some block ->
      Block_services.complete ctxt block prefix
