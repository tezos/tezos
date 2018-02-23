(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

let custom_root = RPC_path.open_root

module S = struct

  open Data_encoding

  let operations =
    RPC_service.post_service
      ~description: "All the operations of the block (fully decoded)."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (list (list (dynamic_size Operation.encoding)))
      RPC_path.(custom_root / "operations")

  let header =
    RPC_service.post_service
      ~description: "The header of the block (fully decoded)."
      ~query: RPC_query.empty
      ~input: empty
      ~output: Block_header.encoding
      RPC_path.(custom_root / "header")

  let priority =
    RPC_service.post_service
      ~description: "Baking priority of the block."
      ~query: RPC_query.empty
      ~input: empty
      ~output: (obj1 (req "priority" uint16))
      RPC_path.(custom_root / "header" / "priority")

  let seed_nonce_hash =
    RPC_service.post_service
      ~description: "Hash of the seed nonce of the block."
      ~query: RPC_query.empty
      ~input: empty
      ~output: Nonce_hash.encoding
      RPC_path.(custom_root / "header" / "seed_nonce_hash")

end

let () =
  let open Services_registration in
  register0_fullctxt S.operations begin fun ctxt () () ->
    ctxt.operation_hashes () >>= fun operation_hashes ->
    ctxt.operations () >>= fun operations ->
    map2_s
      (map2_s (fun x y -> Lwt.return (Operation.parse x y)))
      operation_hashes operations
  end ;
  register0_fullctxt S.header begin fun { block_header ; _ } () () ->
    Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
    return block_header
  end ;
  register0_fullctxt S.priority begin fun { block_header ; _ } () () ->
    Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
    return block_header.protocol_data.priority
  end ;
  opt_register0_fullctxt S.seed_nonce_hash begin fun { block_header ; _ } () ( )->
    Lwt.return (Block_header.parse block_header) >>=? fun block_header ->
    return block_header.protocol_data.seed_nonce_hash
  end

let operations ctxt block =
  RPC_context.make_call0 S.operations ctxt block () ()
let header ctxt block =
  RPC_context.make_call0 S.header ctxt block () ()
let priority ctxt block =
  RPC_context.make_call0 S.priority ctxt block () ()
let seed_nonce_hash ctxt block =
  RPC_context.make_call0 S.seed_nonce_hash ctxt block () ()

module Context = struct

  module S = struct

    open Data_encoding

    let level =
      RPC_service.post_service
        ~description: "Detailled level information for the current block"
        ~query: RPC_query.empty
        ~input: empty
        ~output: Level.encoding
        RPC_path.(custom_root / "context" / "level")

    let next_level =
      RPC_service.post_service
        ~description: "Detailled level information for the next block"
        ~query: RPC_query.empty
        ~input: empty
        ~output: Level.encoding
        RPC_path.(custom_root / "context" / "next_level")

    let voting_period_kind =
      RPC_service.post_service
        ~description: "Voting period kind for the current block"
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (obj1 (req "voting_period_kind" Voting_period.kind_encoding))
        RPC_path.(custom_root / "context" / "voting_period_kind")

  end

  type error += Unexpected_level_in_context

  let () =
    let open Services_registration in
    register0 S.level begin fun ctxt () () ->
      let level = Level.current ctxt in
      match Level.pred ctxt level with
      | None -> fail Unexpected_level_in_context
      | Some level -> return level
    end ;
    register0 S.next_level begin fun ctxt () () ->
      return (Level.current ctxt)
    end ;
    register0 S.voting_period_kind begin fun ctxt () () ->
      Vote.get_current_period_kind ctxt
    end

  let level ctxt block =
    RPC_context.make_call0 S.level ctxt block () ()

  let next_level ctxt block =
    RPC_context.make_call0 S.next_level ctxt block () ()

  let voting_period_kind ctxt block =
    RPC_context.make_call0 S.voting_period_kind ctxt block () ()

end

module Nonce = struct

  type info =
    | Revealed of Nonce.t
    | Missing of Nonce_hash.t
    | Forgotten

  let info_encoding =
    let open Data_encoding in
    union [
      case (Tag 0)
        (obj1 (req "nonce" Nonce.encoding))
        (function Revealed nonce -> Some nonce | _ -> None)
        (fun nonce -> Revealed nonce) ;
      case (Tag 1)
        (obj1 (req "hash" Nonce_hash.encoding))
        (function Missing nonce -> Some nonce | _ -> None)
        (fun nonce -> Missing nonce) ;
      case (Tag 2)
        empty
        (function Forgotten -> Some () | _ -> None)
        (fun () -> Forgotten) ;
    ]

  module S = struct

    open Data_encoding

    let get =
      RPC_service.post_service
        ~description: "Info about the nonce of a previous block."
        ~query: RPC_query.empty
        ~input: empty
        ~output: info_encoding
        RPC_path.(custom_root / "context" / "nonce" /: Raw_level.arg)

    let hash =
      RPC_service.post_service
        ~description: "Hash of the current block's nonce."
        ~query: RPC_query.empty
        ~input: empty
        ~output: Nonce_hash.encoding
        RPC_path.(custom_root / "context" / "nonce")

  end

  let () =
    let open Services_registration in
    register1 S.get begin fun ctxt raw_level () () ->
      let level = Level.from_raw ctxt raw_level in
      Nonce.get ctxt level >>= function
      | Ok (Revealed nonce) -> return (Revealed nonce)
      | Ok (Unrevealed { nonce_hash ; _ }) ->
          return (Missing nonce_hash)
      | Error _ -> return Forgotten
    end ;
    register0 S.hash begin fun ctxt () () ->
      let level = Level.current ctxt in
      match Level.pred ctxt level with
      | None -> fail Context.Unexpected_level_in_context
      | Some level ->
          Nonce.get ctxt level >>=? function
          | Unrevealed { nonce_hash ; _ } -> return nonce_hash
          | _ -> assert false
    end

  let get ctxt block level =
    RPC_context.make_call1 S.get ctxt block level () ()

  let hash ctxt block =
    RPC_context.make_call0 S.hash ctxt block () ()

end

module Contract = Contract_services
module Constants = Constants_services
module Delegate = Delegate_services
module Helpers = Helpers_services
module Forge = Helpers_services.Forge
module Parse = Helpers_services.Parse
