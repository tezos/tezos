(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

include Tezos_stdlib.Logging.Make(struct let name = "node.snapshots" end)

let (//) = Filename.concat
let context_dir data_dir = data_dir // "context"
let store_dir data_dir = data_dir // "store"

type error += Wrong_block_export of
    Block_hash.t * [ `Pruned | `Too_few_predecessors | `Cannot_be_found ]
type error += Inconsistent_imported_block of Block_hash.t * Block_hash.t
type error += Snapshot_import_failure of string
type error += Wrong_reconstrcut_mode
type error += Wrong_protocol_hash of Protocol_hash.t
type error += Inconsistent_operation_hashes of
    (Operation_list_list_hash.t * Operation_list_list_hash.t)

let () = begin
  let open Data_encoding in

  let pp_wrong_block_export_error ppf kind =
    let str =
      match kind with
      | `Pruned -> "is pruned"
      | `Too_few_predecessors -> "has not enough predecessors"
      | `Cannot_be_found ->  "cannot be found" in
    Format.fprintf ppf "%s" str in
  let error_kind_encoding =
    string_enum
      [ "pruned", `Pruned ;
        "too_few_predecessors", `Too_few_predecessors ;
        "cannot_be_found", `Cannot_be_found ] in
  register_error_kind
    `Permanent
    ~id:"WrongBlockExport"
    ~title:"Wrong block export"
    ~description:"The block to export in the snapshot is not valid."
    ~pp:(fun ppf (bh,kind) ->
        Format.fprintf ppf
          "Fails to export snapshot as the block with block hash %a %a."
          Block_hash.pp bh pp_wrong_block_export_error kind)
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "kind" error_kind_encoding))
    (function Wrong_block_export (bh, kind) -> Some (bh, kind) | _ -> None)
    (fun (bh, kind) -> Wrong_block_export (bh, kind)) ;

  register_error_kind
    `Permanent
    ~id:"InconsistentImportedBlock"
    ~title:"Inconsistent imported block"
    ~description:"The imported block is not the expected one."
    ~pp:begin fun ppf (got,exp) ->
      Format.fprintf ppf
        "The block contained in the file is %a instead of %a."
        Block_hash.pp got Block_hash.pp exp
    end
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_hash_expected" Block_hash.encoding))
    (function Inconsistent_imported_block (got, exp) -> Some (got, exp) | _ -> None)
    (fun (got, exp) -> Inconsistent_imported_block (got, exp)) ;

  register_error_kind
    `Permanent
    ~id:"SnapshotImportFailure"
    ~title:"Snapshot import failure"
    ~description:"The imported snapshot is malformed."
    ~pp:begin fun ppf msg ->
      Format.fprintf ppf
        "The data contained in the snapshot is not valid. The import mechanism \
         failed to validate the file: %s."
        msg
    end
    (obj1 (req "message" string))
    (function Snapshot_import_failure str -> Some str | _ -> None)
    (fun str -> Snapshot_import_failure str) ;

  register_error_kind
    `Permanent
    ~id:"WrongReconstructMode"
    ~title:"Wrong reconstruct mode"
    ~description:"Reconstruction of contexts while importing is comptible \
                  with full mode snapshots only"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Contexts reconstruction is available with full mode snapshots only.")
    empty
    (function Wrong_reconstrcut_mode -> Some () | _ -> None)
    (fun () -> Wrong_reconstrcut_mode) ;

  register_error_kind
    `Permanent
    ~id:"WrongProtocolHash"
    ~title:"Wrong protocol hash"
    ~description:"Wrong protocol hash"
    ~pp:(fun ppf p ->
        Format.fprintf ppf
          "Wrong protocol hash (%a) found in snapshot. Snapshot is corrupted."
          Protocol_hash.pp p)
    (obj1 (req "protocol_hash" Protocol_hash.encoding))
    (function Wrong_protocol_hash p -> Some p | _ -> None)
    (fun p -> Wrong_protocol_hash p) ;

  register_error_kind
    `Permanent
    ~id:"InconsistentOperationHashes"
    ~title:"Inconsistent operation hashes"
    ~description:"The operations given do not match their hashes."
    ~pp:(fun ppf (oph, oph') ->
        Format.fprintf ppf
          "Inconsistent operation hashes. Expected: %a, got: %a."
          Operation_list_list_hash.pp oph Operation_list_list_hash.pp oph')
    (obj2
       (req "expected_operation_hashes" Operation_list_list_hash.encoding)
       (req "received_operation_hashes" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operation_hashes (oph, oph') -> Some (oph, oph')
      | _ -> None)
    (fun (oph, oph') -> Inconsistent_operation_hashes (oph, oph')) ;
end

let compute_export_limit
    block_store _chain_data_store
    block_header export_rolling =
  if not export_rolling then
    return 1l
  else
    let block_hash = Block_header.hash block_header in
    Store.Block.Contents.read_opt
      (block_store, block_hash) >>= begin function
      | Some contents -> return contents
      | None -> fail (Wrong_block_export (block_hash, `Pruned))
    end >>=? fun { max_operations_ttl } ->
    let limit = Int32.(sub
                         block_header.Block_header.shell.level
                         (of_int max_operations_ttl)) in
    (* fails when the limit exceeds the genesis or the genesis is
       included in the export limit *)
    fail_when
      (limit <= 0l)
      (Wrong_block_export (block_hash, `Too_few_predecessors)) >>=? fun () ->
    return limit

(** When called with a block, returns its predecessor if it exists and
    its protocol_data if the block is a transition block (i.e. protocol
    level changing block) or when there is no more predecessor. *)
let pruned_block_iterator index block_store limit
  : (Block_header.t -> (Context.Pruned_block.t option * Context.Protocol_data.t option) tzresult Lwt.t) =
  fun header ->
    if header.Block_header.shell.level <= limit then
      Context.get_protocol_data_from_header index header >>= fun protocol_data ->
      return (None, Some protocol_data)
    else
      let pred_hash = header.Block_header.shell.predecessor in
      Store.Block.Contents.read (block_store, pred_hash) >>=? fun { header = pred_header } ->
      Store.Block.Operations.bindings (block_store, pred_hash) >>= fun pred_operations ->
      Store.Block.Operation_hashes.bindings (block_store, pred_hash) >>= fun pred_operation_hashes ->
      let pruned_block = {
        Context.Pruned_block.block_header = pred_header ;
        operations = pred_operations ;
        operation_hashes = pred_operation_hashes ;
      } in
      let header_proto_level = header.Block_header.shell.proto_level in
      let pred_header_proto_level = pred_header.Block_header.shell.proto_level in
      if header_proto_level <> pred_header_proto_level then
        Context.get_protocol_data_from_header index header >>= fun proto_data ->
        return (Some pruned_block, Some proto_data)
      else
        return (Some pruned_block, None)

let export ?(export_rolling=false) ~data_dir ~genesis filename block  =
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis in
  Store.init store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let chain_data_store = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  begin match block with
    | Some block_hash -> return (Block_hash.of_b58check_exn block_hash)
    | None ->
        Store.Chain_data.Checkpoint.read_exn (chain_data_store) >>= fun last_checkpoint ->
        if (fst last_checkpoint) = 0l then
          fail (Wrong_block_export (genesis, `Too_few_predecessors))
        else
          let last_checkpoint_hash = snd last_checkpoint in
          lwt_log_notice "No block hash specified with the `--block` option. Using %a by default (last checkpoint)"
            Block_hash.pp last_checkpoint_hash >>= fun () ->
          return last_checkpoint_hash
  end >>=? fun checkpoint_block_hash ->
  Context.init ~readonly:true context_root >>= fun context_index ->
  begin Store.Block.Contents.read_opt
      (block_store, checkpoint_block_hash) >>= function
    | None ->
        fail (Wrong_block_export (checkpoint_block_hash, `Cannot_be_found))
    | Some { Store.Block.header = block_header } ->
        lwt_log_notice "Dumping: %a" Block_hash.pp checkpoint_block_hash >>= fun () ->
        (* Get block precessor's block header*)
        Store.Block.Predecessors.read
          (block_store, checkpoint_block_hash) 0 >>=? fun pred_block_hash ->
        Store.Block.Contents.read
          (block_store, pred_block_hash) >>=? fun { header = pred_block_header } ->
        (* Get operation list*)
        let validations_passes = block_header.shell.validation_passes in
        map_s
          (fun i -> Store.Block.Operations.read (block_store, checkpoint_block_hash) i)
          (0 -- (validations_passes - 1)) >>=? fun operations ->
        compute_export_limit
          block_store chain_data_store block_header export_rolling >>=? fun export_limit ->
        let iterator = pruned_block_iterator context_index block_store export_limit in
        let block_data = { Context.Block_data.block_header ; operations } in
        let starting_block_header = block_header in
        return (pred_block_header, block_data, iterator, starting_block_header)
  end >>=? fun data_to_dump ->
  Context.dump_contexts context_index [ data_to_dump ] ~filename >>=? fun () ->
  Store.close store ;
  lwt_log_notice "Sucessful export (in file %s)" filename >>= fun () ->
  return_unit
