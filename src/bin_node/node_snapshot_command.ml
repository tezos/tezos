(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

open Genesis_chain
open Node_logging

type wrong_block_export_error =
  | Pruned
  | Too_few_predecessors
  | Cannot_be_found

let pp_wrong_block_export_error ppf = function
  | Pruned ->
      Format.fprintf ppf
        "is pruned"
  | Too_few_predecessors ->
      Format.fprintf ppf
        "has not enough predecessors"
  | Cannot_be_found ->
      Format.fprintf ppf
        "cannot be found"

let wrong_block_export_error_encoding =
  let open Data_encoding in
  union
    [
      case (Tag 0)
        ~title:"Pruned"
        (obj1
           (req "error" (constant "Pruned")))
        (function Pruned -> Some ()
                | _ -> None)
        (fun () -> Pruned) ;
      case (Tag 1)
        ~title:"Too_few_predecessors"
        (obj1
           (req "error" (constant "too_few_predecessors")))
        (function Too_few_predecessors -> Some ()
                | _ -> None)
        (fun () -> Too_few_predecessors) ;
      case (Tag 2)
        ~title:"Connot_be_found"
        (obj1
           (req "error" (constant "Cannot_be_found")))
        (function Cannot_be_found -> Some ()
                | _ -> None)
        (fun () -> Cannot_be_found) ;
    ]

type error += Wrong_block_export of Block_hash.t * wrong_block_export_error

let () =
  register_error_kind
    `Permanent
    ~id:"WrongBlockExport"
    ~title:"Wrong block export"
    ~description:"The block to export in the snapshot is not valid."
    ~pp:begin fun ppf (bh,kind) ->
      Format.fprintf ppf
        "Fails to export snapshot as the block with block hash %a %a."
        Block_hash.pp bh pp_wrong_block_export_error kind
    end
    Data_encoding.(obj2
                     (req "block_hash" Block_hash.encoding)
                     (req "kind" wrong_block_export_error_encoding))
    (function Wrong_block_export (bh, kind) -> Some (bh, kind) | _ -> None)
    (fun (bh, kind) -> Wrong_block_export (bh, kind))

let (//) = Filename.concat
let context_dir data_dir = data_dir // "context"
let store_dir data_dir = data_dir // "store"

let compute_export_limit
    block_store _chain_data_store
    block_header export_rolling =
  let block_hash = Block_header.hash block_header in
  Store.Block.Contents.read_opt
    (block_store, block_hash) >>= begin function
    | Some cnts -> return cnts
    | None -> fail @@ Wrong_block_export (block_hash, Pruned)
  end >>=? fun block_content ->
  let max_op_ttl = block_content.max_operations_ttl in
  begin
    if not export_rolling then
      return 1l
    else
      return (Int32.(sub block_header.Block_header.shell.level (of_int max_op_ttl)))
  end >>=? fun export_limit ->
  (* never include genesis *)
  return (max 1l export_limit)

let load_pruned_blocks block_store block_header export_limit =
  let cpt = ref 0 in
  let rec load_pruned (bh : Block_header.t) acc limit =
    Tezos_stdlib.Utils.display_progress
      ~refresh_rate:(!cpt, 1_000)
      "Retrieving history: %iK/%iK blocks"
      (!cpt / 1_000)
      ((!cpt + (Int32.to_int bh.shell.level - Int32.to_int limit)) / 1_000);
    incr cpt;
    if bh.shell.level <= limit then
      return acc
    else
      let pbh = bh.shell.predecessor in
      Store.Block.Header.read (block_store, pbh) >>=? fun pbhd ->
      Store.Block.Operations.bindings (block_store, pbh) >>= fun operations ->
      Store.Block.Operation_hashes.bindings (block_store, pbh) >>= fun operation_hashes ->
      let pruned_block = ({
          block_header = pbhd ;
          operations ;
          operation_hashes ;
        } : Context.Pruned_block.t ) in
      load_pruned pbhd (pruned_block :: acc) limit in
  load_pruned block_header [] export_limit >>= fun pruned_blocks ->
  Tezos_stdlib.Utils.display_progress_end () ;
  Lwt.return pruned_blocks

let export ?(export_rolling=false) data_dir filename block =
  let data_dir =
    match data_dir with
    | None -> Node_config_file.default_data_dir
    | Some dir -> dir
  in
  Node_data_version.ensure_data_dir data_dir >>=? fun () ->
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis.block in
  Store.init store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let chain_data_store = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  begin
    match block with
    | Some block_hash ->
        return (Block_hash.of_b58check_exn block_hash)
    | None ->
        Store.Chain_data.Checkpoint.read_exn
          (chain_data_store) >>= fun (last_checkpoint_level, last_checkpoint_hash) ->
        if last_checkpoint_level = 0l then
          fail @@ Wrong_block_export (genesis.block, Too_few_predecessors)
        else
          return last_checkpoint_hash >>=? fun last_checkpoint_hash ->
          lwt_log_notice "No block hash specified with the `--block` option. Using %a by default (last checkpoint)"
            Block_hash.pp last_checkpoint_hash >>= fun () ->
          return last_checkpoint_hash
  end >>=? fun block_hash ->
  Context.init ~readonly:true context_root
  >>= fun context_index ->
  Store.Block.Header.read_opt (block_store, block_hash) >>=
  begin function
    | None ->
        fail @@ Wrong_block_export (block_hash, Cannot_be_found)
    | Some block_header ->
        lwt_log_notice "Dumping: %a"
          Block_hash.pp block_hash >>= fun () ->

        (* Get block precessor's block header*)
        Store.Block.Predecessors.read
          (block_store, block_hash) 0 >>=? fun pred_block_hash ->
        Store.Block.Header.read
          (block_store, pred_block_hash) >>=? fun pred_block_header ->

        (* Get operation list*)
        let validations_passes = block_header.shell.validation_passes in
        Error_monad.map_s
          (fun i -> Store.Block.Operations.read (block_store, block_hash) i)
          (0 -- (validations_passes - 1)) >>=? fun operations ->

        compute_export_limit
          block_store
          chain_data_store
          block_header
          export_rolling >>=? fun export_limit ->

        (* Retreive the list of pruned blocks to export *)
        load_pruned_blocks
          block_store
          block_header
          export_limit >>=? fun old_pruned_blocks_rev ->

        let old_pruned_blocks = List.rev old_pruned_blocks_rev in

        Context.load_protocol_data context_index old_pruned_blocks >>= fun protocol_data ->

        let block_data =
          ({block_header = block_header ;
            operations } : Context.Block_data.t ) in

        return (pred_block_header, block_data, old_pruned_blocks, protocol_data)
  end
  >>=? fun data_to_dump ->
  Store.close store;
  Context.dump_contexts
    context_index
    [ data_to_dump ]
    ~filename >>=? fun () ->
  lwt_log_notice "Sucessful export (in file %s)" filename >>= fun () ->
  return_unit

(** Main *)

module Term = struct

  type subcommand = Export

  let process subcommand config_file file blocks export_rolling =
    let res =
      match subcommand with
      | Export -> export ~export_rolling config_file file blocks
    in
    match Lwt_main.run res with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "export" -> `Ok Export
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Export -> Format.fprintf ppf "export"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. \
       Possible value: $(b,export)." in
    required & pos 0 (some (parser, printer)) None & info [] ~docv:"OPERATION" ~doc

  let file_arg =
    let open Cmdliner.Arg in
    required & pos 1 (some string) None & info [] ~docv:"FILE"

  let blocks =
    let open Cmdliner.Arg in
    let doc ="Block hash of the block to export." in
    value & opt (some string) None & info ~docv:"<block_hash>" ~doc ["block"]

  let export_rolling =
    let open Cmdliner in
    let doc =
      "Force export command to dump a minimal snapshot based on the rolling mode." in
    Arg.(value & flag &
         info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["rolling"])

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg
         $ Node_shared_arg.Term.data_dir
         $ file_arg
         $ blocks
         $ export_rolling)

end

module Manpage = struct

  let command_description =
    "The $(b,snapshot) command is meant to export snapshots files."

  let description = [
    `S "DESCRIPTION" ;
    `P (command_description ^ " Several operations are possible: ");
    `P "$(b,export) allows to export a snapshot of the current node state into a file." ;
  ]

  let options = [
    `S "OPTIONS" ;
  ]

  let examples =
    [
      `S "EXAMPLES" ;
      `I ("$(b,Export a snapshot using the rolling mode)",
          "$(mname) snapshot export latest.rolling --rolling") ;
    ]

  let man =
    description @
    options @
    examples @
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Manage snapshots"
      ~man
      "snapshot"

end

let cmd =
  Term.term, Manpage.info
