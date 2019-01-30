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

let (//) = Filename.concat
let context_dir data_dir = data_dir // "context"
let store_dir data_dir = data_dir // "store"

let export ?(export_full=false) data_dir filename blocks =
  let data_dir =
    match data_dir with
    | None -> Node_config_file.default_data_dir
    | Some dir -> dir
  in
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis.block in
  Store.init store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let chain_data_store = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  begin if blocks <> [] then
      Lwt.return (List.map Block_hash.of_b58check_exn blocks)
    else
      Store.Chain_data.Current_head.read_exn chain_data_store >>= fun head ->
      Store.Block.Predecessors.read_exn (block_store, head) 6 >>= fun sixteenth_pred ->
      lwt_log_notice "No block hash specified with the `--block` option. Using %a by default (64th predecessor from the current head)"
        Block_hash.pp sixteenth_pred >>= fun () ->
      Lwt.return [ sixteenth_pred ]
  end >>= fun blocks ->
  Error_monad.filter_map_p begin fun block_hash ->
    Store.Block.Header.read_opt (block_store, block_hash) >>= function
    | None ->
        lwt_log_notice "Skipping unknown block %a"
          Block_hash.pp block_hash >>= fun () ->
        return_none
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

        (* Get block predecessor's content *)
        Store.Block.Contents.read
          (block_store, block_hash) >>=? fun block_content ->
        (* Get the max_ttls previous block headers and their operation hashes*)
        let max_op_ttl = block_content.max_operations_ttl in
        begin
          if export_full then
            Store.Chain_data.Rock_bottom.read chain_data_store >>=? fun (rb_level,_) ->
            return rb_level
          else
            return
              (Int32.(sub block_header.shell.level (of_int max_op_ttl)))
        end >>=? fun export_limit ->
        let cpt = ref 0 in
        let rec load_pruned (bh : Block_header.t) acc limit =
          if Unix.isatty Unix.stderr && !cpt mod 1000 = 0 then
            Format.eprintf "Retrieving history: %iK/%iK blocks%!\
                            \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\
                            \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
              (!cpt / 1000)
              ((!cpt + (Int32.to_int bh.shell.level - Int32.to_int limit)) / 1000);
          incr cpt;
          if bh.shell.level = limit then
            return acc
          else
            let pbh = bh.shell.predecessor in
            Store.Block.Header.read (block_store, pbh) >>=? fun pbhd ->
            (* Get operations *)
            Store.Block.Operations.bindings (block_store, pbh) >>= fun operations ->
            (* Get operation hashes *)
            Store.Block.Operation_hashes.bindings (block_store, pbh) >>= fun operation_hashes ->
            (* Get predecessors*)
            Store.Block.Predecessors.bindings (block_store, pbh) >>= fun predecessors ->
            let pruned_block = ({
                block_header = pbhd ;
                operations ;
                operation_hashes ;
                predecessors ;
              } : Tezos_storage.Context.Pruned_block.t ) in
            load_pruned pbhd (pruned_block :: acc) limit in
        load_pruned block_header [] export_limit >>=? fun old_pruned_blocks_rev ->
        if Unix.isatty Unix.stderr then Format.eprintf "@." ;
        let block_data =
          ({block_header = block_header ;
            operations } : Tezos_storage.Context.Block_data.t ) in
        return (Some (pred_block_header, block_data, List.rev old_pruned_blocks_rev))
  end
    blocks >>=? fun data_to_dump ->
  Store.close store;
  Tezos_storage.Context.init ~readonly:true context_root
  >>= fun context_index ->
  Tezos_storage.Context.dump_contexts
    context_index
    data_to_dump
    ~filename >>=? fun () ->
  lwt_log_notice "Sucessful export (in file %s)" filename >>= fun () ->
  return_unit

let import data_dir filename =
  let data_dir =
    match data_dir with
    | None -> Node_config_file.default_data_dir
    | Some dir -> dir
  in
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis.block in
  Node_data_version.ensure_data_dir data_dir >>=? fun () ->
  Lwt_lock_file.create
    ~unlink_on_exit:true (Node_data_version.lock_file data_dir) >>=? fun () ->
  (* FIXME: use config value ?*)
  Store.init ~mapsize:4_096_000_000_000L store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let block_store = Store.Block.get chain_store in

  State.init
    ~context_root ~store_root ~history_mode:Rolling genesis
  >>=? fun (_state, chain_state, context_index, _history_mode) ->

  (* Restore context *)
  Tezos_storage.Context.restore_contexts context_index ~filename >>=? fun restored_data ->

  (* Process data imported from snapshot *)
  Error_monad.iter_s begin fun ((predecessor_block_header : Block_header.t), meta, old_blocks) ->
    let ({ block_header ; operations } :
           Tezos_storage.Context.Block_data.t) = meta in
    let block_hash = Block_header.hash block_header in
    Store.Block.Contents.known (block_store,block_hash) >>= fun known ->
    if known then
      lwt_log_notice "Skipping already known block %a"
        Block_hash.pp block_hash >>= return
    else
      begin
        lwt_log_notice "Importing block %a"
          Block_hash.pp (Block_header.hash block_header) >>= fun () ->
        (* To validate block_header we need … *)
        (* … its predecessor context … *)
        let pred_context_hash = predecessor_block_header.shell.context in
        Tezos_storage.Context.checkout_exn
          context_index
          pred_context_hash >>= fun predecessor_context ->

        (* … we can now call apply … *)
        Tezos_validation.Block_validation.apply
          chain_id
          ~max_operations_ttl:(max_int-1)
          ~predecessor_block_header:predecessor_block_header
          ~predecessor_context
          ~block_header
          operations >>=? fun block_validation_result ->

        (* … and we expect to match the context_hash …*)
        (* FIXME : Raise a nice exception *)
        fail_when
          (not (Context_hash.equal
                  block_validation_result.context_hash
                  block_header.shell.context))
          (failure "Resulting context hash does not match") >>=? fun () ->

        (* … we check that the max_operations_ttl blocks are chained …*)
        let check_limit =
          Int32.(sub
                   block_header.shell.level
                   (of_int block_validation_result.validation_result.max_operations_ttl)) in
        let rec chain_check (l:Tezos_storage.Context.Pruned_block.t list) limit =
          match l with
          | hd1 :: hd2 :: tl ->
              if hd1.block_header.shell.level = limit then
                return_unit
              else
                begin
                  let expected = hd1.block_header.shell.predecessor in
                  let computed = Block_header.hash hd2.block_header in
                  if Block_hash.equal expected computed then
                    chain_check (hd2::tl) limit
                  else
                    (*FIXME : raise nice exception*)
                    failwith "Error in chain found %a, expected %a"
                      Block_hash.pp computed Block_hash.pp expected
                end
          | hd :: [] ->
              if hd.block_header.shell.level = limit then
                return_unit
              else
                (*FIXME : raise nice exception*)
                failwith
                  "Error not enough blocks found to ensure valid imported block"
          | [] -> assert false
        in
        chain_check old_blocks check_limit >>=? fun () ->

        let operation_checks ops ops_h bh_op_h =
          (* Compute operations hashes and compare*)
          List.iter2
            (fun (_,op) (_,oph) ->
               let expeced_op_hash = List.map Operation.hash op in
               List.iter2 (fun excpected found ->
                   (* FIXME: raise nice expection *)
                   assert (Operation_hash.equal excpected found)
                 ) expeced_op_hash oph;
            )
            ops ops_h;
          (* Check header hashes based on merkel tree*)
          Lwt_list.map_p (fun (_,opl) ->
              Lwt_list.map_p (fun op ->
                  let op_hash = Operation.hash op in
                  Lwt.return op_hash) opl)
            (List.rev ops) >>= fun hashes ->
          let computed_hash =
            Operation_list_list_hash.compute
              (List.map Operation_list_hash.compute hashes) in
          (* FIXME: raise nice expection *)
          assert
            (Operation_list_list_hash.equal computed_hash bh_op_h);
          Lwt.return_unit
        in

        (* … we check prunde blocks validity …*)
        let check_limit =
          Int32.(sub
                   block_header.shell.level
                   (of_int block_validation_result.validation_result.max_operations_ttl)) in
        let rec chain_check (l:Tezos_storage.Context.Pruned_block.t list) limit =
          match l with
          | hd1 :: _ when hd1.block_header.shell.level = limit ->
              return_unit
          | hd1 :: hd2 :: tl ->
              (* Checking operations consistency *)
              let ({block_header; operations ; operation_hashes ; _} :
                     Tezos_storage.Context.Pruned_block.t) = hd1 in
              operation_checks
                operations
                operation_hashes
                block_header.shell.operations_hash
              >>= fun () ->
              begin
                let expected = hd1.block_header.shell.predecessor in
                let computed = Block_header.hash hd2.block_header in
                if Block_hash.equal expected computed then
                  chain_check (hd2::tl) limit
                else
                  (*FIXME : raise nice exception*)
                  failwith "Error in chain found %a, expected %a"
                    Block_hash.pp computed Block_hash.pp expected
              end
          | _ ->
              (*FIXME : raise nice exception*)
              failwith
                "Failed to validate imported block"
        in
        chain_check old_blocks check_limit >>=? fun () ->

        (* … and we write data in store.*)
        let nb_blocks = List.length old_blocks in
        let cpt = ref 1 in
        let rec loop_on_chunks blocks =
          let blocks, rest = List.split_n 5000 blocks in
          Store.with_atomic_rw store begin fun () ->
            Lwt_list.iter_s begin fun pruned_block ->
              if Unix.isatty Unix.stderr && !cpt mod 1000 = 0 then
                Format.eprintf "Storing blocks: %iK/%iK%!\
                                \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\
                                \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
                  (!cpt / 1000)
                  (nb_blocks / 1000);
              incr cpt;
              let ({block_header; operations ; operation_hashes ; predecessors } :
                     Tezos_storage.Context.Pruned_block.t) = pruned_block in
              let pruned_block_hash = Block_header.hash block_header in
              Store.Block.Header.store
                (block_store, Block_header.hash block_header) block_header >>= fun () ->
              Lwt_list.iter_s
                (fun (i,v) -> Store.Block.Operations.store (block_store,pruned_block_hash) i v)
                operations >>= fun () ->
              Lwt_list.iter_s
                (fun (i,v) -> Store.Block.Operation_hashes.store (block_store,pruned_block_hash) i v)
                operation_hashes >>= fun () ->
              Lwt_list.iter_s
                (fun (l,h) -> Store.Block.Predecessors.store (block_store,pruned_block_hash) l h)
                predecessors >>= fun () ->
              Lwt.return_unit
            end
              blocks
          end >>= fun () ->
          if rest = [] then Lwt.return ()
          else loop_on_chunks rest in
        loop_on_chunks old_blocks >>= fun () ->
        if Unix.isatty Unix.stderr then Format.eprintf "@." ;

        let chain_data = Store.Chain_data.get chain_store in

        (* Prepare the new head to be stored *)
        let block_metadata = block_validation_result.block_metadata in
        let ops_metadata = block_validation_result.ops_metadata in
        let validation_result = block_validation_result.validation_result in
        let validation_store = ({
            context_hash = block_validation_result.context_hash ;
            message = validation_result.message ;
            max_operations_ttl = validation_result.max_operations_ttl ;
            last_allowed_fork_level = validation_result.last_allowed_fork_level ;
          } : State.Block.validation_store ) in
        State.Block.store
          chain_state
          block_header
          block_metadata
          operations
          ops_metadata
          validation_store >>=? fun new_head ->

        begin match new_head with
          | None ->
              (*FIXME : raise nice exception*)
              Lwt.fail_with "Failed to store block (already known)"
          | Some new_head ->
              (* New head is set*)
              Store.Chain_data.Known_heads.store chain_data (State.Block.hash new_head) >>= fun () ->
              Store.Chain_data.Current_head.store chain_data (State.Block.hash new_head)
        end  >>= fun _old_head ->

        (* Imported block is set as the current checkpoint/save_point … *)
        let new_checkpoint = (block_header.shell.level, block_hash) in
        State.Chain.set_checkpoint chain_state block_header >>= fun () ->
        State.update_chain_data chain_state begin fun _ data ->
          let new_data = { data with save_point = new_checkpoint ; } in
          Lwt.return (Some new_data, ())
        end >>= fun () ->

        Store.Chain_data.Save_point.store chain_data new_checkpoint >>= fun () ->
        let rock_bottom_level =
          Int32.(sub
                   block_header.shell.level
                   (of_int validation_result.max_operations_ttl)) in
        State.Block.read_exn chain_state block_hash >>= fun rock_bottom_block ->
        State.Block.predecessor_n
          ~below_save_point:true
          rock_bottom_block
          validation_result.max_operations_ttl >>= fun rock_bottom ->
        (* FIXME: raise a nice exception *)
        let rock_bottom_hash =
          Option.unopt_exn
            (Failure "Failed to read rock bottom block")
            rock_bottom in
        Store.Chain_data.Rock_bottom.store
          chain_data
          (rock_bottom_level, rock_bottom_hash) >>= fun () ->
        return_unit
      end
  end
    restored_data >>=? fun () ->
  Store.close store;
  lwt_log_notice "Sucessfull import (from file %s)" filename >>= fun () ->
  return_unit

(** Main *)

module Term = struct

  type subcommand = Export | Import

  let process subcommand config_file file blocks export_full =
    let res =
      match subcommand with
      | Export -> export ~export_full config_file file blocks
      | Import -> import config_file file
    in
    match Lwt_main.run res with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "export" -> `Ok Export
      | "import" -> `Ok Import
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Export -> Format.fprintf ppf "export"
      | Import -> Format.fprintf ppf "import"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. \
       Possible values: $(b,export), $(b,import)." in
    required & pos 0 (some (parser, printer)) None & info [] ~docv:"OPERATION" ~doc

  let file_arg =
    let open Cmdliner.Arg in
    required & pos 1 (some string) None & info [] ~docv:"FILE"

  let blocks =
    let open Cmdliner.Arg in
    let doc ="Block hash of the block to export." in
    value & opt_all string [] & info ~docv:"OPTION" ~doc ["block"]

  let export_full =
    let open Cmdliner in
    let doc =
      "Force export command to dump all blocks known until rock bottom." in
    Arg.(value & flag &
         info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["full"])

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg
         $ Node_shared_arg.Term.data_dir
         $ file_arg
         $ blocks
         $ export_full)

end

module Manpage = struct

  let command_description =
    "The $(b,snapshot) command ... ."

  let description = [
    `S "DESCRIPTION" ;
    `P (command_description ^ " Several operations are possible: ");
    `P "$(b,export) ... " ;
    `P "$(b,import) ... " ;
  ]

  let options = [
    `S "OPTIONS" ;
  ]

  let man =
    description @
    options @
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Manage snapshots"
      ~man
      "snapshot"

end

let cmd =
  Term.term, Manpage.info
