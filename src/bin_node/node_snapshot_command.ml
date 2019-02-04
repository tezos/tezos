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

let export ?(export_rolling=false) data_dir filename blocks =
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
  Store.Configuration.History_mode.read_exn store >>= fun history_mode ->
  begin match history_mode with
    | Archive | Full -> return_unit
    | Rolling ->
        if export_rolling then return_unit else
          failwith "cannot export a full snapshot from a %a node"
            History_mode.pp history_mode
  end >>=? fun () ->
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
          if not export_rolling then
            Store.Chain_data.Rock_bottom.read chain_data_store >>=? fun (rb_level,_) ->
            return rb_level
          else
            return (Int32.(sub block_header.shell.level (of_int max_op_ttl)))
        end >>=? fun export_limit ->
        let export_limit = max 1l export_limit (* never include genesis *) in
        let cpt = ref 0 in
        let rec load_pruned (bh : Block_header.t) acc limit =
          if Unix.isatty Unix.stderr && !cpt mod 1000 = 0 then
            Format.eprintf "Retrieving history: %iK/%iK blocks%!\
                            \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\
                            \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
              (!cpt / 1000)
              ((!cpt + (Int32.to_int bh.shell.level - Int32.to_int limit)) / 1000);
          incr cpt;
          if bh.shell.level < limit then
            return acc
          else
            let pbh = bh.shell.predecessor in
            Store.Block.Header.read (block_store, pbh) >>=? fun pbhd ->
            (* Get operations *)
            Store.Block.Operations.bindings (block_store, pbh) >>= fun operations ->
            (* Get operation hashes *)
            Store.Block.Operation_hashes.bindings (block_store, pbh) >>= fun operation_hashes ->
            let pruned_block = ({
                block_header = pbhd ;
                operations ;
                operation_hashes ;
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
  let chain_data = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in

  State.init
    ~context_root ~store_root ~history_mode:Rolling genesis
  >>=? fun (_state, chain_state, context_index, _history_mode) ->

  let open Tezos_storage.Context in

  (* Restore context *)
  restore_contexts context_index ~filename >>=? fun restored_data ->

  (* Process data imported from snapshot *)
  Error_monad.iter_s begin fun ((predecessor_block_header : Block_header.t), meta, old_blocks) ->
    let ({ block_header ; operations } :
           Block_data.t) = meta in
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
        checkout_exn
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
        fail_when
          (not (Context_hash.equal
                  block_validation_result.context_hash
                  block_header.shell.context))
          (failure "Resulting context hash does not match") >>=? fun () ->

        (* … we check the history and compute the predecessor tables …*)
        let operation_checks ({block_header = bh; operations = ops ; operation_hashes = ops_h ; _} : Pruned_block.t) =
          (* Compute operations hashes and compare*)
          List.iter2
            (fun (_,op) (_,oph) ->
               let expeced_op_hash = List.map Operation.hash op in
               List.iter2 (fun excpected found ->
                   assert (Operation_hash.equal excpected found)
                 ) expeced_op_hash oph;
            )
            ops ops_h;
          (* Check header hashes based on merkel tree*)
          let hashes = List.map (fun (_,opl) ->
              List.map Operation.hash opl)
              (List.rev ops) in
          let computed_hash =
            Operation_list_list_hash.compute
              (List.map Operation_list_hash.compute hashes) in
          assert (Operation_list_list_hash.equal computed_hash bh.Block_header.shell.operations_hash) ;
        in

        lwt_log_notice "Checking history consistency" >>= fun () ->

        let old_blocks = List.rev_map (fun pruned_block ->
            let hash = Block_header.hash pruned_block.Pruned_block.block_header in
            (hash, pruned_block))
            old_blocks in
        let history = Array.of_list old_blocks in
        let nb_blocks = Array.length history in
        assert (Block_hash.equal block_header.shell.predecessor (fst history.(nb_blocks - 1))) ;
        let oldest_header = (snd history.(0)).block_header in
        let oldest_level = oldest_header.shell.level in
        assert (oldest_level >= 1l) ;
        if Compare.Int32.(oldest_level = 1l) then
          assert (Block_hash.equal oldest_header.shell.predecessor genesis.block) ;
        operation_checks (snd history.(0)) ;
        for i = nb_blocks - 1 downto 1 do
          operation_checks (snd history.(i)) ;
          let ({block_header; _} : Pruned_block.t) = snd history.(i) in
          assert (block_header.shell.level >= 2l) ;
          assert (Block_hash.equal block_header.shell.predecessor (fst history.(i - 1))) ;
        done ;

        lwt_log_notice "Computing predecessor tables" >>= fun () ->

        let predecessors =
          Array.init nb_blocks @@ fun i ->
          let rec step s d acc =
            if oldest_level = 1l && i - d = -1 then
              List.rev ((s, genesis.block) :: acc)
            else if i - d < 0 then
              List.rev acc
            else
              step (succ s) (d * 2) ((s, fst history.(i - d)) :: acc) in
          step 0 1 [] in

        (* … we set the history mode to full if it looks like a full snapshot … *)
        begin if (snd history.(0)).Pruned_block.block_header.shell.level = 1l then
            lwt_log_notice "Setting history-mode to %a" History_mode.pp Full >>= fun () ->
            Store.Configuration.History_mode.store store Full
          else
            lwt_log_notice "Setting history-mode to %a" History_mode.pp Rolling >>= fun () ->
            Lwt.return ()
        end >>= fun () ->

        (* … and we write data in store.*)
        let rec loop_on_chunks cpt =
          Store.with_atomic_rw store begin fun () ->
            let rec loop_on_chunk cpt =
              if cpt = nb_blocks then Lwt.return cpt else begin
                if Unix.isatty Unix.stderr && cpt mod 1000 = 0 then
                  Format.eprintf "Storing blocks: %iK/%iK%!\
                                  \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\
                                  \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
                    (cpt / 1000)
                    (nb_blocks / 1000);
                let pruned_block_hash, {Pruned_block.block_header ; operations ; operation_hashes } = history.(cpt) in
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
                  predecessors.(cpt) >>= fun () ->
                begin match predecessors.(cpt) with
                  | (0, pred_hash) :: _ ->
                      Store.Chain_data.In_main_branch.store (chain_data, pred_hash) pruned_block_hash
                  | [] -> Lwt.return_unit
                  | _ :: _ -> assert false
                end >>= fun () ->
                loop_on_chunk (succ cpt)
              end in
            if (succ cpt) mod 5000 = 0 then Lwt.return cpt else
              loop_on_chunk cpt end >>= fun cpt ->
          if cpt = nb_blocks then Lwt.return () else
            loop_on_chunks cpt in
        loop_on_chunks 0 >>= fun () ->
        if Unix.isatty Unix.stderr then Format.eprintf "@." ;

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
              Store.Chain_data.Known_heads.remove chain_data genesis.block >>= fun () ->
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
          if oldest_level = 1l then 0l else oldest_level in
        let rock_bottom_hash =
          if oldest_level = 1l then genesis.block else Block_header.hash oldest_header in
        let minimal_rock_bottom_level =
          Int32.(sub
                   block_header.shell.level
                   (of_int validation_result.max_operations_ttl)) in
        assert Compare.Int32.(rock_bottom_level <= minimal_rock_bottom_level) ;
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

  let process subcommand config_file file blocks export_rolling =
    let res =
      match subcommand with
      | Export -> export ~export_rolling config_file file blocks
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
