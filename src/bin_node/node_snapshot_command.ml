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

let export data_dir filename commits =
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
  begin if commits <> [] then
      Lwt.return (List.map Block_hash.of_b58check_exn commits)
    else
      Store.Chain_data.Current_head.read_exn chain_data_store >>= fun head ->
      Store.Block.Predecessors.read_exn (block_store, head) 6 >>= fun sixteenth_pred ->
      lwt_log_notice "No block hash specified with the `--block` option. Using %a by default (64th predecessor from the current head)"
        Block_hash.pp sixteenth_pred >>= fun () ->
      Lwt.return [ sixteenth_pred ]
  end >>= fun commits ->
  Error_monad.filter_map_p begin fun commit_block_hash ->
    Store.Block.Header.read_opt (block_store, commit_block_hash) >>= function
    | None ->
        lwt_log_notice "Skipping unknown block %a"
          Block_hash.pp commit_block_hash >>= fun () ->
        return_none
    | Some commit_block_header ->
        lwt_log_notice "Dumping: %a"
          Block_hash.pp commit_block_hash >>= fun () ->

        (* Get branch precessor's block header*)
        Store.Block.Predecessors.read
          (block_store, commit_block_hash) 0 >>=? fun pred_block_hash ->
        Store.Block.Header.read
          (block_store, pred_block_hash) >>=? fun pred_block_header ->

        (* Get branch predecessor's operation list*)
        let validations_passes = pred_block_header.shell.validation_passes in
        Error_monad.map_s
          (fun i -> Store.Block.Operations.read (block_store, commit_block_hash) i)
          (0 -- (validations_passes - 1)) >>=? fun operations ->

        (* Get branch predecessor's content *)
        Store.Block.Contents.read
          (block_store, pred_block_hash) >>=? fun pred_block_content ->
        (* Get the max_ttls previous block headers and their operation hashes*)
        let max_op_ttl = pred_block_content.max_operations_ttl in
        (*TODO: Raise a nice exception ?*)
        if (max_op_ttl < 60) then
          failwith "Number of confirmations for block %a is too low (%i < 60)"
            Block_hash.pp commit_block_hash max_op_ttl
        else return_unit >>=? fun () ->

          (* Load all data needed for previous blocks *)
          (* that is to say [pred :: max_ttl_blocks] *)
          let rec load_pruned bh n acc =
            if n = max_op_ttl + 1 then return acc
            else
              Store.Block.Predecessors.read (block_store, bh) 0 >>=? fun pbh ->
              (* Get header *)
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
                } : Tezos_storage.Context.Block_data.pruned_block ) in
              load_pruned pbh (n + 1) (acc @ [pruned_block])
          in
          load_pruned commit_block_hash 0 [] >>=? fun old_pruned_blocks ->

          let block_data =
            ({block_header = commit_block_header ;
              operations ;
              old_blocks = old_pruned_blocks } : Tezos_storage.Context.Block_data.t ) in
          return (Some (pred_block_header, block_data))
  end
    commits >>=? fun data_to_dump ->
  Store.close store;
  Tezos_storage.Context.init ~readonly:true context_root
  >>= fun context_index ->
  Tezos_storage.Context.dump_contexts
    context_index
    data_to_dump
    ~filename >>=? fun () ->
  lwt_log_notice "Sucessful export (in file %s)" filename >>= fun () ->
  return_unit

let patch_context json ctxt =
  begin
    match json with
    | None -> Lwt.return ctxt
    | Some json ->
        Tezos_storage.Context.set ctxt
          ["sandbox_parameter"]
          (Data_encoding.Binary.to_bytes_exn Data_encoding.json json)
  end >>= fun ctxt ->
  let module Proto = (val Registered_protocol.get_exn genesis.protocol) in
  Proto.init ctxt {
    level = 0l ;
    proto_level = 0 ;
    predecessor = genesis.block ;
    timestamp = genesis.time ;
    validation_passes = 0 ;
    operations_hash = Operation_list_list_hash.empty ;
    fitness = [] ;
    context = Context_hash.zero ;
  } >>= function
  | Error _ -> assert false (* FIXME error *)
  | Ok { context = ctxt ; _ } ->
      Lwt.return ctxt

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
  Store.init store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let block_store = Store.Block.get chain_store in

  (* TODO : Check if really needed *)
  let patch_context = Some (patch_context None) in

  State.init ~context_root ~store_root ~history_mode:Rolling ?patch_context genesis
  >>=? fun (_state, chain_state, context_index, _history_mode) ->
  Printf.printf "State.init OK\n%!";

  (* Restore context *)
  Tezos_storage.Context.restore_contexts context_index ~filename >>=? fun restored_data ->

  (* Process data imported from snapshot *)
  Error_monad.iter_s begin fun ((predecessor_block_header : Block_header.t), meta) ->
    let ({ block_header ; operations ; old_blocks } :
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

        let pruned_block_pred, pruned_max_ttl_blocks = List.hd old_blocks, List.tl old_blocks in

        (* … to check that the max_operations_ttl blocks are chained …*)
        (* FIXME*)
        let max_operations_ttl = 60 in
        (*TODO: Raise a nice exception*)
        assert (List.length pruned_max_ttl_blocks >= max_operations_ttl);
        let rec chain_check n (l:Tezos_storage.Context.Block_data.pruned_block list) =
          match l with
          | hd1 :: hd2 :: tl ->
              let expected = hd1.block_header.shell.predecessor in
              let found = Block_header.hash hd2.block_header in
              if Block_hash.equal expected found then
                chain_check (n+1) (hd2::tl)
              else
                failwith "Error in chain found %a, expected %a"
                  Block_hash.pp found Block_hash.pp expected
          | _ :: [] ->
              return_unit
          | [] -> assert false
        in
        chain_check 0 (pruned_block_pred :: pruned_max_ttl_blocks) >>=? fun () ->

        Format.printf "Chained_check : Ok\n%!";

        (* … we can now call apply … *)
        Tezos_validation.Block_validation.apply
          chain_id
          ~max_operations_ttl
          ~predecessor_block_header:predecessor_block_header
          ~predecessor_context
          ~block_header
          operations >>=? fun block_validation_result ->

        (* … and we expect to match the context_hash …*)
        (* TODO: Raise a nice exception *)
        fail_when
          (not (Context_hash.equal
                  block_validation_result.context_hash
                  block_header.shell.context))
          (failure "Resulting context hash does not match") >>=? fun () ->

        Format.printf "Apply: Ok\n%!";

        (* … and we write data in store.*)
        Lwt_list.iter_s begin fun pruned_block ->
          let ({block_header; operations ; operation_hashes ; predecessors } :
                 Tezos_storage.Context.Block_data.pruned_block) = pruned_block in
          let pruned_block_hash = Block_header.hash block_header in
          (* Headers …*)
          Store.Block.Header.store
            (block_store, Block_header.hash block_header) block_header >>= fun () ->
          (* Operations …*)
          Lwt_list.iter_s
            (fun (i,v) -> Store.Block.Operations.store (block_store,pruned_block_hash) i v)
            operations >>= fun () ->
          (* Operation_hashes …*)
          Lwt_list.iter_s
            (fun (i,v) -> Store.Block.Operation_hashes.store (block_store,pruned_block_hash) i v)
            operation_hashes >>= fun () ->
          (* and predecessors *)
          Lwt_list.iter_s
            (fun (l,h) -> Store.Block.Predecessors.store (block_store,pruned_block_hash) l h)
            predecessors >>= fun () ->
          Lwt.return_unit
        end
          (pruned_block_pred :: pruned_max_ttl_blocks) >>=fun () ->

        Format.printf "Storing all imported data: Ok\n%!";

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
          | None -> Lwt.fail_with "Failed to store block (already known)"
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
        let rock_bottom_level = Int32.sub block_header.shell.level (Int32.of_int validation_result.max_operations_ttl) in
        State.Block.read_exn chain_state block_hash >>= fun block ->
        State.Block.predecessor_n block (validation_result.max_operations_ttl - 1) >>= fun rock_bottom ->
        let rock_bottom_hash = Option.unopt_exn (Failure "rock bottom not in snapshot") rock_bottom in
        Store.Chain_data.Rock_bottom.store chain_data (rock_bottom_level, rock_bottom_hash) >>= fun () ->
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

  let process subcommand config_file file commits =
    let res =
      match subcommand with
      | Export -> export config_file file commits
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

  let commit =
    let open Cmdliner.Arg in
    let doc ="Commit to export" in
    value & opt_all string [] & info ~docv:"OPTION" ~doc ["commit"]

  let term =
    let open Cmdliner.Term in
    ret (const process $ subcommand_arg
         $ Node_shared_arg.Term.data_dir
         $ file_arg
         $ commit)

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
