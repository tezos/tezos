(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Proto_alpha
open Alpha_context

type t = {
  predecessor: Block.t ;
  state: M.validation_state ;
  rev_operations: Operation.packed list ;
  rev_tickets: operation_receipt list ;
  header: Block_header.t ;
  delegate: Account.t ;
}
type incremental = t

let predecessor { predecessor ; _ } = predecessor
let header { header ; _ } = header
let rev_tickets { rev_tickets ; _ } = rev_tickets
let level st = st.header.shell.level

let rpc_context st =
  let result = Alpha_context.finalize st.state.ctxt in
  {
    Alpha_environment.Updater.block_hash = Block_hash.zero ;
    block_header = { st.header.shell with fitness = result.fitness } ;
    context = result.context ;
  }

let rpc_ctxt =
  new Alpha_environment.proto_rpc_context_of_directory
    rpc_context Proto_alpha.rpc_services

let begin_construction ?(priority=0) ?timestamp
    ?(policy=Block.By_priority priority) (predecessor : Block.t) =
  Block.get_next_baker ~policy
    predecessor >>=? fun (delegate, priority, real_timestamp) ->
  Account.find delegate >>=? fun delegate ->
  let timestamp = Option.unopt ~default:real_timestamp timestamp in
  let contents = Block.Forge.contents ~priority () in
  let protocol_data = {
    Block_header.contents ;
    signature = Signature.zero ;
  } in
  let header = {
    Block_header.shell = {
      predecessor = predecessor.hash ;
      proto_level = predecessor.header.shell.proto_level ;
      validation_passes = predecessor.header.shell.validation_passes ;
      fitness = predecessor.header.shell.fitness ;
      timestamp ;
      level = predecessor.header.shell.level ;
      context = Context_hash.zero ;
      operations_hash = Operation_list_list_hash.zero ;
    } ;
    protocol_data = {
      contents ;
      signature = Signature.zero ;
    } ;
  } in
  M.begin_construction
    ~chain_id: Chain_id.zero
    ~predecessor_context: predecessor.context
    ~predecessor_timestamp: predecessor.header.shell.timestamp
    ~predecessor_fitness: predecessor.header.shell.fitness
    ~predecessor_level: predecessor.header.shell.level
    ~predecessor:predecessor.hash
    ~timestamp
    ~protocol_data
    () >>=? fun state ->
  return {
    predecessor ;
    state ;
    rev_operations = [] ;
    rev_tickets = [] ;
    header ;
    delegate ;
  }

let detect_script_failure :
  type kind. kind Apply_results.operation_metadata -> _ =
  let rec detect_script_failure :
    type kind. kind Apply_results.contents_result_list -> _ =
    let open Apply_results in
    let detect_script_failure_single
        (type kind)
        (Manager_operation_result { operation_result ;
                                    internal_operation_results ; _ }
         : kind Kind.manager Apply_results.contents_result) =
      let detect_script_failure (type kind) (result : kind manager_operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Backtracked (_, None) ->
            (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) ->
            Alpha_environment.wrap_error (Error errs)
        | Failed (_, errs) ->
            Alpha_environment.wrap_error (Error errs) in
      List.fold_left
        (fun acc (Internal_operation_result (_, r)) ->
           acc >>? fun () ->
           detect_script_failure r)
        (detect_script_failure operation_result)
        internal_operation_results in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ ->
        Ok ()
    | Cons_result (res, rest) ->
        detect_script_failure_single res >>? fun () ->
        detect_script_failure rest in
  fun { contents } -> detect_script_failure contents

let add_operation ?expect_failure st op =
  let open Apply_results in
  M.apply_operation st.state op >>=? function
  | state, (Operation_metadata result as metadata) ->
      Lwt.return @@ detect_script_failure result >>= fun result ->
      begin match expect_failure with
        | None ->
            Lwt.return result
        | Some f ->
            match result with
            | Ok _ ->
                failwith "Error expected while adding operation"
            | Error e ->
                f e
      end >>=? fun () ->
      return { st with state ; rev_operations = op :: st.rev_operations ;
                       rev_tickets = metadata :: st.rev_tickets }
  | state, (No_operation_metadata as metadata) ->
      return { st with state ; rev_operations = op :: st.rev_operations ;
                       rev_tickets = metadata :: st.rev_tickets }

let finalize_block st =
  M.finalize_block st.state >>=? fun (result, _) ->
  let operations = List.rev st.rev_operations in
  let operations_hash =
    Operation_list_list_hash.compute [
      Operation_list_hash.compute (List.map Operation.hash_packed operations)
    ] in
  let header =
    { st.header with
      shell = {
        st.header.shell with
        level = Int32.succ st.header.shell.level ;
        operations_hash ; fitness = result.fitness ;
      } } in
  let hash = Block_header.hash header in
  return {
    Block.hash ;
    header ;
    operations ;
    context = result.context ;
  }
