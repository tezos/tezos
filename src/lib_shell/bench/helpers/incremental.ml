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
  header: Block_header.t ;
  delegate: Account.t ;
}
type incremental = t

let predecessor { predecessor ; _ } = predecessor

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

let begin_construction ?(priority=0) ?timestamp ?seed_nonce_hash (predecessor : Block.t) =
  Block.get_next_baker ~policy:(Block.By_priority priority)
    predecessor >>=? fun (delegate, priority, real_timestamp) ->
  Account.find delegate >>=? fun delegate ->
  let timestamp = Option.unopt ~default:real_timestamp timestamp in
  let contents = Block.Forge.contents ~priority ?seed_nonce_hash () in
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
    ~chain_id:Chain_id.zero
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
    header ;
    delegate ;
  }

let add_operation st op =
  M.apply_operation st.state op >>=? fun (state, _result) ->
  return { st with state ; rev_operations = op :: st.rev_operations }

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
        operations_hash ; fitness = result.fitness ;
        level = Int32.succ st.header.shell.level
      } } in
  let hash = Block_header.hash header in
  return {
    Block.hash ;
    header ;
    operations ;
    context = result.context ;
  }
