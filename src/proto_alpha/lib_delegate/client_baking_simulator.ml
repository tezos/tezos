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

type error += Failed_to_checkout_context
type error += Invalid_context

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.failed_to_checkout_context"
    ~title: "Failed to checkout context"
    ~description: "The given context hash does not exists in the context."
    ~pp:(fun ppf () -> Format.fprintf ppf "Failed to checkout the context")
    Data_encoding.unit
    (function
      | Failed_to_checkout_context -> Some ()
      | _ -> None)
    (fun () -> Failed_to_checkout_context) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.invalid_context"
    ~title: "Invalid context"
    ~description: "Occurs when the context is inconsistent."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "The given context is invalid.")
    Data_encoding.unit
    (function
      | Invalid_context -> Some ()
      | _ -> None)
    (fun () -> Invalid_context)

type incremental = {
  predecessor: Client_baking_blocks.block_info ;
  context : Context.t ;
  state: LiftedMain.validation_state ;
  rev_operations: Operation.packed list ;
  header: Tezos_base.Block_header.shell_header ;
}

let load_context ~context_path =
  Context.init ~readonly:true context_path

let check_context_consistency index context_hash =
  (* Hypothesis : the version key exists *)
  let version_key = ["version"] in
  Context.checkout index context_hash >>= function
  | None -> fail Failed_to_checkout_context
  | Some context ->
      Context.mem context version_key >>= function
      | true -> return_unit
      | false -> fail Invalid_context

let begin_construction ~timestamp ?protocol_data index predecessor =
  let { Client_baking_blocks.context ; _ } = predecessor in
  Context.checkout index context >>= function
  | None -> fail Failed_to_checkout_context
  | Some context ->
      let header : Tezos_base.Block_header.shell_header = Tezos_base.Block_header.{
          predecessor = predecessor.hash  ;
          proto_level = predecessor.proto_level ;
          validation_passes = 0 ;
          fitness = predecessor.fitness ;
          timestamp ;
          level = Raw_level.to_int32 predecessor.level ;
          context = Context_hash.zero ;
          operations_hash = Operation_list_list_hash.zero ;
        } in
      LiftedMain.begin_construction
        ~chain_id: predecessor.chain_id
        ~predecessor_context: context
        ~predecessor_timestamp: predecessor.timestamp
        ~predecessor_fitness: predecessor.fitness
        ~predecessor_level: (Raw_level.to_int32 predecessor.level)
        ~predecessor: predecessor.hash
        ?protocol_data
        ~timestamp
        () >>=? fun state ->
      return {
        predecessor ;
        context ;
        state ;
        rev_operations = [] ;
        header ;
      }

let add_operation st ( op : Operation.packed ) =
  LiftedMain.apply_operation st.state op >>=? fun (state, receipt) ->
  return ({ st with state ; rev_operations = op :: st.rev_operations }, receipt)

let finalize_construction inc =
  LiftedMain.finalize_block inc.state
