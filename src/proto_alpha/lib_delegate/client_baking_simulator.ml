(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type error +=
  | Failed_to_checkout_context

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.failed_to_checkout_context"
    ~title: "Fail during checkout context"
    ~description: ""
    ~pp:(fun ppf () -> Format.fprintf ppf "@[Failed to checkout the context@]")
    Data_encoding.unit
    (function
      | Failed_to_checkout_context -> Some ()
      | _ -> None)
    (fun () -> Failed_to_checkout_context)

type incremental = {
  predecessor: Client_baking_blocks.block_info ;
  context : Context.t ;
  state: LiftedMain.validation_state ;
  rev_operations: Operation.packed list ;
  header: Tezos_base.Block_header.shell_header ;
}

let load_context ~context_path =
  Context.init ~readonly:true context_path

let begin_construction ~timestamp ?protocol_data index predecessor =
  let { Client_baking_blocks.context } = predecessor in
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
        ~chain_id:predecessor.chain_id
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
  LiftedMain.apply_operation st.state op >>=? fun (state, _) ->
  return { st with state ; rev_operations = op :: st.rev_operations }

let finalize_construction inc =
  LiftedMain.finalize_block inc.state
