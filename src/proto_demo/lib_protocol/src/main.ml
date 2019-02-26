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


type block_header_data = MBytes.t
type block_header = {
  shell : Block_header.shell_header ;
  protocol_data : block_header_data ;
}

let block_header_data_encoding =
  Data_encoding.(obj1 (req "random_data" Variable.bytes))

type block_header_metadata = unit
let block_header_metadata_encoding = Data_encoding.unit

type operation_data = unit
let operation_data_encoding = Data_encoding.unit

type operation_receipt = unit
let operation_receipt_encoding = Data_encoding.unit

let operation_data_and_receipt_encoding =
  Data_encoding.conv
    (function ((), ()) -> ())
    (fun () -> ((), ()))
    Data_encoding.unit

type operation = {
  shell: Operation.shell_header ;
  protocol_data: operation_data ;
}

let max_block_length = 42
let max_operation_data_length = 0
let validation_passes = []
let acceptable_passes _op = []

let compare_operations _ _ = 0

type validation_state = {
  context : Context.t ;
  fitness : Fitness.t ;
}

let current_context { context ; _ } =
  return context

let begin_application
    ~chain_id:_
    ~predecessor_context:context
    ~predecessor_timestamp:_
    ~predecessor_fitness:_
    (raw_block: block_header) =
  return { context ; fitness = raw_block.shell.fitness }

let begin_partial_application
    ~chain_id
    ~ancestor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    block_header =
  begin_application
    ~chain_id
    ~predecessor_context:ancestor_context
    ~predecessor_timestamp
    ~predecessor_fitness
    block_header

let begin_construction
    ~chain_id:_
    ~predecessor_context:context
    ~predecessor_timestamp:_
    ~predecessor_level:_
    ~predecessor_fitness:pred_fitness
    ~predecessor:_
    ~timestamp:_
    ?protocol_data:_ () =

  let increase_fitness = function
    | [ v ; b ] ->
        let f = MBytes.get_int64 b 0 in
        let b' = MBytes.copy b in
        MBytes.set_int64 b' 0 (Int64.succ f) ;
        return [ v ;  b' ]
    | [ ] -> return MBytes.[create 0; create 0]
    | _ -> assert false
  in
  increase_fitness pred_fitness >>=? fun fitness ->
  return { context ; fitness }

let apply_operation ctxt _ =
  return (ctxt, ())

let finalize_block ctxt =
  let fitness = ctxt.fitness in
  let message = Some (Format.asprintf "fitness <- %a" Fitness.pp fitness) in
  return ({ Updater.message ; context = ctxt.context ; fitness ;
            max_operations_ttl = 0 ; last_allowed_fork_level = 0l ;
          }, ())

let init context block_header =
  return { Updater.message = None ; context ;
           fitness = block_header.Block_header.fitness ;
           max_operations_ttl = 0 ;
           last_allowed_fork_level = block_header.level ;
         }

let rpc_services = Services.rpc_services
