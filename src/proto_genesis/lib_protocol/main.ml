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

type error += Parsing_error
type error += Invalid_signature

let () =
  register_error_kind
    `Permanent
    ~id:"parsing_error"
    ~title:"Parsing error"
    ~description:"Raised when a block header has not been parsed correctly"
    ~pp:(fun ppf () -> Format.fprintf ppf "Block header parsing error")
    Data_encoding.empty
    (function Parsing_error -> Some () | _ -> None)
    (fun () -> Parsing_error)

let () =
  register_error_kind
    `Permanent
    ~id:"invalid_signature"
    ~title:"Invalid signature"
    ~description:"Raised when the provided signature is invalid"
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid signature")
    Data_encoding.empty
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature)

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

let acceptable_passes _op = []
let compare_operations _ _ = 0
let validation_passes = []

type block_header_data = {
  command: Data.Command.t ;
  signature: Signature.t ;
}
type block_header = {
  shell: Block_header.shell_header ;
  protocol_data: block_header_data ;
}

let block_header_data_encoding =
  Data_encoding.conv
    (fun { command ; signature } -> (command, signature))
    (fun (command, signature) ->  { command ; signature })
    Data.Command.signed_encoding

type block_header_metadata = unit
let block_header_metadata_encoding = Data_encoding.unit

let max_block_length =
  Data_encoding.Binary.length
    Data.Command.encoding
    (Activate_testchain { protocol = Protocol_hash.zero ;
                          fitness = [ MBytes.create 1 ] ;
                          protocol_parameters = MBytes.create 1 ;
                          delay = 0L })
  + Signature.size

let max_operation_data_length = 0

let check_signature ctxt ~chain_id { shell ; protocol_data = { command ; signature } } =
  let bytes = Data.Command.forge shell command in
  Data.Pubkey.get_pubkey ctxt >>= fun public_key ->
  fail_unless
    (Signature.check ~watermark:(Block_header chain_id) public_key signature bytes)
    Invalid_signature

type validation_state = Updater.validation_result

let current_context ({ context ; _ } : validation_state) =
  return context

(* temporary hardcoded key to be removed... *)
let protocol_parameters_key = [ "protocol_parameters" ]

let prepare_application ctxt command level timestamp _fitness =
  match command with
  | Data.Command.Activate { protocol = hash ; fitness ; protocol_parameters } ->
      let message =
        Some (Format.asprintf "activate %a" Protocol_hash.pp_short hash) in
      Context.set ctxt protocol_parameters_key protocol_parameters >>= fun ctxt ->
      Updater.activate ctxt hash >>= fun ctxt ->
      return { Updater.message ; context = ctxt ;
               fitness ; max_operations_ttl = 0 ;
               last_allowed_fork_level = level ;
             }
  | Activate_testchain { protocol = hash ; fitness ; protocol_parameters ; delay } ->
      let message =
        Some (Format.asprintf "activate testchain %a" Protocol_hash.pp_short hash) in
      Context.set ctxt protocol_parameters_key protocol_parameters >>= fun ctxt ->
      let expiration = Time.add timestamp delay in
      Updater.fork_test_chain ctxt ~protocol:hash ~expiration >>= fun ctxt ->
      return { Updater.message ; context = ctxt ; fitness ;
               max_operations_ttl = 0 ;
               last_allowed_fork_level = level ;
             }

let begin_application
    ~chain_id
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor_fitness:_
    block_header =
  Data.Init.check_inited ctxt >>=? fun () ->
  check_signature ctxt ~chain_id block_header >>=? fun () ->
  prepare_application ctxt block_header.protocol_data.command
    block_header.shell.level block_header.shell.timestamp block_header.shell.fitness

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
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor_level:level
    ~predecessor_fitness:fitness
    ~predecessor:_
    ~timestamp
    ?protocol_data
    () =
  match protocol_data with
  | None ->
      (* Dummy result. *)
      return { Updater.message = None ; context = ctxt ;
               fitness ; max_operations_ttl = 0 ;
               last_allowed_fork_level = 0l ;
             }
  | Some { command ; _ }->
      Data.Init.check_inited ctxt >>=? fun () ->
      prepare_application ctxt command level timestamp fitness

let apply_operation _vctxt _ =
  Lwt.return (Error []) (* absurd *)

let finalize_block state = return (state, ())

let rpc_services = Services.rpc_services

(* temporary hardcoded key to be removed... *)
let sandbox_param_key = [ "sandbox_parameter" ]
let get_sandbox_param ctxt =
  Context.get ctxt sandbox_param_key >>= function
  | None -> return_none
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None ->
          failwith "Internal error: failed to parse the sandbox parameter."
      | Some json -> return_some json

let init ctxt block_header =
  Data.Init.tag_first_block ctxt >>=? fun ctxt ->
  get_sandbox_param ctxt >>=? fun sandbox_param ->
  begin
    match sandbox_param with
    | None -> return ctxt
    | Some json ->
        Data.Pubkey.may_change_default ctxt json >>= fun ctxt ->
        return ctxt
  end >>=? fun ctxt ->
  return { Updater.message = None ; context = ctxt ;
           fitness = block_header.Block_header.fitness ;
           max_operations_ttl = 0 ;
           last_allowed_fork_level = block_header.level ;
         }
