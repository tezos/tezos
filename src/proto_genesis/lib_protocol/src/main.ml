(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

type operation = unit
let parse_operation _h _op = Error []
let acceptable_passes _op = []
let compare_operations _ _ = 0
let validation_passes = []

type block = {
  shell: Block_header.shell_header ;
  command: Data.Command.t ;
  signature: Ed25519.t ;
}

let max_block_length =
  Data_encoding.Binary.length
    Data.Command.encoding
    (Activate_testchain { protocol = Protocol_hash.zero ;
                          delay = 0L })
  + Ed25519.size

let parse_block { Block_header.shell ; protocol_data } : block tzresult =
  match
    Data_encoding.Binary.of_bytes Data.Command.signed_encoding protocol_data
  with
  | None -> Error [Parsing_error]
  | Some (command, signature) -> Ok { shell ; command ; signature }

let check_signature ctxt { shell ; command ; signature } =
  let bytes = Data.Command.forge shell command in
  Data.Pubkey.get_pubkey ctxt >>= fun public_key ->
  fail_unless
    (Ed25519.check public_key signature bytes)
    Invalid_signature

type validation_state = Updater.validation_result

let current_context ({ context ; _ } : validation_state) =
  return context

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    raw_block =
  Lwt.return (parse_block raw_block) >>=? fun _ ->
  return ()

let prepare_application ctxt command level timestamp fitness =
  match command with
  | Data.Command.Activate { protocol = hash ; fitness } ->
      let message =
        Some (Format.asprintf "activate %a" Protocol_hash.pp_short hash) in
      Updater.activate ctxt hash >>= fun ctxt ->
      return { Updater.message ; context = ctxt ;
               fitness ; max_operations_ttl = 0 ;
               max_operation_data_length = 0 ;
               last_allowed_fork_level = level }
  | Activate_testchain { protocol = hash ; delay } ->
      let message =
        Some (Format.asprintf "activate testchain %a" Protocol_hash.pp_short hash) in
      let expiration = Time.add timestamp delay in
      Updater.fork_test_chain ctxt ~protocol:hash ~expiration >>= fun ctxt ->
      return { Updater.message ; context = ctxt ; fitness ;
               max_operations_ttl = 0 ;
               max_operation_data_length = 0 ;
               last_allowed_fork_level = Int32.succ level ;
             }


let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor_fitness:_
    raw_block =
  Data.Init.check_inited ctxt >>=? fun () ->
  Lwt.return (parse_block raw_block) >>=? fun block ->
  check_signature ctxt block >>=? fun () ->
  prepare_application ctxt block.command
    block.shell.level block.shell.timestamp block.shell.fitness

let begin_construction
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
               max_operation_data_length = 0 ;
               last_allowed_fork_level = 0l ;
             }
  | Some command ->
      match Data_encoding.Binary.of_bytes Data.Command.encoding command with
      | None -> failwith "Failed to parse proto header"
      | Some command ->
          Data.Init.check_inited ctxt >>=? fun () ->
          prepare_application ctxt command level timestamp fitness

let apply_operation _vctxt _ =
  Lwt.return (Error []) (* absurd *)

let finalize_block state = return state

let rpc_services = Services.rpc_services

(* temporary hardcoded key to be removed... *)
let sandbox_param_key = [ "sandbox_parameter" ]
let get_sandbox_param ctxt =
  Context.get ctxt sandbox_param_key >>= function
  | None -> return None
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None ->
          failwith "Internal error: failed to parse the sandbox parameter."
      | Some json -> return (Some json)

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
           max_operation_data_length = 0 ;
           last_allowed_fork_level = block_header.level ;
         }

