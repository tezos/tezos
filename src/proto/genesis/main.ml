(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
let max_operation_data_length = 0
let parse_operation _h _op = Error []
let compare_operations _ _ = 0
let max_number_of_operations = 0

type block = {
  shell: Block_header.shell_header ;
  command: Data.Command.t ;
  signature: Ed25519.Signature.t ;
}

let max_block_length =
  Data_encoding.Binary.length
    Data.Command.encoding
    (Activate_testnet (Protocol_hash.hash_bytes [], 0L))
  +
  begin
    match Data_encoding.Binary.fixed_length Ed25519.Signature.encoding with
    | None -> assert false
    | Some len -> len
  end

let parse_block { Block_header.shell ; proto } : block tzresult =
  match Data_encoding.Binary.of_bytes Data.Command.signed_encoding proto with
  | None -> Error [Parsing_error]
  | Some (command, signature) -> Ok { shell ; command ; signature }

let check_signature ctxt { shell ; command ; signature } =
  let bytes = Data.Command.forge shell command in
  Data.Pubkey.get_pubkey ctxt >>= fun public_key ->
  fail_unless
    (Ed25519.Signature.check public_key signature bytes)
    Invalid_signature

type validation_state = Updater.validation_result

let current_context ({ context } : validation_state) =
  return context

let precheck_block
    ~ancestor_context:_
    ~ancestor_timestamp:_
    raw_block =
  Lwt.return (parse_block raw_block) >>=? fun _ ->
  return ()

let begin_application
    ~predecessor_context:ctxt
    ~predecessor_timestamp:_
    ~predecessor_fitness:_
    raw_block =
  Data.Init.may_initialize ctxt >>=? fun ctxt ->
  Lwt.return (parse_block raw_block) >>=? fun block ->
  check_signature ctxt block >>=? fun () ->
  let fitness = raw_block.shell.fitness in
  match block.command with
  | Data.Command.Activate hash ->
      let message =
        Some (Format.asprintf "activate %a" Protocol_hash.pp_short hash) in
      Updater.activate ctxt hash >>= fun ctxt ->
      return { Updater.message ; context = ctxt ;
               fitness ; max_operations_ttl = 0 }
  | Activate_testnet (hash, delay) ->
      let message =
        Some (Format.asprintf "activate testnet %a" Protocol_hash.pp_short hash) in
      let expiration = Time.add raw_block.shell.timestamp delay in
      Updater.fork_test_network ctxt hash expiration >>= fun ctxt ->
      return { Updater.message ; context = ctxt ; fitness ;
               max_operations_ttl = 0 }

let begin_construction
    ~predecessor_context:context
    ~predecessor_timestamp:_
    ~predecessor_level:_
    ~predecessor_fitness:fitness
    ~predecessor:_
    ~timestamp:_
    ?proto_header:_
    () =
  (* Dummy result. *)
  return { Updater.message = None ; context ;
           fitness ; max_operations_ttl = 0 }

let apply_operation _vctxt _ =
  Lwt.return (Error []) (* absurd *)

let finalize_block state = return state

let rpc_services = Services.rpc_services

let configure_sandbox = Data.Init.configure_sandbox
