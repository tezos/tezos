(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Types

module Init = struct

  let version_key = ["version"]
  (* This key should always be populated for every version of the
     protocol.  It's absence meaning that the context is empty. *)
  let version_value = "genesis"

  (* This is the genesis protocol: initialise the state *)
  let initialize (ctxt:Context.t) =
    Context.set ctxt version_key (MBytes.of_string version_value) >>= fun ctxt ->
    Fitness.set_fitness ctxt 0L >>= fun ctxt ->
    return ctxt

  type error +=
    | Incompatiple_protocol_version
    | Decreasing_fitness

  let may_initialize ctxt init_fitness =
    Context.get ctxt version_key >>= function
    | None ->
        Context.set ctxt version_key (MBytes.of_string version_value) >>= fun ctxt ->
        Fitness.set_fitness ctxt init_fitness >>= fun ctxt ->
        return ctxt
    | Some bytes ->
        let s = MBytes.to_string bytes in
        if Compare.String.(s = version_value) then
          Fitness.get_fitness ctxt >>= fun prev_fitness ->
          if Compare.Int64.(prev_fitness >= init_fitness) then
            fail Decreasing_fitness
          else
            Fitness.set_fitness ctxt init_fitness >>= fun ctxt ->
            return ctxt
        else
          fail Incompatiple_protocol_version
end

let pubkey = "4d5373455738070434f214826d301a1c206780d7f789fcbf94c2149b2e0718cc"

let public_key =
  Ed25519.public_key_of_bytes
    (Bytes.of_string (Hex_encode.hex_decode pubkey))

let validate shell proto signature =
  let header_bytes =
    Data_encoding.Binary.to_bytes
      (Data_encoding.tup2 Updater.shell_block_encoding Block.encoding)
      (shell, proto) in
  Ed25519.check_signature public_key signature header_bytes

type operation = ()
let max_operation_data_length = 0

type block = Block.t
let max_block_length = 1024

let max_number_of_operations = 0

let parse_block { Updater.shell ; proto } =
  match Data_encoding.Binary.of_bytes Block.signed_encoding proto with
  | Some (({ command = Activate ; hash ; fitness } as proto), signature) ->
      if validate shell proto signature then
        ok ({ Block.command = Activate ; hash ; fitness })
      else Error.invalid_signature
  | Some (({ command = Activate_testnet ; hash ; fitness } as proto), signature) ->
      if validate shell proto signature then
        ok ({ Block.command = Activate_testnet ; hash ; fitness })
      else Error.invalid_signature
  | None -> Error.parsing_error

let parse_operation _h _op = Ok ()

let fitness _ctxt = Lwt.return_nil

let apply ctxt { Block.command ; hash ; fitness } _ops =
  Init.may_initialize ctxt fitness >>=? fun ctxt ->
  match command with
  | Activate ->
      Updater.activate ctxt hash >>= fun ctxt ->
      return ctxt
  | Activate_testnet ->
      Updater.set_test_protocol ctxt hash >>= fun ctxt ->
      Updater.fork_test_network ctxt >>= fun ctxt ->
      return ctxt

let preapply ctxt _block_pred _timestamp _sort _ops =
  return ( ctxt,
           { Updater.applied = [] ;
             refused = Operation_hash_map.empty ;
             branch_refused = Operation_hash_map.empty ;
             branch_delayed = Operation_hash_map.empty ;
           })

let rpc_services = Services.rpc_services

let configure_sandbox ctxt _ = Lwt.return (Ok ctxt)
