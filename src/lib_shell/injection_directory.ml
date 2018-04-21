(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let read_chain_id validator chain =
  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.state distributed_db in
  begin
    match chain with
    | None -> Lwt.return_none
    | Some chain ->
        Chain_directory.get_chain_id state chain >>= Lwt.return_some
  end

let inject_block validator ?force ?chain bytes operations =
  read_chain_id validator chain >>= fun chain_id ->
  Validator.validate_block
    validator ?force ?chain_id bytes operations >>=? fun (hash, block) ->
  return (hash, (block >>=? fun _ -> return ()))

let inject_operation validator ?chain bytes =
  read_chain_id validator chain >>= fun chain_id ->
  let t =
    match Data_encoding.Binary.of_bytes Operation.encoding bytes with
    | None -> failwith "Can't parse the operation"
    | Some op ->
        Validator.inject_operation validator ?chain_id op in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_protocol state ?force:_ proto =
  let proto_bytes =
    Data_encoding.Binary.to_bytes_exn Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    Updater.compile hash proto >>= function
    | false ->
        failwith
          "Compilation failed (%a)"
          Protocol_hash.pp_short hash
    | true ->
        State.Protocol.store state proto >>= function
        | None ->
            failwith
              "Previously registered protocol (%a)"
              Protocol_hash.pp_short hash
        | Some _ -> return ()
  in
  Lwt.return (hash, validation)

let build_rpc_directory validator =

  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.state distributed_db in

  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q) in

  register0 Injection_services.S.block begin fun q (raw, operations) ->
    inject_block validator
      ?chain:q#chain ~force:q#force raw operations >>=? fun (hash, wait) ->
    (if q#async then return () else wait) >>=? fun () ->
    return hash
  end ;

  register0 Injection_services.S.operation begin fun q contents ->
    inject_operation validator ?chain:q#chain contents >>= fun (hash, wait) ->
    (if q#async then return () else wait) >>=? fun () ->
    return hash
  end ;

  register0 Injection_services.S.protocol begin fun q protocol ->
    inject_protocol state ~force:q#force protocol >>= fun (hash, wait) ->
    (if q#async then return () else wait) >>=? fun () ->
    return hash
  end ;

  !dir
