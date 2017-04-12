(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Proto : Protocol.PACKED_PROTOCOL) = struct
  type proto_error = Proto.error
  type Error_monad.error += Ecoproto_error of Proto.error list
  let wrap_error = function
    | Ok _ as ok -> ok
    | Error errors -> Error [Ecoproto_error errors]
  let () =
    let id = Format.asprintf "Ecoproto.%a" Protocol_hash.pp Proto.hash in
    Error_monad.register_wrapped_error_kind
      (fun ecoerrors -> Proto.classify_errors ecoerrors)
      ~id ~title:"Error returned by the protocol"
      ~description:"Wrapped error for the economic protocol."
      ~pp:(fun ppf ->
          Format.fprintf ppf
            "@[<v 2>Economic error:@ %a@]"
            (Format.pp_print_list Proto.pp))
      Data_encoding.(obj1 (req "ecoproto" (list Proto.error_encoding)))
      (function Ecoproto_error ecoerrors -> Some ecoerrors
              | _ -> None )
      (function ecoerrors -> Ecoproto_error ecoerrors)
end

let register (module Proto : Protocol.PACKED_PROTOCOL) =
  let module V = struct
    include Proto
    include Make(Proto)
    let precheck_block
        ~ancestor_context ~ancestor_timestamp
        raw_block =
      precheck_block
        ~ancestor_context ~ancestor_timestamp
        raw_block >|= wrap_error
    let begin_application
        ~predecessor_context ~predecessor_timestamp
        ~predecessor_fitness
        raw_block =
      begin_application
        ~predecessor_context ~predecessor_timestamp
        ~predecessor_fitness
        raw_block >|= wrap_error
    let begin_construction
        ~predecessor_context ~predecessor_timestamp
        ~predecessor_level ~predecessor_fitness
        ~predecessor ~timestamp =
      begin_construction
        ~predecessor_context ~predecessor_timestamp
        ~predecessor_level ~predecessor_fitness
        ~predecessor ~timestamp >|= wrap_error
    let current_context c =
      current_context c >|= wrap_error
    let apply_operation c o =
      apply_operation c o >|= wrap_error
    let finalize_block c = finalize_block c >|= wrap_error
    let parse_operation h b = parse_operation h b |> wrap_error
    let configure_sandbox c j =
      configure_sandbox c j >|= wrap_error
  end in
  Updater.register Proto.hash (module V)
