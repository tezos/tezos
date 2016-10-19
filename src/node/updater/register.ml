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
      ~description:"Wrapped error for the economical protocol."
      ~pp:(fun ppf ->
          Format.fprintf ppf
            "@[<v 2>Economical error:@ %a@]"
            (Format.pp_print_list Proto.pp))
      Data_encoding.(obj1 (req "ecoproto" (list Proto.error_encoding)))
      (function Ecoproto_error ecoerrors -> Some ecoerrors
              | _ -> None )
      (function ecoerrors -> Ecoproto_error ecoerrors)
end

let register proto =
  let module Proto = (val Proto_environment.__cast proto) in
  let module V = struct
    include Proto
    include Make(Proto)
    let parse_block d = parse_block d |> wrap_error
    let parse_operation h b = parse_operation h b |> wrap_error
    let apply c h ops = apply c h ops >|= wrap_error
    let preapply c h t b ops =
      (preapply c h t b ops >|= wrap_error) >>=? fun (ctxt, r) ->
      return (ctxt, Updater.map_result (fun l -> [Ecoproto_error l]) r)
    let configure_sandbox c j =
      configure_sandbox c j >|= wrap_error
  end in
  Updater.register Proto.hash (module V)
