(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

let custom_root = RPC_path.open_root

module Nonce = struct

  type info =
    | Revealed of Nonce.t
    | Missing of Nonce_hash.t
    | Forgotten

  let info_encoding =
    let open Data_encoding in
    union [
      case (Tag 0)
        (obj1 (req "nonce" Nonce.encoding))
        (function Revealed nonce -> Some nonce | _ -> None)
        (fun nonce -> Revealed nonce) ;
      case (Tag 1)
        (obj1 (req "hash" Nonce_hash.encoding))
        (function Missing nonce -> Some nonce | _ -> None)
        (fun nonce -> Missing nonce) ;
      case (Tag 2)
        empty
        (function Forgotten -> Some () | _ -> None)
        (fun () -> Forgotten) ;
    ]

  module S = struct

    let get =
      RPC_service.get_service
        ~description: "Info about the nonce of a previous block."
        ~query: RPC_query.empty
        ~output: info_encoding
        RPC_path.(custom_root / "context" / "nonces" /: Raw_level.arg)

  end

  let () =
    let open Services_registration in
    register1 S.get begin fun ctxt raw_level () () ->
      let level = Level.from_raw ctxt raw_level in
      Nonce.get ctxt level >>= function
      | Ok (Revealed nonce) -> return (Revealed nonce)
      | Ok (Unrevealed { nonce_hash ; _ }) ->
          return (Missing nonce_hash)
      | Error _ -> return Forgotten
    end

  let get ctxt block level =
    RPC_context.make_call1 S.get ctxt block level () ()

end

module Contract = Contract_services
module Constants = Constants_services
module Delegate = Delegate_services
module Helpers = Helpers_services
module Forge = Helpers_services.Forge
module Parse = Helpers_services.Parse
