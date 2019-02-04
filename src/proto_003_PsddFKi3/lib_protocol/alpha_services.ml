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

open Alpha_context

let custom_root = RPC_path.open_root

module Seed = struct

  module S = struct

    open Data_encoding

    let seed =
      RPC_service.post_service
        ~description: "Seed of the cycle to which the block belongs."
        ~query: RPC_query.empty
        ~input: empty
        ~output: Seed.seed_encoding
        RPC_path.(custom_root / "context" / "seed")

  end

  let () =
    let open Services_registration in
    register0 S.seed begin fun ctxt () () ->
      let l = Level.current ctxt in
      Seed.for_cycle ctxt l.cycle
    end


  let get ctxt block =
    RPC_context.make_call0 S.seed ctxt block () ()

end

module Nonce = struct

  type info =
    | Revealed of Nonce.t
    | Missing of Nonce_hash.t
    | Forgotten

  let info_encoding =
    let open Data_encoding in
    union [
      case (Tag 0)
        ~title:"Revealed"
        (obj1 (req "nonce" Nonce.encoding))
        (function Revealed nonce -> Some nonce | _ -> None)
        (fun nonce -> Revealed nonce) ;
      case (Tag 1)
        ~title:"Missing"
        (obj1 (req "hash" Nonce_hash.encoding))
        (function Missing nonce -> Some nonce | _ -> None)
        (fun nonce -> Missing nonce) ;
      case (Tag 2)
        ~title:"Forgotten"
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
        RPC_path.(custom_root / "context" / "nonces" /: Raw_level.rpc_arg)

  end

  let register () =
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
module Voting = Voting_services

let register () =
  Contract.register () ;
  Constants.register () ;
  Delegate.register () ;
  Helpers.register () ;
  Nonce.register () ;
  Voting.register ()
