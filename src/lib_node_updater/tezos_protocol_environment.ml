(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Param : sig val name: string end)() = struct

  include Pervasives
  module Pervasives = Pervasives
  module Compare = Compare
  module Array = Array
  module List = List
  module Bytes = struct
    include Bytes
    include EndianBytes.BigEndian
    module LE = EndianBytes.LittleEndian
  end
  module String = struct
    include String
    include EndianString.BigEndian
    module LE = EndianString.LittleEndian
  end
  module Set = Set
  module Map = Map
  module Int32 = Int32
  module Int64 = Int64
  module Nativeint = Nativeint
  module Buffer = Buffer
  module Format = Format
  module Z = Z
  module Lwt_sequence = Lwt_sequence
  module Lwt = Lwt
  module Lwt_list = Lwt_list
  module MBytes = MBytes
  module Uri = Uri
  module Data_encoding = Data_encoding
  module Time = Time
  module Ed25519 = Ed25519
  module Hash = struct
    include Tezos_crypto
    include Tezos_crypto.S
    module Make_minimal_Blake2B = Blake2B.Make_minimal
    module Make_Blake2B = Blake2B.Make
  end
  module Blake2B = Blake2B
  module Tezos_data = struct
    module type DATA = Tezos_base.S.T
    module type HASHABLE_DATA = Tezos_base.S.HASHABLE
    module Fitness = Fitness
    module Operation = Operation
    module Block_header = Block_header
    module Protocol = Protocol
  end
  module RPC_arg = RPC_arg
  module RPC_path = RPC_path
  module RPC_query = RPC_query
  module RPC_service = RPC_service
  module RPC_answer = RPC_answer
  module RPC_directory = RPC_directory
  module Micheline = Tezos_micheline.Micheline
  module Fitness = Fitness
  module Error_monad = struct
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
  end
  module Updater = struct
    include Updater
    module type PROTOCOL =
      RAW_PROTOCOL with type error := Error_monad.error
                    and type 'a tzresult := 'a Error_monad.tzresult
  end
  module Logging = Logging.Make(Param)
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
    include Make(struct type context = Context.t end)
    let decode s = decode s
  end
  module Context = struct
    include Context
    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end

  type error += Ecoproto_error of Error_monad.error list

  let () =
    let id = Format.asprintf "Ecoproto.%s" Param.name in
    register_wrapped_error_kind
      (fun ecoerrors -> Error_monad.classify_errors ecoerrors)
      ~id ~title:"Error returned by the protocol"
      ~description:"Wrapped error for the economic protocol."
      ~pp:(fun ppf ->
          Format.fprintf ppf
            "@[<v 2>Economic error:@ %a@]"
            (Format.pp_print_list Error_monad.pp))
      Data_encoding.(obj1 (req "ecoproto"
                             (list Error_monad.error_encoding)))
      (function Ecoproto_error ecoerrors -> Some ecoerrors
              | _ -> None )
      (function ecoerrors -> Ecoproto_error ecoerrors)

  let wrap_error = function
    | Ok _ as ok -> ok
    | Error errors -> Error [Ecoproto_error errors]

end
