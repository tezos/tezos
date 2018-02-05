(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

module type CONTEXT = sig
  type t
  type key = string list
  type value = MBytes.t
  val mem: t -> key -> bool Lwt.t
  val dir_mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val fold:
    t -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
end

module type UPDATER = sig

  module Context : CONTEXT

  type validation_result = {
    context: Context.t ;
    fitness: Fitness.t ;
    message: string option ;
    max_operation_data_length: int ;
    max_operations_ttl: int ;
  }

  type quota = {
    max_size: int ;
    max_op: int option ;
  }

  type rpc_context = {
    block_hash: Block_hash.t ;
    block_header: Block_header.t ;
    operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
    operations: unit -> Operation.t list list Lwt.t ;
    context: Context.t ;
  }

  val compile: Protocol_hash.t -> Protocol.t -> bool Lwt.t
  val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t
  val fork_test_network:
    Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

end

module MakeV1
    (Param : sig val name: string end)
    (Context : CONTEXT)
    (Updater : UPDATER with module Context := Context)
    () = struct

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
  module Option = Option
  module Z = Z
  module Lwt_sequence = Lwt_sequence
  module Lwt = Lwt
  module Lwt_list = Lwt_list
  module MBytes = MBytes
  module Uri = Uri
  module Data_encoding = Data_encoding
  module Time = Time
  module Ed25519 = Ed25519
  module S = struct
    include Tezos_crypto.S
    include S
  end
  module Block_hash = Block_hash
  module Operation_hash = Operation_hash
  module Operation_list_hash = Operation_list_hash
  module Operation_list_list_hash = Operation_list_list_hash
  module Context_hash = Context_hash
  module Protocol_hash = Protocol_hash
  module Blake2B = Blake2B
  module Fitness = Fitness
  module Operation = Operation
  module Block_header = Block_header
  module Protocol = Protocol
  module RPC_arg = RPC_arg
  module RPC_path = RPC_path
  module RPC_query = RPC_query
  module RPC_service = RPC_service
  module RPC_answer = RPC_answer
  module RPC_directory = RPC_directory
  module Error_monad = struct
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
  end
  module Micheline = Micheline
  module Logging = Logging.Make(Param)

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

  module Updater = struct

    include Updater

    module type PROTOCOL = sig
      open Error_monad
      val max_block_length: int
      val validation_passes: quota list
      type operation
      val parse_operation:
        Operation_hash.t -> Operation.t -> operation tzresult
      val acceptable_passes: operation -> int list
      val compare_operations: operation -> operation -> int
      type validation_state
      val current_context: validation_state -> Context.t tzresult Lwt.t
      val precheck_block:
        ancestor_context: Context.t ->
        ancestor_timestamp: Time.t ->
        Block_header.t ->
        unit tzresult Lwt.t
      val begin_application:
        predecessor_context: Context.t ->
        predecessor_timestamp: Time.t ->
        predecessor_fitness: Fitness.t ->
        Block_header.t ->
        validation_state tzresult Lwt.t
      val begin_construction:
        predecessor_context: Context.t ->
        predecessor_timestamp: Time.t ->
        predecessor_level: Int32.t ->
        predecessor_fitness: Fitness.t ->
        predecessor: Block_hash.t ->
        timestamp: Time.t ->
        ?proto_header: MBytes.t ->
        unit -> validation_state tzresult Lwt.t
      val apply_operation:
        validation_state -> operation -> validation_state tzresult Lwt.t
      val finalize_block:
        validation_state -> validation_result tzresult Lwt.t
      val rpc_services: rpc_context RPC_directory.t
      val configure_sandbox:
        Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t
    end

  end
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
    include Make(struct type context = Context.t end)
    let decode s = decode s
  end
  module Context = struct
    include Context

    let fold_keys s k ~init ~f =
      let rec loop k acc =
        fold s k ~init:acc
          ~f:(fun file acc ->
              match file with
              | `Key k -> f k acc
              | `Dir k -> loop k acc) in
      loop k init

    let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end

end


