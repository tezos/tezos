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

open Error_monad

module type CONTEXT = sig
  type t
  type key = string list
  type value = MBytes.t
  val mem: t -> key -> bool Lwt.t
  val dir_mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val copy: t -> from:key -> to_:key -> t option Lwt.t
  val del: t -> key -> t Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val fold:
    t -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t
  val set_protocol: t -> Protocol_hash.t -> t Lwt.t
  val fork_test_chain:
    t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t
end

module Make (Context : CONTEXT) = struct

  type validation_result = {
    context: Context.t ;
    fitness: Fitness.t ;
    message: string option ;
    max_operations_ttl: int ;
    last_allowed_fork_level: Int32.t ;
  }

  type quota = {
    max_size: int ;
    max_op: int option ;
  }

  type rpc_context = {
    block_hash: Block_hash.t ;
    block_header: Block_header.shell_header ;
    context: Context.t ;
  }

  module type T = sig
    type context
    type quota
    type validation_result
    type rpc_context
    type 'a tzresult
    val max_block_length: int
    val max_operation_data_length: int
    val validation_passes: quota list
    type block_header_data
    val block_header_data_encoding: block_header_data Data_encoding.t
    type block_header = {
      shell: Block_header.shell_header ;
      protocol_data: block_header_data ;
    }
    type block_header_metadata
    val block_header_metadata_encoding: block_header_metadata Data_encoding.t
    type operation_data
    type operation_receipt
    type operation = {
      shell: Operation.shell_header ;
      protocol_data: operation_data ;
    }
    val operation_data_encoding: operation_data Data_encoding.t
    val operation_receipt_encoding: operation_receipt Data_encoding.t
    val operation_data_and_receipt_encoding:
      (operation_data * operation_receipt) Data_encoding.t
    val acceptable_passes: operation -> int list
    val compare_operations: operation -> operation -> int
    type validation_state
    val current_context: validation_state -> context tzresult Lwt.t
    val begin_partial_application:
      chain_id: Chain_id.t ->
      ancestor_context: context ->
      predecessor_timestamp: Time.Protocol.t ->
      predecessor_fitness: Fitness.t ->
      block_header ->
      validation_state tzresult Lwt.t
    val begin_application:
      chain_id: Chain_id.t ->
      predecessor_context: context ->
      predecessor_timestamp: Time.Protocol.t ->
      predecessor_fitness: Fitness.t ->
      block_header ->
      validation_state tzresult Lwt.t
    val begin_construction:
      chain_id: Chain_id.t ->
      predecessor_context: context ->
      predecessor_timestamp: Time.Protocol.t ->
      predecessor_level: Int32.t ->
      predecessor_fitness: Fitness.t ->
      predecessor: Block_hash.t ->
      timestamp: Time.Protocol.t ->
      ?protocol_data: block_header_data ->
      unit -> validation_state tzresult Lwt.t
    val apply_operation:
      validation_state -> operation ->
      (validation_state * operation_receipt) tzresult Lwt.t
    val finalize_block:
      validation_state ->
      (validation_result * block_header_metadata) tzresult Lwt.t
    val rpc_services: rpc_context RPC_directory.t
    val init:
      context -> Block_header.shell_header -> validation_result tzresult Lwt.t
  end

  module type PROTOCOL =
    T with type context := Context.t
       and type quota := quota
       and type validation_result := validation_result
       and type rpc_context := rpc_context
       and type 'a tzresult := 'a Error_monad.tzresult

  module type V1 = sig

    include Tezos_protocol_environment_sigs.V1.T
      with type Format.formatter = Format.formatter
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Data_encoding.lazy_t = 'a Data_encoding.lazy_t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Chain_id.t = Chain_id.t
       and type Block_hash.t = Block_hash.t
       and type Operation_hash.t = Operation_hash.t
       and type Operation_list_hash.t = Operation_list_hash.t
       and type Operation_list_list_hash.t = Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Context_hash.t = Context_hash.t
       and type Protocol_hash.t = Protocol_hash.t
       and type Time.t = Time.Protocol.t
       and type MBytes.t = MBytes.t
       and type Operation.shell_header = Operation.shell_header
       and type Operation.t = Operation.t
       and type Block_header.shell_header = Block_header.shell_header
       and type Block_header.t = Block_header.t
       and type 'a RPC_directory.t = 'a RPC_directory.t
       and type Ed25519.Public_key_hash.t = Ed25519.Public_key_hash.t
       and type Ed25519.Public_key.t = Ed25519.Public_key.t
       and type Ed25519.t = Ed25519.t
       and type Secp256k1.Public_key_hash.t = Secp256k1.Public_key_hash.t
       and type Secp256k1.Public_key.t = Secp256k1.Public_key.t
       and type Secp256k1.t = Secp256k1.t
       and type P256.Public_key_hash.t = P256.Public_key_hash.t
       and type P256.Public_key.t = P256.Public_key.t
       and type P256.t = P256.t
       and type Signature.public_key_hash = Signature.public_key_hash
       and type Signature.public_key = Signature.public_key
       and type Signature.t = Signature.t
       and type Signature.watermark = Signature.watermark
       and type 'a Micheline.canonical = 'a Micheline.canonical
       and type ('a, 'b) RPC_path.t = ('a, 'b) RPC_path.t
       and type Z.t = Z.t
       and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
       and type Data_encoding.json_schema = Data_encoding.json_schema
       and type RPC_service.meth = RPC_service.meth
       and type (+'m,'pr,'p,'q,'i,'o) RPC_service.t = ('m,'pr,'p,'q,'i,'o) RPC_service.t
       and type Error_monad.shell_error = Error_monad.error
       and type Z.t = Z.t

    type error += Ecoproto_error of Error_monad.error
    val wrap_error : 'a Error_monad.tzresult -> 'a tzresult

    module Lift (P : Updater.PROTOCOL) : PROTOCOL
      with type block_header_data = P.block_header_data
       and type block_header = P.block_header
       and type operation_data = P.operation_data
       and type operation_receipt = P.operation_receipt
       and type operation = P.operation
       and type validation_state = P.validation_state

    class ['chain, 'block] proto_rpc_context :
      Tezos_rpc.RPC_context.t -> (unit, (unit * 'chain) * 'block) RPC_path.t ->
      [('chain * 'block)] RPC_context.simple

    class ['block] proto_rpc_context_of_directory :
      ('block -> RPC_context.t) -> RPC_context.t RPC_directory.t ->
      ['block] RPC_context.simple

  end

  module MakeV1 (Param : sig val name: string end) () = struct

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
    module MBytes = MBytes
    module Raw_hashes = struct
      let sha256 msg = Hacl.Hash.SHA256.digest msg
      let sha512 msg = Hacl.Hash.SHA512.digest msg
      let blake2b msg = Blake2B.to_bytes (Blake2B.hash_bytes [ msg ])
    end
    module Z = struct
      include Z
      let to_bits ?(pad_to = 0) z =
        let bits = to_bits z in
        let len = Pervasives.((numbits z + 7) / 8) in
        let full_len = Compare.Int.max pad_to len in
        if full_len = 0 then
          MBytes.empty
        else
          let res = MBytes.make full_len '\000' in
          MBytes.blit_of_string bits 0 res 0 len ;
          res
      let of_bits bytes =
        of_bits (MBytes.to_string bytes)
    end
    module Lwt_sequence = Lwt_sequence
    module Lwt = Lwt
    module Lwt_list = Lwt_list
    module Uri = Uri
    module Data_encoding = Data_encoding
    module Time = Time.Protocol
    module Ed25519 = Ed25519
    module Secp256k1 = Secp256k1
    module P256 = P256
    module Signature = Signature
    module S = struct
      module type T = Tezos_base.S.T
      module type HASHABLE = Tezos_base.S.HASHABLE
      module type MINIMAL_HASH = S.MINIMAL_HASH
      module type B58_DATA = sig

        type t

        val to_b58check: t -> string
        val to_short_b58check: t -> string

        val of_b58check_exn: string -> t
        val of_b58check_opt: string -> t option

        type Base58.data += Data of t
        val b58check_encoding: t Base58.encoding

      end
      module type RAW_DATA = sig
        type t
        val size: int (* in bytes *)
        val to_bytes: t -> MBytes.t
        val of_bytes_opt: MBytes.t -> t option
        val of_bytes_exn: MBytes.t -> t
      end
      module type ENCODER = sig
        type t
        val encoding: t Data_encoding.t
        val rpc_arg: t RPC_arg.t
      end
      module type SET = Tezos_base.S.SET
      module type MAP = Tezos_base.S.MAP
      module type INDEXES = sig

        type t

        val to_path: t -> string list -> string list
        val of_path: string list -> t option
        val of_path_exn: string list -> t

        val prefix_path: string -> string list
        val path_length: int

        module Set : sig
          include Set.S with type elt = t
          val encoding: t Data_encoding.t
        end

        module Map : sig
          include Map.S with type key = t
          val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
        end

      end
      module type HASH = sig
        include MINIMAL_HASH
        include RAW_DATA with type t := t
        include B58_DATA with type t := t
        include ENCODER with type t := t
        include INDEXES with type t := t
      end

      module type MERKLE_TREE = sig
        type elt
        include HASH
        val compute: elt list -> t
        val empty: t
        type path =
          | Left of path * t
          | Right of t * path
          | Op
        val compute_path: elt list -> int -> path
        val check_path: path -> elt -> t * int
        val path_encoding: path Data_encoding.t
      end

      module type SIGNATURE = sig

        module Public_key_hash : sig

          type t

          val pp: Format.formatter -> t -> unit
          val pp_short: Format.formatter -> t -> unit
          include Compare.S with type t := t
          include RAW_DATA with type t := t
          include B58_DATA with type t := t
          include ENCODER with type t := t
          include INDEXES with type t := t

          val zero: t

        end

        module Public_key : sig

          type t

          val pp: Format.formatter -> t -> unit
          include Compare.S with type t := t
          include B58_DATA with type t := t
          include ENCODER with type t := t

          val hash: t -> Public_key_hash.t

        end

        type t

        val pp: Format.formatter -> t -> unit
        include RAW_DATA with type t := t
        include Compare.S with type t := t
        include B58_DATA with type t := t
        include ENCODER with type t := t

        val zero: t

        type watermark

        (** Check a signature *)
        val check: ?watermark:watermark -> Public_key.t -> t -> MBytes.t -> bool

      end

    end
    module Error_monad = struct
      type 'a shell_tzresult = 'a Error_monad.tzresult
      type shell_error = Error_monad.error = ..
      type error_category = [ `Branch | `Temporary | `Permanent ]
      include Error_monad.Make(struct let id = Format.asprintf "proto.%s." Param.name end)
    end

    type error += Ecoproto_error of Error_monad.error

    module Wrapped_error_monad = struct
      type unwrapped = Error_monad.error = ..
      include (Error_monad : Error_monad_sig.S with type error := unwrapped)
      let unwrap = function
        | Ecoproto_error ecoerror -> Some ecoerror
        | _ -> None
      let wrap ecoerror =
        Ecoproto_error ecoerror
    end

    let () =
      let id = Format.asprintf "proto.%s.wrapper" Param.name in
      register_wrapped_error_kind
        (module Wrapped_error_monad)
        ~id ~title: ("Error returned by protocol " ^ Param.name)
        ~description: ("Wrapped error for economic protocol " ^ Param.name ^ ".")

    let wrap_error = function
      | Ok _ as ok -> ok
      | Error errors -> Error (List.map (fun error -> Ecoproto_error error) errors)

    module Chain_id = Chain_id
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
    module RPC_answer = struct

      type 'o t =
        [ `Ok of 'o (* 200 *)
        | `OkStream of 'o stream (* 200 *)
        | `Created of string option (* 201 *)
        | `No_content (* 204 *)
        | `Unauthorized of Error_monad.error list option (* 401 *)
        | `Forbidden of Error_monad.error list option (* 403 *)
        | `Not_found of Error_monad.error list option (* 404 *)
        | `Conflict of Error_monad.error list option (* 409 *)
        | `Error of Error_monad.error list option (* 500 *)
        ]

      and 'a stream = 'a Resto_directory.Answer.stream = {
        next: unit -> 'a option Lwt.t ;
        shutdown: unit -> unit ;
      }

      let return x = Lwt.return (`Ok x)
      let return_stream x = Lwt.return (`OkStream x)
      let not_found = Lwt.return (`Not_found None)

      let fail err = Lwt.return (`Error (Some err))
    end
    module RPC_directory = struct
      include RPC_directory
      let gen_register dir service handler =
        gen_register dir service
          (fun p q i ->
             handler p q i >>= function
             | `Ok o -> RPC_answer.return o
             | `OkStream s -> RPC_answer.return_stream s
             | `Created s -> Lwt.return (`Created s)
             | `No_content -> Lwt.return (`No_content)
             | `Unauthorized e ->
                 let e = Option.map e ~f:(List.map (fun e -> Ecoproto_error e)) in
                 Lwt.return (`Unauthorized e)
             | `Forbidden e ->
                 let e = Option.map e ~f:(List.map (fun e -> Ecoproto_error e)) in
                 Lwt.return (`Forbidden e)
             | `Not_found e ->
                 let e = Option.map e ~f:(List.map (fun e -> Ecoproto_error e)) in
                 Lwt.return (`Not_found e)
             | `Conflict e ->
                 let e = Option.map e ~f:(List.map (fun e -> Ecoproto_error e)) in
                 Lwt.return (`Conflict e)
             | `Error e ->
                 let e = Option.map e ~f:(List.map (fun e -> Ecoproto_error e)) in
                 Lwt.return (`Error e))

      let register dir service handler =
        gen_register dir service
          (fun p q i ->
             handler p q i >>= function
             | Ok o -> RPC_answer.return o
             | Error e -> RPC_answer.fail e)

      let opt_register dir service handler =
        gen_register dir service
          (fun p q i ->
             handler p q i >>= function
             | Ok (Some o) -> RPC_answer.return o
             | Ok None -> RPC_answer.not_found
             | Error e -> RPC_answer.fail e)

      let lwt_register dir service handler =
        gen_register dir service
          (fun p q i ->
             handler p q i >>= fun o ->
             RPC_answer.return o)

      open Curry

      let register0 root s f = register root s (curry Z f)
      let register1 root s f = register root s (curry (S Z) f)
      let register2 root s f = register root s (curry (S (S Z)) f)
      let register3 root s f = register root s (curry (S (S (S Z))) f)
      let register4 root s f = register root s (curry (S (S (S (S Z)))) f)
      let register5 root s f = register root s (curry (S (S (S (S (S Z))))) f)

      let opt_register0 root s f = opt_register root s (curry Z f)
      let opt_register1 root s f = opt_register root s (curry (S Z) f)
      let opt_register2 root s f = opt_register root s (curry (S (S Z)) f)
      let opt_register3 root s f = opt_register root s (curry (S (S (S Z))) f)
      let opt_register4 root s f = opt_register root s (curry (S (S (S (S Z)))) f)
      let opt_register5 root s f = opt_register root s (curry (S (S (S (S (S Z))))) f)

      let gen_register0 root s f = gen_register root s (curry Z f)
      let gen_register1 root s f = gen_register root s (curry (S Z) f)
      let gen_register2 root s f = gen_register root s (curry (S (S Z)) f)
      let gen_register3 root s f = gen_register root s (curry (S (S (S Z))) f)
      let gen_register4 root s f = gen_register root s (curry (S (S (S (S Z)))) f)
      let gen_register5 root s f = gen_register root s (curry (S (S (S (S (S Z))))) f)

      let lwt_register0 root s f = lwt_register root s (curry Z f)
      let lwt_register1 root s f = lwt_register root s (curry (S Z) f)
      let lwt_register2 root s f = lwt_register root s (curry (S (S Z)) f)
      let lwt_register3 root s f = lwt_register root s (curry (S (S (S Z))) f)
      let lwt_register4 root s f = lwt_register root s (curry (S (S (S (S Z)))) f)
      let lwt_register5 root s f = lwt_register root s (curry (S (S (S (S (S Z))))) f)

    end
    module RPC_context = struct

      type t = rpc_context

      class type ['pr] simple = object
        method call_proto_service0 :
          'm 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, t, t, 'q, 'i, 'o) RPC_service.t ->
          'pr -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
        method call_proto_service1 :
          'm 'a 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, t, t * 'a, 'q, 'i, 'o) RPC_service.t ->
          'pr -> 'a -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
        method call_proto_service2 :
          'm 'a 'b 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, t, (t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
          'pr -> 'a -> 'b -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
        method call_proto_service3 :
          'm 'a 'b 'c 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, t, ((t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
          'pr -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o Error_monad.shell_tzresult Lwt.t
      end

      let make_call0 s (ctxt : _ simple) =
        ctxt#call_proto_service0 s
      let make_call0 = (make_call0 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

      let make_call1 s (ctxt: _ simple) =
        ctxt#call_proto_service1 s
      let make_call1 = (make_call1 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

      let make_call2 s (ctxt: _ simple) =
        ctxt#call_proto_service2 s
      let make_call2 = (make_call2 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

      let make_call3 s (ctxt: _ simple) =
        ctxt#call_proto_service3 s
      let make_call3 = (make_call3 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

      let make_opt_call0 s ctxt block q i =
        make_call0 s ctxt block q i >>= function
        | Error [RPC_context.Not_found _] -> Lwt.return_ok None
        | Error _ as v -> Lwt.return v
        | Ok v -> Lwt.return_ok (Some v)

      let make_opt_call1 s ctxt block a1 q i =
        make_call1 s ctxt block a1 q i >>= function
        | Error [RPC_context.Not_found _] -> Lwt.return_ok None
        | Error _ as v -> Lwt.return v
        | Ok v -> Lwt.return_ok (Some v)

      let make_opt_call2 s ctxt block a1 a2 q i =
        make_call2 s ctxt block a1 a2 q i >>= function
        | Error [RPC_context.Not_found _] -> Lwt.return_ok None
        | Error _ as v -> Lwt.return v
        | Ok v -> Lwt.return_ok (Some v)

      let make_opt_call3 s ctxt block a1 a2 a3 q i =
        make_call3 s ctxt block a1 a2 a3 q i >>= function
        | Error [RPC_context.Not_found _] -> Lwt.return_ok None
        | Error _ as v -> Lwt.return v
        | Ok v -> Lwt.return_ok (Some v)

    end
    module Micheline = struct
      include Micheline
      let canonical_encoding_v1 = canonical_encoding_v1
      let canonical_encoding = canonical_encoding_v0
    end
    module Logging = Internal_event.Legacy_logging.Make(Param)

    module Updater = struct

      type nonrec validation_result = validation_result = {
        context: Context.t ;
        fitness: Fitness.t ;
        message: string option ;
        max_operations_ttl: int ;
        last_allowed_fork_level: Int32.t ;
      }

      type nonrec quota = quota = {
        max_size: int ;
        max_op: int option ;
      }

      type nonrec rpc_context = rpc_context = {
        block_hash: Block_hash.t ;
        block_header: Block_header.shell_header ;
        context: Context.t ;
      }

      let activate = Context.set_protocol
      let fork_test_chain = Context.fork_test_chain

      module type PROTOCOL =
        T with type context := Context.t
           and type quota := quota
           and type validation_result := validation_result
           and type rpc_context := rpc_context
           and type 'a tzresult := 'a Error_monad.tzresult

    end
    module Base58 = struct
      include Tezos_crypto.Base58
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

    module Lift(P : Updater.PROTOCOL) = struct
      include P
      let begin_partial_application
          ~chain_id ~ancestor_context ~predecessor_timestamp ~predecessor_fitness
          raw_block =
        begin_partial_application
          ~chain_id ~ancestor_context ~predecessor_timestamp ~predecessor_fitness
          raw_block >|= wrap_error
      let begin_application
          ~chain_id ~predecessor_context ~predecessor_timestamp
          ~predecessor_fitness
          raw_block =
        begin_application
          ~chain_id ~predecessor_context ~predecessor_timestamp
          ~predecessor_fitness
          raw_block >|= wrap_error
      let begin_construction
          ~chain_id ~predecessor_context ~predecessor_timestamp
          ~predecessor_level ~predecessor_fitness
          ~predecessor ~timestamp ?protocol_data () =
        begin_construction
          ~chain_id ~predecessor_context ~predecessor_timestamp
          ~predecessor_level ~predecessor_fitness
          ~predecessor ~timestamp ?protocol_data () >|= wrap_error
      let current_context c =
        current_context c >|= wrap_error
      let apply_operation c o =
        apply_operation c o >|= wrap_error
      let finalize_block c = finalize_block c >|= wrap_error
      let init c bh = init c bh >|= wrap_error
    end

    class ['chain, 'block] proto_rpc_context
        (t : Tezos_rpc.RPC_context.t)
        (prefix : (unit, (unit * 'chain) * 'block) RPC_path.t) =
      object
        method call_proto_service0
          : 'm 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             RPC_context.t, 'q, 'i, 'o) RPC_service.t ->
            ('chain * 'block) -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s (chain, block) q i ->
            let s = RPC_service.subst0 s in
            let s = RPC_service.prefix prefix s in
            t#call_service s (((), chain), block) q i
        method call_proto_service1
          : 'm 'a 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             RPC_context.t * 'a, 'q, 'i, 'o) RPC_service.t ->
            ('chain * 'block) -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s (chain, block) a1 q i ->
            let s = RPC_service.subst1 s in
            let s = RPC_service.prefix prefix s in
            t#call_service s ((((), chain), block), a1) q i
        method call_proto_service2
          : 'm 'a 'b 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             (RPC_context.t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
            ('chain * 'block) -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s (chain, block) a1 a2 q i ->
            let s = RPC_service.subst2 s in
            let s = RPC_service.prefix prefix s in
            t#call_service s (((((), chain), block), a1), a2) q i
        method call_proto_service3
          : 'm 'a 'b 'c 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             ((RPC_context.t * 'a) * 'b) * 'c,
             'q, 'i, 'o) RPC_service.t ->
            ('chain * 'block) -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s (chain, block) a1 a2 a3 q i ->
            let s = RPC_service.subst3 s in
            let s = RPC_service.prefix prefix s in
            t#call_service s ((((((), chain), block), a1), a2), a3) q i
      end

    class ['block] proto_rpc_context_of_directory conv dir : ['block] RPC_context.simple =
      let lookup = new Tezos_rpc.RPC_context.of_directory dir in
      object
        method call_proto_service0
          : 'm 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             RPC_context.t, 'q, 'i, 'o) RPC_service.t ->
            'block -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s block q i ->
            let rpc_context = conv block in
            lookup#call_service s rpc_context q i
        method call_proto_service1
          : 'm 'a 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             RPC_context.t * 'a, 'q, 'i, 'o) RPC_service.t ->
            'block -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s block a1 q i ->
            let rpc_context = conv block in
            lookup#call_service s (rpc_context, a1) q i
        method call_proto_service2
          : 'm 'a 'b 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             (RPC_context.t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
            'block -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s block a1 a2 q i ->
            let rpc_context = conv block in
            lookup#call_service s ((rpc_context, a1), a2) q i
        method call_proto_service3
          : 'm 'a 'b 'c 'q 'i 'o.
            ([< RPC_service.meth ] as 'm, RPC_context.t,
             ((RPC_context.t * 'a) * 'b) * 'c,
             'q, 'i, 'o) RPC_service.t ->
            'block -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t
          = fun s block a1 a2 a3 q i ->
            let rpc_context = conv block in
            lookup#call_service s (((rpc_context, a1), a2), a3) q i
      end

  end

end
