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
  val fork_test_chain:
    Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

end

module type T = sig
  type context
  type quota
  type validation_result
  type rpc_context
  type 'a tzresult
  val max_block_length: int
  val validation_passes: quota list
  type operation
  val parse_operation:
    Operation_hash.t -> Operation.t -> operation tzresult
  val acceptable_passes: operation -> int list
  val compare_operations: operation -> operation -> int
  type validation_state
  val current_context: validation_state -> context tzresult Lwt.t
  val precheck_block:
    ancestor_context: context ->
    ancestor_timestamp: Time.t ->
    Block_header.t ->
    unit tzresult Lwt.t
  val begin_application:
    predecessor_context: context ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    Block_header.t ->
    validation_state tzresult Lwt.t
  val begin_construction:
    predecessor_context: context ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?protocol_data: MBytes.t ->
    unit -> validation_state tzresult Lwt.t
  val apply_operation:
    validation_state -> operation -> validation_state tzresult Lwt.t
  val finalize_block:
    validation_state -> validation_result tzresult Lwt.t
  val rpc_services: rpc_context Lwt.t RPC_directory.t
  val configure_sandbox:
    context -> Data_encoding.json option -> context tzresult Lwt.t
end

module type V1 = sig

  include Tezos_protocol_environment_sigs.V1.T
    with type Format.formatter = Format.formatter
     and type 'a Data_encoding.t = 'a Data_encoding.t
     and type 'a Lwt.t = 'a Lwt.t
     and type ('a, 'b) Pervasives.result = ('a, 'b) result
     and type Block_hash.t = Block_hash.t
     and type Operation_hash.t = Operation_hash.t
     and type Operation_list_hash.t = Operation_list_hash.t
     and type Operation_list_list_hash.t = Operation_list_list_hash.t
     and type Context_hash.t = Context_hash.t
     and type Protocol_hash.t = Protocol_hash.t
     and type Time.t = Time.t
     and type MBytes.t = MBytes.t
     and type Operation.shell_header = Operation.shell_header
     and type Operation.t = Operation.t
     and type Block_header.shell_header = Block_header.shell_header
     and type Block_header.t = Block_header.t
     and type 'a RPC_directory.t = 'a RPC_directory.t
     and type Ed25519.Public_key_hash.t = Ed25519.Public_key_hash.t
     and type Ed25519.Public_key.t = Ed25519.Public_key.t
     and type Ed25519.Secret_key.t = Ed25519.Secret_key.t
     and type Ed25519.Signature.t = Ed25519.Signature.t
     and type 'a Micheline.canonical = 'a Micheline.canonical
     and type ('a, 'b) RPC_path.t = ('a, 'b) RPC_path.t
     and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
     and type Data_encoding.json_schema = Data_encoding.json_schema
     and type RPC_service.meth = RPC_service.meth
     and type (+'m,'pr,'p,'q,'i,'o) RPC_service.t = ('m,'pr,'p,'q,'i,'o) RPC_service.t
     and type Error_monad.shell_error = Error_monad.error

  type error += Ecoproto_error of Error_monad.error list
  val wrap_error : 'a Error_monad.tzresult -> 'a tzresult

  module Lift (P : Updater.PROTOCOL) :
    T with type context := Context.t
       and type quota := Updater.quota
       and type validation_result := Updater.validation_result
       and type rpc_context := Updater.rpc_context
       and type 'a tzresult := 'a tzresult

  class ['block] proto_rpc_context :
    Tezos_rpc.RPC_context.t -> (unit, unit * 'block) RPC_path.t ->
    ['block] RPC_context.simple

  class ['block] proto_rpc_context_of_directory :
    ('block -> RPC_context.t) -> RPC_context.t RPC_directory.t ->
    ['block] RPC_context.simple

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
  module S = S
  module Error_monad = struct
    type 'a shell_tzresult = 'a Error_monad.tzresult
    type shell_error = Error_monad.error = ..
    type error_category = [ `Branch | `Temporary | `Permanent ]
    include Error_monad.Make()
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
               let e = Option.map e ~f:(fun e -> [Ecoproto_error e]) in
               Lwt.return (`Unauthorized e)
           | `Forbidden e ->
               let e = Option.map e ~f:(fun e -> [Ecoproto_error e]) in
               Lwt.return (`Forbidden e)
           | `Not_found e ->
               let e = Option.map e ~f:(fun e -> [Ecoproto_error e]) in
               Lwt.return (`Not_found e)
           | `Conflict e ->
               let e = Option.map e ~f:(fun e -> [Ecoproto_error e]) in
               Lwt.return (`Conflict e)
           | `Error e ->
               let e = Option.map e ~f:(fun e -> [Ecoproto_error e]) in
               Lwt.return (`Error e))

    let register dir service handler =
      gen_register dir service
        (fun p q i ->
           handler p q i >>= function
           | Ok o -> RPC_answer.return o
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

    type t = Updater.rpc_context Lwt.t

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
      | Error [RPC_context.Not_found _] -> Lwt.return (Ok None)
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return (Ok (Some v))

    let make_opt_call1 s ctxt block a1 q i =
      make_call1 s ctxt block a1 q i >>= function
      | Error [RPC_context.Not_found _] -> Lwt.return (Ok None)
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return (Ok (Some v))

    let make_opt_call2 s ctxt block a1 a2 q i =
      make_call2 s ctxt block a1 a2 q i >>= function
      | Error [RPC_context.Not_found _] -> Lwt.return (Ok None)
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return (Ok (Some v))

    let make_opt_call3 s ctxt block a1 a2 a3 q i =
      make_call3 s ctxt block a1 a2 a3 q i >>= function
      | Error [RPC_context.Not_found _] -> Lwt.return (Ok None)
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return (Ok (Some v))

  end
  module Micheline = Micheline
  module Logging = Logging.Make(Param)

  module Updater = struct

    include Updater

    module type PROTOCOL =
      T with type context := Context.t
         and type quota := Updater.quota
         and type validation_result := Updater.validation_result
         and type rpc_context := Updater.rpc_context
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
        ~predecessor ~timestamp ?protocol_data () =
      begin_construction
        ~predecessor_context ~predecessor_timestamp
        ~predecessor_level ~predecessor_fitness
        ~predecessor ~timestamp ?protocol_data () >|= wrap_error
    let current_context c =
      current_context c >|= wrap_error
    let apply_operation c o =
      apply_operation c o >|= wrap_error
    let finalize_block c = finalize_block c >|= wrap_error
    let parse_operation h b = parse_operation h b |> wrap_error
    let configure_sandbox c j =
      configure_sandbox c j >|= wrap_error
  end

  class ['block] proto_rpc_context
      (t : Tezos_rpc.RPC_context.t)
      (prefix : (unit, unit * 'block) RPC_path.t) =
    object
      method call_proto_service0
        : 'm 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, RPC_context.t,
           RPC_context.t, 'q, 'i, 'o) RPC_service.t ->
          'block -> 'q -> 'i -> 'o tzresult Lwt.t
        = fun s block q i ->
          let s = RPC_service.subst0 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s ((), block) q i
      method call_proto_service1
        : 'm 'a 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, RPC_context.t,
           RPC_context.t * 'a, 'q, 'i, 'o) RPC_service.t ->
          'block -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t
        = fun s block a1 q i ->
          let s = RPC_service.subst1 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s (((), block), a1) q i
      method call_proto_service2
        : 'm 'a 'b 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, RPC_context.t,
           (RPC_context.t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
          'block -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t
        = fun s block a1 a2 q i ->
          let s = RPC_service.subst2 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s ((((), block), a1), a2) q i
      method call_proto_service3
        : 'm 'a 'b 'c 'q 'i 'o.
          ([< RPC_service.meth ] as 'm, RPC_context.t,
           ((RPC_context.t * 'a) * 'b) * 'c,
           'q, 'i, 'o) RPC_service.t ->
          'block -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t
        = fun s block a1 a2 a3 q i ->
          let s = RPC_service.subst3 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s (((((), block), a1), a2), a3) q i
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


