
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

module Make (Context : CONTEXT) : sig

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
       and type Z.t = Z.t
       and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
       and type Data_encoding.json_schema = Data_encoding.json_schema
       and type ('a, 'b) RPC_path.t = ('a, 'b) RPC_path.t
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

  module MakeV1 (Param : sig val name: string end)()
    : V1 with type Context.t = Context.t
          and type Updater.validation_result = validation_result
          and type Updater.quota = quota
          and type Updater.rpc_context = rpc_context

end
