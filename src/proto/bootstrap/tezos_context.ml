(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Storage.t
type context = t

module type BASIC_DATA = sig
  type t
  include Compare.S with type t := t
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
end

module Tez = Tez_repr
module Period = Period_repr
module Timestamp = struct
  include Time_repr
  let get_current = Storage.Current_timestamp.get
  let set_current = Storage.Current_timestamp.set
end


include Operation_repr
module Operation = Operation_repr
module Block = Block_repr
module Vote = struct
  include Vote_repr
  include Vote_storage
end
module Raw_level = Raw_level_repr
module Cycle = Cycle_repr
module Script_int = Script_int_repr
module Script = Script_repr

type public_key = Ed25519.public_key
type public_key_hash = Ed25519.Public_key_hash.t
type secret_key = Ed25519.secret_key
type signature = Ed25519.signature

include Tezos_hash

module Constants = struct
  include Constants_repr
  let cycle_length c =
    let constants = Storage.constants c in
    constants.cycle_length
  let voting_period_length c =
    let constants = Storage.constants c in
    constants.voting_period_length
  let time_before_reward c =
    let constants = Storage.constants c in
    constants.time_before_reward
  let time_between_slots c =
    let constants = Storage.constants c in
    constants.time_between_slots
  let first_free_mining_slot c =
    let constants = Storage.constants c in
    constants.first_free_mining_slot
  let max_signing_slot c =
    let constants = Storage.constants c in
    constants.max_signing_slot
  let instructions_per_transaction c =
    let constants = Storage.constants c in
    constants.instructions_per_transaction
  let proof_of_work_threshold c =
    let constants = Storage.constants c in
    constants.proof_of_work_threshold
end

module Public_key = struct

  let get = Storage.Public_key.get
  let get_option = Storage.Public_key.get_option
  let set = Storage.Public_key.init_set
  let remove = Storage.Public_key.remove

  let list ctxt =
    Storage.Public_key.fold ctxt [] ~f:(fun pk_h pk acc ->
        Lwt.return @@ (pk_h, pk) :: acc) >>= fun res ->
    return res

end

module Voting_period = Voting_period_repr

module Level = struct
  include Level_repr
  include Level_storage
end
module Contract = struct
  include Contract_repr
  include Contract_storage
end
module Roll = struct
  include Roll_repr
  include Roll_storage
end
module Nonce = Nonce_storage
module Seed = struct
  include Seed_repr
  include Seed_storage
end
module Bootstrap = Bootstrap_storage
module Reward = Reward_storage

module Fitness = struct

  include Fitness_repr
  include Fitness
  type t = fitness
  include Fitness_storage

end

module Asset = Asset_repr

let init = Init_storage.may_initialize
let finalize c = return (Storage.recover c)
let configure_sandbox = Init_storage.configure_sandbox
let get_prevalidation = Storage.get_prevalidation
let set_prevalidation = Storage.set_prevalidation

let activate = Storage.activate
let fork_test_network = Storage.fork_test_network
let set_test_protocol = Storage.set_test_protocol
