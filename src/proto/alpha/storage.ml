(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash
open Storage_functors

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]
let version_value = "alpha"

type error += Incompatiple_protocol_version

let is_first_block ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      return true
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        return false
      else if Compare.String.(s = "genesis") then
        return true
      else
        fail Incompatiple_protocol_version

let version = "v1"
let first_level_key = [ version ; "first_level" ]
let sandboxed_key = [ version ; "sandboxed" ]

type t = Storage_functors.context

type error += Invalid_sandbox_parameter

let current_level { level } = level
let current_timestamp { timestamp } = timestamp
let current_fitness { fitness } = fitness
let set_current_fitness c fitness = { c with fitness }

let get_first_level ctxt =
  Context.get ctxt first_level_key >>= function
  | None -> failwith "Invalid context"
  | Some bytes ->
      match
        Data_encoding.Binary.of_bytes Raw_level_repr.encoding bytes
      with
      | None -> failwith "Invalid context"
      | Some level -> return level

let set_first_level ctxt level =
  let bytes =
    Data_encoding.Binary.to_bytes Raw_level_repr.encoding level in
  Context.set ctxt first_level_key bytes >>= fun ctxt ->
  return ctxt

let get_sandboxed c =
  Context.get c sandboxed_key >>= function
  | None -> return None
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None -> fail Invalid_sandbox_parameter
      | Some json -> return (Some json)

let set_sandboxed c json =
  Context.set c sandboxed_key
    (Data_encoding.Binary.to_bytes Data_encoding.json json)

let may_tag_first_block ctxt level =
  is_first_block ctxt >>=? function
  | false ->
      get_first_level ctxt >>=? fun level ->
      return (ctxt, false, level)
  | true ->
      Context.set ctxt version_key
        (MBytes.of_string version_value) >>= fun ctxt ->
      set_first_level ctxt level >>=? fun ctxt ->
      return (ctxt, true, level)

let prepare ~level ~timestamp ~fitness ctxt =
  Lwt.return (Raw_level_repr.of_int32 level ) >>=? fun level ->
  Lwt.return (Fitness_repr.to_int64 fitness) >>=? fun fitness ->
  may_tag_first_block ctxt level >>=? fun (ctxt, first_block, first_level) ->
  get_sandboxed ctxt >>=? fun sandbox ->
  Constants_repr.read sandbox >>=? function constants ->
  let level =
    Level_repr.from_raw
      ~first_level
      ~cycle_length:constants.Constants_repr.cycle_length
      ~voting_period_length:constants.Constants_repr.voting_period_length
    level in
  return ({ context = ctxt ; constants ; level ;
            timestamp ; fitness ; first_level},
          first_block)
let recover { context } : Context.t = context

let first_level { first_level } = first_level
let constants { constants } = constants

module Key = struct

  let store_root tail = version :: "store" :: tail

  let global_counter = store_root ["global_counter"]

  let next_cycle_to_be_rewarded = store_root ["next_cycle_to_be_rewarded"]
  let rewards = store_root ["rewards"]

  let public_keys = ["public_keys" ; "ed25519"]

  module Roll = struct
    let store_root l = store_root ("rolls" :: l)
    let next = store_root [ "next" ]
    let limbo = store_root [ "limbo" ]
    let roll_store roll l =
      store_root @@ Int32.to_string (Roll_repr.to_int32 roll) :: l
    let successor r = roll_store r ["successor"]
    let owner r = roll_store r ["owner"]
  end

  module Cycle = struct
    let store_root l = store_root ("cycles" :: l)
    let cycle_store c l =
      store_root @@ Int32.to_string (Cycle_repr.to_int32 c) :: l
    let last_roll c = cycle_store c [ "last_roll" ]
    let random_seed c = cycle_store c [ "random_seed" ]
    let reward_date c = cycle_store c [ "reward_date" ]
    let roll_owner (c, r) =
      cycle_store c [ "roll_owners" ; Int32.to_string (Roll_repr.to_int32 r)]
    let unrevealed_nonce_hash l =
      let c = l.Level_repr.cycle in
      cycle_store c [ "unrevealed_nonce_hash" ;
                      Int32.to_string l.Level_repr.cycle_position ]
  end

  module Contract = struct

    let store_root l = store_root ("contracts" :: l)
    let set = store_root ["set"]
    let pubkey_contract l = store_root ("pubkey" :: l)
    let generic_contract l = store_root ("generic" :: l)
    let contract_store c l =
      match c with
      | Contract_repr.Default k ->
          pubkey_contract @@ Ed25519.Public_key_hash.to_path k @ l
      | Contract_repr.Originated h ->
          generic_contract @@ Contract_hash.to_path h @ l
    let roll_list c = contract_store c ["roll_list"]
    let change c = contract_store c ["change"]
    let balance c = contract_store c ["balance"]
    let manager c = contract_store c ["manager"]
    let spendable c = contract_store c ["spendable"]
    let delegatable c = contract_store c ["delegatable"]
    let delegate c = contract_store c ["delegate"]
    let counter c = contract_store c ["counter"]
    let code c = contract_store c ["code"]
    let storage c = contract_store c ["storage"]
    let code_fees c = contract_store c ["code_fees"]
    let storage_fees c = contract_store c ["storage_fees"]
  end

  module Vote = struct
    let store_root l = store_root ("votes" :: l)
    let period_kind = store_root ["current_period_kind"]
    let quorum = store_root ["current_quorum"]
    let proposition = store_root ["current_proposition"]
    let proposals = store_root ["proposals"]
    let ballots = store_root ["ballots"]
    let listings_size = store_root ["listings_size"]
    let listings = store_root ["listings"]
  end

end

(** Rolls *)

module Roll = struct

  module Next =
    Make_single_data_storage(struct
      type value = Roll_repr.t
      let name = "next fresh roll"
      let key = Key.Roll.next
      let encoding = Roll_repr.encoding
    end)

  module Limbo =
    Make_single_optional_data_storage(struct
      type value = Roll_repr.t
      let name = "limbo"
      let key = Key.Roll.limbo
      let encoding = Roll_repr.encoding
    end)

  module Last_for_cycle =
    Make_indexed_data_storage(struct
      type key = Cycle_repr.t
      type value = Roll_repr.t
      let name = "last roll for current cycle"
      let key = Key.Cycle.last_roll
      let encoding = Roll_repr.encoding
    end)

  module Successor =
    Make_indexed_optional_data_storage(struct
      type key = Roll_repr.t
      type value = Roll_repr.t
      let name = "roll successor"
      let key = Key.Roll.successor
      let encoding = Roll_repr.encoding
    end)

  module Owner =
    Make_indexed_data_storage(struct
      type key = Roll_repr.t
      type value = Contract_repr.t
      let name = "roll owner"
      let key = Key.Roll.owner
      let encoding = Contract_repr.encoding
    end)

  module Owner_for_cycle =
    Make_indexed_data_storage(struct
      type key = Cycle_repr.t * Roll_repr.t
      type value = Ed25519.Public_key_hash.t
      let name = "roll owner for current cycle"
      let key = Key.Cycle.roll_owner
      let encoding = Ed25519.Public_key_hash.encoding
    end)

   module Contract_roll_list =
    Make_indexed_optional_data_storage(struct
      type key = Contract_repr.t
      type value = Roll_repr.t
      let name = "contract roll list"
      let key = Key.Contract.roll_list
      let encoding = Roll_repr.encoding
    end)

  module Contract_change =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Tez_repr.t
      let name = "contract change"
      let key = Key.Contract.change
      let encoding = Tez_repr.encoding
    end)

end

(** Contracts handling *)

module Contract = struct

  module Global_counter =
    Make_single_data_storage(struct
      type value = int32
      let name = "global counter"
      let key = Key.global_counter
      let encoding = Data_encoding.int32
    end)

  (** FIXME REMOVE : use 'list' *)
  module Set =
    Make_data_set_storage(struct
      type value = Contract_repr.t
      let name = "contract set"
      let key = Key.Contract.set
      let encoding = Contract_repr.encoding
    end)

  module Balance =
    Make_indexed_data_storage(
    struct
      type key = Contract_repr.t
      type value = Tez_repr.t
      let name = "contract balance"
      let key = Key.Contract.balance
      let encoding = Tez_repr.encoding
    end)

  module Manager =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Ed25519.Public_key_hash.t
      let name = "contract manager"
      let key = Key.Contract.manager
      let encoding = Ed25519.Public_key_hash.encoding
    end)

  module Spendable =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = bool
      let name = "contract spendable"
      let key = Key.Contract.spendable
      let encoding = Data_encoding.bool
    end)

  module Delegatable =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = bool
      let name = "contract delegatable"
      let key = Key.Contract.delegatable
      let encoding = Data_encoding.bool
    end)

  module Delegate =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Ed25519.Public_key_hash.t
      let name = "contract delegate"
      let key = Key.Contract.delegate
      let encoding = Ed25519.Public_key_hash.encoding
    end)

  module Counter =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Int32.t
      let name = "contract counter"
      let key = Key.Contract.counter
      let encoding = Data_encoding.int32
    end)

  module Code =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Script_repr.code
      let name = "contract code"
      let key = Key.Contract.code
      let encoding = Script_repr.code_encoding
    end)

  module Storage =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Script_repr.storage
      let name = "contract storage"
      let key = Key.Contract.storage
      let encoding = Script_repr.storage_encoding
    end)

  module Code_fees =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Tez_repr.t
      let name = "contract code fees"
      let key = Key.Contract.code_fees
      let encoding = Tez_repr.encoding
    end)

  module Storage_fees =
    Make_indexed_data_storage(struct
      type key = Contract_repr.t
      type value = Tez_repr.t
      let name = "contract storage fees"
      let key = Key.Contract.storage_fees
      let encoding = Tez_repr.encoding
    end)

end

(** Votes **)

module Vote = struct

  module Current_period_kind =
    Make_single_data_storage(struct
      type value = Voting_period_repr.kind
      let name = "current period kind"
      let key = Key.Vote.period_kind
      let encoding = Voting_period_repr.kind_encoding
    end)

  module Current_quorum =
    Make_single_data_storage(struct
      type value = int32
      let name = "current quorum"
      let key = Key.Vote.quorum
      let encoding = Data_encoding.int32
    end)

  module Current_proposal =
    Make_single_data_storage(struct
      type value = Protocol_hash.t
      let name = "current proposal"
      let key = Key.Vote.proposition
      let encoding = Protocol_hash.encoding
    end)

  module Listings_size =
    Make_single_data_storage(struct
      type value = int32
      let name = "listing size"
      let key = Key.Vote.listings_size
      let encoding = Data_encoding.int32
    end)

  module Listings =
    Make_iterable_data_storage (Ed25519.Public_key_hash)
      (struct
        type value = int32
        let key = Key.Vote.listings
        let name = "listings"
        let encoding = Data_encoding.int32
      end)

  module Proposals =
    Make_data_set_storage
      (struct
        type value = Protocol_hash.t * Ed25519.Public_key_hash.t
        let name = "proposals"
        let encoding =
          Data_encoding.tup2
            Protocol_hash.encoding Ed25519.Public_key_hash.encoding
        let key = Key.Vote.proposals
      end)

  module Ballots =
    Make_iterable_data_storage (Ed25519.Public_key_hash)
      (struct
        type value = Vote_repr.ballot
        let key = Key.Vote.ballots
        let name = "ballot"
        let encoding = Vote_repr.ballot_encoding
  end)

end

(** Keys *)

module Public_key =
  Make_iterable_data_storage (Ed25519.Public_key_hash)
    (struct
      type value = Ed25519.Public_key.t
      let key = Key.public_keys
      let name = "public keys"
      let encoding = Ed25519.Public_key.encoding
    end)

(** Seed *)

module Seed = struct

  type nonce_status =
    | Unrevealed of {
        nonce_hash: Tezos_hash.Nonce_hash.t ;
        delegate_to_reward: Ed25519.Public_key_hash.t ;
        reward_amount: Tez_repr.t ;
      }
    | Revealed of Seed_repr.nonce

  module Nonce =
    Make_indexed_data_storage(struct
      type key = Level_repr.level
      type value = nonce_status
      let name = "unrevealed nonce hash"
      let key = Key.Cycle.unrevealed_nonce_hash
      let encoding =
        let open Data_encoding in
        union [
          case ~tag:0
            (tup3
               Nonce_hash.encoding
               Ed25519.Public_key_hash.encoding
               Tez_repr.encoding
            )
            (function
              | Unrevealed { nonce_hash ; delegate_to_reward ; reward_amount } ->
                  Some (nonce_hash, delegate_to_reward, reward_amount)
              | _ -> None)
            (fun (nonce_hash, delegate_to_reward, reward_amount) ->
               Unrevealed { nonce_hash ; delegate_to_reward ; reward_amount }) ;
          case ~tag:1
            Seed_repr.nonce_encoding
            (function
              | Revealed nonce -> Some nonce
              | _ -> None)
            (fun nonce -> Revealed nonce)
        ]
    end)

  module For_cycle =
    Make_indexed_data_storage(struct
      type key = Cycle_repr.t
      type value = Seed_repr.seed
      let name = "cycle random seed"
      let key = Key.Cycle.random_seed
      let encoding = Seed_repr.seed_encoding
    end)

end

(** Rewards *)

module Rewards = struct

  module Next =
    Make_single_data_storage(struct
      type value = Cycle_repr.t
      let name = "reward cycle"
      let key = Key.next_cycle_to_be_rewarded
      let encoding = Cycle_repr.encoding
    end)

  module Date =
    Make_indexed_data_storage(struct
      type key = Cycle_repr.t
      type value = Time_repr.t
      let name = "reward timestamp"
      let key = Key.Cycle.reward_date
      let encoding = Time_repr.encoding
    end)

  module Amount =
    Raw_make_iterable_data_storage(struct
      type t = Ed25519.Public_key_hash.t * Cycle_repr.t
      let prefix = Key.rewards
      let length = Ed25519.Public_key_hash.path_length + 1
      let to_path (pkh, c) =
        Ed25519.Public_key_hash.to_path pkh @
        [Int32.to_string (Cycle_repr.to_int32 c)]
      let of_path p =
        match List.rev p with
        | [] -> assert false
        | cycle :: rev_pkh ->
            (Ed25519.Public_key_hash.of_path_exn (List.rev rev_pkh),
             Cycle_repr.of_int32_exn @@ Int32.of_string cycle)
      let compare (pkh1, c1) (pkh2, c2) =
        let cmp1 = Ed25519.Public_key_hash.compare pkh1 pkh2 in
        if Compare.Int.(cmp1 = 0) then Cycle_repr.compare c1 c2
        else cmp1
    end)(struct
      type value = Tez_repr.t
      let name = "level miner contract"
      let encoding = Tez_repr.encoding
    end)

end

let activate ({ context = c } as s) h =
  Updater.activate c h >>= fun c -> Lwt.return { s with context = c }
let fork_test_network ({ context = c } as s) protocol expiration =
  Updater.fork_test_network c ~protocol ~expiration >>= fun c ->
  Lwt.return { s with context = c }

(** Resolver *)

let () =
  Storage_functors.register_resolvers
    (module Contract_hash)
    [ Key.Contract.generic_contract [] ] ;
  Storage_functors.register_resolvers
    (module Ed25519.Public_key_hash)
    [ Key.Contract.pubkey_contract [] ;
      Key.public_keys ]
