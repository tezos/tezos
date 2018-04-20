(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Storage_functors

module Int = struct
  type t = int
  let encoding = Data_encoding.uint16
end

module Int32 = struct
  type t = Int32.t
  let encoding = Data_encoding.int32
end

module Int_index = struct
  type t = int
  let path_length = 1
  let to_path c l = string_of_int c :: l
  let of_path = function
    | [] | _ :: _ :: _ -> None
    | [ c ] ->
        try Some (int_of_string c)
        with _ -> None
end

module String_index = struct
  type t = string
  let path_length = 1
  let to_path c l = c :: l
  let of_path = function
    | [ c ] -> Some c
    | [] | _ :: _ :: _ -> None
end

module Last_block_priority =
  Make_single_data_storage
    (Raw_context)
    (struct let name = ["last_block_priority"] end)
    (Make_value(Int))

(** Contracts handling *)

module Contract = struct

  module Raw_context =
    Make_subcontext(Raw_context)(struct let name = ["contracts"] end)

  module Global_counter =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["global_counter"] end)
      (Make_value(Int32))

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext(Raw_context)(struct let name = ["index"] end))
      (Contract_repr.Index)

  let fold = Indexed_context.fold_keys
  let list = Indexed_context.keys

  module Balance =
    Indexed_context.Make_map
      (struct let name = ["balance"] end)
      (Make_value(Tez_repr))

  module Frozen_balance_index =
    Make_indexed_subcontext
      (Make_subcontext
         (Indexed_context.Raw_context)
         (struct let name = ["frozen_balance"] end))
      (Cycle_repr.Index)

  module Frozen_deposits =
    Frozen_balance_index.Make_map
      (struct let name = ["deposits"] end)
      (Make_value(Tez_repr))

  module Frozen_fees =
    Frozen_balance_index.Make_map
      (struct let name = ["fees"] end)
      (Make_value(Tez_repr))

  module Frozen_rewards =
    Frozen_balance_index.Make_map
      (struct let name = ["rewards"] end)
      (Make_value(Tez_repr))

  module Manager =
    Indexed_context.Make_map
      (struct let name = ["manager"] end)
      (Make_value(Manager_repr))

  module Spendable =
    Indexed_context.Make_set
      (struct let name = ["spendable"] end)

  module Delegatable =
    Indexed_context.Make_set
      (struct let name = ["delegatable"] end)

  module Delegate =
    Indexed_context.Make_map
      (struct let name = ["delegate"] end)
      (Make_value(Signature.Public_key_hash))

  module Inactive_delegate =
    Indexed_context.Make_set
      (struct let name = ["inactive_delegate"] end)

  module Delegate_desactivation =
    Indexed_context.Make_map
      (struct let name = ["delegate_desactivation"] end)
      (Make_value(Cycle_repr))

  module Delegated =
    Make_data_set_storage
      (Make_subcontext
         (Indexed_context.Raw_context)
         (struct let name = ["delegated"] end))
      (Contract_hash)

  module Counter =
    Indexed_context.Make_map
      (struct let name = ["counter"] end)
      (Make_value(Int32))

  module Code =
    Indexed_context.Make_carbonated_map
      (struct let name = ["code"] end)
      (Make_carbonated_value(struct
         type t = Script_repr.expr
         let encoding = Script_repr.expr_encoding
       end))

  module Storage =
    Indexed_context.Make_carbonated_map
      (struct let name = ["storage"] end)
      (Make_carbonated_value(struct
         type t = Script_repr.expr
         let encoding = Script_repr.expr_encoding
       end))

  type bigmap_key = Raw_context.t * Contract_repr.t

  module Big_map =
    Storage_functors.Make_indexed_carbonated_data_storage
      (Make_subcontext
         (Indexed_context.Raw_context)
         (struct let name = ["big_map"] end))
      (String_index)
      (Make_carbonated_value (struct
         type t = Script_repr.expr
         let encoding = Script_repr.expr_encoding
       end))

  module Paid_fees =
    Indexed_context.Make_map
      (struct let name = ["paid_fees"] end)
      (Make_value(Tez_repr))

  module Fees =
    Indexed_context.Make_map
      (struct let name = ["fees"] end)
      (Make_value(Tez_repr))

  module Roll_list =
    Indexed_context.Make_map
      (struct let name = ["roll_list"] end)
      (Make_value(Roll_repr))

  module Change =
    Indexed_context.Make_map
      (struct let name = ["change"] end)
      (Make_value(Tez_repr))

end

module Delegates =
  Make_data_set_storage
    (Make_subcontext(Raw_context)(struct let name = ["delegates"] end))
    (Signature.Public_key_hash)

(** Rolls *)

module Cycle = struct

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext(Raw_context)(struct let name = ["cycle"] end))
      (Cycle_repr.Index)

  module Last_roll =
    Make_indexed_data_storage
      (Make_subcontext
         (Indexed_context.Raw_context)
         (struct let name = ["last_roll"] end))
      (Int_index)
      (Make_value(Roll_repr))

  module Roll_snapshot =
    Indexed_context.Make_map
      (struct let name = ["roll_snapshot"] end)
      (Make_value(Int))

  type unrevealed_nonce = {
    nonce_hash: Nonce_hash.t ;
    delegate: Signature.Public_key_hash.t ;
    rewards: Tez_repr.t ;
    fees: Tez_repr.t ;
  }

  type nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  let nonce_status_encoding =
    let open Data_encoding in
    union [
      case (Tag 0)
        (tup4
           Nonce_hash.encoding
           Signature.Public_key_hash.encoding
           Tez_repr.encoding
           Tez_repr.encoding)
        (function
          | Unrevealed { nonce_hash ; delegate ; rewards ; fees } ->
              Some (nonce_hash, delegate, rewards, fees)
          | _ -> None)
        (fun (nonce_hash, delegate, rewards, fees) ->
           Unrevealed { nonce_hash ; delegate ; rewards ; fees }) ;
      case (Tag 1)
        Seed_repr.nonce_encoding
        (function
          | Revealed nonce -> Some nonce
          | _ -> None)
        (fun nonce -> Revealed nonce)
    ]

  module Nonce =
    Make_indexed_data_storage
      (Make_subcontext
         (Indexed_context.Raw_context)
         (struct let name = ["nonces"] end))
      (Raw_level_repr.Index)
      (Make_value(struct
         type t = nonce_status
         let encoding = nonce_status_encoding
       end))

  module Seed =
    Indexed_context.Make_map
      (struct let name = ["random_seed"] end)
      (Make_value(struct
         type t = Seed_repr.seed
         let encoding = Seed_repr.seed_encoding
       end))

end

module Roll = struct

  module Raw_context =
    Make_subcontext(Raw_context)(struct let name = ["rolls"] end)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext(Raw_context)(struct let name = ["index"] end))
      (Roll_repr.Index)

  module Next =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["next"] end)
      (Make_value(Roll_repr))

  module Limbo =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["limbo"] end)
      (Make_value(Roll_repr))

  module Delegate_roll_list =
    Wrap_indexed_data_storage(Contract.Roll_list)(struct
      type t = Signature.Public_key_hash.t
      let wrap = Contract_repr.implicit_contract
      let unwrap = Contract_repr.is_implicit
    end)

  module Successor =
    Indexed_context.Make_map
      (struct let name = ["successor"] end)
      (Make_value(Roll_repr))

  module Delegate_change =
    Wrap_indexed_data_storage(Contract.Change)(struct
      type t = Signature.Public_key_hash.t
      let wrap = Contract_repr.implicit_contract
      let unwrap = Contract_repr.is_implicit
    end)

  module Snapshoted_owner_index = struct
    type t = Cycle_repr.t * int
    let path_length = Cycle_repr.Index.path_length + 1
    let to_path (c, n) s =
      Cycle_repr.Index.to_path c (string_of_int n :: s)
    let of_path l =
      match Misc.take Cycle_repr.Index.path_length l with
      | None | Some (_, ([] | _ :: _ :: _ ))-> None
      | Some (l1, [l2]) ->
          match Cycle_repr.Index.of_path l1 with
          | None -> None
          | Some c -> begin
              try Some (c, int_of_string l2)
              with _ -> None
            end
  end

  module Owner =
    Make_indexed_data_snapshotable_storage
      (Make_subcontext(Raw_context)(struct let name = ["owner"] end))
      (Snapshoted_owner_index)
      (Roll_repr.Index)
      (Make_value(Signature.Public_key))

  module Snapshot_for_cycle = Cycle.Roll_snapshot
  module Last_for_snapshot = Cycle.Last_roll

  let clear = Indexed_context.clear

end

(** Votes **)

module Vote = struct

  module Raw_context =
    Make_subcontext(Raw_context)(struct let name = ["votes"] end)

  module Current_period_kind =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["current_period_kind"] end)
      (Make_value(struct
         type t = Voting_period_repr.kind
         let encoding = Voting_period_repr.kind_encoding
       end))

  module Current_quorum =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["current_quorum"] end)
      (Make_value(Int32))

  module Current_proposal =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["current_proposal"] end)
      (Make_value(Protocol_hash))

  module Listings_size =
    Make_single_data_storage
      (Raw_context)
      (struct let name = ["listings_size"] end)
      (Make_value(Int32))

  module Listings =
    Make_indexed_data_storage
      (Make_subcontext(Raw_context)(struct let name = ["listings"] end))
      (Signature.Public_key_hash)
      (Make_value(Int32))

  module Proposals =
    Make_data_set_storage
      (Make_subcontext(Raw_context)(struct let name = ["proposals"] end))
      (Pair(Protocol_hash)(Signature.Public_key_hash))

  module Ballots =
    Make_indexed_data_storage
      (Make_subcontext(Raw_context)(struct let name = ["ballots"] end))
      (Signature.Public_key_hash)
      (Make_value(struct
         type t = Vote_repr.ballot
         let encoding = Vote_repr.ballot_encoding
       end))

end

(** Seed *)

module Seed = struct

  type unrevealed_nonce = Cycle.unrevealed_nonce = {
    nonce_hash: Nonce_hash.t ;
    delegate: Signature.Public_key_hash.t ;
    rewards: Tez_repr.t ;
    fees: Tez_repr.t ;
  }

  type nonce_status = Cycle.nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  module Nonce = struct
    open Level_repr
    type context = Raw_context.t
    let mem ctxt l = Cycle.Nonce.mem (ctxt, l.cycle) l.level
    let get ctxt l = Cycle.Nonce.get (ctxt, l.cycle) l.level
    let get_option ctxt l = Cycle.Nonce.get_option (ctxt, l.cycle) l.level
    let set ctxt l v = Cycle.Nonce.set (ctxt, l.cycle) l.level v
    let init ctxt l v = Cycle.Nonce.init (ctxt, l.cycle) l.level v
    let init_set ctxt l v = Cycle.Nonce.init_set (ctxt, l.cycle) l.level v
    let set_option ctxt l v = Cycle.Nonce.set_option (ctxt, l.cycle) l.level v
    let delete ctxt l = Cycle.Nonce.delete (ctxt, l.cycle) l.level
    let remove ctxt l = Cycle.Nonce.remove (ctxt, l.cycle) l.level
  end
  module For_cycle = Cycle.Seed

end

(** Commitments *)

module Commitments =
  Make_indexed_data_storage
    (Make_subcontext(Raw_context)(struct let name = ["commitments"] end))
    (Unclaimed_public_key_hash.Index)
    (Make_value(Commitment_repr))

(** Ramp up security deposits... *)

module Ramp_up = struct

  module Rewards =
    Make_indexed_data_storage
      (Make_subcontext(Raw_context)(struct let name = ["ramp_up"; "rewards"] end))
      (Cycle_repr.Index)
      (Make_value(struct
         type t = Tez_repr.t * Tez_repr.t
         let encoding = Data_encoding.tup2 Tez_repr.encoding Tez_repr.encoding
       end))

  module Security_deposits =
    Make_indexed_data_storage
      (Make_subcontext(Raw_context)(struct let name = ["ramp_up"; "deposits"] end))
      (Cycle_repr.Index)
      (Make_value(struct
         type t = Tez_repr.t * Tez_repr.t
         let encoding = Data_encoding.tup2 Tez_repr.encoding Tez_repr.encoding
       end))

end

(** Resolver *)

let () =
  Raw_context.register_resolvers
    Contract_hash.b58check_encoding
    (fun ctxt p ->
       let p = Contract_repr.Index.contract_prefix p in
       Contract.Indexed_context.resolve ctxt p >|= fun l ->
       List.map
         (function
           | Contract_repr.Implicit _ -> assert false
           | Contract_repr.Originated s -> s)
         l) ;
  Raw_context.register_resolvers
    Ed25519.Public_key_hash.b58check_encoding
    (fun ctxt p ->
       let p = Contract_repr.Index.pkh_prefix_ed25519 p in
       Contract.Indexed_context.resolve ctxt p >|= fun l ->
       List.map
         (function
           | Contract_repr.Implicit (Ed25519 pkh) -> pkh
           | Contract_repr.Implicit _ -> assert false
           | Contract_repr.Originated _ -> assert false)
         l) ;
  Raw_context.register_resolvers
    Secp256k1.Public_key_hash.b58check_encoding
    (fun ctxt p ->
       let p = Contract_repr.Index.pkh_prefix_secp256k1 p in
       Contract.Indexed_context.resolve ctxt p >|= fun l ->
       List.map
         (function
           | Contract_repr.Implicit (Secp256k1 pkh) -> pkh
           | Contract_repr.Implicit _ -> assert false
           | Contract_repr.Originated _ -> assert false)
         l)
