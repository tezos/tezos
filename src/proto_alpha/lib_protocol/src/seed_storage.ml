(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Precomputed_seed
  | Invalid_cycle

let compute_for_cycle c cycle =
  begin
    begin
      match Cycle_repr.pred cycle with
      | None -> fail Precomputed_seed
      | Some previous_cycle -> return previous_cycle
    end >>=? fun previous_cycle ->
    begin
      match Cycle_repr.pred previous_cycle with
      | None -> fail Precomputed_seed
      | Some pprevious_cycle ->
          match Cycle_repr.pred pprevious_cycle with
          | None -> fail Precomputed_seed
          | Some revealed_cycle -> return revealed_cycle
    end >>=? fun revealed_cycle ->
    begin
      let levels = Level_storage.levels_in_cycle c revealed_cycle in
      let combine (c, random_seed) level =
        Storage.Seed.Nonce.get c level >>=? function
        | Revealed nonce ->
            Storage.Seed.Nonce.delete c level >>=? fun c ->
            return (c, Seed_repr.nonce random_seed nonce)
        | Unrevealed _ ->
            Storage.Seed.Nonce.delete c level >>=? fun c ->
            return (c, random_seed)
      in
      Storage.Seed.For_cycle.get c previous_cycle >>=? fun seed ->
      fold_left_s combine (c, seed) levels
    end >>=? fun (c, seed) ->
    Storage.Seed.For_cycle.init c cycle seed >>=? fun c ->
    return c
  end >>= function
  | Error [Precomputed_seed] -> return c
  | c -> Lwt.return c

let for_cycle c cycle =
  let current_level = Level_storage.current c in
  let current_cycle = current_level.cycle in
  let next_cycle = (Level_storage.succ c current_level).cycle in
  fail_unless
    Cycle_repr.(cycle = current_cycle || cycle = next_cycle)
    Invalid_cycle >>=? fun () ->
  Storage.Seed.For_cycle.get c cycle

let clear_cycle c cycle =
  Storage.Seed.For_cycle.delete c cycle

let init c =
  Storage.Seed.For_cycle.init c
    Cycle_repr.root
    Seed_repr.initial_seed_0 >>=? fun c ->
  Storage.Seed.For_cycle.init c
    Cycle_repr.(succ root)
    Seed_repr.initial_seed_1 >>=? fun c ->
  Storage.Seed.For_cycle.init c
    Cycle_repr.(succ (succ root))
    Seed_repr.initial_seed_2
