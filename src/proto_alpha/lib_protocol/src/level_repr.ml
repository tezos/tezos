(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  level: Raw_level_repr.t ;
  level_position: int32 ;
  cycle: Cycle_repr.t ;
  cycle_position: int32 ;
  voting_period: Voting_period_repr.t ;
  voting_period_position: int32 ;
  expected_commitment: bool ;
}

include Compare.Make(struct
    type nonrec t = t
    let compare { level = l1 } { level = l2 } = Raw_level_repr.compare l1 l2
  end)

type level = t

let pp ppf { level } = Raw_level_repr.pp ppf level

let pp_full ppf l =
  Format.fprintf ppf
    "%a.%ld (cycle %a.%ld) (vote %a.%ld)"
    Raw_level_repr.pp l.level l.level_position
    Cycle_repr.pp l.cycle l.cycle_position
    Voting_period_repr.pp l.voting_period l.voting_period_position

let encoding =
  let open Data_encoding in
  conv
    (fun { level ; level_position ;
           cycle ; cycle_position ;
           voting_period; voting_period_position ;
           expected_commitment } ->
      (level, level_position,
       cycle, cycle_position,
       voting_period, voting_period_position,
       expected_commitment))
    (fun (level, level_position,
          cycle, cycle_position,
          voting_period, voting_period_position,
          expected_commitment) ->
      { level ; level_position ;
        cycle ; cycle_position ;
        voting_period ; voting_period_position ;
        expected_commitment })
    (obj7
       (req "level" Raw_level_repr.encoding)
       (req "level_position" int32)
       (req "cycle" Cycle_repr.encoding)
       (req "cycle_position" int32)
       (req "voting_period" Voting_period_repr.encoding)
       (req "voting_period_position" int32)
       (req "expected_commitment" bool))

let root first_level =
  { level = first_level ;
    level_position = 0l ;
    cycle = Cycle_repr.root ;
    cycle_position = 0l ;
    voting_period = Voting_period_repr.root ;
    voting_period_position = 0l ;
    expected_commitment = false ;
  }

let from_raw
    ~first_level ~blocks_per_cycle ~blocks_per_voting_period
    ~blocks_per_commitment
    level =
  let raw_level = Raw_level_repr.to_int32 level in
  let first_level = Raw_level_repr.to_int32 first_level in
  let level_position =
    Compare.Int32.max 0l (Int32.sub raw_level first_level) in
  let cycle =
    Cycle_repr.of_int32_exn (Int32.div level_position blocks_per_cycle) in
  let cycle_position = Int32.rem level_position blocks_per_cycle in
  let voting_period =
    Voting_period_repr.of_int32_exn
      (Int32.div level_position blocks_per_voting_period) in
  let voting_period_position =
    Int32.rem level_position blocks_per_voting_period in
  let expected_commitment =
    Compare.Int32.(Int32.rem cycle_position blocks_per_commitment =
                   Int32.pred blocks_per_commitment) in
  { level ; level_position ;
    cycle ; cycle_position ;
    voting_period ; voting_period_position ;
    expected_commitment }

let diff { level = l1 ; _ } { level = l2 ; _ } =
  Int32.sub (Raw_level_repr.to_int32 l1) (Raw_level_repr.to_int32 l2)

