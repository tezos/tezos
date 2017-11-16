(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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
}

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
           voting_period; voting_period_position } ->
      (level, level_position,
       cycle, cycle_position,
       voting_period, voting_period_position))
    (fun (level, level_position,
          cycle, cycle_position,
          voting_period, voting_period_position) ->
      { level ; level_position ;
        cycle ; cycle_position ;
        voting_period ; voting_period_position })
    (obj6
       (req "level" Raw_level_repr.encoding)
       (req "level_position" int32)
       (req "cycle" Cycle_repr.encoding)
       (req "cycle_position" int32)
       (req "voting_period" Voting_period_repr.encoding)
       (req "voting_period_position" int32))

let root first_level =
  { level = first_level ;
    level_position = 0l ;
    cycle = Cycle_repr.root ;
    cycle_position = 0l ;
    voting_period = Voting_period_repr.root ;
    voting_period_position = 0l ;
  }

let from_raw ~first_level ~cycle_length ~voting_period_length level =
  let raw_level = Raw_level_repr.to_int32 level in
  let first_level = Raw_level_repr.to_int32 first_level in
  let level_position =
    Compare.Int32.max 0l (Int32.sub raw_level first_level) in
  let cycle =
    Cycle_repr.of_int32_exn (Int32.div level_position cycle_length) in
  let cycle_position = Int32.rem level_position cycle_length in
  let voting_period =
    Voting_period_repr.of_int32_exn
      (Int32.div level_position voting_period_length) in
  let voting_period_position =
    Int32.rem level_position voting_period_length in
  { level ; level_position ;
    cycle ; cycle_position ;
    voting_period ; voting_period_position }

let diff { level = l1 } { level = l2 } =
  Int32.sub (Raw_level_repr.to_int32 l1) (Raw_level_repr.to_int32 l2)

let compare { level = l1 } { level = l2 } = Raw_level_repr.compare l1 l2
let (=) { level = l1 } { level = l2 } = Raw_level_repr.(=) l1 l2
let (<>) { level = l1 } { level = l2 } = Raw_level_repr.(<>) l1 l2
let (>) { level = l1 } { level = l2 } = Raw_level_repr.(>) l1 l2
let (>=) { level = l1 } { level = l2 } = Raw_level_repr.(>=) l1 l2
let (<=) { level = l1 } { level = l2 } = Raw_level_repr.(<=) l1 l2
let (<) { level = l1 } { level = l2 } = Raw_level_repr.(<) l1 l2
let min l1 l2 = if l1 <= l2 then l1 else l2
let max l1 l2 = if l1 >= l2 then l1 else l2

