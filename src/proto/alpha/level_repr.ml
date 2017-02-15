(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type t = {
  level: Raw_level_repr.t ;
  cycle: Cycle_repr.t ;
  cycle_position: int32 ;
  voting_period: Voting_period_repr.t ;
  voting_period_position: int32 ;
}

type level = t

let pp ppf { level } = Raw_level_repr.pp ppf level

let pp_full ppf l =
  Format.fprintf ppf
    "%a (cycle %a.%ld) (vote %a.%ld)"
    Raw_level_repr.pp l.level
    Cycle_repr.pp l.cycle l.cycle_position
    Voting_period_repr.pp l.voting_period l.voting_period_position

let encoding =
  let open Data_encoding in
  conv
    (fun { level ; cycle ; cycle_position ;
           voting_period; voting_period_position } ->
      (level, cycle, cycle_position,
       voting_period, voting_period_position))
    (fun (level, cycle, cycle_position,
          voting_period, voting_period_position) ->
       { level ; cycle ; cycle_position ;
         voting_period ; voting_period_position })
    (obj5
       (req "level" Raw_level_repr.encoding)
       (req "cycle" Cycle_repr.encoding)
       (req "cycle_position" int32)
       (req "voting_period" Voting_period_repr.encoding)
       (req "voting_period_position" int32))

let root =
  { level = Raw_level_repr.root ;
    cycle = Cycle_repr.root ;
    cycle_position = 0l ;
    voting_period = Voting_period_repr.root ;
    voting_period_position = 0l ;
  }

let from_raw ~cycle_length ~voting_period_length level =
  let raw_level = Raw_level_repr.to_int32 level in
  let cycle = Cycle_repr.of_int32_exn (Int32.div raw_level cycle_length) in
  let cycle_position = Int32.rem raw_level cycle_length in
  let voting_period =
    Voting_period_repr.of_int32_exn
      (Int32.div raw_level voting_period_length) in
  let voting_period_position =
    Int32.rem raw_level voting_period_length in
  { level ; cycle ; cycle_position ;
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
