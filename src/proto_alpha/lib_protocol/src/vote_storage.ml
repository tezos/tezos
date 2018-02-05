(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let record_proposal ctxt delegate proposal =
  Storage.Vote.Proposals.add ctxt (delegate, proposal)

let get_proposals ctxt =
  Storage.Vote.Proposals.fold ctxt
    ~init:Protocol_hash.Map.empty
    ~f:(fun (proposal, _delegate) acc ->
        let previous =
          try Protocol_hash.Map.find proposal acc
          with Not_found -> 0l in
        Lwt.return (Protocol_hash.Map.add proposal (Int32.succ previous) acc))

let clear_proposals ctxt =
  Storage.Vote.Proposals.clear ctxt

type ballots = {
  yay: int32 ;
  nay: int32 ;
  pass: int32 ;
}

let record_ballot = Storage.Vote.Ballots.init_set

let get_ballots ctxt =
  Storage.Vote.Ballots.fold ctxt
    ~f:(fun delegate ballot (ballots: ballots tzresult) ->
        Storage.Vote.Listings.get ctxt delegate >>=? fun weight ->
        let count = Int32.add weight in
        Lwt.return begin
          ballots >>? fun ballots ->
          match ballot with
          | Yay  -> ok { ballots with yay = count ballots.yay }
          | Nay  -> ok { ballots with nay = count ballots.nay }
          | Pass  -> ok { ballots with pass = count ballots.pass }
        end)
    ~init:(ok { yay = 0l ; nay = 0l; pass = 0l })

let clear_ballots = Storage.Vote.Ballots.clear

let freeze_listings ctxt =
  Roll_storage.fold ctxt (ctxt, 0l)
    ~f:(fun _roll contract (ctxt, total as acc) ->
        Contract_storage.get_delegate_opt ctxt contract >>=? function
        | None -> return acc
        | Some delegate ->
            begin
              Storage.Vote.Listings.get_option ctxt delegate >>=? function
              | None -> return 0l
              | Some count -> return count
            end >>=? fun count ->
            Storage.Vote.Listings.init_set
              ctxt delegate (Int32.succ count) >>= fun ctxt ->
            return (ctxt, Int32.succ total)) >>=? fun (ctxt, total) ->
  Storage.Vote.Listings_size.init ctxt total >>=? fun ctxt ->
  return ctxt

let listing_size = Storage.Vote.Listings_size.get
let in_listings = Storage.Vote.Listings.mem

let clear_listings ctxt =
  Storage.Vote.Listings.clear ctxt >>= fun ctxt ->
  Storage.Vote.Listings_size.remove ctxt >>= fun ctxt ->
  return ctxt

let get_current_period_kind = Storage.Vote.Current_period_kind.get
let set_current_period_kind = Storage.Vote.Current_period_kind.set

let get_current_quorum = Storage.Vote.Current_quorum.get
let set_current_quorum = Storage.Vote.Current_quorum.set

let get_current_proposal = Storage.Vote.Current_proposal.get
let init_current_proposal = Storage.Vote.Current_proposal.init
let clear_current_proposal = Storage.Vote.Current_proposal.delete

let init ctxt =
  Storage.Vote.Current_quorum.init ctxt 80_00l >>=? fun ctxt ->
  Storage.Vote.Current_period_kind.init ctxt Proposal >>=? fun ctxt ->
  return ctxt
