(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let recorded_proposal_count_for_delegate ctxt proposer =
  Storage.Vote.Proposals_count.get_option ctxt proposer >>=? function
  | None -> return 0
  | Some count -> return count

let record_proposal ctxt proposal proposer =
  recorded_proposal_count_for_delegate ctxt proposer >>=? fun count ->
  Storage.Vote.Proposals_count.init_set ctxt proposer (count + 1) >>= fun ctxt ->
  Storage.Vote.Proposals.add ctxt (proposal, proposer) >>= fun ctxt ->
  return ctxt

let get_proposals ctxt =
  Storage.Vote.Proposals.fold ctxt
    ~init:(ok Protocol_hash.Map.empty)
    ~f:(fun (proposal, delegate) acc ->
        (* Assuming the same listings is used at votings *)
        Storage.Vote.Listings.get ctxt delegate >>=? fun weight ->
        Lwt.return begin acc >>? fun acc ->
          let previous =
            match Protocol_hash.Map.find_opt proposal acc with
            | None -> 0l
            | Some x -> x
          in
          ok (Protocol_hash.Map.add proposal (Int32.add weight previous) acc)
        end)

let clear_proposals ctxt =
  Storage.Vote.Proposals_count.clear ctxt >>= fun ctxt ->
  Storage.Vote.Proposals.clear ctxt

type ballots = {
  yay: int32 ;
  nay: int32 ;
  pass: int32 ;
}

let ballots_encoding =
  let open Data_encoding in
  conv
    (fun { yay ; nay ; pass } -> ( yay , nay , pass ))
    (fun ( yay , nay , pass ) -> { yay ; nay ; pass })
  @@ obj3
    (req "yay"  int32)
    (req "nay"  int32)
    (req "pass" int32)

let has_recorded_ballot = Storage.Vote.Ballots.mem
let record_ballot = Storage.Vote.Ballots.init

let get_ballots ctxt =
  Storage.Vote.Ballots.fold ctxt
    ~f:(fun delegate ballot (ballots: ballots tzresult) ->
        (* Assuming the same listings is used at votings *)
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

let get_ballot_list = Storage.Vote.Ballots.bindings

let clear_ballots = Storage.Vote.Ballots.clear

let listings_encoding =
  Data_encoding.(list (obj2
                         (req "pkh" Signature.Public_key_hash.encoding)
                         (req "rolls" int32)))

let freeze_listings ctxt =
  Roll_storage.fold ctxt (ctxt, 0l)
    ~f:(fun _roll delegate (ctxt, total) ->
        (* TODO use snapshots *)
        let delegate = Signature.Public_key.hash delegate in
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
let get_listings = Storage.Vote.Listings.bindings

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
