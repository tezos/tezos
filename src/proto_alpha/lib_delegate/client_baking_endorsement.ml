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

[@@@ocaml.warning "-30"]

open Proto_alpha
open Alpha_context

include Internal_event.Legacy_logging.Make_semantic(struct
    let name = Proto_alpha.Name.name ^ ".client.endorsement"
  end)

open Logging

let get_signing_slots cctxt ~chain ~block delegate level =
  Alpha_services.Delegate.Endorsing_rights.get cctxt
    ~levels:[level]
    ~delegates:[delegate]
    (chain, block) >>=? function
  | [{ slots ; _ }] -> return_some slots
  | _ -> return_none

let inject_endorsement
    (cctxt : #Proto_alpha.full)
    ?async
    ~chain ~block
    hash level
    delegate_sk delegate_pkh =
  Alpha_services.Forge.endorsement cctxt
    (chain, block)
    ~branch:hash
    ~level:level
    () >>=? fun bytes ->
  let wallet = (cctxt :> Client_context.wallet) in
  (* Double-check the right to inject an endorsement *)
  let open Client_baking_highwatermarks in
  wallet#with_lock begin fun () ->
    Client_baking_files.resolve_location cctxt ~chain `Endorsement >>=? fun endorsement_location ->
    may_inject_endorsement cctxt endorsement_location ~delegate:delegate_pkh level >>=? function
    | true ->
        record_endorsement cctxt endorsement_location ~delegate:delegate_pkh level >>=? fun () ->
        return_true
    | false -> return_false
  end >>=? fun is_allowed_to_endorse ->
  if is_allowed_to_endorse then
    Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
    Client_keys.append cctxt
      delegate_sk ~watermark:(Endorsement chain_id) bytes >>=? fun signed_bytes ->
    Shell_services.Injection.operation cctxt ?async ~chain signed_bytes >>=? fun oph ->
    return oph
  else
    lwt_log_error Tag.DSL.(fun f ->
        f "Level %a : previously endorsed."
        -% t event "double_endorsement_near_miss"
        -% a level_tag level) >>= fun () ->
    fail (Level_previously_endorsed level)

let forge_endorsement
    (cctxt : #Proto_alpha.full)
    ?async
    ~chain ~block
    ~src_sk src_pk =
  let src_pkh = Signature.Public_key.hash src_pk in
  Alpha_block_services.metadata cctxt
    ~chain ~block () >>=? fun { protocol_data = { level = { level ; _ } ; _ } ; _ } ->
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  inject_endorsement cctxt ?async ~chain ~block hash level src_sk src_pkh >>=? fun oph ->
  Client_keys.get_key cctxt src_pkh >>=? fun (name, _pk, _sk) ->
  lwt_log_notice Tag.DSL.(fun f ->
      f "Injected endorsement for block '%a' \
         (level %a, contract %s) '%a'"
      -% t event "injected_endorsement"
      -% a Block_hash.Logging.tag hash
      -% a level_tag level
      -% s Client_keys.Logging.tag name
      -% t Signature.Public_key_hash.Logging.tag src_pkh
      -% a Operation_hash.Logging.tag oph) >>= fun () ->
  return oph

(** Worker *)

type state = {
  delegates: public_key_hash list ;
  delay: int64 ;
  mutable pending: endorsements option ;
}

and endorsements = {
  time: Time.Protocol.t ;
  delegates: public_key_hash list ;
  block: Client_baking_blocks.block_info ;
}

let create_state delegates delay =
  { delegates ; delay ; pending = None }

let get_delegates cctxt state = match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      let delegates = List.map (fun (_,pkh,_,_) -> pkh) keys in
      return delegates
  | (_ :: _) as delegates -> return delegates

let endorse_for_delegate cctxt block delegate_pkh =
  let { Client_baking_blocks.hash ; level ; chain_id ; _ } = block in
  Client_keys.get_key cctxt delegate_pkh >>=? fun (name, _pk, delegate_sk) ->
  lwt_debug Tag.DSL.(fun f ->
      f "Endorsing %a for %s (level %a)!"
      -% t event "endorsing"
      -% a Block_hash.Logging.tag hash
      -% s Client_keys.Logging.tag name
      -% a level_tag level) >>= fun () ->
  let chain = `Hash chain_id in
  let block = `Hash (hash, 0) in
  inject_endorsement cctxt
    ~chain ~block
    hash level
    delegate_sk delegate_pkh >>=? fun oph ->
  lwt_log_notice Tag.DSL.(fun f ->
      f "Injected endorsement for block '%a' \
         (level %a, contract %s) '%a'"
      -% t event "injected_endorsement"
      -% a Block_hash.Logging.tag hash
      -% a level_tag level
      -% s Client_keys.Logging.tag name
      -% t Signature.Public_key_hash.Logging.tag delegate_pkh
      -% a Operation_hash.Logging.tag oph) >>= fun () ->
  return_unit

let allowed_to_endorse cctxt bi delegate  =
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
  lwt_debug Tag.DSL.(fun f ->
      f "Checking if allowed to endorse block %a for %s"
      -% t event "check_endorsement_ok"
      -% a Block_hash.Logging.tag bi.Client_baking_blocks.hash
      -% s Client_keys.Logging.tag name) >>= fun () ->
  let chain = `Hash bi.chain_id in
  let block = `Hash (bi.hash, 0) in
  let level = bi.level in
  get_signing_slots cctxt ~chain ~block delegate level >>=? function
  | None | Some [] ->
      lwt_debug Tag.DSL.(fun f ->
          f "No slot found for %a/%s"
          -% t event "endorsement_no_slots_found"
          -% a Block_hash.Logging.tag bi.hash
          -% s Client_keys.Logging.tag name) >>= fun () ->
      return_false
  | Some (_ :: _ as slots) ->
      lwt_debug Tag.DSL.(fun f ->
          f "Found slots for %a/%s (%a)"
          -% t event "endorsement_slots_found"
          -% a Block_hash.Logging.tag bi.hash
          -% s Client_keys.Logging.tag name
          -% a endorsement_slots_tag slots) >>= fun () ->
      cctxt#with_lock begin fun () ->
        Client_baking_files.resolve_location cctxt ~chain `Endorsement >>=? fun endorsement_location ->
        Client_baking_highwatermarks.may_inject_endorsement cctxt endorsement_location ~delegate level
      end >>=? function
      | false ->
          lwt_debug Tag.DSL.(fun f ->
              f "Level %a (or higher) previously endorsed: do not endorse."
              -% t event "previously_endorsed"
              -% a level_tag level) >>= fun () ->
          return_false
      | true -> return_true

let prepare_endorsement ~(max_past:int64) () (cctxt : #Proto_alpha.full) state bi =
  let past =
    Time.Protocol.diff
      (Time.System.to_protocol (Systime_os.now ()))
      bi.Client_baking_blocks.timestamp in
  if past > max_past then
    lwt_log_info Tag.DSL.(fun f ->
        f "Ignore block %a: forged too far the past"
        -% t event "endorsement_stale_block"
        -% a Block_hash.Logging.tag bi.hash) >>= fun () ->
    return_unit
  else
    lwt_log_info Tag.DSL.(fun f ->
        f "Received new block %a"
        -% t event "endorsement_got_block"
        -% a Block_hash.Logging.tag bi.hash) >>= fun () ->
    let time =
      Time.Protocol.add
        (Time.System.to_protocol (Systime_os.now ()))
        state.delay in
    get_delegates cctxt state >>=? fun delegates ->
    filter_p (allowed_to_endorse cctxt bi) delegates >>=? fun delegates ->
    state.pending <- Some {
        time ;
        block = bi ;
        delegates ;
      } ;
    return_unit

let compute_timeout state =
  match state.pending with
  | None -> Lwt_utils.never_ending ()
  | Some { time ; block ; delegates } ->
      match Client_baking_scheduling.sleep_until time with
      | None -> Lwt.return (block, delegates)
      | Some timeout ->
          let timespan =
            let timespan =
              Ptime.diff (Time.System.of_protocol_exn time) (Systime_os.now ()) in
            if Ptime.Span.compare timespan Ptime.Span.zero > 0 then
              timespan
            else
              Ptime.Span.zero in
          lwt_log_info Tag.DSL.(fun f ->
              f "Waiting until %a (%a) to inject endorsements"
              -% t event "wait_before_injecting"
              -% a timestamp_tag (Time.System.of_protocol_exn time)
              -% a timespan_tag timespan
            ) >>= fun () ->
          timeout >>= fun () -> Lwt.return (block, delegates)

let create
    (cctxt: #Proto_alpha.full)
    ?(max_past=110L)
    ~delay
    delegates
    block_stream
  =

  let state_maker _ =
    let state = create_state delegates (Int64.of_int delay) in
    return state
  in

  let timeout_k cctxt state (block, delegates) =
    state.pending <- None ;
    iter_s
      (fun delegate -> endorse_for_delegate cctxt block delegate >>= function
         | Ok () -> return_unit
         | Error errs ->
             lwt_log_error Tag.DSL.(fun f ->
                 f "@[<v 2>Error while injecting endorsement for delegate %a : @[%a@]@]@."
                 -% t event "error_while_endorsing"
                 -% a Signature.Public_key_hash.Logging.tag delegate
                 -% a errs_tag errs) >>= fun () ->
             (* We continue anyway *)
             return_unit
      ) delegates
  in

  let event_k cctxt state bi =
    state.pending <- None ;
    prepare_endorsement ~max_past () cctxt state bi
  in

  Client_baking_scheduling.main
    ~name:"endorser"
    ~cctxt
    ~stream:block_stream
    ~state_maker
    ~pre_loop:(prepare_endorsement ~max_past ())
    ~compute_timeout
    ~timeout_k
    ~event_k
