(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

include Logging.Make(struct let name = "client.endorsement" end)

module State = Daemon_state.Make(struct let name = "endorsement" end)

let get_signing_slots cctxt ?(chain = `Main) block delegate level =
  Alpha_services.Delegate.Endorsing_rights.get cctxt
    ~levels:[level]
    ~delegates:[delegate]
    (chain, block) >>=? function
  | [{ slots }] -> return_some slots
  | _ -> return_none

let inject_endorsement
    (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block hash level ?async
    src_sk pkh =
  Alpha_services.Forge.endorsement cctxt
    (chain, block)
    ~branch:hash
    ~level:level
    () >>=? fun bytes ->
  Client_keys.append cctxt
    src_sk ~watermark:Endorsement bytes >>=? fun signed_bytes ->
  Shell_services.Injection.operation cctxt ?async ~chain signed_bytes >>=? fun oph ->
  State.record cctxt pkh level >>=? fun () ->
  return oph

let check_endorsement cctxt level pkh =
  State.get cctxt pkh >>=? function
  | None -> return_unit
  | Some recorded_level ->
      if Raw_level.(level = recorded_level) then
        Error_monad.failwith "Level %a already endorsed" Raw_level.pp recorded_level
      else
        return_unit

let previously_endorsed_level cctxt pkh new_lvl  =
  State.get cctxt pkh >>=? function
  | None -> return_false
  | Some last_lvl ->
      return (Raw_level.(last_lvl >= new_lvl))

let forge_endorsement (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block ?async
    ~src_sk src_pk =
  let src_pkh = Signature.Public_key.hash src_pk in
  Alpha_block_services.metadata cctxt
    ~chain ~block () >>=? fun { protocol_data = { level = { level } } } ->
  check_endorsement cctxt level src_pkh >>=? fun () ->
  previously_endorsed_level cctxt src_pkh level >>=? function
  | true ->
      cctxt#error "Level %a : previously endorsed."
        Raw_level.pp level
  | false ->
      Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
      inject_endorsement cctxt ~chain ?async block hash level src_sk src_pkh >>=? fun oph ->
      Client_keys.get_key cctxt src_pkh >>=? fun (name, _pk, _sk) ->
      cctxt#message
        "Injected endorsement level %a, contract %s '%a'"
        Raw_level.pp level
        name
        Operation_hash.pp_short oph >>= fun () ->
      return oph

(** Worker *)

type state = {
  delegates: public_key_hash list ;
  delay: int64 ;
  mutable pending: endorsements option ;
}

and endorsements = {
  time: Time.t ;
  timeout: unit Lwt.t ;
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

let endorse_for_delegate cctxt block delegate =
  let { Client_baking_blocks.hash ; level } = block in
  let b = `Hash (hash, 0) in
  Client_keys.get_key cctxt delegate >>=? fun (name, _pk, sk) ->
  lwt_debug "Endorsing %a for %s (level %a)!"
    Block_hash.pp_short hash name
    Raw_level.pp level >>= fun () ->
  inject_endorsement cctxt
    b hash level
    sk delegate >>=? fun oph ->
  lwt_log_notice
    "Injected endorsement for block '%a' \
     (level %a, contract %s) '%a'"
    Block_hash.pp_short hash
    Raw_level.pp level
    name
    Operation_hash.pp_short oph >>= fun () ->
  return_unit

let allowed_to_endorse cctxt bi delegate  =
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
  lwt_debug "Checking if allowed to endorse block %a for %s"
    Block_hash.pp_short bi.Client_baking_blocks.hash name >>= fun () ->
  let b = `Hash (bi.hash, 0) in
  let level = bi.level in
  get_signing_slots cctxt b delegate level >>=? function
  | None | Some [] ->
      lwt_debug "No slot found for %a/%s"
        Block_hash.pp_short bi.hash name >>= fun () ->
      return_false
  | Some (_ :: _ as slots) ->
      lwt_debug "Found slots for %a/%s (%d)"
        Block_hash.pp_short bi.hash name (List.length slots) >>= fun () ->
      previously_endorsed_level cctxt delegate level >>=? function
      | true ->
          lwt_debug "Level %a (or higher) previously endorsed: do not endorse."
            Raw_level.pp level >>= fun () ->
          return_false
      | false ->
          return_true

let prepare_endorsement ~(max_past:int64) () (cctxt : #Proto_alpha.full) state bi =
  if Time.diff (Time.now ()) bi.Client_baking_blocks.timestamp > max_past then
    lwt_log_info "Ignore block %a: forged too far the past"
      Block_hash.pp_short bi.hash >>= fun () ->
    return_unit
  else
    lwt_log_info "Received new block %a"
      Block_hash.pp_short bi.hash >>= fun () ->
    let time = Time.(add (now ()) state.delay) in
    let timeout = Lwt_unix.sleep (Int64.to_float state.delay) in
    get_delegates cctxt state >>=? fun delegates ->
    filter_p (allowed_to_endorse cctxt bi) delegates >>=? fun delegates ->
    state.pending <- Some {
        time ;
        timeout ;
        block = bi ;
        delegates ;
      } ;
    return_unit

let compute_timeout state =
  match state.pending with
  | None -> Lwt_utils.never_ending ()
  | Some { timeout ; block ; delegates } ->
      timeout >>= fun () ->
      Lwt.return (block, delegates)

let check_error f =
  f >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> lwt_log_error "Error while endorsing:@\n%a" pp_print_error errs

let create
    (cctxt: #Proto_alpha.full)
    ?(max_past=110L)
    ~delay
    delegates
    block_stream
  =

  let state_maker _ _ =
    let state = create_state delegates (Int64.of_int delay) in
    return state
  in

  let timeout_k cctxt state (block, delegates) =
    state.pending <- None ;
    iter_p (endorse_for_delegate cctxt block) delegates
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
