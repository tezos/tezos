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
  | [{ slots }] -> return (Some slots)
  | _ -> return None

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
  | None -> return ()
  | Some recorded_level ->
      if Raw_level.(level = recorded_level) then
        Error_monad.failwith "Level %a already endorsed" Raw_level.pp recorded_level
      else
        return ()

let previously_endorsed_level cctxt pkh new_lvl  =
  State.get cctxt pkh >>=? function
  | None -> return false
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

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_, pkh, _, _) -> pkh) keys)
  | _ :: _ as delegates ->
      return delegates

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
  return ()

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
      return false
  | Some (_ :: _ as slots) ->
      lwt_debug "Found slots for %a/%s (%d)"
        Block_hash.pp_short bi.hash name (List.length slots) >>= fun () ->
      previously_endorsed_level cctxt delegate level >>=? function
      | true ->
          lwt_debug "Level %a (or higher) previously endorsed: do not endorse."
            Raw_level.pp level >>= fun () ->
          return false
      | false ->
          return true

let prepare_endorsement (cctxt : #Proto_alpha.full) ~(max_past:int64) state bi =
  if Time.diff (Time.now ()) bi.Client_baking_blocks.timestamp > max_past then
    lwt_log_info "Ignore block %a: forged too far the past"
      Block_hash.pp_short bi.hash >>= fun () ->
    return ()
  else
    lwt_log_info "Received new block %a"
      Block_hash.pp_short bi.hash >>= fun () ->
    let time = Time.(add (now ()) state.delay) in
    let timeout = Lwt_unix.sleep (Int64.to_float state.delay) in
    get_delegates cctxt state >>=? fun delegates ->
    filter_map_p
      (fun delegate ->
         allowed_to_endorse cctxt bi delegate >>=? function
         | true -> return (Some delegate)
         | false -> return None)
      delegates >>=? fun delegates ->
    state.pending <- Some {
        time ;
        timeout ;
        block = bi ;
        delegates ;
      } ;
    return ()

let compute_timeout state =
  match state.pending with
  | None -> Lwt_utils.never_ending ()
  | Some { timeout ; block ; delegates } ->
      timeout >>= fun () ->
      Lwt.return (`Timeout (block, delegates))

let check_error f =
  f >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> lwt_log_error "Error while endorsing:@\n%a" pp_print_error errs

let create
    (cctxt: #Proto_alpha.full)
    ~max_past
    ~delay
    contracts
    block_stream
    bi =
  lwt_log_info "Preparing endorsement daemon" >>= fun () ->
  (* statefulness setup *)
  let last_get_block = ref None in
  let get_block () =
    match !last_get_block with
    | None ->
        let t = Lwt_stream.get block_stream in
        last_get_block := Some t ;
        t
    | Some t -> t in
  let state = create_state contracts (Int64.of_int delay) in

  (* main loop *)
  let rec worker_loop () =
    begin
      Lwt.choose [ compute_timeout state ;
                   (get_block () >|= fun b -> `Hash b) ] >>=  function
      | `Hash None ->
          last_get_block := None ;
          lwt_log_error "Connection to node lost, exiting." >>= fun () ->
          exit 1
      | `Hash (Some (Error _)) ->
          last_get_block := None ;
          Lwt.return_unit
      | `Hash (Some (Ok bi)) ->
          last_get_block := None ;
          state.pending <- None ;
          check_error @@ prepare_endorsement cctxt ~max_past state bi
      | `Timeout (block, delegates) ->
          state.pending <- None ;
          check_error @@ iter_p (endorse_for_delegate cctxt block) delegates
    end >>= fun () ->
    worker_loop () in

  (* ignition *)
  check_error (prepare_endorsement cctxt ~max_past state bi) >>= fun () ->
  lwt_log_notice "Starting endorsement daemon" >>= fun () ->
  worker_loop ()

(* A wrapper around the main create function (above) to wait for the initial
   block. *)
let create
    (cctxt: #Proto_alpha.full)
    ?(max_past=110L)
    ~delay
    contracts
    (block_stream: Client_baking_blocks.block_info tzresult Lwt_stream.t) =
  Client_baking_scheduling.wait_for_first_block
    ~info:lwt_log_info
    block_stream
    (create cctxt ~max_past ~delay contracts block_stream)
