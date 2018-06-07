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

module State : sig

  type t = Raw_level.t

  val get_endorsement:
    #Client_context.wallet -> t tzresult Lwt.t

  val record_endorsement:
    #Client_context.wallet -> t -> unit tzresult Lwt.t

end = struct

  type t = Raw_level.t
  let encoding : t Data_encoding.t =
    Data_encoding.(obj1 (req "max_level" Raw_level.encoding))

  let name = "endorsed_level"

  let load (wallet : #Client_context.wallet) =
    wallet#load name encoding ~default:Raw_level.root

  let save (wallet : #Client_context.wallet) map =
    wallet#write name encoding map

  let lock = Lwt_mutex.create ()

  let get_endorsement (wallet : #Client_context.wallet) =
    Lwt_mutex.with_lock lock (fun () -> load wallet)

  let record_endorsement (wallet : #Client_context.wallet) level =
    Lwt_mutex.with_lock lock
      (fun () ->
         load wallet >>=? fun last ->
         wallet#write name (Raw_level.max last level) encoding)

end

let previously_endorsed_level cctxt level =
  State.get_endorsement cctxt >>=? fun last ->
  return Raw_level.(level <= last)

let check_endorsement cctxt level =
  previously_endorsed_level cctxt level >>=? function
  | false -> return ()
  | true ->
      Error_monad.failwith
        "Already signed a block at level %a"
        Raw_level.pp level

let get_signing_slots cctxt ?(chain = `Main) block delegate level =
  Alpha_services.Delegate.Endorsing_rights.get cctxt
    ~levels:[level]
    ~delegates:[delegate]
    (chain, block) >>=? fun possibilities ->
  match possibilities with
  | [{ slots }] -> return slots
  | _ -> return []

let inject_endorsement
    (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block level ?async
    src_sk slots =
  check_endorsement cctxt level >>=? fun () ->
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Alpha_services.Forge.endorsement cctxt
    (chain, block)
    ~branch:hash
    ~block:hash
    ~level:level
    ~slots
    () >>=? fun bytes ->
  Client_keys.append
    src_sk ~watermark:Endorsement bytes >>=? fun signed_bytes ->
  Shell_services.Injection.operation cctxt
    ?async ~chain signed_bytes >>=? fun oph ->
  State.record_endorsement cctxt level >>=? fun () ->
  return oph

let forge_endorsement (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block
    ~src_sk ?slots src_pk =
  let src_pkh = Signature.Public_key.hash src_pk in
  Alpha_block_services.metadata cctxt
    ~chain ~block () >>=? fun { protocol_data = { level = { level } } } ->
  begin
    match slots with
    | Some slots -> return slots
    | None ->
        get_signing_slots
          cctxt ~chain block src_pkh level >>=? function
        | [] -> cctxt#error "No slot found at level %a" Raw_level.pp level
        | slots -> return slots
  end >>=? fun slots ->
  inject_endorsement cctxt
    ~chain block level
    src_sk slots


(** Worker *)

type state = {
  delegates: public_key_hash list ;
  mutable best: Client_baking_blocks.block_info ;
  mutable to_endorse:
    (Client_baking_blocks.block_info * Time.t * endorsement list) option ;
  delay: int64;
}
and endorsement = {
  delegate: public_key_hash ;
  slots: int list;
}

let create_state delegates best delay =
  { delegates ;
    best ;
    to_endorse = None ;
    delay ;
  }

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_,pkh,_,_) -> pkh) keys)
  | _ :: _ as delegates ->
      return delegates

let schedule_endorsements (cctxt : #Proto_alpha.full) state bis =
  let best =
    List.fold_left
      (fun best bi ->
         let current_fitness =
           match best with
           | None -> state.best.fitness
           | Some best -> best.Client_baking_blocks.fitness in
         if Fitness.(current_fitness < bi.Client_baking_blocks.fitness) then
           Some bi
         else
           best)
      None bis in
  match best with
  | None ->
      (* nothing to do *)
      return ()
  | Some bi ->
      state.best <- bi ;
      state.to_endorse <- None ;
      previously_endorsed_level cctxt bi.level.level >>=? function
      | true ->
          (* do not endorse a shorter chain... *)
          return ()
      | false ->
          (* proceed... *)
          let time = Time.(add (now ()) state.delay) in
          get_delegates cctxt state >>=? fun delegates ->
          let toto delegate =
            Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
            lwt_log_info "May endorse block %a for %s"
              Block_hash.pp_short bi.hash name >>= fun () ->
            let b = `Hash (bi.hash, 0) in
            let level = bi.level.level in
            get_signing_slots cctxt b delegate level >>=? fun slots ->
            lwt_debug "Found slots for %a/%s (%d)"
              Block_hash.pp_short bi.hash name (List.length slots) >>= fun () ->
            match slots with
            | [] -> return None
            | _ :: _ as slots -> return (Some { delegate ; slots }) in
          filter_map_p toto delegates >>=? fun slots ->
          match slots with
          | [] ->
              (* no rights ... *)
              return ()
          | _ :: _ as _slots ->
              lwt_log_info
                "Schedule endorsement for block %a \
                 (level %a, time %a)"
                Block_hash.pp_short bi.hash
                Raw_level.pp bi.level.level
                Time.pp_hum time >>= fun () ->
              state.to_endorse <- Some (bi, time, slots) ;
              return ()

let schedule_endorsements (cctxt : #Proto_alpha.full) state bis =
  schedule_endorsements cctxt state bis >>= function
  | Error exns ->
      lwt_log_error
        "@[<v 2>Error(s) while scheduling endorsements@,%a@]"
        pp_print_error exns
  | Ok () -> Lwt.return_unit

let endorse cctxt state =
  match state.to_endorse with
  | None -> return ()
  | Some (bi, _, slots) ->
      state.to_endorse <- None ;
      let hash = bi.hash in
      let b = `Hash (hash, 0) in
      let level = bi.level.level in
      iter_p
        (fun { delegate ; slots } ->
           Client_keys.get_key cctxt delegate >>=? fun (name, _pk, sk) ->
           lwt_debug "Endorsing %a for %s!"
             Block_hash.pp_short hash name >>= fun () ->
           inject_endorsement cctxt
             b level
             sk slots >>=? fun oph ->
           cctxt#message
             "Injected endorsement for block '%a' \
              (level %a, contract %s) '%a'"
             Block_hash.pp_short hash
             Raw_level.pp level
             name
             Operation_hash.pp_short oph >>= fun () ->
           return ())
        slots

let compute_timeout state =
  match state.to_endorse with
  | None -> Lwt_utils.never_ending
  | Some (_, time, _) ->
      let delay = (Time.diff time (Time.now ())) in
      if delay <= 0L then
        Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)

let create (cctxt : #Proto_alpha.full) ~delay contracts block_stream =
  lwt_log_info "Starting endorsement daemon" >>= fun () ->
  Lwt_stream.get block_stream >>= function
  | None | Some (Error _) ->
      cctxt#error "Can't fetch the current block head."
  | Some (Ok head) ->
      let last_get_block = ref None in
      let get_block () =
        match !last_get_block with
        | None ->
            let t = Lwt_stream.get block_stream in
            last_get_block := Some t ;
            t
        | Some t -> t in
      let state = create_state contracts head (Int64.of_int delay) in
      let rec worker_loop () =
        let timeout = compute_timeout state in
        Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                     (get_block () >|= fun b -> `Hash b) ] >>= function
        | `Hash (None | Some (Error _)) ->
            Lwt.return_unit
        | `Hash (Some (Ok bi)) ->
            Lwt.cancel timeout ;
            last_get_block := None ;
            schedule_endorsements cctxt state [bi] >>= fun () ->
            worker_loop ()
        | `Timeout ->
            begin
              endorse cctxt state >>= function
              | Ok () -> Lwt.return_unit
              | Error errs ->
                  lwt_log_error "Error while endorsing:@\n%a"
                    pp_print_error
                    errs >>= fun () ->
                  Lwt.return_unit
            end >>= fun () ->
            worker_loop () in
      schedule_endorsements cctxt state [head] >>= fun () ->
      worker_loop ()
