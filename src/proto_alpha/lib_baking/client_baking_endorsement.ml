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

  val get_endorsement:
    #Client_context.wallet ->
    Signature.Public_key_hash.t ->
    Raw_level.t option tzresult Lwt.t

  val record_endorsement:
    #Client_context.wallet ->
    Signature.Public_key_hash.t ->
    Raw_level.t ->
    unit tzresult Lwt.t

end = struct

  type t = (string * Raw_level.t) list

  let encoding : t Data_encoding.t =
    Data_encoding.(
      list (obj2
              (req "delegate" string)
              (req "last_level" Raw_level.encoding)
           ))

  let name =
    "endorsements"

  let load (wallet : #Client_context.wallet) =
    wallet#load name encoding ~default:[]

  let save (wallet : #Client_context.wallet) list =
    wallet#write name list encoding


  let get_endorsement (wallet : #Client_context.wallet) (delegate_key:Signature.public_key_hash) =
    wallet#with_lock
      (fun () ->
         load wallet >>=? fun l ->
         return (List.assoc_opt (Signature.Public_key_hash.to_short_b58check delegate_key) l)
      )

  let record_endorsement (wallet : #Client_context.wallet) (delegate:Signature.public_key_hash) (new_lvl:Raw_level.t) =
    begin
      wallet#with_lock (fun () ->
          begin
            load wallet >>=? fun l  ->
            let delegate_key = Signature.Public_key_hash.to_short_b58check delegate
            in
            let rec remove_old acc = function
              | [] -> List.rev acc
              | ((_,lvl) as hd)::tl ->
                  if Raw_level.diff new_lvl  lvl > 50l (*?*)  then
                    remove_old (hd::acc) tl
                  else
                    List.rev acc
            in
            save wallet ((delegate_key, new_lvl)::
                         List.remove_assoc delegate_key (remove_old [] l))
          end)
    end
end

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
    src_sk slots pkh =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  Alpha_services.Forge.endorsement cctxt
    (chain, block)
    ~branch:hash
    ~block:hash
    ~level:level
    ~slots
    () >>=? fun bytes ->
  Client_keys.append cctxt
    src_sk ~watermark:Endorsement bytes >>=? fun signed_bytes ->
  Shell_services.Injection.operation cctxt ?async ~chain signed_bytes >>=? fun oph ->
  State.record_endorsement cctxt pkh level >>=? fun () ->
  return oph

let check_endorsement cctxt level pkh =
  State.get_endorsement cctxt pkh >>=? function
  | None -> return ()
  | Some recorded_level ->
      if Raw_level.(level = recorded_level) then
        Error_monad.failwith "Level %a already endorsed" Raw_level.pp recorded_level
      else
        return ()

let previously_endorsed_level cctxt pkh new_lvl  =
  State.get_endorsement cctxt pkh  >>=? function
  | None -> return false
  | Some last_lvl ->
      return (not Raw_level.(last_lvl < new_lvl))

let forge_endorsement (cctxt : #Proto_alpha.full)
    ?(chain = `Main) block ?async
    ~src_sk  ?slots src_pk =
  let src_pkh = Signature.Public_key.hash src_pk in
  Alpha_block_services.metadata cctxt
    ~chain ~block () >>=? fun { protocol_data = { level = { level } } } ->
  check_endorsement cctxt level src_pkh >>=? fun () ->
  previously_endorsed_level cctxt src_pkh level >>=? function
  | true ->
      cctxt#error "Level %a : previously endorsed."
        Raw_level.pp level
  | false ->
      begin
        match slots with
        | Some slots -> return slots
        | None ->
            get_signing_slots
              cctxt ~chain block src_pkh level >>=? function
            | [] -> cctxt#error "No slot found at level %a" Raw_level.pp level
            | slots -> return slots
      end >>=? fun slots ->
      inject_endorsement cctxt ~chain ?async block level src_sk slots src_pkh >>=? fun oph ->
      Client_keys.get_key cctxt src_pkh >>=? fun (name, _pk, _sk) ->
      cctxt#message
        "Injected endorsement level %a, contract %s '%a'"
        Raw_level.pp level
        name
        Operation_hash.pp_short oph >>=
      fun () -> return oph

(** Worker *)

type state = {
  delegates: public_key_hash list ;
  delay: int64;
  mutable to_endorse : endorsement option
}
and endorsement = {
  time: Time.t ;
  delegate: public_key_hash ;
  block: Client_baking_blocks.block_info ;
  slots: int list;
}

let create_state delegates delay =
  { delegates ;
    delay ;
    to_endorse = None ;
  }

let rec insert ({time} as e) = function
  | [] -> [e]
  | ({time = time'} :: _) as l when Time.compare time time' < 0 ->
      e :: l
  | e' :: l -> e' :: insert e l

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      return (List.map (fun (_,pkh,_,_) -> pkh) keys)
  | _ :: _ as delegates ->
      return delegates

let endorse_for cctxt = function
    None -> return ()
  | Some {delegate; block ; slots} ->
      let hash = block.hash in
      let b = `Hash (hash, 0) in
      let level = block.level.level in
      Client_keys.get_key cctxt delegate >>=? fun (name, _pk, sk) ->
      lwt_debug "Endorsing %a for %s (level %a using %d slots)!"
        Block_hash.pp_short hash name
        Raw_level.pp level
        (List.length slots) >>= fun () ->
      inject_endorsement cctxt
        b level
        sk slots delegate >>=? fun oph ->
      lwt_log_info
        "Injected endorsement for block '%a' \
         (level %a, contract %s) '%a'"
        Block_hash.pp_short hash
        Raw_level.pp level
        name
        Operation_hash.pp_short oph >>= return

let prepare_endorsement (cctxt : #Proto_alpha.full) ~(max_past:Time.t) state bis  =
  let may_endorse (block: Client_baking_blocks.block_info) delegate time =
    Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
    lwt_log_info "May endorse block %a for %s"
      Block_hash.pp_short block.hash name >>= fun () ->
    let b = `Hash (block.hash, 0) in
    let level = block.level.level in
    get_signing_slots cctxt b delegate level >>=? fun slots ->
    lwt_debug "Found slots for %a/%s (%d)"
      Block_hash.pp_short block.hash name (List.length slots) >>= fun () ->
    previously_endorsed_level cctxt delegate level >>=? function
    | true ->
        lwt_debug "Level %a : previously endorsed."
          Raw_level.pp level >>= return
    | false ->
        return
          (match state.to_endorse with
           | None ->
               state.to_endorse <- Some {time ; delegate ; block; slots}
           | Some old ->
               if Fitness.compare old.block.fitness block.fitness < 0 then
                 state.to_endorse <- Some {time ; delegate ; block; slots})
  in
  get_delegates cctxt state >>=? fun delegates ->
  iter_p
    (fun delegate ->
       iter_p
         (fun (bi : Client_baking_blocks.block_info) ->
            if Time.compare bi.timestamp (Time.now ()) > 0  then
              lwt_log_info "Ignore block %a: forged in the future"
                Block_hash.pp_short bi.hash >>= return
            else if Time.(min (now ()) bi.timestamp > max_past) then
              lwt_log_info "Ignore block %a: forged too far the past"
                Block_hash.pp_short bi.hash >>= return
            else
              let time = Time.(add (now ()) state.delay) in
              may_endorse bi delegate time
         ) bis
    )
    delegates

(* let endorse (cctxt : #Proto_alpha.full) ~(max_past:Time.t) state bis =
 *   let may_endorse (block: Client_baking_blocks.block_info) delegate =
 *     Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
 *     lwt_log_info "May endorse block %a for %s"
 *       Block_hash.pp_short block.hash name >>= fun () ->
 *     let b = `Hash (block.hash, 0) in
 *     let level = block.level.level in
 *     get_signing_slots cctxt b delegate level >>=? fun slots ->
 *     lwt_debug "Found %d slots for %a/%s"
 *       (List.length slots) Block_hash.pp_short block.hash name >>= fun () ->
 *     previously_endorsed_level cctxt delegate level >>=? function
 *     | true ->
 *         lwt_debug "Level %a : previously endorsed."
 *           Raw_level.pp level >>= fun () ->
 *         return []
 *     | false ->
 *         return slots
 *   in
 *   get_delegates cctxt state >>=? fun delegates ->
 *   iter_p
 *     (fun delegate ->
 *        iter_p
 *          (fun (bi : Client_baking_blocks.block_info) ->
 *             if Time.compare bi.timestamp (Time.now ()) > 0  then
 *               lwt_log_info "Ignore block %a: forged in the future"
 *                 Block_hash.pp_short bi.hash >>= return
 *             else if Time.(min (now ()) bi.timestamp > max_past) then
 *               lwt_log_info "Ignore block %a: forged too far the past"
 *                 Block_hash.pp_short bi.hash >>= return
 *             else
 *               may_endorse bi delegate  >>=? function
 *               | [] ->
 *                   return ()
 *               | slots ->
 *                   endorse_for cctxt delegate bi slots )
 *          bis)
 *     delegates *)

let compute_timeout state =
  match state.to_endorse with
  | None -> Lwt_utils.never_ending
  | Some {time} ->
      let delay = (Time.diff time (Time.now ())) in
      if delay <= 0L then
        Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)

let check_error f =
  f >>= function
  | Ok () -> Lwt.return_unit
  | Error errs ->
      lwt_log_error "Error while endorsing:@\n%a"
        pp_print_error
        errs >>= fun () ->
      Lwt.return_unit

let create (cctxt : #Proto_alpha.full) ?(max_past=(Time.of_seconds 110L)) ~delay contracts block_stream =
  lwt_log_info "Starting endorsement daemon" >>= fun () ->
  Lwt_stream.get block_stream >>= function
  | None | Some (Ok []) | Some (Error _) ->
      cctxt#error "Can't fetch the current block head."
  | Some (Ok initial_heads) ->
      let last_get_block = ref None in
      let get_block () =
        match !last_get_block with
        | None ->
            let t = Lwt_stream.get block_stream in
            last_get_block := Some t ;
            t
        | Some t -> t in
      let state = create_state contracts (Int64.of_int delay) in
      let rec worker_loop () =
        let timeout = compute_timeout state in
        Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                     (get_block () >|= fun b -> `Hash b) ] >>=  function
        | `Hash (None | Some (Error _)) ->
            Lwt.return_unit
        |  `Hash (Some (Ok bis)) ->
            Lwt.cancel timeout;
            last_get_block := None ;
            check_error (prepare_endorsement cctxt ~max_past state bis) >>= fun () ->
            worker_loop ()
        | `Timeout ->
            check_error (endorse_for cctxt state.to_endorse) >>= fun () ->
            worker_loop () in
      check_error (prepare_endorsement cctxt ~max_past state initial_heads) >>= fun () ->
      worker_loop ()
