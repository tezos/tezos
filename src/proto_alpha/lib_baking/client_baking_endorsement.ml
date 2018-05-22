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
            match List.assoc_opt delegate_key l  with
            | None ->
                save wallet ((delegate_key, new_lvl)::l)
            | Some _ ->
                save wallet ((delegate_key, new_lvl)::
                             List.remove_assoc delegate_key l)
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
  begin
    match slots with
    | Some slots -> return slots
    | None ->
        get_signing_slots
          cctxt ~chain block src_pkh level >>=? function
        | [] -> cctxt#error "No slot found at level %a" Raw_level.pp level
        | slots -> return slots
  end >>=? fun slots ->
  check_endorsement cctxt level src_pkh >>=? fun () ->
  inject_endorsement cctxt ~chain ?async block level src_sk slots src_pkh

(** Worker *)

type state = {
  delegates: public_key_hash list ;
  mutable best: Client_baking_blocks.block_info ;
  mutable to_endorse: endorsement list ;
  delay: int64;
}
and endorsement = {
  time: Time.t ;
  delegate: public_key_hash ;
  block: Client_baking_blocks.block_info ;
  slots: int list;
}

let create_state delegates best delay =
  { delegates ;
    best ;
    to_endorse = [] ;
    delay ;
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

let drop_old_endorsement ~before state =
  state.to_endorse <-
    List.filter
      (fun { block } -> Fitness.compare before block.fitness <= 0)
      state.to_endorse

let schedule_endorsements (cctxt : #Proto_alpha.full) ~(max_past:Time.t) state bis =
  let may_endorse (block: Client_baking_blocks.block_info) delegate time =
    Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
    lwt_log_info "May endorse block %a for %s"
      Block_hash.pp_short block.hash name >>= fun () ->
    let b = `Hash (block.hash, 0) in
    let level = block.level.level in
    get_signing_slots cctxt b delegate level >>=? fun slots ->
    lwt_debug "Found %d slots for %a/%s"
      (List.length slots) Block_hash.pp_short block.hash name >>= fun () ->
    previously_endorsed_level cctxt delegate level >>=? function
    | true ->
        lwt_debug "Level %a : previously endorsed."
          Raw_level.pp level >>= return
    | false ->
        if Fitness.compare state.best.fitness block.fitness < 0 then begin
          state.best <- block ;
          drop_old_endorsement ~before:block.fitness state ;
        end ;
        begin try
            let same_slot endorsement =
              endorsement.block.level = block.level && endorsement.slots = slots in
            let old = List.find same_slot state.to_endorse in
            if Fitness.compare old.block.fitness block.fitness < 0
            then begin
              lwt_log_info
                "Schedule endorsement for block %a \
                 (level %a, slots { %a }, time %a) (replace block %a)"
                Block_hash.pp_short block.hash
                Raw_level.pp level
                (Format.pp_print_list Format.pp_print_int) slots
                Time.pp_hum time
                Block_hash.pp_short old.block.hash
              >>= fun () ->
              state.to_endorse <-
                insert
                  { time ; delegate ; block ; slots }
                  (List.filter
                     (fun e -> not (same_slot e))
                     state.to_endorse) ;
              return ()
            end else begin
              lwt_debug
                "slot { %a } : better pending endorsement"
                (Format.pp_print_list Format.pp_print_int) slots >>= fun () ->
              return ()
            end
          with Not_found ->
            lwt_log_info
              "Schedule endorsement for block %a \
               (level %a, slot { %a }, time %a)"
              Block_hash.pp_short block.hash
              Raw_level.pp level
              (Format.pp_print_list Format.pp_print_int) slots
              Time.pp_hum time >>= fun () ->
            state.to_endorse <-
              insert { time ; delegate ; block ; slots } state.to_endorse ;
            return ()
        end
  in
  let time = Time.(add (now ()) state.delay) in
  get_delegates cctxt state >>=? fun delegates ->

  ignore max_past;

  iter_s
    (fun delegate -> may_endorse bis delegate time)
    delegates

let schedule_endorsements (cctxt : #Proto_alpha.full) ~max_past state bis =
  schedule_endorsements cctxt ~max_past state bis >>= function
  | Error exns ->
      lwt_log_error
        "@[<v 2>Error(s) while scheduling endorsements@,%a@]"
        pp_print_error exns
  | Ok () -> Lwt.return_unit

let pop_endorsements state =
  let now = Time.now () in
  let rec pop acc = function
    | [] -> List.rev acc, []
    | {time} :: _ as slots when Time.compare now time <= 0 ->
        List.rev acc, slots
    | slot :: slots -> pop (slot :: acc) slots in
  let to_endorse, future_endorsement = pop [] state.to_endorse in
  state.to_endorse <- future_endorsement ;
  to_endorse

let endorse cctxt state =
  let to_endorse = pop_endorsements state in
  iter_p (fun { time = _ ; block ; slots ; delegate } ->
      let hash = block.hash in
      let b = `Hash (hash, 0) in
      let level = block.level.level in
      Client_keys.get_key cctxt delegate >>=? fun (name, pk, sk) ->
      let pkh = Signature.Public_key.hash pk in
      lwt_debug "Endorsing %a for %s (slots : { %a } )!"
        Block_hash.pp_short block.hash name
        (Format.pp_print_list Format.pp_print_int) slots >>= fun () ->
      inject_endorsement cctxt b block.level.level sk slots pkh >>=? fun oph ->
      cctxt#message
        "Injected endorsement for block '%a' \
         (level %a, slots { %a }, contract %s) '%a'"
        Block_hash.pp_short hash
        Raw_level.pp level
        (Format.pp_print_list Format.pp_print_int) slots name
        Operation_hash.pp_short oph >>= fun () -> return ()
    ) to_endorse

let compute_timeout state =
  match state.to_endorse with
  | [] -> Lwt_utils.never_ending
  | {time} :: _ ->
      let delay = (Time.diff time (Time.now ())) in
      if delay <= 0L then
        Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)


let create (cctxt : #Proto_alpha.full) ?(max_past=(Time.of_seconds 110L)) ~delay contracts block_stream =
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
            schedule_endorsements cctxt ~max_past state bi >>= fun () ->
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

      schedule_endorsements cctxt  ~max_past state head >>= fun () ->
      worker_loop ()
