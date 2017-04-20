(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Client.Endorsement
open Client_commands
open Cli_entries

module Ed25519 = Environment.Ed25519

module State : sig

  val get_endorsement:
    Client_commands.context ->
    Raw_level.t ->
    int ->
    (Block_hash.t * Operation_hash.t) option tzresult Lwt.t

  val record_endorsement:
    Client_commands.context ->
    Raw_level.t ->
    Block_hash.t ->
    int -> Operation_hash.t -> unit tzresult Lwt.t

end = struct

  module LevelMap = Map.Make(Raw_level)

  type t = (int * Block_hash.t * Operation_hash.t) list LevelMap.t
  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun x -> LevelMap.bindings x)
      (fun l ->
         List.fold_left
           (fun x (y, z) -> LevelMap.add y z x)
           LevelMap.empty l)
      (list (obj2
               (req "level" Raw_level.encoding)
               (req "endorsement"
                  (list (obj3
                           (req "slot" int31)
                           (req "block" Block_hash.encoding)
                           (req "operation" Operation_hash.encoding))))))

  let filename cctxt =
    Client_commands.(Filename.concat cctxt.config.base_dir "endorsements")

  let load cctxt =
    let filename = filename cctxt in
    if not (Sys.file_exists filename) then return LevelMap.empty else
      Data_encoding_ezjsonm.read_file filename >>= function
      | Error _ ->
          cctxt.Client_commands.error
            "couldn't to read the endorsement file"
      | Ok json ->
          match Data_encoding.Json.destruct encoding json with
          | exception _ -> (* TODO print_error *)
              cctxt.Client_commands.error
                "didn't understand the endorsement file"
          | map ->
              return map

  let save cctxt map =
    Lwt.catch
      (fun () ->
         let dirname = Client_commands.(cctxt.config.base_dir) in
         (if not (Sys.file_exists dirname) then Lwt_utils.create_dir dirname
          else Lwt.return ()) >>= fun () ->
         let filename = filename cctxt in
         let json = Data_encoding.Json.construct encoding map in
         Data_encoding_ezjsonm.write_file filename json >>= function
         | Error _ -> failwith "Json.write_file"
         | Ok () -> return ())
      (fun exn ->
         cctxt.Client_commands.error
           "could not write the endorsement file: %s."
           (Printexc.to_string exn))

  let lock = Lwt_mutex.create ()

  let get_endorsement cctxt level slot =
    Lwt_mutex.with_lock lock
      (fun () ->
         load cctxt >>=? fun map ->
         try
           let _, block, op =
             LevelMap.find level map
             |> List.find (fun (slot',_,_) -> slot = slot') in
           return (Some (block, op))
         with Not_found -> return None)

  let record_endorsement cctxt level hash slot oph =
    Lwt_mutex.with_lock lock
      (fun () ->
         load cctxt >>=? fun map ->
         let previous =
           try LevelMap.find level map
           with Not_found -> [] in
         save cctxt
           (LevelMap.add level ((slot, hash, oph) :: previous) map))

end

let get_signing_slots cctxt ?max_priority block delegate level =
  Client_proto_rpcs.Helpers.Rights.endorsement_rights_for_delegate cctxt
    ?max_priority ~first_level:level ~last_level:level
    block delegate () >>=? fun possibilities ->
  let slots =
    List.map (fun (_,slot) -> slot)
    @@ List.filter (fun (l, _) -> l = level) possibilities in
  return slots

let inject_endorsement cctxt
    block level ?async ?force
    src_sk source slot =
  let block = Client_rpcs.last_mined_block block in
  Client_node_rpcs.Blocks.info cctxt.rpc_config block >>=? fun bi ->
  Client_proto_rpcs.Helpers.Forge.Delegate.endorsement cctxt.rpc_config
    block
    ~net_id:bi.net_id
    ~branch:bi.hash
    ~source
    ~block:bi.hash
    ~slot:slot
    () >>=? fun bytes ->
  let signed_bytes = Ed25519.Signature.append src_sk bytes in
  Client_node_rpcs.inject_operation
    cctxt.rpc_config ?force ?async signed_bytes >>=? fun oph ->
  State.record_endorsement cctxt level bi.hash slot oph >>=? fun () ->
  return oph


let previously_endorsed_slot cctxt level slot =
  State.get_endorsement cctxt level slot >>=? function
  | None -> return false
  | Some _ -> return true

let check_endorsement cctxt level slot =
  State.get_endorsement cctxt level slot >>=? function
  | None -> return ()
  | Some (block, _) ->
      Error_monad.failwith
        "Already signed block %a at level %a, slot %d"
        Block_hash.pp_short block Raw_level.pp level slot


let forge_endorsement cctxt
    block ?(force = false)
    ~src_sk ?slot ?max_priority src_pk =
  let block = Client_rpcs.last_mined_block block in
  let src_pkh = Ed25519.Public_key.hash src_pk in
  Client_proto_rpcs.Context.next_level cctxt.rpc_config block >>=? fun { level } ->
  begin
    match slot with
    | Some slot -> return slot
    | None ->
        get_signing_slots
          cctxt.rpc_config ?max_priority block src_pkh level >>=? function
        | slot::_ -> return slot
        | [] -> cctxt.error "No slot found at level %a" Raw_level.pp level
  end >>=? fun slot ->
  begin
    if force then return ()
    else check_endorsement cctxt level slot
  end >>=? fun () ->
  inject_endorsement cctxt
    block level ~force
    src_sk src_pk slot


(** Worker *)

type state = {
  delegates: public_key_hash list ;
  mutable best: Client_mining_blocks.block_info ;
  mutable to_endorse: endorsement list ;
  delay: int64;
}
and endorsement = {
  time: Time.t ;
  delegate: public_key_hash ;
  block: Client_mining_blocks.block_info ;
  slot: int;
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

let schedule_endorsements cctxt state bis =
  let may_endorse (block: Client_mining_blocks.block_info) delegate time =
    Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
    lwt_log_info "May endorse block %a for %s"
      Block_hash.pp_short block.hash name >>= fun () ->
    let b = `Hash block.hash in
    let level = Raw_level.succ block.level.level in
    get_signing_slots cctxt.rpc_config b delegate level >>=? fun slots ->
    lwt_debug "Found slots for %a/%s (%d)"
      Block_hash.pp_short block.hash name (List.length slots) >>= fun () ->
    iter_p
      (fun slot ->
         if Fitness.compare state.best.fitness block.fitness < 0 then begin
           state.best <- block ;
           drop_old_endorsement ~before:block.fitness state ;
         end ;
         previously_endorsed_slot cctxt level slot >>=? function
         | true ->
             lwt_debug "slot %d: previously endorsed." slot >>= fun () ->
             return ()
         | false ->
             try
               let same_slot e =
                 e.block.level = block.level && e.slot = slot in
               let old = List.find same_slot state.to_endorse in
               if Fitness.compare old.block.fitness block.fitness < 0
               then begin
                 lwt_log_info
                   "Schedule endorsement for block %a \
                 \ (level %a, slot %d, time %a) (replace block %a)"
                   Block_hash.pp_short block.hash
                   Raw_level.pp level
                   slot
                   Time.pp_hum time
                   Block_hash.pp_short old.block.hash
                 >>= fun () ->
                 state.to_endorse <-
                   insert
                     { time ; delegate ; block ; slot }
                     (List.filter
                        (fun e -> not (same_slot e))
                        state.to_endorse) ;
                 return ()
               end else begin
                 lwt_debug
                   "slot %d: better pending endorsement"
                   slot >>= fun () ->
                 return ()
               end
             with Not_found ->
               lwt_log_info
                 "Schedule endorsement for block %a \
                 \ (level %a, slot %d, time %a)"
                 Block_hash.pp_short block.hash
                 Raw_level.pp level
                 slot
                 Time.pp_hum time >>= fun () ->
               state.to_endorse <-
                 insert { time ; delegate ; block ; slot } state.to_endorse ;
               return ())
      slots in
  let time = Time.(add (now ()) state.delay) in
  get_delegates cctxt state >>=? fun delegates ->
  iter_p
    (fun delegate ->
       iter_p
         (fun bi -> may_endorse bi delegate time)
         bis)
    delegates

let schedule_endorsements cctxt state bis =
  schedule_endorsements cctxt state bis >>= function
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
  iter_p
    (fun { delegate ; block ; slot } ->
       let hash = block.hash in
       let b = `Hash hash in
       let level = Raw_level.succ block.level.level in
       previously_endorsed_slot cctxt level slot >>=? function
       | true -> return ()
       | false ->
           Client_keys.get_key cctxt delegate >>=? fun (name, pk, sk) ->
           lwt_debug "Endorsing %a for %s (slot %d)!"
             Block_hash.pp_short hash name slot >>= fun () ->
           inject_endorsement cctxt
             b level ~async:true ~force:true
             sk pk slot >>=? fun oph ->
           cctxt.message
             "Injected endorsement for block '%a' \
             \ (level %a, slot %d, contract %s) '%a'"
             Block_hash.pp_short hash
             Raw_level.pp level
             slot name
             Operation_hash.pp_short oph >>= fun () ->
           return ())
    to_endorse

let compute_timeout state =
  match state.to_endorse with
  | [] -> Lwt_utils.never_ending
  | {time} :: _ ->
      let delay = (Time.diff time (Time.now ())) in
      if delay <= 0L then
        Lwt.return_unit
      else
        Lwt_unix.sleep (Int64.to_float delay)

let create cctxt ~delay contracts block_stream =
  lwt_log_info "Starting endorsement daemon" >>= fun () ->
  Lwt_stream.get block_stream >>= function
  | None | Some (Ok []) | Some (Error _) ->
      cctxt.Client_commands.error "Can't fetch the current block head."
  | Some (Ok (bi :: _ as initial_heads)) ->
      let last_get_block = ref None in
      let get_block () =
        match !last_get_block with
        | None ->
            let t = Lwt_stream.get block_stream in
            last_get_block := Some t ;
            t
        | Some t -> t in
      let state = create_state contracts bi (Int64.of_int delay) in
      let rec worker_loop () =
        let timeout = compute_timeout state in
        Lwt.choose [ (timeout >|= fun () -> `Timeout) ;
                     (get_block () >|= fun b -> `Hash b) ] >>= function
        | `Hash (None | Some (Error _)) ->
            Lwt.return_unit
        | `Hash (Some (Ok bis)) ->
            Lwt.cancel timeout ;
            last_get_block := None ;
            schedule_endorsements cctxt state bis >>= fun () ->
            worker_loop ()
        | `Timeout ->
            begin
              endorse cctxt state >>= function
              | Ok () -> Lwt.return_unit
              | Error errs ->
                  lwt_log_error "Error while endorsing:\n%a"
                    pp_print_error
                    errs >>= fun () ->
                  Lwt.return_unit
            end >>= fun () ->
            worker_loop () in
      schedule_endorsements cctxt state initial_heads >>= fun () ->
      worker_loop ()
