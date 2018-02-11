(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

let slots_range_encoding =
  let open Data_encoding in
  (obj3
     (opt "max_priority" int31)
     (opt "first_level" Raw_level.encoding)
     (opt "last_level" Raw_level.encoding))

module Baker = struct

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "rights" / "baking")

    let slot_encoding =
      (obj3
         (req "level" Raw_level.encoding)
         (req "priority" int31)
         (req "timestamp" Timestamp.encoding))

    let rights =
      RPC_service.post_service
        ~description:
          "List delegates allowed to bake for the next level, \
           ordered by priority."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "baking_rights"
                       (list
                          (obj2
                             (req "delegate" Ed25519.Public_key_hash.encoding)
                             (req "timestamp" Timestamp.encoding)))))
        custom_root

    let rights_for_level =
      RPC_service.post_service
        ~description:
          "List delegate allowed to bake for a given level, \
           ordered by priority."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        RPC_path.(custom_root / "level" /: Raw_level.arg)

    (* let levels = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List level for which we might computed baking rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "levels" (list Raw_level.encoding))) *)
    (* RPC_path.(custom_root / "level") *)

    let rights_for_delegate =
      RPC_service.post_service
        ~description: "Future baking rights for a given delegate."
        ~query: RPC_query.empty
        ~input: slots_range_encoding
        ~output: (Data_encoding.list slot_encoding)
        RPC_path.(custom_root / "delegate" /: Ed25519.Public_key_hash.rpc_arg)

    (* let delegates = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List delegates with baking rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "delegates" *)
    (* (list Ed25519.Public_key_hash.encoding))) *)
    (* RPC_path.(custom_root / "delegate") *)

  end

  module I = struct

    let default_max_baking_priority ctxt arg =
      let default = Constants.first_free_baking_slot ctxt in
      match arg with
      | None -> 2 * default
      | Some m -> m

    let baking_rights_for_level ctxt level max =
      let max = default_max_baking_priority ctxt max in
      Baking.baking_priorities ctxt level >>=? fun contract_list ->
      let rec loop l n =
        match n with
        | 0 -> return []
        | n ->
            let Misc.LCons (h, t) = l in
            t () >>=? fun t ->
            loop t (pred n) >>=? fun t ->
            return (h :: t)
      in
      loop contract_list max >>=? fun prio ->
      return (level.level, prio)

    let baking_rights ctxt () max =
      let level = Level.current ctxt in
      baking_rights_for_level ctxt level max >>=? fun (raw_level, slots) ->
      begin
        Lwt_list.filter_map_p (fun x -> x) @@
        List.mapi
          (fun prio c ->
             let timestamp = Timestamp.current ctxt in
             Baking.minimal_time ctxt prio timestamp >>= function
             | Error _ -> Lwt.return None
             | Ok minimal_timestamp -> Lwt.return (Some (c, minimal_timestamp)))
          slots
      end >>= fun timed_slots ->
      return (raw_level, timed_slots)

    let baking_rights_for_delegate
        ctxt contract () (max_priority, min_level, max_level) =
      let max_priority = default_max_baking_priority ctxt max_priority in
      let current_level = Level.current ctxt in
      let min_level = match min_level with
        | None -> current_level
        | Some l -> Level.from_raw ctxt l in
      let max_level =
        match max_level with
        | Some max_level -> Level.from_raw ctxt max_level
        | None ->
            Level.last_level_in_cycle ctxt @@
            current_level.cycle in
      let rec loop level =
        if Level.(>) level max_level
        then return []
        else
          loop (Level.succ ctxt level) >>=? fun t ->
          Baking.first_baking_priorities
            ctxt ~max_priority contract level >>=? fun priorities ->
          let raw_level = level.level in
          Error_monad.map_s
            (fun priority ->
               let timestamp = Timestamp.current ctxt in
               Baking.minimal_time ctxt priority timestamp >>=? fun time ->
               return (raw_level, priority, time))
            priorities >>=? fun priorities ->
          return (priorities @ t)
      in
      loop min_level

  end

  let () =
    let open Services_registration in
    register0 S.rights I.baking_rights ;
    register1 S.rights_for_level begin fun ctxt raw_level () max ->
      let level = Level.from_raw ctxt raw_level in
      I.baking_rights_for_level ctxt level max
    end;
    register1 S.rights_for_delegate I.baking_rights_for_delegate

  let rights ctxt ?max_priority block =
    RPC_context.make_call0 S.rights ctxt block () max_priority

  let rights_for_level ctxt ?max_priority block level =
    RPC_context.make_call1 S.rights_for_level ctxt block level () max_priority

  let rights_for_delegate ctxt ?max_priority ?first_level ?last_level block delegate =
    RPC_context.make_call1 S.rights_for_delegate ctxt block delegate ()
      (max_priority, first_level, last_level)

end

module Endorser = struct

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "helpers" / "rights" / "endorsement")

    let slot_encoding =
      (obj2
         (req "level" Raw_level.encoding)
         (req "priority" int31))

    let rights =
      RPC_service.post_service
        ~description:
          "List delegates allowed to endorse for the current block."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        custom_root

    let rights_for_level =
      RPC_service.post_service
        ~description:
          "List delegates allowed to endorse blocks for a given level."
        ~query: RPC_query.empty
        ~input: (obj1 (opt "max_priority" int31))
        ~output: (obj2
                    (req "level" Raw_level.encoding)
                    (req "delegates"
                       (list Ed25519.Public_key_hash.encoding)))
        RPC_path.(custom_root / "level" /: Raw_level.arg)

    (* let levels = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List level for which we might computed endorsement rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "levels" (list Raw_level.encoding))) *)
    (* RPC_path.(custom_root / "level") *)

    let rights_for_delegate =
      RPC_service.post_service
        ~description: "Compute endorsement rights for a given delegate."
        ~query: RPC_query.empty
        ~input: slots_range_encoding
        ~output: (Data_encoding.list slot_encoding)
        RPC_path.(custom_root / "delegate" /: Ed25519.Public_key_hash.rpc_arg)

    (* let delegates = *)
    (* RPC_service.post_service *)
    (* ~description: *)
    (* "List delegates with endorsement rights." *)
    (* ~query: RPC_query.empty *)
    (* ~input: empty *)
    (* ~output: (obj1 (req "delegates" *)
    (* (list Ed25519.Public_key_hash.encoding))) *)
    (* RPC_path.(custom_root / "delegate") *)

  end

  module I = struct

    let default_max_endorsement_priority ctxt arg =
      let default = Constants.max_signing_slot ctxt in
      match arg with
      | None -> default
      | Some m -> m

    let endorsement_rights ctxt level max =
      let max = default_max_endorsement_priority ctxt max in
      Baking.endorsement_priorities ctxt level >>=? fun contract_list ->
      let rec loop l n =
        match n with
        | 0 -> return []
        | n ->
            let Misc.LCons (h, t) = l in
            t () >>=? fun t ->
            loop t (pred n) >>=? fun t ->
            return (h :: t)
      in
      loop contract_list max >>=? fun prio ->
      return (level.level, prio)

    let endorsement_rights_for_delegate
        ctxt contract () (max_priority, min_level, max_level) =
      let current_level = Level.current ctxt in
      let max_priority = default_max_endorsement_priority ctxt max_priority in
      let min_level = match min_level with
        | None -> Level.succ ctxt current_level
        | Some l -> Level.from_raw ctxt l in
      let max_level =
        match max_level with
        | None -> min_level
        | Some l -> Level.from_raw ctxt l in
      let rec loop level =
        if Level.(>) level max_level
        then return []
        else
          loop (Level.succ ctxt level) >>=? fun t ->
          Baking.first_endorsement_slots
            ctxt ~max_priority contract level >>=? fun slots ->
          let raw_level = level.level in
          let slots = List.rev_map (fun slot -> (raw_level, slot)) slots in
          return (List.rev_append slots t)
      in
      loop min_level

  end

  let () =
    let open Services_registration in
    register0 S.rights begin fun ctxt () max ->
      let level = Level.current ctxt in
      I.endorsement_rights ctxt level max
    end ;
    register1 S.rights_for_level begin fun ctxt raw_level () max ->
      let level = Level.from_raw ctxt raw_level in
      I.endorsement_rights ctxt level max
    end ;
    register1 S.rights_for_delegate I.endorsement_rights_for_delegate

  let rights ctxt ?max_priority block =
    RPC_context.make_call0 S.rights ctxt block () max_priority

  let rights_for_level ctxt ?max_priority block level =
    RPC_context.make_call1 S.rights_for_level ctxt block level () max_priority

  let rights_for_delegate ctxt ?max_priority ?first_level ?last_level block delegate =
    RPC_context.make_call1 S.rights_for_delegate ctxt block delegate ()
      (max_priority, first_level, last_level)

end

module Key = struct

  module S = struct

    open Data_encoding

    let custom_root =
      RPC_path.(open_root / "context" / "key")

    let pk_encoding =
      (obj2
         (req "hash" Ed25519.Public_key_hash.encoding)
         (req "public_key" Ed25519.Public_key.encoding))

    let list =
      RPC_service.post_service
        ~description: "List the known public keys"
        ~query: RPC_query.empty
        ~input: empty
        ~output: (list pk_encoding)
        custom_root

    let get =
      RPC_service.post_service
        ~description: "Fetch the stored public key"
        ~query: RPC_query.empty
        ~input: empty
        ~output: pk_encoding
        RPC_path.(custom_root /: Ed25519.Public_key_hash.rpc_arg )

  end

  let () =
    let open Services_registration in
    register1 S.get begin fun ctxt hash () () ->
      Delegates_pubkey.get ctxt hash >>=? fun pk ->
      return (hash, pk)
    end ;
    register0 S.list begin fun ctxt () () ->
      Delegates_pubkey.list ctxt >>= return
    end

  let list ctxt block =
    RPC_context.make_call0 S.list ctxt block () ()

  let get ctxt block pkh =
    RPC_context.make_call1 S.get ctxt block pkh () ()

end

let baking_rights = Baker.I.baking_rights
let endorsement_rights = Endorser.I.endorsement_rights
