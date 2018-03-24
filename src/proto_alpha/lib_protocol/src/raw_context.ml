(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Int_set = Set.Make (Compare.Int)

type t = {
  context: Context.t ;
  constants: Constants_repr.parametric ;
  first_level: Raw_level_repr.t ;
  level: Level_repr.t ;
  timestamp: Time.t ;
  fitness: Int64.t ;
  endorsements_received: Int_set.t;
  fees: Tez_repr.t ;
  rewards: Tez_repr.t ;
  gas: Gas_repr.t;
}

type context = t
type root_context = t

let current_level ctxt = ctxt.level
let current_timestamp ctxt = ctxt.timestamp
let current_fitness ctxt = ctxt.fitness
let first_level ctxt = ctxt.first_level
let constants ctxt = ctxt.constants
let recover ctxt = ctxt.context

let record_endorsement ctxt k = { ctxt with endorsements_received = Int_set.add k ctxt.endorsements_received }
let endorsement_already_recorded ctxt k = Int_set.mem k ctxt.endorsements_received

let set_current_fitness ctxt fitness = { ctxt with fitness }

let add_fees ctxt fees =
  Lwt.return Tez_repr.(ctxt.fees +? fees) >>=? fun fees ->
  return { ctxt with fees}

let add_rewards ctxt rewards =
  Lwt.return Tez_repr.(ctxt.rewards +? rewards) >>=? fun rewards ->
  return { ctxt with rewards}

let get_rewards ctxt = ctxt.rewards
let get_fees ctxt = ctxt.fees

let set_gas_limit ctxt remaining = { ctxt with gas = Limited { remaining } }
let set_gas_unlimited ctxt = { ctxt with gas = Unaccounted }
let consume_gas ctxt cost =
  Gas_repr.consume ctxt.gas cost >>? fun gas ->
  ok { ctxt with gas }
let gas_level ctxt = ctxt.gas


type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * [`Get | `Set | `Del | `Copy]
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union [
    case (Tag 0)
      (obj1 (req "incompatible_protocol_version" string))
      (function Incompatible_protocol_version arg -> Some arg | _ -> None)
      (fun arg -> Incompatible_protocol_version arg) ;
    case (Tag 1)
      (obj2
         (req "missing_key" (list string))
         (req "function" (string_enum ["get", `Get ; "set", `Set ; "del", `Del ; "copy", `Copy ])))
      (function Missing_key (key, f) -> Some (key, f) | _ -> None)
      (fun (key, f) -> Missing_key (key, f)) ;
    case (Tag 2)
      (obj1 (req "existing_key" (list string)))
      (function Existing_key key -> Some key | _ -> None)
      (fun key -> Existing_key key) ;
    case (Tag 3)
      (obj1 (req "corrupted_data" (list string)))
      (function Corrupted_data key -> Some key | _ -> None)
      (fun key -> Corrupted_data key) ;
  ]

let pp_storage_error ppf = function
  | Incompatible_protocol_version version ->
      Format.fprintf ppf
        "Found a context with an unexpected version '%s'."
        version
  | Missing_key (key, `Get) ->
      Format.fprintf ppf
        "Missing key '%s'."
        (String.concat "/" key)
  | Missing_key (key, `Set) ->
      Format.fprintf ppf
        "Cannot set undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, `Del) ->
      Format.fprintf ppf
        "Cannot delete undefined key '%s'."
        (String.concat "/" key)
  | Missing_key (key, `Copy) ->
      Format.fprintf ppf
        "Cannot copy undefined key '%s'."
        (String.concat "/" key)
  | Existing_key key ->
      Format.fprintf ppf
        "Cannot initialize defined key '%s'."
        (String.concat "/" key)
  | Corrupted_data key ->
      Format.fprintf ppf
        "Failed to parse the data at '%s'."
        (String.concat "/" key)

type error += Storage_error of storage_error

let () =
  register_error_kind
    `Permanent
    ~id:"context.storage_error"
    ~title: "Storage error (fatal internal error)"
    ~description:
      "An error that should never happen unless something \
       has been deleted or corrupted in the database."
    ~pp:(fun ppf err ->
        Format.fprintf ppf
          "@[<v 2>Storage error:@ %a@]"
          pp_storage_error err)
    storage_error_encoding
    (function Storage_error err -> Some err | _ -> None)
    (fun err -> Storage_error err)

let storage_error err = fail (Storage_error err)

(* Initialization *********************************************************)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]
let version_value = "alpha"

let version = "v1"
let first_level_key = [ version ; "first_level" ]
let constants_key = [ version ; "constants" ]
let protocol_param_key = [ "protocol_parameters" ]

let get_first_level ctxt =
  Context.get ctxt first_level_key >>= function
  | None -> storage_error (Missing_key (first_level_key, `Get))
  | Some bytes ->
      match
        Data_encoding.Binary.of_bytes Raw_level_repr.encoding bytes
      with
      | None -> storage_error (Corrupted_data first_level_key)
      | Some level -> return level

let set_first_level ctxt level =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Raw_level_repr.encoding level in
  Context.set ctxt first_level_key bytes >>= fun ctxt ->
  return ctxt

type error += Failed_to_parse_parameter of MBytes.t
type error += Failed_to_decode_parameter of Data_encoding.json * string

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_parameter"
    ~title: "Failed to parse parameter"
    ~description:
      "The protocol parameters are not valid JSON."
    ~pp:begin fun ppf bytes ->
      Format.fprintf ppf
        "@[<v 2>Cannot parse the protocol parameter:@ %s@]"
        (MBytes.to_string bytes)
    end
    Data_encoding.(obj1 (req "contents" bytes))
    (function Failed_to_parse_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_parameter data) ;
  register_error_kind
    `Temporary
    ~id:"context.failed_to_decode_parameter"
    ~title: "Failed to decode parameter"
    ~description:
      "Unexpected JSON object."
    ~pp:begin fun ppf (json, msg) ->
      Format.fprintf ppf
        "@[<v 2>Cannot decode the protocol parameter:@ %s@ %a@]"
        msg
        Data_encoding.Json.pp json
    end
    Data_encoding.(obj2
                     (req "contents" json)
                     (req "error" string))
    (function
      | Failed_to_decode_parameter (json, msg) -> Some (json, msg)
      | _ -> None)
    (fun (json, msg) -> Failed_to_decode_parameter (json, msg))

let get_proto_param ctxt =
  Context.get ctxt protocol_param_key >>= function
  | None ->
      failwith "Missing protocol parameters."
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None -> fail (Failed_to_parse_parameter bytes)
      | Some json -> begin
          Context.del ctxt protocol_param_key >>= fun ctxt ->
          match Data_encoding.Json.destruct Parameters_repr.encoding json with
          | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
              Format.kasprintf
                failwith "Invalid protocol_parameters: %a %a"
                (fun ppf -> Data_encoding.Json.print_error ppf) exn
                Data_encoding.Json.pp json
          | param -> return (param, ctxt)
        end

let set_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Parameters_repr.constants_encoding constants in
  Context.set ctxt constants_key bytes

let get_constants ctxt =
  Context.get ctxt constants_key >>= function
  | None ->
      failwith "Internal error: cannot read constants in context."
  | Some bytes ->
      match
        Data_encoding.Binary.of_bytes Parameters_repr.constants_encoding bytes
      with
      | None ->
          failwith "Internal error: cannot parse constants in context."
      | Some constants -> return constants

let patch_constants ctxt f =
  let constants = f ctxt.constants in
  set_constants ctxt.context constants >>= fun context ->
  Lwt.return { ctxt with context ; constants }

let check_inited ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        return ()
      else
        storage_error (Incompatible_protocol_version s)

let prepare ~level ~timestamp ~fitness ctxt =
  Lwt.return (Raw_level_repr.of_int32 level) >>=? fun level ->
  Lwt.return (Fitness_repr.to_int64 fitness) >>=? fun fitness ->
  check_inited ctxt >>=? fun () ->
  get_constants ctxt >>=? fun constants ->
  get_first_level ctxt >>=? fun first_level ->
  let level =
    Level_repr.from_raw
      ~first_level
      ~blocks_per_cycle:constants.Constants_repr.blocks_per_cycle
      ~blocks_per_voting_period:constants.Constants_repr.blocks_per_voting_period
      ~blocks_per_commitment:constants.Constants_repr.blocks_per_commitment
      level in
  return {
    context = ctxt ; constants ; level ;
    timestamp ; fitness ; first_level ;
    endorsements_received = Int_set.empty ;
    fees = Tez_repr.zero ;
    rewards = Tez_repr.zero ;
    gas = Unaccounted ;
  }

let check_first_block ctxt =
  Context.get ctxt version_key >>= function
  | None -> return ()
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        failwith "Internal error: previously initialized context."
      else if Compare.String.(s = "genesis") then
        return ()
      else
        storage_error (Incompatible_protocol_version s)

let prepare_first_block ~level ~timestamp ~fitness ctxt =
  check_first_block ctxt >>=? fun () ->
  Lwt.return (Raw_level_repr.of_int32 level) >>=? fun first_level ->
  get_proto_param ctxt >>=? fun (param, ctxt) ->
  Context.set ctxt version_key
    (MBytes.of_string version_value) >>= fun ctxt ->
  set_first_level ctxt first_level >>=? fun ctxt ->
  set_constants ctxt param.constants >>= fun ctxt ->
  prepare ctxt ~level ~timestamp ~fitness >>=? fun ctxt ->
  return (param, ctxt)

let activate ({ context = c ; _ } as s) h =
  Updater.activate c h >>= fun c -> Lwt.return { s with context = c }

let fork_test_chain ({ context = c ; _ } as s) protocol expiration =
  Updater.fork_test_chain c ~protocol ~expiration >>= fun c ->
  Lwt.return { s with context = c }

let register_resolvers enc resolve =
  let resolve context str =
    let faked_context = {
      context ;
      constants = Constants_repr.default ;
      first_level = Raw_level_repr.root ;
      level =  Level_repr.root Raw_level_repr.root ;
      timestamp = Time.of_seconds 0L ;
      fitness = 0L ;
      endorsements_received = Int_set.empty ;
      fees = Tez_repr.zero ;
      rewards = Tez_repr.zero ;
      gas = Unaccounted ;
    } in
    resolve faked_context str in
  Context.register_resolver enc  resolve

(* Generic context ********************************************************)

type key = string list

type value = MBytes.t

module type T = sig

  type t
  type context = t

  val mem: context -> key -> bool Lwt.t
  val dir_mem: context -> key -> bool Lwt.t
  val get: context -> key -> value tzresult Lwt.t
  val get_option: context -> key -> value option Lwt.t
  val init: context -> key -> value -> context tzresult Lwt.t
  val set: context -> key -> value -> context tzresult Lwt.t
  val init_set: context -> key -> value -> context Lwt.t
  val set_option: context -> key -> value option -> context Lwt.t
  val delete: context -> key -> context tzresult Lwt.t
  val remove: context -> key -> context Lwt.t
  val remove_rec: context -> key -> context Lwt.t
  val copy: context -> from:key -> to_:key -> context tzresult Lwt.t

  val fold:
    context -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val keys: context -> key -> key list Lwt.t

  val fold_keys:
    context -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  val project: context -> root_context
end

let mem ctxt k = Context.mem ctxt.context k
let dir_mem ctxt k = Context.dir_mem ctxt.context k

let get ctxt k =
  Context.get ctxt.context k >>= function
  | None -> storage_error (Missing_key (k, `Get))
  | Some v -> return v

let get_option ctxt k =
  Context.get ctxt.context k

(* Verify that the k is present before modifying *)
let set ctxt k v =
  Context.mem ctxt.context k >>= function
  | false -> storage_error (Missing_key (k, `Set))
  | true ->
      Context.set ctxt.context k v >>= fun context ->
      return { ctxt with context }

(* Verify that the k is not present before inserting *)
let init ctxt k v =
  Context.mem ctxt.context k >>= function
  | true -> storage_error (Existing_key k)
  | false ->
      Context.set ctxt.context k v >>= fun context ->
      return { ctxt with context }

(* Does not verify that the key is present or not *)
let init_set ctxt k v =
  Context.set ctxt.context k v >>= fun context ->
  Lwt.return { ctxt with context }

(* Verify that the key is present before deleting *)
let delete ctxt k =
  Context.mem ctxt.context k >>= function
  | false -> storage_error (Missing_key (k, `Del))
  | true ->
      Context.del ctxt.context k >>= fun context ->
      return { ctxt with context }

(* Do not verify before deleting *)
let remove ctxt k =
  Context.del ctxt.context k >>= fun context ->
  Lwt.return { ctxt with context }

let set_option ctxt k = function
  | None -> remove ctxt k
  | Some v -> init_set ctxt k v

let remove_rec ctxt k =
  Context.remove_rec ctxt.context k >>= fun context ->
  Lwt.return { ctxt with context }

let copy ctxt ~from ~to_ =
  Context.copy ctxt.context ~from ~to_ >>= function
  | None -> storage_error (Missing_key (from, `Copy))
  | Some context ->
      return { ctxt with context }

let fold ctxt k ~init ~f =
  Context.fold ctxt.context k ~init ~f

let keys ctxt k =
  Context.keys ctxt.context k

let fold_keys ctxt k ~init ~f =
  Context.fold_keys ctxt.context k ~init ~f

let project x = x
