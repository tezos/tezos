(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Int_set = Set.Make (Compare.Int)

type t = {
  context: Context.t ;
  constants: Constants_repr.constants ;
  first_level: Raw_level_repr.t ;
  level: Level_repr.t ;
  timestamp: Time.t ;
  fitness: Int64.t ;
  roll_value: Tez_repr.t ;
  faucet_count: int;
  endorsements_received: Int_set.t;
}

type context = t
type root_context = t

let current_level ctxt = ctxt.level
let current_timestamp ctxt = ctxt.timestamp
let current_fitness ctxt = ctxt.fitness
let first_level ctxt = ctxt.first_level
let faucet_count ctxt = ctxt.faucet_count
let constants ctxt = ctxt.constants
let roll_value ctxt = ctxt.roll_value
let recover ctxt = ctxt.context

let record_endorsement ctxt k = { ctxt with endorsements_received = Int_set.add k ctxt.endorsements_received }
let endorsement_already_recorded ctxt k = Int_set.mem k ctxt.endorsements_received

let incr_faucet_count ctxt = { ctxt with faucet_count = ctxt.faucet_count + 1 }
let set_current_fitness ctxt fitness = { ctxt with fitness }

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * [`Get | `Set | `Del]
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union [
    case ~tag:0
      (obj1 (req "incompatible_protocol_version" string))
      (function Incompatible_protocol_version arg -> Some arg | _ -> None)
      (fun arg -> Incompatible_protocol_version arg) ;
    case ~tag:1
      (obj2
         (req "missing_key" (list string))
         (req "function" (string_enum ["get", `Get ; "set", `Set])))
      (function Missing_key (key, f) -> Some (key, f) | _ -> None)
      (fun (key, f) -> Missing_key (key, f)) ;
    case ~tag:2
      (obj1 (req "existing_key" (list string)))
      (function Existing_key key -> Some key | _ -> None)
      (fun key -> Existing_key key) ;
    case ~tag:3
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

let is_first_block ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      return true
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        return false
      else if Compare.String.(s = "genesis") then
        return true
      else
        storage_error (Incompatible_protocol_version s)

let version = "v1"
let first_level_key = [ version ; "first_level" ]
let roll_value_key = [ version ; "roll_value" ]
let sandboxed_key = [ version ; "sandboxed" ]

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
    Data_encoding.Binary.to_bytes Raw_level_repr.encoding level in
  Context.set ctxt first_level_key bytes >>= fun ctxt ->
  return ctxt

let get_roll_value ctxt =
  Context.get ctxt roll_value_key >>= function
  | None -> storage_error (Missing_key (roll_value_key, `Get))
  | Some bytes ->
      match
        Data_encoding.Binary.of_bytes Tez_repr.encoding bytes
      with
      | None -> storage_error (Corrupted_data roll_value_key)
      | Some level -> return level

let set_roll_value ctxt level =
  let bytes =
    Data_encoding.Binary.to_bytes Tez_repr.encoding level in
  Context.set ctxt roll_value_key bytes >>= fun ctxt ->
  return ctxt

type error += Failed_to_parse_sandbox_parameter of MBytes.t

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_sandbox_parameter"
    ~title: "Failed to parse sandbox parameter"
    ~description:
      "The sandbox paramater is not a valid JSON string."
    ~pp:begin fun ppf bytes ->
      Format.fprintf ppf
        "@[<v 2>Cannot parse the sandbox parameter:@ %s@]"
        (MBytes.to_string bytes)
    end
    Data_encoding.(obj1 (req "contents" bytes))
    (function Failed_to_parse_sandbox_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_sandbox_parameter data)

let get_sandboxed c =
  Context.get c sandboxed_key >>= function
  | None -> return None
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None -> fail (Failed_to_parse_sandbox_parameter bytes)
      | Some json -> return (Some json)

let set_sandboxed c json =
  Context.set c sandboxed_key
    (Data_encoding.Binary.to_bytes Data_encoding.json json)

let may_tag_first_block ctxt level =
  is_first_block ctxt >>=? function
  | false ->
      get_first_level ctxt >>=? fun level ->
      return (ctxt, false, level)
  | true ->
      Context.set ctxt version_key
        (MBytes.of_string version_value) >>= fun ctxt ->
      set_first_level ctxt level >>=? fun ctxt ->
      return (ctxt, true, level)

let prepare ~level ~timestamp ~fitness ctxt =
  Lwt.return (Raw_level_repr.of_int32 level ) >>=? fun level ->
  Lwt.return (Fitness_repr.to_int64 fitness) >>=? fun fitness ->
  may_tag_first_block ctxt level >>=? fun (ctxt, first_block, first_level) ->
  get_sandboxed ctxt >>=? fun sandbox ->
  Constants_repr.read sandbox >>=? fun constants ->
  begin
    if first_block then begin
      set_roll_value ctxt constants.initial_roll_value >>=? fun ctxt ->
      return (ctxt, constants.initial_roll_value)
    end else begin
      get_roll_value ctxt >>=? fun roll_value ->
      return (ctxt, roll_value)
    end
  end >>=? fun (ctxt, roll_value) ->
  let level =
    Level_repr.from_raw
      ~first_level
      ~cycle_length:constants.Constants_repr.cycle_length
      ~voting_period_length:constants.Constants_repr.voting_period_length
      level in
  return ({ context = ctxt ; constants ; level ;
            timestamp ; fitness ; first_level ; roll_value ;
            faucet_count = 0 ; endorsements_received = Int_set.empty ;
          },
          first_block)

let rec double_roll_value ctxt i =
  if Compare.Int.(i <= 0) then
    return ctxt
  else
    Lwt.return Tez_repr.(ctxt.roll_value +? ctxt.roll_value) >>=? fun roll_value ->
    set_roll_value ctxt.context roll_value >>=? fun context ->
    double_roll_value { ctxt with context ; roll_value } (i-1)

let activate ({ context = c } as s) h =
  Updater.activate c h >>= fun c -> Lwt.return { s with context = c }
let fork_test_network ({ context = c } as s) protocol expiration =
  Updater.fork_test_network c ~protocol ~expiration >>= fun c ->
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
      roll_value = Tez_repr.zero ;
      faucet_count = 0 ;
      endorsements_received = Int_set.empty ;
    } in
    resolve faked_context str in
  Context.register_resolver enc  resolve

type error += Unimplemented_sandbox_migration

let configure_sandbox ctxt json =
  let rec json_equals x y = match x, y with
    | `Float x, `Float y -> Compare.Float.(x = y)
    | `Bool x, `Bool y -> Compare.Bool.(x = y)
    | `String x, `String y -> Compare.String.(x = y)
    | `Null, `Null -> true
    | `O x, `O y ->
        let sort =
          List.sort (fun (a, _) (b, _) -> Compare.String.compare a b) in
        Compare.Int.(=) (List.length x) (List.length y) &&
        List.for_all2
          (fun (nx, vx) (ny, vy) ->
             Compare.String.(nx = ny) && json_equals vx vy)
          (sort x) (sort y)
    | `A x, `A y ->
        Compare.Int.(=) (List.length x) (List.length y) &&
        List.for_all2 json_equals x y
    | _, _ -> false
  in
  let json =
    match json with
    | None -> `O []
    | Some json -> json in
  is_first_block ctxt >>=? function
  | true ->
      set_sandboxed ctxt json >>= fun ctxt ->
      return ctxt
  | false ->
      get_sandboxed ctxt >>=? function
      | None ->
          fail Unimplemented_sandbox_migration
      | Some existing ->
          if json_equals existing json then
            return ctxt
          else
            failwith "Changing sandbox parameter is not yet implemented"

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

let fold ctxt k ~init ~f =
  Context.fold ctxt.context k ~init ~f

let keys ctxt k =
  Context.keys ctxt.context k

let fold_keys ctxt k ~init ~f =
  Context.fold_keys ctxt.context k ~init ~f

let project x = x
