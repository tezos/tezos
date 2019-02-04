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

module Int_set = Set.Make (Compare.Int)

type t = {
  context: Context.t ;
  constants: Constants_repr.parametric ;
  first_level: Raw_level_repr.t ;
  level: Level_repr.t ;
  timestamp: Time.t ;
  fitness: Int64.t ;
  deposits: Tez_repr.t Signature.Public_key_hash.Map.t ;
  allowed_endorsements:
    (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t ;
  fees: Tez_repr.t ;
  rewards: Tez_repr.t ;
  block_gas: Z.t ;
  operation_gas: Gas_limit_repr.t ;
  storage_space_to_pay: Z.t option ;
  allocated_contracts: int option ;
  origination_nonce: Contract_repr.origination_nonce option ;
  internal_nonce: int ;
  internal_nonces_used: Int_set.t ;
}

type context = t
type root_context = t

let current_level ctxt = ctxt.level
let current_timestamp ctxt = ctxt.timestamp
let current_fitness ctxt = ctxt.fitness
let first_level ctxt = ctxt.first_level
let constants ctxt = ctxt.constants
let recover ctxt = ctxt.context

let record_endorsement ctxt k =
  match Signature.Public_key_hash.Map.find_opt k ctxt.allowed_endorsements with
  | None -> assert false
  | Some (_, _, true) -> assert false (* right already used *)
  | Some (d, s, false) ->
      { ctxt with
        allowed_endorsements =
          Signature.Public_key_hash.Map.add k (d,s,true) ctxt.allowed_endorsements }

let init_endorsements ctxt allowed_endorsements =
  if Signature.Public_key_hash.Map.is_empty allowed_endorsements
  then assert false (* can't initialize to empty *)
  else begin
    if Signature.Public_key_hash.Map.is_empty ctxt.allowed_endorsements
    then { ctxt with allowed_endorsements }
    else assert false (* can't initialize twice *)
  end

let allowed_endorsements ctxt =
  ctxt.allowed_endorsements

type error += Too_many_internal_operations (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"too_many_internal_operations"
    ~title: "Too many internal operations"
    ~description:
      "A transaction exceeded the hard limit \
       of internal operations it can emit"
    empty
    (function Too_many_internal_operations -> Some () | _ -> None)
    (fun () -> Too_many_internal_operations)

let fresh_internal_nonce ctxt =
  if Compare.Int.(ctxt.internal_nonce >= 65_535) then
    error Too_many_internal_operations
  else
    ok ({ ctxt with internal_nonce = ctxt.internal_nonce + 1 }, ctxt.internal_nonce)
let reset_internal_nonce ctxt =
  { ctxt with internal_nonces_used = Int_set.empty ; internal_nonce = 0 }
let record_internal_nonce ctxt k =
  { ctxt with internal_nonces_used = Int_set.add k ctxt.internal_nonces_used }
let internal_nonce_already_recorded ctxt k =
  Int_set.mem k ctxt.internal_nonces_used

let set_current_fitness ctxt fitness = { ctxt with fitness }

let add_fees ctxt fees =
  Lwt.return Tez_repr.(ctxt.fees +? fees) >>=? fun fees ->
  return { ctxt with fees}

let add_rewards ctxt rewards =
  Lwt.return Tez_repr.(ctxt.rewards +? rewards) >>=? fun rewards ->
  return { ctxt with rewards}

let add_deposit ctxt delegate deposit =
  let previous =
    match Signature.Public_key_hash.Map.find_opt delegate ctxt.deposits with
    | Some tz -> tz
    | None -> Tez_repr.zero in
  Lwt.return Tez_repr.(previous +? deposit) >>=? fun deposit ->
  let deposits =
    Signature.Public_key_hash.Map.add delegate deposit ctxt.deposits in
  return { ctxt with deposits }

let get_deposits ctxt = ctxt.deposits
let get_rewards ctxt = ctxt.rewards
let get_fees ctxt = ctxt.fees

type error += Undefined_operation_nonce (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"undefined_operation_nonce"
    ~title: "Ill timed access to the origination nonce"
    ~description:
      "An origination was attemped out of the scope of a manager operation"
    empty
    (function Undefined_operation_nonce -> Some () | _ -> None)
    (fun () -> Undefined_operation_nonce)

let init_origination_nonce ctxt operation_hash =
  let origination_nonce =
    Some (Contract_repr.initial_origination_nonce operation_hash) in
  { ctxt with origination_nonce }

let origination_nonce ctxt =
  match ctxt.origination_nonce with
  | None -> error Undefined_operation_nonce
  | Some origination_nonce -> ok origination_nonce

let increment_origination_nonce ctxt =
  match ctxt.origination_nonce with
  | None -> error Undefined_operation_nonce
  | Some cur_origination_nonce ->
      let origination_nonce =
        Some (Contract_repr.incr_origination_nonce cur_origination_nonce) in
      ok ({ ctxt with origination_nonce }, cur_origination_nonce)

let unset_origination_nonce ctxt =
  { ctxt with origination_nonce = None }

type error += Gas_limit_too_high (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"gas_limit_too_high"
    ~title: "Gas limit out of protocol hard bounds"
    ~description:
      "A transaction tried to exceed the hard limit on gas"
    empty
    (function Gas_limit_too_high -> Some () | _ -> None)
    (fun () -> Gas_limit_too_high)

let check_gas_limit ctxt remaining =
  if Compare.Z.(remaining > ctxt.constants.hard_gas_limit_per_operation)
  || Compare.Z.(remaining < Z.zero) then
    error Gas_limit_too_high
  else
    ok ()
let set_gas_limit ctxt remaining =
  { ctxt with operation_gas = Limited { remaining } }
let set_gas_unlimited ctxt =
  { ctxt with operation_gas = Unaccounted }
let consume_gas ctxt cost =
  Gas_limit_repr.consume ctxt.block_gas ctxt.operation_gas cost >>? fun (block_gas, operation_gas) ->
  ok { ctxt with block_gas ; operation_gas }
let check_enough_gas ctxt cost =
  Gas_limit_repr.check_enough ctxt.block_gas ctxt.operation_gas cost
let gas_level ctxt = ctxt.operation_gas
let block_gas_level ctxt = ctxt.block_gas
let gas_consumed ~since ~until =
  match gas_level since, gas_level until with
  | Limited { remaining = before }, Limited { remaining = after } -> Z.sub before after
  | _, _ -> Z.zero

let init_storage_space_to_pay ctxt =
  match ctxt.storage_space_to_pay with
  | Some _ ->
      assert false
  | None ->
      { ctxt with storage_space_to_pay = Some Z.zero ; allocated_contracts = Some 0 }

let update_storage_space_to_pay ctxt n =
  match ctxt.storage_space_to_pay with
  | None ->
      assert false
  | Some storage_space_to_pay ->
      { ctxt with storage_space_to_pay = Some (Z.add n storage_space_to_pay) }

let update_allocated_contracts_count ctxt =
  match ctxt.allocated_contracts with
  | None ->
      assert false
  | Some allocated_contracts ->
      { ctxt with allocated_contracts = Some (succ allocated_contracts) }

let clear_storage_space_to_pay ctxt =
  match ctxt.storage_space_to_pay, ctxt.allocated_contracts with
  | None, _ | _, None ->
      assert false
  | Some storage_space_to_pay, Some allocated_contracts ->
      { ctxt with storage_space_to_pay = None ;
                  allocated_contracts = None},
      storage_space_to_pay,
      allocated_contracts

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * [`Get | `Set | `Del | `Copy]
  | Existing_key of string list
  | Corrupted_data of string list

let storage_error_encoding =
  let open Data_encoding in
  union [
    case (Tag 0)
      ~title:"Incompatible_protocol_version"
      (obj1 (req "incompatible_protocol_version" string))
      (function Incompatible_protocol_version arg -> Some arg | _ -> None)
      (fun arg -> Incompatible_protocol_version arg) ;
    case (Tag 1)
      ~title:"Missing_key"
      (obj2
         (req "missing_key" (list string))
         (req "function" (string_enum ["get", `Get ; "set", `Set ; "del", `Del ; "copy", `Copy ])))
      (function Missing_key (key, f) -> Some (key, f) | _ -> None)
      (fun (key, f) -> Missing_key (key, f)) ;
    case (Tag 2)
      ~title:"Existing_key"
      (obj1 (req "existing_key" (list string)))
      (function Existing_key key -> Some key | _ -> None)
      (fun key -> Existing_key key) ;
    case (Tag 3)
      ~title:"Corrupted_data"
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
let version_value = "alpha_current"

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
        return_unit
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
    allowed_endorsements = Signature.Public_key_hash.Map.empty ;
    fees = Tez_repr.zero ;
    rewards = Tez_repr.zero ;
    deposits = Signature.Public_key_hash.Map.empty ;
    operation_gas = Unaccounted ;
    storage_space_to_pay = None ;
    allocated_contracts = None ;
    block_gas = constants.Constants_repr.hard_gas_limit_per_block ;
    origination_nonce = None ;
    internal_nonce = 0 ;
    internal_nonces_used = Int_set.empty ;
  }

type 'a previous_protocol =
  | Genesis of 'a
  | Alpha_previous

let check_first_block ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      failwith "Internal error: un-initialized context in check_first_block."
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        failwith "Internal error: previously initialized context."
      else if Compare.String.(s = "genesis") then
        return (Genesis ())
      else if Compare.String.(s = "alpha_previous") then
        return Alpha_previous
      else
        storage_error (Incompatible_protocol_version s)

let prepare_first_block ~level ~timestamp ~fitness ctxt =
  check_first_block ctxt >>=? fun previous_protocol ->
  begin
    match previous_protocol with
    | Genesis () ->
        Lwt.return (Raw_level_repr.of_int32 level) >>=? fun first_level ->
        get_proto_param ctxt >>=? fun (param, ctxt) ->
        set_first_level ctxt first_level >>=? fun ctxt ->
        set_constants ctxt param.constants >>= fun ctxt ->
        return (Genesis param, ctxt)
    | Alpha_previous ->
        return (Alpha_previous, ctxt)
  end >>=? fun (previous_proto, ctxt) ->
  Context.set ctxt version_key
    (MBytes.of_string version_value) >>= fun ctxt ->
  prepare ctxt ~level ~timestamp ~fitness >>=? fun ctxt ->
  return (previous_proto, ctxt)

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
      allowed_endorsements = Signature.Public_key_hash.Map.empty ;
      storage_space_to_pay = None ;
      allocated_contracts = None ;
      fees = Tez_repr.zero ;
      rewards = Tez_repr.zero ;
      deposits = Signature.Public_key_hash.Map.empty ;
      block_gas = Constants_repr.default.hard_gas_limit_per_block ;
      operation_gas = Unaccounted ;
      origination_nonce = None ;
      internal_nonce = 0 ;
      internal_nonces_used = Int_set.empty ;
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

  val absolute_key: context -> key -> key

  val consume_gas: context -> Gas_limit_repr.cost -> context tzresult

  val check_enough_gas: context -> Gas_limit_repr.cost -> unit tzresult

  val description: context Storage_description.t

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

let absolute_key _ k = k

let description = Storage_description.create ()
