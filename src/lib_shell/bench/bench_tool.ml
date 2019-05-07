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

module Helpers_Nonce = Nonce
open Proto_alpha
open Parameters_repr
open Constants_repr
open Alpha_context

(** Args *)

type args = {
  mutable length : int ;
  mutable seed : int ;
  mutable accounts : int ;
  mutable nb_commitments : int ;
  mutable params : Parameters_repr.t;
}

let default_args = {
  length = 100 ;
  seed = 0;
  accounts = 100 ;
  nb_commitments = 200 ;
  params = { bootstrap_accounts = [] ;
             commitments = [] ;
             bootstrap_contracts = [] ;
             constants = default ;
             security_deposit_ramp_up_cycles = None ;
             no_reward_cycles = None ;
           }
}

let debug = ref false

let if_debug k =
  if !debug then k ()

let if_debug_s k =
  if !debug then k () else return_unit

let args = default_args

let parse_param_file name =
  if not (Sys.file_exists name) then
    failwith "Parameters : Inexistent JSON file"
  else begin
    Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file name >>=? fun json ->
    match Data_encoding.Json.destruct Parameters_repr.encoding json with
    | exception exn ->
        failwith "Parameters : Invalid JSON file - %a" Error_monad.pp_exn exn
    | param -> return param
  end

let read_args () =
  let parse_param name =
    parse_param_file name >>= begin function
      | Ok p -> Lwt.return p
      | Error errs ->
          Format.printf "Parameters parsing error : %a ==> using \
                         default parameters\n%!" Error_monad.pp_print_error errs ;
          Lwt.return default_args.params end |> Lwt_main.run
  in

  let specific =
    [
      ("--length", Arg.Int (fun n -> args.length <- n), "Length of the chain (nb of blocks)") ;
      ("--seed",   Arg.Int (fun n -> args.seed <- n), "Used seed (default 0)") ;
      ("--random-commitments", Arg.Int (fun n -> args.nb_commitments <- n),
       "Number of randomly generated commitments. Defaults to 200. If \
        less than 0, commitments in protocol parameter files are used.") ;
      ("--accounts", Arg.Int (fun n -> args.accounts <- n),
       "Number of initial randomly generated accounts. Still adds \
        bootstrap account if present in the parameters file.") ;
      ("--parameters", Arg.String (fun s -> args.params <- parse_param s), "JSON protocol parameters file") ;

      ("--debug", Arg.Set debug, "Print more info") ;
    ]
  in
  let usage = "Usage: [--length n] [--seed n] [--accounts n] [--parameters json_file]" in
  Arg.parse specific (fun _ -> ()) usage

(** Utils *)

let choose_exp_nat n =
  (* seems fine *)
  let lambda = 1. /. (log (float n)) in
  let u = Random.float 1. in
  (-. (log u)) /. lambda |> int_of_float

let pi = 3.1415926502
let two_pi = 2. *. 3.1415926502
let round x = x +. 0.5 |> int_of_float

let rec choose_gaussian_nat (a, b) =
  assert (b >= a);
  let sigma = 4. in
  let mu = ((b - a) / 2 + a) |> float in
  let gauss () =
    let u1 = Random.float 1. (* |> fun x -> 1. -. x *) in
    let u2 = Random.float 1. in
    let r = sqrt (-. (2. *. log u1)) in
    let theta = cos (two_pi *. u2) in
    r *. theta
  in
  let z = gauss () in
  let z = z *. sigma +. mu |> round in
  if z > a && z < b then z else choose_gaussian_nat (a, b)

let list_shuffle l =
  List.map (fun c -> (Random.bits (), c)) l |>
  List.sort compare |> List.map snd

(******************************************************************)

type gen_state = { mutable possible_transfers : (Account.t * Account.t) list ;
                   mutable remaining_transfers : (Account.t * Account.t) list ;
                   mutable remaining_activations : (Account.t * Commitment_repr.t) list;
                   mutable nonce_to_reveal : (Cycle.t * Raw_level.t * Nonce.t) list ;
                 }

let generate_random_endorsement ctxt n =
  let slot = n in
  Context.get_endorser ctxt slot >>=? fun delegate ->
  Op.endorsement ~delegate ctxt [ slot ]

let generate_and_add_random_endorsements inc =
  let pred inc = Incremental.predecessor inc in
  let nb_endorsements =
    let n = args.params.constants.endorsers_per_block in
    n - (choose_exp_nat n)
  in
  if_debug begin fun () ->
    Format.printf "[DEBUG] Generating up to %d endorsements...\n%!" nb_endorsements end;

  map_s (generate_random_endorsement (B (pred inc))) (0-- (nb_endorsements -1)) >>=? fun endorsements ->

  let compare op1 op2 =
    Operation_hash.compare (Operation.hash op1) (Operation.hash op2)
  in

  let endorsements = List.sort_uniq compare endorsements in
  let endorsements = List.map Operation.pack endorsements in
  fold_left_s Incremental.add_operation inc endorsements

let regenerate_transfers = ref false
let generate_random_activation ({ remaining_activations ; _ } as gen_state) inc =
  regenerate_transfers := true ;
  let open Account in
  match remaining_activations with
  | [] -> assert false
  | (({ pkh ; _ } as account), _)::l ->
      if_debug begin fun () ->
        Format.printf "[DEBUG] Generating an activation.\n%!" end;
      gen_state.remaining_activations <- l ;
      add_account account;
      Op.activation inc pkh Account.commitment_secret

exception No_transfer_left
let rec generate_random_transfer ({ remaining_transfers ; _ } as gen_state) ctxt =
  if remaining_transfers = [] then raise No_transfer_left;
  let (a1, a2) = List.hd remaining_transfers in
  gen_state.remaining_transfers <- List.tl remaining_transfers;
  let open Account in
  let c1 = Alpha_context.Contract.implicit_contract a1.pkh in
  let c2 = Alpha_context.Contract.implicit_contract a2.pkh in
  Context.Contract.balance ctxt c1 >>=? fun b1 ->
  if Tez.(b1 < Tez.one) then
    generate_random_transfer gen_state ctxt
  else
    Op.transaction ctxt c1 c2 Tez.one


let generate_random_operation (inc : Incremental.t) gen_state =
  let rnd = Random.int 100 in
  match rnd with
  | x when x < 2 && gen_state.remaining_activations <> [] ->
      generate_random_activation gen_state (I inc)
  | _ -> generate_random_transfer gen_state (I inc)

(* Build a random block *)
let step gen_state blk : Block.t tzresult Lwt.t =
  let priority = choose_exp_nat 5 in
  (* let nb_operations_per_block = choose_gaussian_nat (10, List.length (Account.get_known_accounts ())) in *)
  let nb_operations_per_block = choose_gaussian_nat (10, 100) in

  if !regenerate_transfers then begin
    let l = Signature.Public_key_hash.Table.fold
        (fun _ v acc -> v::acc ) Account.known_accounts [] in
    (* TODO : make possible transfer computations efficient.. *)
    gen_state.possible_transfers <- List.product l l |> List.filter (fun (a,b) -> a <> b);
    regenerate_transfers := false
  end;
  gen_state.remaining_transfers <- list_shuffle gen_state.possible_transfers ;

  let nb_operations =
    min nb_operations_per_block (List.length gen_state.remaining_transfers)
  in
  (* Nonce *)
  begin Alpha_services.Helpers.current_level ~offset:1l (Block.rpc_ctxt) blk >>|? function
    | Level.{ expected_commitment = true ; cycle ; level ; _ } ->
        if_debug begin fun () -> Format.printf "[DEBUG] Commiting a nonce\n%!" end;
        begin
          let (hash, nonce) =
            Helpers_Nonce.generate () in
          gen_state.nonce_to_reveal <- (cycle, level, nonce) :: gen_state.nonce_to_reveal;
          Some hash
        end
    | _ -> None
  end >>=? fun seed_nonce_hash ->

  Incremental.begin_construction ~priority ?seed_nonce_hash blk >>=? fun inc ->
  let open Cycle in

  if_debug begin fun () -> Format.printf "[DEBUG] Generating %d random operations...\n%!" nb_operations end;

  (* Generate random operations *)
  fold_left_s
    (fun inc _ ->
       try
         generate_random_operation inc gen_state >>=? fun op ->
         Incremental.add_operation inc op
       with No_transfer_left -> return inc
    )
    inc (1 -- nb_operations) >>=? fun inc ->

  (* Endorsements *)
  generate_and_add_random_endorsements inc >>=? fun inc ->

  (* Revelations *)
  (* TODO debug cycle *)
  begin Alpha_services.Helpers.current_level ~offset:1l Incremental.rpc_ctxt inc >>|? function { cycle ; level ; _ } ->
      if_debug begin fun () -> Format.printf "[DEBUG] Current cycle : %a\n%!" Cycle.pp cycle end ;
      if_debug begin fun () -> Format.printf "[DEBUG] Current level : %a\n%!" Raw_level.pp level end ;
      begin match gen_state.nonce_to_reveal with
        | ((pred_cycle, _, _)::_) as l when succ pred_cycle = cycle ->
            if_debug begin fun () -> Format.printf "[DEBUG] Seed nonce revelation : %d nonces to reveal.\n%!"
              @@ List.length l end;
            gen_state.nonce_to_reveal <- [] ;
            (* fold_left_s (fun inc (_, level, nonce) -> *)
            (* Op.seed_nonce_revelation inc level nonce >>=? fun op ->
             * Incremental.add_operation inc op *)
            (* return *) inc (* TODO reactivate the seeds *)
        (* ) inc l *)
        | _ -> inc
      end
  end >>=? fun inc ->
  (* (\* Shuffle the operations a bit (why not) *\)
   * let operations = endorsements @ operations |> list_shuffle in *)

  Incremental.finalize_block inc

let init () =
  Random.init args.seed ;
  let parameters = args.params in

  (* keys randomness is delegated to module Signature's bindings *)
  (* TODO : distribute the tokens randomly *)
  (* Right now, we split half of 80.000 rolls between generated accounts *)
  (* TODO : ensure we don't overflow with the underlying commitments *)
  Tez_repr.(
    Lwt.return @@ Alpha_environment.wrap_error @@
    args.params.Parameters_repr.constants.Constants_repr.tokens_per_roll
    *? 80_000L >>=? fun total_amount ->
    Lwt.return @@ Alpha_environment.wrap_error @@
    total_amount /? 2L >>=? fun amount ->
    Lwt.return @@ Alpha_environment.wrap_error @@
    amount /? (Int64.of_int args.accounts) ) >>=? fun initial_amount ->

  (* Ensure a deterministic run *)
  let new_seed () : MBytes.t =
    String.(make 32 '\000' |> map (fun _ -> Random.int 0x100 |> char_of_int)) |>
    MBytes.of_string
  in

  map_s
    (fun _ -> return (Account.new_account ~seed:(new_seed ()) (), initial_amount))
    (1--args.accounts) >>=? fun initial_accounts ->
  if_debug begin fun () ->
    List.iter
      (fun (Account.{pkh; _ },_) -> Format.printf "[DEBUG] Account %a created\n%!" Signature.Public_key_hash.pp_short pkh )
      initial_accounts end;

  let possible_transfers =
    let l = List.map fst initial_accounts in
    List.product l l |> List.filter (fun (a,b) -> a <> b)
  in

  begin match args.nb_commitments with
    | x when x < 0 -> return ([], parameters)
    | x ->
        map_s
          (fun _ -> Account.new_commitment ~seed:(new_seed ()) ()) (1 -- x) >>=? fun commitments ->
        return (commitments, { parameters with commitments = List.map snd commitments })
  end >>=? fun (remaining_activations, { bootstrap_accounts=_ ; commitments ;
                                         constants ; security_deposit_ramp_up_cycles ;
                                         no_reward_cycles ; _ }) ->
  let gen_state = { possible_transfers ; remaining_transfers = [] ;
                    nonce_to_reveal = [] ; remaining_activations } in

  Block.genesis_with_parameters constants
    ~commitments
    ~security_deposit_ramp_up_cycles
    ~no_reward_cycles initial_accounts
  >>=? fun genesis ->

  if_debug_s begin fun () ->
    iter_s (let open Account in fun ({ pkh ; _ } as acc, _) ->
        let contract = Alpha_context.Contract.implicit_contract acc.pkh in
        Context.Contract.manager (B genesis) contract >>=? fun { pkh = pkh' ; _ } ->
        Context.Contract.balance (B genesis) contract >>=? fun balance ->
        return @@ Format.printf "[DEBUG] %a's manager is %a with a balance of %a\n%!"
          Signature.Public_key_hash.pp_short pkh
          Signature.Public_key_hash.pp_short pkh'
          Tez.pp balance
      ) initial_accounts end >>=? fun () ->

  if_debug begin fun () ->
    Format.printf "[DEBUG] Constants : %a\n%!"
      Data_encoding.Json.pp
      (Data_encoding.Json.construct
         Constants_repr.parametric_encoding parameters.Parameters_repr.constants)
  end;

  Format.printf "@[<v 2>Starting generation with :@ \
                 @[length    = %d@]@ \
                 @[seed      = %d@]@ \
                 @[nb_commi. = %d@]@ \
                 @[#accounts = %d@]@ @]@." args.length args.seed args.nb_commitments args.accounts;
  let rec loop gen_state blk = function
    | 0 ->  return (gen_state, blk)
    | n -> begin
        Block.print_block blk;
        step gen_state blk >>=? fun blk' ->
        loop gen_state blk' (n-1)
      end
  in
  return (loop gen_state genesis args.length)

let () =
  Lwt_main.run (read_args (); init ()) |> function
  | Ok _head ->
      Format.printf "Success.@." ;
      exit 0
  | Error err ->
      Format.eprintf "%a@." pp_print_error err ;
      exit 1
