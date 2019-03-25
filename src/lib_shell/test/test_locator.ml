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

let (//) = Filename.concat

(** Basic blocks *)

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time = Time.Protocol.of_seconds 0L

let state_genesis_block =
  {
    State.Chain.time = genesis_time ;
    State.Chain.block= genesis_hash ;
    State.Chain.protocol = genesis_protocol
  }

let chain_id = Chain_id.of_block_hash genesis_hash

module Proto = (val Registered_protocol.get_exn genesis_protocol)

let incr_timestamp timestamp =
  Time.Protocol.add timestamp (Int64.add 1L (Random.int64 10L))

let incr_fitness fitness =
  let new_fitness =
    match fitness with
    | [ fitness ] ->
        Pervasives.(
          Data_encoding.Binary.of_bytes Data_encoding.int64 fitness
          |> Option.unopt ~default:0L
          |> Int64.succ
          |> Data_encoding.Binary.to_bytes_exn Data_encoding.int64
        )
    | _ -> Data_encoding.Binary.to_bytes_exn Data_encoding.int64 1L
  in
  [ new_fitness ]


(* returns a new state with a single block, genesis *)
let init_chain base_dir : State.Chain.t Lwt.t =
  let store_root = base_dir // "store" in
  let context_root = base_dir // "context" in
  State.init
    ~store_root ~context_root state_genesis_block >>= function
  | Error _ -> Pervasives.failwith "read err"
  | Ok (_state, chain, _index) ->
      Lwt.return chain


let block_header
    ?(context = Context_hash.zero)
    (pred : State.Block.t) : Block_header.t =
  let pred_header = State.Block.shell_header pred in
  let timestamp = incr_timestamp pred_header.timestamp in
  let fitness = incr_fitness pred_header.fitness in
  {
    Block_header.shell =
      {
        level = Int32.add Int32.one (State.Block.level pred);
        proto_level = 0;
        predecessor = State.Block.hash pred;
        timestamp = timestamp;
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        fitness = fitness ;
        context ;
      } ;
    Block_header.protocol_data = MBytes.of_string "" ;
  }

let zero = MBytes.create 0

(* adds n blocks on top of an initialized chain *)
let make_empty_chain (chain:State.Chain.t) n : Block_hash.t Lwt.t =
  State.Block.read_opt chain genesis_hash >|= Option.unopt_assert ~loc:__POS__ >>= fun genesis ->
  State.Block.context genesis >>= fun empty_context ->
  let header = State.Block.header genesis in
  let timestamp = State.Block.timestamp genesis in
  Context.hash ~time:timestamp empty_context
  >>= fun empty_context_hash ->
  Context.commit
    ~time:header.shell.timestamp empty_context >>= fun context ->
  let header = { header with shell = { header.shell with context } } in
  let empty_result = {
    State.Block.
    context_hash = empty_context_hash ;
    message = None ;
    max_operations_ttl = 0 ;
    last_allowed_fork_level = 0l ;
  } in
  let rec loop lvl pred =
    if lvl >= n then
      return pred
    else
      let header =
        { header with
          shell = { header.shell with predecessor = pred ;
                                      level = Int32.of_int lvl } } in
      State.Block.store chain header zero [] [] empty_result ~forking_testchain:false >>=? fun _ ->
      loop (lvl+1) (Block_header.hash header)
  in
  loop 1 genesis_hash >>= function
  | Ok b -> Lwt.return b
  | Error err ->
      Error_monad.pp_print_error Format.err_formatter err ;
      assert false




(* helper functions ------------------------------------- *)

(* wall clock time of a unit function *)
let time1 (f: unit -> 'a) : 'a * float =
  let t = Unix.gettimeofday () in
  let res = f () in
  let wall_clock = Unix.gettimeofday () -. t in
  (res,wall_clock)

(* returns result from first run and average time of [runs] runs *)
let time ?(runs=1) f =
  if runs < 1 then invalid_arg "time negative arg" else
    let rec loop cnt sum =
      if cnt = (runs)
      then sum
      else
        let (_,t) = time1 f in
        loop (cnt+1) (sum+.t)
    in
    let (res,t) = time1 f in
    let sum = loop 1 t in
    (res, sum /. (float runs))

let rec repeat f n =
  if n<0 then invalid_arg "repeat: negative arg" else
  if n=0 then ()
  else let _ = f () in repeat f (n-1)

(* ----------------------------------------------------- *)

let print_block b =
  Printf.printf "%6i %s\n"
    (Int32.to_int (State.Block.level b))
    (Block_hash.to_b58check (State.Block.hash b))

let print_block_h chain bh =
  State.Block.read_opt chain bh >|= Option.unopt_assert ~loc:__POS__ >|= fun b ->
  print_block b



(* returns the predecessor at distance one, reading the header *)
let linear_predecessor chain (bh: Block_hash.t) : Block_hash.t option Lwt.t =
  State.Block.read_opt chain bh >|= Option.unopt_assert ~loc:__POS__ >>= fun b ->
  State.Block.predecessor b >|= function
  | None -> None
  | Some pred -> Some (State.Block.hash pred)

let print_chain chain bh =
  let rec loop bh cnt =
    let _ = print_block_h chain bh in
    linear_predecessor chain bh >>= function
    | Some pred -> loop pred (cnt+1)
    | None -> Lwt.return_unit
  in
  loop bh 0


(* returns the predecessors at ditance n, traversing all n intermediate blocks *)
let linear_predecessor_n (chain:State.Chain.t) (bh:Block_hash.t) (distance:int)
  : Block_hash.t option Lwt.t =
  (* let _ = Printf.printf "LP: %4i " distance; print_block_h chain bh in *)
  if distance < 1 then invalid_arg "distance<1" else
    let rec loop bh distance =
      if distance = 0
      then Lwt.return_some bh   (* reached distance *)
      else
        linear_predecessor chain bh >>= function
        | None -> Lwt.return_none
        | Some pred ->
            loop pred (distance-1)
    in
    loop bh distance



(* Tests that the linear predecessor defined above and the
   exponential predecessor implemented in State.predecessor_n
   return the same block and it is the block at the distance
   requested *)
let test_pred (base_dir:string) : unit tzresult Lwt.t =
  let size_chain = 1000 in
  init_chain base_dir >>= fun chain ->
  make_empty_chain chain size_chain >>= fun head ->

  let test_once distance =
    linear_predecessor_n chain head distance >>= fun lin_res ->
    State.Block.read_opt chain head >|= Option.unopt_assert ~loc:__POS__ >>= fun head_block ->
    State.Block.predecessor_n head_block distance >>= fun exp_res ->
    match lin_res,exp_res with
    | None, None ->
        Lwt.return_unit
    | None,Some _ | Some _,None ->
        Assert.fail_msg "mismatch between exponential and linear predecessor_n"
    | Some lin_res, Some exp_res ->
        (* check that the two results are the same *)
        (assert (lin_res = exp_res));
        State.Block.read_opt chain lin_res >|= Option.unopt_assert ~loc:__POS__ >>= fun pred ->
        let level_pred = Int32.to_int (State.Block.level pred) in
        State.Block.read_opt chain head >|= Option.unopt_assert ~loc:__POS__ >>= fun head ->
        let level_start = Int32.to_int (State.Block.level head) in
        (* check distance using the level *)
        assert (level_start - distance = level_pred);
        Lwt.return_unit
  in
  let _ = Random.self_init () in
  let range = size_chain+(size_chain/10) in
  let repeats = 100 in
  return (repeat (fun () -> test_once (1 + Random.int range)) repeats)

let seed =
  let receiver_id = P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 'r') in
  let sender_id = P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 's') in
  {Block_locator.receiver_id=receiver_id ; sender_id }

(* compute locator using the linear predecessor *)
let compute_linear_locator (chain:State.Chain.t) ~size block =
  let genesis = State.Chain.genesis chain in
  let block_hash = State.Block.hash block in
  let header = State.Block.header block in
  Block_locator.compute ~predecessor:(linear_predecessor_n chain)
    ~genesis:genesis.block block_hash header ~size seed


(* given the size of a chain, returns the size required for a locator
   to reach genesis *)
let compute_size_locator size_chain =
  let repeats = 10. in
  int_of_float ((log ((float size_chain) /. repeats)) /. (log 2.) -. 1.) * 10

(* given the size of a locator, returns the size of the chain that it
   can cover back to genesis *)
let compute_size_chain size_locator =
  let repeats = 10. in
  int_of_float (repeats *. (2. ** (float (size_locator + 1))))


(* test if the linear and exponential locator are the same and outputs
   their timing.
   Run the test with:
   $ dune build @runbench_locator
   Copy the output to a file timing.dat and plot it with:
   $ test_locator_plot.sh timing.dat
*)
(*
   chain 1 year   518k   covered by locator 150
   chain 2 months 86k    covered by locator 120
*)
let test_locator base_dir =
  let size_chain = 80000 in
  (* timing locators with average over [runs] times *)
  let runs = 10 in
  let _ = Printf.printf "#runs %i\n" runs in
  (* limit after which exp should go linear *)
  let exp_limit = compute_size_chain 120 in
  let _ = Printf.printf "#exp_limit %i\n" exp_limit in
  (* size after which locator always reaches genesis *)
  let locator_limit = compute_size_locator size_chain in
  let _ = Printf.printf "#locator_limit %i\n" locator_limit in

  init_chain base_dir >>= fun chain ->
  time1 (fun () ->
      make_empty_chain chain size_chain) |>
  fun (res, t_chain) ->
  let _ = Printf.printf
      "#size_chain %i built in %f sec\n#      size      exp       lins\n"
      size_chain t_chain in
  res >>= fun head ->

  let check_locator size : unit tzresult Lwt.t =
    State.Block.read chain head >>=? fun block ->
    time ~runs:runs (fun () ->
        State.compute_locator chain ~size:size block seed) |>
    fun (l_exp,t_exp) ->
    time ~runs:runs (fun () ->
        compute_linear_locator chain ~size:size block) |>
    fun (l_lin,t_lin) ->
    l_exp >>= fun l_exp ->
    l_lin >>= fun l_lin ->
    let _, l_exp = (l_exp : Block_locator.t :> _ * _) in
    let _, l_lin = (l_lin : Block_locator.t :> _ * _) in
    let _ = Printf.printf "%10i %f %f\n" size t_exp t_lin in
    List.iter2
      (fun hn ho ->
         if not (Block_hash.equal hn ho)
         then
           Assert.fail_msg "Invalid locator %i" size)
      l_exp l_lin;
    return_unit
  in
  let stop = locator_limit + 20 in
  let rec loop size =
    if size < stop then (
      check_locator size >>=? fun _ ->
      loop (size+5)
    )
    else return_unit
  in
  loop 1

let wrap n f =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    Lwt_utils_unix.with_tempdir "tezos_test_" begin fun dir ->
      f dir >>= function
      | Ok () -> Lwt.return_unit
      | Error error ->
          Format.kasprintf Pervasives.failwith "%a" pp_print_error error
    end
  end

let tests =
  [ wrap "test pred" test_pred ]

let bench = [ wrap "test locator" test_locator ]

let tests =
  try
    if Sys.argv.(1) = "--no-bench" then
      tests
    else
      tests @ bench
  with _ -> tests @ bench


let () =
  Alcotest.run ~argv:[|""|] "tezos-shell" [
    "locator", tests
  ]
