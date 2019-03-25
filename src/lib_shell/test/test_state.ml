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

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time = Time.Protocol.of_seconds 0L

module Proto = (val Registered_protocol.get_exn genesis_protocol)

let genesis : State.Chain.genesis = {
  time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

let chain_id = Chain_id.of_block_hash genesis_block

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

let incr_timestamp timestamp =
  Time.Protocol.add timestamp (Int64.add 1L (Random.int64 10L))

let operation op =
  let op : Operation.t = {
    shell = { branch = genesis_block } ;
    proto = MBytes.of_string op ;
  } in
  Operation.hash op,
  op,
  Data_encoding.Binary.to_bytes Operation.encoding op


let block _state ?(context = Context_hash.zero) ?(operations = []) (pred: State.Block.t) name
  : Block_header.t =
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  let pred_header = State.Block.shell_header pred in
  let fitness = incr_fitness pred_header.fitness in
  let timestamp = incr_timestamp pred_header.timestamp in
  { shell = { level = Int32.succ pred_header.level ;
              proto_level = pred_header.proto_level ;
              predecessor = State.Block.hash pred ;
              validation_passes = 1 ;
              timestamp ; operations_hash ; fitness ;
              context } ;
    protocol_data = MBytes.of_string name ;
  }

let parsed_block ({ shell ; protocol_data } : Block_header.t) =
  let protocol_data =
    Data_encoding.Binary.of_bytes_exn
      Proto.block_header_data_encoding
      protocol_data in
  ({ shell ; protocol_data } : Proto.block_header)

let zero = MBytes.create 0

let build_valid_chain state vtbl pred names =
  Lwt_list.fold_left_s
    (fun pred name ->
       State.Block.context pred >>= fun predecessor_context ->
       let rec attempt context =
         begin
           let oph, op, _bytes = operation name in
           let block = block ?context state ~operations:[oph] pred name in
           let hash = Block_header.hash block in
           let pred_header = State.Block.header pred in
           begin
             Proto.begin_application
               ~chain_id: Chain_id.zero
               ~predecessor_context
               ~predecessor_timestamp: pred_header.shell.timestamp
               ~predecessor_fitness: pred_header.shell.fitness
               (parsed_block block) >>=? fun vstate ->
             (* no operations *)
             Proto.finalize_block vstate
           end >>=? fun (ctxt, _metadata) ->
           Context.commit ~time:block.shell.timestamp ctxt.context
           >>= fun context_hash ->
           State.Block.store state
             block zero [[op]] [[zero]]
             ({context_hash;
               message = ctxt.message;
               max_operations_ttl = ctxt.max_operations_ttl;
               last_allowed_fork_level = ctxt.last_allowed_fork_level} :
                State.Block.validation_store)
             ~forking_testchain:false >>=? fun _vblock ->
           State.Block.read state hash >>=? fun vblock ->
           Hashtbl.add vtbl name vblock ;
           return vblock
         end >>= function
         | Ok v -> Lwt.return v
         | Error [ Validation_errors.Inconsistent_hash (got, _) ] ->
             (* Kind of a hack, but at least it tests idempotence to some extent. *)
             attempt (Some got)
         | Error err ->
             Error_monad.pp_print_error Format.err_formatter err ;
             assert false in
       attempt None)
    pred
    names >>= fun _ ->
  Lwt.return_unit

let build_example_tree chain =
  let vtbl = Hashtbl.create 23 in
  Chain.genesis chain >>= fun genesis ->
  Hashtbl.add vtbl "Genesis" genesis ;
  let c = [ "A1" ; "A2" ; "A3" ; "A4" ; "A5" ; "A6" ; "A7" ; "A8" ] in
  build_valid_chain chain vtbl genesis c >>= fun () ->
  let a3 = Hashtbl.find vtbl "A3" in
  let c = [ "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] in
  build_valid_chain chain vtbl a3 c >>= fun () ->
  Lwt.return vtbl

type state = {
  vblock: (string, State.Block.t) Hashtbl.t ;
  state: State.t ;
  chain: State.Chain.t ;
}

let vblock s = Hashtbl.find s.vblock

exception Found of string

let vblocks s =
  Hashtbl.fold (fun k v acc -> (k,v) :: acc) s.vblock []
  |> List.sort Pervasives.compare

let wrap_state_init f base_dir =
  begin
    let store_root = base_dir // "store" in
    let context_root = base_dir // "context" in
    State.init
      ~store_mapsize:4_096_000_000L
      ~context_mapsize:4_096_000_000L
      ~store_root
      ~context_root
      genesis >>=? fun (state, chain, _index) ->
    build_example_tree chain >>= fun vblock ->
    f { state ; chain ; vblock } >>=? fun () ->
    return_unit
  end

let test_init (_ : state) =
  return_unit



(****************************************************************************)

(** State.Block.read *)

let test_read_block (s: state) =
  Lwt_list.iter_s (fun (name, vblock) ->
      let hash = State.Block.hash vblock in
      State.Block.read s.chain hash >>= function
      | Error _ ->
          Assert.fail_msg "Error while reading valid block %s" name
      | Ok _vblock' ->
          (* FIXME COMPARE read operations ??? *)
          Lwt.return_unit
    ) (vblocks s) >>= fun () ->
  return_unit


(****************************************************************************)

(** Chain_traversal.path *)

let rec compare_path p1 p2 = match p1, p2 with
  | [], [] -> true
  | h1 :: p1, h2 :: p2 -> Block_hash.equal h1 h2 && compare_path p1 p2
  | _ -> false

let test_path (s: state) =
  let check_path h1 h2 p2 =
    Chain_traversal.path (vblock s h1) (vblock s h2) >>= function
    | None ->
        Assert.fail_msg "cannot compute path %s -> %s" h1 h2 ;
    | Some (p: State.Block.t list) ->
        let p = List.map State.Block.hash p in
        let p2 = List.map (fun b -> State.Block.hash (vblock s b)) p2 in
        if not (compare_path p p2) then
          Assert.fail_msg "bad path %s -> %s" h1 h2 ;
        Lwt.return_unit in
  check_path "Genesis" "Genesis" [] >>= fun () ->
  check_path "A1" "A1" [] >>= fun () ->
  check_path "A2" "A6" ["A3"; "A4"; "A5"; "A6"] >>= fun () ->
  check_path "B2" "B6" ["B3"; "B4"; "B5"; "B6"] >>= fun () ->
  check_path "A1" "B3" ["A2"; "A3"; "B1"; "B2"; "B3"] >>= fun () ->
  return_unit


(****************************************************************************)

(** Chain_traversal.common_ancestor *)

let test_ancestor s =
  let check_ancestor h1 h2 expected =
    Chain_traversal.common_ancestor
      (vblock s h1) (vblock s h2) >>= fun a ->
    if not (Block_hash.equal (State.Block.hash a) (State.Block.hash expected)) then
      Assert.fail_msg "bad ancestor %s %s" h1 h2 ;
    Lwt.return_unit in
  check_ancestor "Genesis" "Genesis" (vblock s "Genesis") >>= fun () ->
  check_ancestor "Genesis" "A3" (vblock s "Genesis") >>= fun () ->
  check_ancestor "A3" "Genesis" (vblock s "Genesis") >>= fun () ->
  check_ancestor "A1" "A1" (vblock s "A1") >>= fun () ->
  check_ancestor "A1" "A3" (vblock s "A1") >>= fun () ->
  check_ancestor "A3" "A1" (vblock s "A1") >>= fun () ->
  check_ancestor "A6" "B6" (vblock s "A3") >>= fun () ->
  check_ancestor "B6" "A6" (vblock s "A3") >>= fun () ->
  check_ancestor "A4" "B1" (vblock s "A3") >>= fun () ->
  check_ancestor "B1" "A4" (vblock s "A3") >>= fun () ->
  check_ancestor "A3" "B1" (vblock s "A3") >>= fun () ->
  check_ancestor "B1" "A3" (vblock s "A3") >>= fun () ->
  check_ancestor "A2" "B1" (vblock s "A2") >>= fun () ->
  check_ancestor "B1" "A2" (vblock s "A2") >>= fun () ->
  return_unit


(****************************************************************************)

let seed =
  let receiver_id = P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 'r') in
  let sender_id = P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 's') in
  {Block_locator.receiver_id=receiver_id ; sender_id }

(** Chain_traversal.block_locator *)

let test_locator s =
  let check_locator length h1 expected =
    State.compute_locator s.chain
      ~size:length (vblock s h1) seed >>= fun l ->
    let _, l = (l : Block_locator.t :> _ * _) in
    if List.length l <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h1 (List.length l) (List.length expected) ;
    List.iter2
      (fun h h2 ->
         if not (Block_hash.equal h (State.Block.hash @@ vblock s h2)) then
           Assert.fail_msg "Invalid locator %s (expected: %s)" h1 h2)
      l expected ;
    Lwt.return_unit in
  check_locator 6 "A8" ["A7";"A6";"A5";"A4";"A3";"A2"] >>= fun () ->
  check_locator 8 "B8" ["B7";"B6";"B5";"B4";"B3";"B2";"B1";"A3"] >>= fun () ->
  check_locator 4 "B8" ["B7";"B6";"B5";"B4"] >>= fun () ->
  check_locator 0 "A5" [] >>= fun () ->
  check_locator 100 "A5" ["A4";"A3";"A2";"A1";"Genesis"] >>= fun () ->
  return_unit


(****************************************************************************)

(** Chain.known_heads *)

let compare s name heads l =
  if List.length heads <> List.length l then
    Assert.fail_msg
      "unexpected known_heads size (%s: %d %d)"
      name (List.length heads) (List.length l) ;
  List.iter
    (fun bname ->
       let hash = State.Block.hash (vblock s bname) in
       if not (List.exists (fun b -> Block_hash.equal hash (State.Block.hash b)) heads) then
         Assert.fail_msg "missing block in known_heads (%s: %s)" name bname)
    l

let test_known_heads s =
  Chain.known_heads s.chain >>= fun heads ->
  compare s "initial" heads ["A8";"B8"] ;
  return_unit


(****************************************************************************)

(** Chain.head/set_head *)

let test_head s =
  Chain.head s.chain >>= fun head ->
  if not (Block_hash.equal (State.Block.hash head) genesis_block) then
    Assert.fail_msg "unexpected head" ;
  Chain.set_head s.chain (vblock s "A6") >>= fun _ ->
  Chain.head s.chain >>= fun head ->
  if not (Block_hash.equal (State.Block.hash head) (State.Block.hash @@ vblock s "A6")) then
    Assert.fail_msg "unexpected head" ;
  return_unit


(****************************************************************************)

(** Chain.mem *)

let test_mem s =
  let mem s x =
    Chain.mem s.chain (State.Block.hash @@ vblock s x) in
  let test_mem s x =
    mem s x >>= function
    | true -> Lwt.return_unit
    | false -> Assert.fail_msg "mem %s" x in
  let test_not_mem s x =
    mem s x >>= function
    | false -> Lwt.return_unit
    | true -> Assert.fail_msg "not (mem %s)" x in
  test_not_mem s "A3" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.chain (vblock s "A8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.chain (vblock s "A6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.chain (vblock s "B6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.chain (vblock s "B8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_mem s "B8" >>= fun () ->
  return_unit


(****************************************************************************)

(** Chain_traversal.new_blocks *)

let test_new_blocks s =
  let test s head h expected_ancestor expected =
    let to_block = vblock s head
    and from_block = vblock s h in
    Chain_traversal.new_blocks ~from_block ~to_block >>= fun (ancestor, blocks) ->
    if not (Block_hash.equal (State.Block.hash ancestor) (State.Block.hash @@ vblock s expected_ancestor)) then
      Assert.fail_msg "Invalid ancestor %s -> %s (expected: %s)" head h expected_ancestor ;
    if List.length blocks <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h (List.length blocks) (List.length expected) ;
    List.iter2
      (fun h1 h2 ->
         if not (Block_hash.equal (State.Block.hash h1) (State.Block.hash @@ vblock s h2)) then
           Assert.fail_msg "Invalid new blocks %s -> %s (expected: %s)" head h h2)
      blocks expected ;
    Lwt.return_unit
  in
  test s "A6" "A6" "A6" [] >>= fun () ->
  test s "A8" "A6" "A6" ["A7";"A8"] >>= fun () ->
  test s "A8" "B7" "A3" ["A4";"A5";"A6";"A7";"A8"] >>= fun () ->
  return_unit


(****************************************************************************)


let tests : (string * (state -> unit tzresult Lwt.t)) list = [
  "init", test_init ;
  "read_block", test_read_block ;
  "path", test_path ;
  "ancestor", test_ancestor ;
  "locator", test_locator ;
  "known_heads", test_known_heads ;
  "head", test_head ;
  "mem", test_mem ;
  "new_blocks", test_new_blocks ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    Lwt_utils_unix.with_tempdir "tezos_test_" begin fun dir ->
      wrap_state_init f dir >>= function
      | Ok () -> Lwt.return_unit
      | Error error ->
          Format.kasprintf Pervasives.failwith "%a" pp_print_error error
    end
  end

let tests = List.map wrap tests
