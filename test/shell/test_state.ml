(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Error_monad

let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time =
  Time.of_seconds 0L

module Proto = (val Updater.get_exn genesis_protocol)

let genesis : State.Net.genesis = {
  time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

let net_id = Net_id.of_block_hash genesis_block

let incr_fitness fitness =
  let new_fitness =
    match fitness with
    | [ fitness ] ->
        Pervasives.(
          Data_encoding.Binary.of_bytes Data_encoding.int64 fitness
          |> Utils.unopt ~default:0L
          |> Int64.succ
          |> Data_encoding.Binary.to_bytes Data_encoding.int64
        )
    | _ -> Data_encoding.Binary.to_bytes Data_encoding.int64 1L
  in
  [ new_fitness ]

let incr_timestamp timestamp =
  Time.add timestamp (Int64.add 1L (Random.int64 10L))

let operation op =
  let op : Operation.t = {
    shell = { net_id ; branch = genesis_block } ;
    proto = MBytes.of_string op ;
  } in
  Operation.hash op,
  op,
  Data_encoding.Binary.to_bytes Operation.encoding op

let block _state ?(operations = []) pred_hash pred name : Block_header.t =
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  let fitness = incr_fitness pred.Block_header.shell.fitness in
  let timestamp = incr_timestamp pred.shell.timestamp in
  { shell = {
        net_id = pred.shell.net_id ;
        level = Int32.succ pred.shell.level ;
        proto_level = pred.shell.proto_level ;
        predecessor = pred_hash ;
        timestamp ; operations_hash ; fitness } ;
    proto = MBytes.of_string name ;
  }

let equal_operation ?msg op1 op2 =
  let msg = Assert.format_msg msg in
  let eq op1 op2 =
    match op1, op2 with
    | None, None -> true
    | Some op1, Some op2 ->
        Operation.equal op1 op2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some op -> Hash.Operation_hash.to_hex (Operation.hash op) in
  Assert.equal ?msg ~prn ~eq op1 op2

let equal_block ?msg st1 st2 =
  let msg = Assert.format_msg msg in
  let eq st1 st2 =
    match st1, st2 with
    | None, None -> true
    | Some st1, Some st2 -> Block_header.equal st1 st2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some st ->
        Hash.Block_hash.to_hex (Block_header.hash st) in
  Assert.equal ?msg ~prn ~eq st1 st2

let block _state ?(operations = []) (pred: State.Block.t) name
  : Block_header.t =
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  let pred_header = State.Block.shell_header pred in
  let fitness = incr_fitness pred_header.fitness in
  let timestamp = incr_timestamp pred_header.timestamp in
  { shell = { net_id = pred_header.net_id ;
              level = Int32.succ pred_header.level ;
              proto_level = pred_header.proto_level ;
              predecessor = State.Block.hash pred ;
              timestamp ; operations_hash ; fitness } ;
    proto = MBytes.of_string name ;
  }

let build_valid_chain state vtbl pred names =
  Lwt_list.fold_left_s
    (fun pred name ->
       begin
         let oph, op, _bytes = operation name in
         let block = block state ~operations:[oph] pred name in
         let hash = Block_header.hash block in
         let pred_header = State.Block.header pred in
         State.Block.context pred >>= fun predecessor_context ->
         begin
           Proto.begin_application
             ~predecessor_context
             ~predecessor_timestamp: pred_header.shell.timestamp
             ~predecessor_fitness: pred_header.shell.fitness
             block >>=? fun vstate ->
           (* no operations *)
           Proto.finalize_block vstate
         end >>=? fun ctxt ->
         State.Block.store state block [[op]] ctxt >>=? fun _vblock ->
         State.Block.read state hash >>=? fun vblock ->
         Hashtbl.add vtbl name vblock ;
         return vblock
       end >>= function
       | Ok v -> Lwt.return v
       | Error err ->
           Error_monad.pp_print_error Format.err_formatter err ;
           assert false)
    pred
    names >>= fun _ ->
  Lwt.return ()

let build_example_tree net =
  let vtbl = Hashtbl.create 23 in
  Chain.genesis net >>= fun genesis ->
  Hashtbl.add vtbl "Genesis" genesis ;
  let chain = [ "A1" ; "A2" ; "A3" ; "A4" ; "A5" ; "A6" ; "A7" ; "A8" ] in
  build_valid_chain net vtbl genesis chain >>= fun () ->
  let a3 = Hashtbl.find vtbl "A3" in
  let chain = [ "B1" ; "B2" ; "B3" ; "B4" ; "B5" ; "B6" ; "B7" ; "B8" ] in
  build_valid_chain net vtbl a3 chain >>= fun () ->
  Lwt.return vtbl

type state = {
  vblock: (string, State.Block.t) Hashtbl.t ;
  state: State.t ;
  net: State.Net.t ;
  init: unit -> State.t tzresult Lwt.t;
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
    let init () =
      State.read
        ~store_root
        ~context_root
        () in
    init () >>=? fun state ->
    State.Net.create state genesis >>= fun net ->
    build_example_tree net >>= fun vblock ->
    f { state ; net ; vblock ; init } >>=? fun () ->
    return ()
  end

let test_init (_ : state) =
  return ()



(****************************************************************************)

(** State.Block.read *)

let test_read_block (s: state) =
  Lwt_list.iter_s (fun (name, vblock) ->
      let hash = State.Block.hash vblock in
      State.Block.read s.net hash >>= function
      | Error _ ->
          Assert.fail_msg "Error while reading valid block %s" name
      | Ok _vblock' ->
          (* FIXME COMPARE read operations ??? *)
          Lwt.return_unit
    ) (vblocks s) >>= fun () ->
  return ()


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
  return ()


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
  return ()


(****************************************************************************)

(** Chain_traversal.block_locator *)

let test_locator s =
  let check_locator h1 expected =
    Chain_traversal.block_locator
      (vblock s h1) (List.length expected) >>= fun l ->
    if List.length l <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h1 (List.length l) (List.length expected) ;
    List.iter2
      (fun h h2 ->
         if not (Block_hash.equal h (State.Block.hash @@ vblock s h2)) then
           Assert.fail_msg "Invalid locator %s (expectd: %s)" h1 h2)
      l expected ;
    Lwt.return_unit in
  check_locator "A8" ["A8";"A7";"A6";"A5";"A4";"A3";"A2"] >>= fun () ->
  check_locator "B8" ["B8";"B7";"B6";"B5";"B4";"B3";"B2";"B1";"A3"] >>= fun () ->
  check_locator "B8" ["B8";"B7";"B6";"B5";"B4"] >>= fun () ->
  return ()


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
  Chain.known_heads s.net >>= fun heads ->
  compare s "initial" heads ["A8";"B8"] ;
  return ()


(****************************************************************************)

(** Chain.head/set_head *)

let test_head s =
  Chain.head s.net >>= fun head ->
  if not (Block_hash.equal (State.Block.hash head) genesis_block) then
    Assert.fail_msg "unexpected head" ;
  Chain.set_head s.net (vblock s "A6") >>= fun _ ->
  Chain.head s.net >>= fun head ->
  if not (Block_hash.equal (State.Block.hash head) (State.Block.hash @@ vblock s "A6")) then
    Assert.fail_msg "unexpected head" ;
  return ()


(****************************************************************************)

(** Chain.mem *)

let test_mem s =
  let mem s x =
    Chain.mem s.net (State.Block.hash @@ vblock s x) in
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
  Chain.set_head s.net (vblock s "A8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.net (vblock s "A6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_not_mem s "B1" >>= fun () ->
  test_not_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.net (vblock s "B6") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_not_mem s "B8" >>= fun () ->
  Chain.set_head s.net (vblock s "B8") >>= fun _ ->
  test_mem s "A3" >>= fun () ->
  test_not_mem s "A4" >>= fun () ->
  test_not_mem s "A6" >>= fun () ->
  test_not_mem s "A8" >>= fun () ->
  test_mem s "B1" >>= fun () ->
  test_mem s "B6" >>= fun () ->
  test_mem s "B8" >>= fun () ->
  return ()


(****************************************************************************)

(** Chain_traversal.new_blocks *)

let test_new_blocks s =
  let test s head h expected_ancestor expected =
    let to_block = vblock s head
    and from_block = vblock s h in
    Chain_traversal.new_blocks ~from_block ~to_block >>= fun (ancestor, blocks) ->
    if not (Block_hash.equal (State.Block.hash ancestor) (State.Block.hash @@ vblock s expected_ancestor)) then
      Assert.fail_msg "Invalid locator %s (expected: %s)" h expected_ancestor ;
    if List.length blocks <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h (List.length blocks) (List.length expected) ;
    List.iter2
      (fun h1 h2 ->
         if not (Block_hash.equal (State.Block.hash h1) (State.Block.hash @@ vblock s h2)) then
           Assert.fail_msg "Invalid locator %s (expected: %s)" h h2)
      blocks expected ;
    Lwt.return_unit
  in
  test s "A6" "A6" "A6" [] >>= fun () ->
  test s "A8" "A6" "A6" ["A7";"A8"] >>= fun () ->
  test s "A8" "B7" "A3" ["A4";"A5";"A6";"A7";"A8"] >>= fun () ->
  return ()


(****************************************************************************)

(** Chain.find_new *)

let test_find_new s =
  let test s h expected =
    Chain_traversal.block_locator (vblock s h) 50 >>= fun loc ->
    Chain.find_new s.net loc (List.length expected) >>= fun blocks ->
    if List.length blocks <> List.length expected then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h (List.length blocks) (List.length expected) ;
    List.iter2
      (fun h1 h2 ->
         if not (Block_hash.equal h1 (State.Block.hash @@ vblock s h2)) then
           Assert.fail_msg "Invalid locator %s (expected: %s)" h h2)
      blocks expected ;
    Lwt.return_unit
  in
  test s "A6" [] >>= fun () ->
  Chain.set_head s.net (vblock s "A8") >>= fun _ ->
  test s "A6" ["A7";"A8"] >>= fun () ->
  test s "A6" ["A7"] >>= fun () ->
  test s "B4" ["A4"] >>= fun () ->
  test s "B7" ["A4";"A5";"A6";"A7"] >>= fun () ->
  return ()


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
  "find_new", test_find_new ;
]

let () =
  Test.run "state." (List.map (fun (s, f) -> s, wrap_state_init f) tests)
