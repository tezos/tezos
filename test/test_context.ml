
open Utils
open Hash
open Context

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b48check
    "Et22nEeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

let genesis_protocol =
  Protocol_hash.of_b48check
    "JF7Fxgeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

let genesis_time =
  Time.of_seconds 0L

let genesis = {
  Store.time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

(** Context creation *)

let block2 =
  Block_hash.of_hex
    "2222222222222222222222222222222222222222222222222222222222222222"

let faked_block : Store.block_header = {
  shell = {
    net_id = Net genesis_block ;
    predecessor = genesis_block ;
    operations = [] ;
    fitness = [] ;
    timestamp = Time.of_seconds 0L ;
  } ;
  proto = MBytes.of_string "" ;
}

let create_block2 idx =
  checkout idx genesis_block >>= function
  | None | Some (Error _) ->
      Test.fail "checkout genesis_block"
  | Some (Ok ctxt) ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["version";] (MBytes.of_string "0.0") >>= fun ctxt ->
      commit idx faked_block block2 ctxt

let block3a =
  Block_hash.of_hex
    "3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a"

let create_block3a idx =
  checkout idx block2 >>= function
  | None | Some (Error _) ->
      Test.fail "checkout block2"
  | Some (Ok ctxt) ->
      del ctxt ["a"; "b"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Mars") >>= fun ctxt ->
      commit idx faked_block block3a ctxt

let block3b =
  Block_hash.of_hex
    "3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b"

let block3c =
  Block_hash.of_hex
    "3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c"

let create_block3b idx =
  checkout idx block2 >>= function
  | None | Some (Error _) ->
      Test.fail "checkout block3b"
  | Some (Ok ctxt) ->
      del ctxt ["a"; "c"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Février") >>= fun ctxt ->
      commit idx faked_block block3b ctxt

let wrap_context_init f base_dir =
  let root = base_dir // "context" in
  Context.init root >>= fun idx ->
  Context.create_genesis_context idx genesis genesis_protocol >>= fun _ ->
  create_block2 idx >>= fun () ->
  create_block3a idx >>= fun () ->
  create_block3b idx >>= fun () ->
  commit_invalid idx faked_block block3c [Error_monad.Unclassified "TEST"] >>= fun () ->
  f idx


(** Simple test *)

let c = function
  | None -> None
  | Some s -> Some (MBytes.to_string s)

let test_simple idx =
  checkout idx block2 >>= function
  | None | Some (Error _) ->
      Test.fail "checkout block2"
  | Some (Ok ctxt) ->
      get ctxt ["version"] >>= fun version ->
      assert (c version = Some "0.0");
      get ctxt ["a";"b"] >>= fun novembre ->
      assert (c novembre = Some "Novembre");
      get ctxt ["a";"c"] >>= fun juin ->
      assert (c juin = Some "Juin");
      Lwt.return ()

let test_continuation idx =
  checkout idx block3a >>= function
  | None | Some (Error _) ->
      Test.fail "checkout block3a"
  | Some (Ok ctxt) ->
      get ctxt ["version"] >>= fun version ->
      assert (c version = Some "0.0");
      get ctxt ["a";"b"] >>= fun novembre ->
      assert (c novembre = None);
      get ctxt ["a";"c"] >>= fun juin ->
      assert (c juin = Some "Juin");
      get ctxt ["a";"d"] >>= fun mars ->
      assert (c mars = Some "Mars");
      Lwt.return ()

let test_fork idx =
  checkout idx block3b >>= function
  | None | Some (Error _) ->
      Test.fail "checkout block3b"
  | Some (Ok ctxt) ->
      get ctxt ["version"] >>= fun version ->
      assert (c version = Some "0.0");
      get ctxt ["a";"b"] >>= fun novembre ->
      assert (c novembre = Some "Novembre");
      get ctxt ["a";"c"] >>= fun juin ->
      assert (c juin = None);
      get ctxt ["a";"d"] >>= fun mars ->
      assert (c mars = Some "Février");
      Lwt.return ()

let test_replay idx =
  checkout idx genesis_block >>= function
  | None | Some (Error _) ->
      Test.fail "checkout genesis_block"
  | Some (Ok ctxt0) ->
      set ctxt0 ["version"] (MBytes.of_string "0.0") >>= fun ctxt1 ->
      set ctxt1 ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt2 ->
      set ctxt2 ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt3 ->
      set ctxt3 ["a"; "d"] (MBytes.of_string "July") >>= fun ctxt4a ->
      set ctxt3 ["a"; "d"] (MBytes.of_string "Juillet") >>= fun ctxt4b ->
      set ctxt4a ["a"; "b"] (MBytes.of_string "November") >>= fun ctxt5a ->
      get ctxt4a ["a";"b"] >>= fun novembre ->
      assert (c novembre = Some "Novembre");
      get ctxt5a ["a";"b"] >>= fun november ->
      assert (c november = Some "November");
      get ctxt5a ["a";"d"] >>= fun july ->
      assert (c july = Some "July");
      get ctxt4b ["a";"b"] >>= fun novembre ->
      assert (c novembre = Some "Novembre");
      get ctxt4b ["a";"d"] >>= fun juillet ->
      assert (c juillet = Some "Juillet");
      Lwt.return ()

let test_list idx =
  checkout idx genesis_block >>= function
  | None | Some (Error _) ->
      Test.fail "checkout genesis_block"
  | Some (Ok ctxt) ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun ctxt ->
      set ctxt ["f";] (MBytes.of_string "Avril") >>= fun ctxt ->
      set ctxt ["g"; "h"] (MBytes.of_string "Avril") >>= fun ctxt ->
      list ctxt [[]] >>= fun l ->
      assert (l = [["a"];["f"];["g"]]);
      list ctxt [["a"]] >>= fun l ->
      assert (l = [["a";"b"]; ["a";"c"]; ["a";"d"]]);
      list ctxt [["f"]] >>= fun l ->
      assert (l = []);
      list ctxt [["g"]] >>= fun l ->
      assert (l = [["g";"h"]]);
      list ctxt [["i"]] >>= fun l ->
      assert (l = []);
      list ctxt [["a"];["g"]] >>= fun l ->
      assert (l = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["g"; "h"]]);
      Lwt.return ()

let test_invalid idx =
  checkout idx block3c >>= function
  | Some (Error [exn]) ->
      assert (exn = Error_monad.Unclassified "TEST") ;
      Lwt.return_unit
  | Some (Error _) ->
      Test.fail "checkout unexpected error in block3c"
  | Some (Ok _) ->
      Test.fail "checkout valid block3c"
  | None ->
      Test.fail "checkout absent block3c"



(******************************************************************************)

let tests : (string * (index -> unit Lwt.t)) list = [
  "simple", test_simple ;
  "continuation", test_continuation ;
  "fork", test_fork ;
  "replay", test_replay ;
  "list", test_list ;
  "invalid", test_invalid ;
]

let res =
  Test.run "context." (List.map (fun (s, f) -> s, wrap_context_init f) tests)
