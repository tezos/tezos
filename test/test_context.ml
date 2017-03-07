(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Context

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time =
  Time.of_seconds 0L

let genesis : State.Net.genesis = {
  time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

let net_id = State.Net_id.Id genesis_block

(** Context creation *)

let block2 =
  Block_hash.of_hex_exn
    "2222222222222222222222222222222222222222222222222222222222222222"

let create_block2 idx =
  checkout idx genesis_block >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["version";] (MBytes.of_string "0.0") >>= fun ctxt ->
      commit block2 ctxt

let block3a =
  Block_hash.of_hex_exn
    "3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a"

let create_block3a idx =
  checkout idx block2 >>= function
  | None  ->
      Assert.fail_msg "checkout block2"
  | Some ctxt ->
      del ctxt ["a"; "b"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Mars") >>= fun ctxt ->
      commit block3a ctxt

let block3b =
  Block_hash.of_hex_exn
    "3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b"

let block3c =
  Block_hash.of_hex_exn
    "3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c"

let create_block3b idx =
  checkout idx block2 >>= function
  | None ->
      Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      del ctxt ["a"; "c"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Février") >>= fun ctxt ->
      commit block3b ctxt

let wrap_context_init f base_dir =
  let root = base_dir // "context" in
  Context.init root >>= fun idx ->
  Context.commit_genesis idx
    ~id:genesis.block
    ~time:genesis.time
    ~protocol:genesis.protocol
    ~test_protocol:genesis.protocol >>= fun _ ->
  create_block2 idx >>= fun () ->
  create_block3a idx >>= fun () ->
  create_block3b idx >>= fun () ->
  f idx >>= fun result ->
  Error_monad.return result

(** Simple test *)

let c = function
  | None -> None
  | Some s -> Some (MBytes.to_string s)

let test_simple idx =
  checkout idx block2 >>= function
  | None ->
      Assert.fail_msg "checkout block2"
  | Some ctxt ->
      get ctxt ["version"] >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
      get ctxt ["a";"b"] >>= fun novembre ->
      Assert.equal_string_option (Some "Novembre") (c novembre) ;
      get ctxt ["a";"c"] >>= fun juin ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      Lwt.return ()

let test_continuation idx =
  checkout idx block3a >>= function
  | None  ->
      Assert.fail_msg "checkout block3a"
  | Some ctxt ->
      get ctxt ["version"] >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      get ctxt ["a";"b"] >>= fun novembre ->
      Assert.is_none ~msg:__LOC__ (c novembre) ;
      get ctxt ["a";"c"] >>= fun juin ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
      get ctxt ["a";"d"] >>= fun mars ->
      Assert.equal_string_option ~msg:__LOC__  (Some "Mars") (c mars) ;
      Lwt.return ()

let test_fork idx =
  checkout idx block3b >>= function
  | None  ->
      Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      get ctxt ["version"] >>= fun version ->
      Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
      get ctxt ["a";"b"] >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt ["a";"c"] >>= fun juin ->
      Assert.is_none ~msg:__LOC__ (c juin) ;
      get ctxt ["a";"d"] >>= fun mars ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
      Lwt.return ()

let test_replay idx =
  checkout idx genesis_block >>= function
  | None  ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt0 ->
      set ctxt0 ["version"] (MBytes.of_string "0.0") >>= fun ctxt1 ->
      set ctxt1 ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt2 ->
      set ctxt2 ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt3 ->
      set ctxt3 ["a"; "d"] (MBytes.of_string "July") >>= fun ctxt4a ->
      set ctxt3 ["a"; "d"] (MBytes.of_string "Juillet") >>= fun ctxt4b ->
      set ctxt4a ["a"; "b"] (MBytes.of_string "November") >>= fun ctxt5a ->
      get ctxt4a ["a";"b"] >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt5a ["a";"b"] >>= fun november ->
      Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
      get ctxt5a ["a";"d"] >>= fun july ->
      Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
      get ctxt4b ["a";"b"] >>= fun novembre ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
      get ctxt4b ["a";"d"] >>= fun juillet ->
      Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
      Lwt.return ()

let test_list idx =
  checkout idx genesis_block >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun ctxt ->
      set ctxt ["f";] (MBytes.of_string "Avril") >>= fun ctxt ->
      set ctxt ["g"; "h"] (MBytes.of_string "Avril") >>= fun ctxt ->
      list ctxt [[]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [["a"];["f"];["g"]] l ;
      list ctxt [["a"]] >>= fun l ->
      Assert.equal_persist_list
        ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d"]] l ;
      list ctxt [["f"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [] l ;
      list ctxt [["g"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [["g";"h"]] l ;
      list ctxt [["i"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__ [] l ;
      list ctxt [["a"];["g"]] >>= fun l ->
      Assert.equal_persist_list ~msg:__LOC__
        [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["g"; "h"]] l ;
      Lwt.return ()


(******************************************************************************)

let tests : (string * (index -> unit Lwt.t)) list = [
  "simple", test_simple ;
  "continuation", test_continuation ;
  "fork", test_fork ;
  "replay", test_replay ;
  "list", test_list ;
]

let () =
  Test.run "context." (List.map (fun (s, f) -> s, wrap_context_init f) tests)
