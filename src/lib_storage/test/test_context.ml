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

open Context

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoDemoDemoDemoDemoDemoDemoDemoDemoDemoD3c8k9"

let genesis_time = Time.Protocol.of_seconds 0L

let chain_id = Chain_id.of_block_hash genesis_block

(** Context creation *)

let commit = commit ~time:Time.Protocol.epoch ~message:""

let block2 =
  Block_hash.of_hex_exn
    (`Hex "2222222222222222222222222222222222222222222222222222222222222222")

let create_block2 idx genesis_commit =
  checkout idx genesis_commit >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["version";] (MBytes.of_string "0.0") >>= fun ctxt ->
      commit ctxt

let block3a =
  Block_hash.of_hex_exn
    (`Hex "3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a")

let create_block3a idx block2_commit =
  checkout idx block2_commit >>= function
  | None  ->
      Assert.fail_msg "checkout block2"
  | Some ctxt ->
      del ctxt ["a"; "b"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Mars") >>= fun ctxt ->
      commit ctxt

let block3b =
  Block_hash.of_hex_exn
    (`Hex "3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b")

let block3c =
  Block_hash.of_hex_exn
    (`Hex "3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c")

let create_block3b idx block2_commit =
  checkout idx block2_commit >>= function
  | None ->
      Assert.fail_msg "checkout block3b"
  | Some ctxt ->
      del ctxt ["a"; "c"] >>= fun ctxt ->
      set ctxt ["a"; "d"] (MBytes.of_string "Février") >>= fun ctxt ->
      commit ctxt

type t = {
  idx: Context.index ;
  genesis: Context_hash.t ;
  block2: Context_hash.t ;
  block3a: Context_hash.t ;
  block3b: Context_hash.t ;
}

let wrap_context_init f _ () =
  Lwt_utils_unix.with_tempdir "tezos_test_" begin fun base_dir ->
    let root = base_dir // "context" in
    Context.init ~mapsize:4_096_000L root >>= fun idx ->
    Context.commit_genesis idx
      ~chain_id
      ~time:genesis_time
      ~protocol:genesis_protocol >>= fun genesis ->
    create_block2 idx genesis >>= fun block2 ->
    create_block3a idx block2 >>= fun block3a ->
    create_block3b idx block2  >>= fun block3b ->
    f { idx; genesis; block2 ; block3a; block3b } >>= fun result ->
    Lwt.return result
  end

(** Simple test *)

let c = function
  | None -> None
  | Some s -> Some (MBytes.to_string s)

let test_simple { idx ; block2 ; _ } =
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
      Lwt.return_unit

let test_continuation { idx ; block3a ; _ } =
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
      Lwt.return_unit

let test_fork { idx ; block3b ; _ } =
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
      Lwt.return_unit

let test_replay { idx ; genesis ; _ }  =
  checkout idx genesis >>= function
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
      Lwt.return_unit

let fold_keys s k ~init ~f =
  let rec loop k acc =
    fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init
let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let test_fold { idx ; genesis ; _ } =
  checkout idx genesis >>= function
  | None ->
      Assert.fail_msg "checkout genesis_block"
  | Some ctxt ->
      set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
      set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
      set ctxt ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun ctxt ->
      set ctxt ["f";] (MBytes.of_string "Avril") >>= fun ctxt ->
      set ctxt ["g"; "h"] (MBytes.of_string "Avril") >>= fun ctxt ->
      keys ctxt [] >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__
        [["a";"b"];
         ["a";"c"];
         ["a";"d";"e"];
         ["f"];
         ["g";"h"]] (List.sort compare l) ;
      keys ctxt ["a"] >>= fun l ->
      Assert.equal_string_list_list
        ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d";"e"]]
        (List.sort compare l) ;
      keys ctxt ["f"] >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      keys ctxt ["g"] >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [["g";"h"]] l ;
      keys ctxt ["i"] >>= fun l ->
      Assert.equal_string_list_list ~msg:__LOC__ [] l ;
      Lwt.return_unit

(******************************************************************************)

let tests : (string * (t -> unit Lwt.t)) list = [
  "simple", test_simple ;
  "continuation", test_continuation ;
  "fork", test_fork ;
  "replay", test_replay ;
  "fold", test_fold ;
]


let tests =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case s `Quick (wrap_context_init f))
    tests
