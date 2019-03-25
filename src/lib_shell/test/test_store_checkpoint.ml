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

let mapsize = 4_096_000_000L (* ~4 GiB *)

let (//) = Filename.concat

let wrap_raw_store_init f _ () =
  Lwt_utils_unix.with_tempdir "tezos_test_" begin fun base_dir ->
    let root = base_dir // "store" in
    Raw_store.init ~mapsize root >>= function
    | Ok store ->
        Lwt.finalize
          (fun () -> f store)
          (fun () -> Raw_store.close store ; Lwt.return_unit)
    | Error err ->
        Format.kasprintf Pervasives.failwith
          "@[Cannot initialize store:@ %a@]" pp_print_error err
  end

(**************************************************************************)
(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

(**************************************************************************)
(** Block store *)

let lolblock ?(operations = []) header =
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute operations] in
  ( { Block_header.shell =
        { timestamp = Time.Protocol.of_seconds (Random.int64 1500L) ;
          level = 0l ; (* dummy *)
          proto_level = 0 ; (* dummy *)
          validation_passes = Random.int 32 ;
          predecessor = genesis_block ; operations_hash ;
          fitness = [MBytes.of_string @@ string_of_int @@ String.length header;
                     MBytes.of_string @@ string_of_int @@ 12] ;
          context = Context_hash.zero } ;
      protocol_data = MBytes.of_string header ; } ,
    { Store.Block.metadata = MBytes.create 0 ;
      max_operations_ttl = 0 ;
      message = None ;
      context = Context_hash.zero ;
      last_allowed_fork_level = 0l ;
    } )


let (block_header,_) = lolblock "A1"
let block_hash = Block_header.hash block_header

(****************************************************)

open Store_helpers

let test_single (type t)
    (module Store:Store_sigs.STORE with type t = t) (s: Store.t) =
  let module Single =
    Make_single_store
      (Store)
      (struct let name = ["checkpoint"] end)
      (Store_helpers.Make_value (struct
         type t = Int32.t * Block_hash.t
         let encoding = Data_encoding.(tup2 int32 Block_hash.encoding)
       end
       ))
  in
  (* is there any checkpoint in store *)
  Single.known s >>= fun is_known ->
  Assert.is_false ~msg:__LOC__ is_known;
  Single.read_opt s >>= fun checkpoint' ->
  Assert.equal_checkpoint ~msg:__LOC__ None checkpoint';
  (* store new checkpoint: (1, A1) *)
  let checkpoint =  (1l, block_hash) in
  Single.store s checkpoint >>= fun () ->
  Single.known s >>= fun is_known ->
  Assert.is_true ~msg:__LOC__ is_known;
  Single.read_opt s >>= fun checkpoint' ->
  Assert.equal_checkpoint ~msg:__LOC__ (Some checkpoint) checkpoint';
  (* remove the checkpoint just store *)
  Single.remove s >>= fun () ->
  Single.known s >>= fun is_known ->
  Assert.is_false ~msg:__LOC__ is_known;
  Single.read_opt s >>= fun checkpoint' ->
  Assert.equal_checkpoint ~msg:__LOC__ None checkpoint';
  Lwt.return_unit

(**************************************************************************)

let tests_raw : (string * (Raw_store.t -> unit Lwt.t)) list =
  [
    "single", test_single (module Raw_store)

  ]

let tests =
  List.map (fun (s, f) -> Alcotest_lwt.test_case s `Quick (wrap_raw_store_init f))
    tests_raw
