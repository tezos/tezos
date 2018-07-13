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

let make_simple blocks =
  let rec loop pred n =
    if n <= 0 then
      return pred
    else
      Block.bake pred >>=? fun block ->
      loop block (n - 1) in
  Context.init 5 >>=? fun (genesis, _) ->
  loop genesis blocks

type args = {
  blocks : int ;
  accounts : int ;
}

let default_args = {
  blocks = 1000 ;
  accounts = 5 ;
}

let set_blocks cf blocks =
  cf := { !cf with blocks }

let set_accounts cf accounts =
  cf := { !cf with accounts }

let read_args () =
  let args = ref default_args in
  let specific =
    [
      ("--blocks", Arg.Int (set_blocks args), "number of blocks");
      ("--accounts", Arg.Int (set_accounts args), "number of acount");
    ]
  in
  let usage = "Usage: [--blocks n] [--accounts n] " in
  Arg.parse specific (fun _ -> ()) usage ;
  !args

let () =
  let args = read_args () in
  match Lwt_main.run (make_simple args.blocks) with
  | Ok _head ->
      Format.printf "Success.@." ;
      exit 0
  | Error err ->
      Format.eprintf "%a@." pp_print_error err ;
      exit 1
