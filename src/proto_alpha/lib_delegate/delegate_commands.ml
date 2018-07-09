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

open Client_proto_args
open Client_baking_lib

let group =
  { Clic.name = "delegate" ;
    title = "Commands related to delegate operations." }

let directory_parameter =
  Clic.parameter (fun _ p ->
      if not (Sys.file_exists p && Sys.is_directory p) then
        failwith "Directory doesn't exist: '%s'" p
      else
        return p)

let delegate_commands () =
  let open Clic in
  [
    command ~group ~desc: "Forge and inject block using the delegate rights."
      (args4 max_priority_arg fee_threshold_arg force_switch minimal_timestamp_switch)
      (prefixes [ "bake"; "for" ]
       @@ Client_keys.Public_key_hash.source_param
         ~name:"baker" ~desc: "name of the delegate owning the baking right"
       @@ stop)
      (fun (max_priority, threshold, force, minimal_timestamp) delegate cctxt ->
         bake_block cctxt cctxt#block
           ?threshold ~force ?max_priority ~minimal_timestamp delegate) ;
    command ~group ~desc: "Forge and inject a seed-nonce revelation operation."
      no_options
      (prefixes [ "reveal"; "nonce"; "for" ]
       @@ seq_of_param Block_hash.param)
      (fun () block_hashes cctxt ->
         reveal_block_nonces cctxt block_hashes) ;
    command ~group ~desc: "Forge and inject all the possible seed-nonce revelation operations."
      no_options
      (prefixes [ "reveal"; "nonces" ]
       @@ stop)
      (fun () cctxt ->
         reveal_nonces cctxt ()) ;
    command ~group ~desc: "Forge and inject an endorsement operation."
      no_options
      (prefixes [ "endorse"; "for" ]
       @@ Client_keys.Public_key_hash.source_param
         ~name:"baker" ~desc: "name of the delegate owning the endorsement right"
       @@ stop)
      (fun () delegate cctxt -> endorse_block cctxt delegate) ;
  ]

let baker_commands () =
  let open Clic in
  let group =
    { Clic.name = "delegate.baker" ;
      title = "Commands related to the baker daemon." }
  in
  [
    command ~group ~desc: "Launch the baker daemon."
      (args2 max_priority_arg fee_threshold_arg)
      (prefixes [ "run" ; "with" ; "local" ; "node" ]
       @@ param
         ~name:"context_path"
         ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
         directory_parameter
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun (max_priority, threshold) node_path delegates cctxt ->
         Tezos_signer_backends.Encrypted.decrypt_list
           cctxt (List.map fst delegates) >>=? fun () ->
         Client_daemon.Baker.run cctxt
           ?threshold
           ?max_priority
           ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
           ~context_path:(Filename.concat node_path "context")
           (List.map snd delegates)
      )
  ]

let endorser_commands () =
  let open Clic in
  let group =
    { Clic.name = "delegate.endorser" ;
      title = "Commands related to endorser daemon." }
  in
  [
    command ~group ~desc: "Launch the endorser daemon"
      (args1 endorsement_delay_arg)
      (prefixes [ "run" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun endorsement_delay delegates cctxt ->
         Tezos_signer_backends.Encrypted.decrypt_list
           cctxt (List.map fst delegates) >>=? fun () ->
         Client_daemon.Endorser.run cctxt
           ~delay:endorsement_delay
           ~min_date:((Time.add (Time.now ()) (Int64.neg 1800L)))
           (List.map snd delegates)
      )
  ]

let accuser_commands () =
  let open Clic in
  let group =
    { Clic.name = "delegate.accuser" ;
      title = "Commands related to the accuser daemon." }
  in
  [
    command ~group ~desc: "Launch the accuser daemon"
      (args1 preserved_levels_arg)
      (prefixes [ "run" ]
       @@ stop)
      (fun preserved_levels cctxt ->
         Client_daemon.Accuser.run ~preserved_levels cctxt) ;
  ]
