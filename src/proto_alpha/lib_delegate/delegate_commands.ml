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

let mempool_arg =
  Clic.arg
    ~long:"mempool"
    ~placeholder:"file"
    ~doc:"When used the client will read the mempool in the provided file instead of querying the node through an RPC (useful for debugging only)."
    string_parameter

let context_path_arg =
  Clic.arg
    ~long:"context"
    ~placeholder:"path"
    ~doc:"When use the client will read in the local context at the provided path in order to build the block, instead of relying on the 'preapply' RPC."
    string_parameter

let pidfile_arg =
  Clic.arg
    ~doc: "write process id in file"
    ~short: 'P'
    ~long: "pidfile"
    ~placeholder: "filename"
    (Clic.parameter (fun _ s -> return s))

let may_lock_pidfile = function
  | None -> return_unit
  | Some pidfile ->
      trace (failure "Failed to create the pidfile: %s" pidfile) @@
      Lwt_lock_file.create ~unlink_on_exit:true pidfile

let delegate_commands () =
  let open Clic in
  [
    command ~group ~desc: "Forge and inject block using the delegate rights."
      (args9
         max_priority_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         await_endorsements_arg
         force_switch
         minimal_timestamp_switch
         mempool_arg
         context_path_arg)
      (prefixes [ "bake"; "for" ]
       @@ Client_keys.Public_key_hash.source_param
         ~name:"baker" ~desc: "name of the delegate owning the baking right"
       @@ stop)
      (fun (max_priority, minimal_fees,
            minimal_nanotez_per_gas_unit, minimal_nanotez_per_byte,
            await_endorsements, force,
            minimal_timestamp, mempool, context_path)
        delegate cctxt ->
        bake_block cctxt cctxt#block
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ~await_endorsements
          ~force ?max_priority ~minimal_timestamp
          ?mempool ?context_path delegate) ;
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

let init_signal () =
  let handler name id =
    try
      Format.eprintf "Received the %s signal, triggering shutdown.@." name ;
      Lwt_exit.exit id
    with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint (handler "INT") : Lwt_unix.signal_handler_id) ;
  ignore (Lwt_unix.on_signal Sys.sigterm (handler "TERM") : Lwt_unix.signal_handler_id)

let baker_commands () =
  let open Clic in
  let group =
    { Clic.name = "delegate.baker" ;
      title = "Commands related to the baker daemon." }
  in
  [
    command ~group ~desc: "Launch the baker daemon."
      (args6
         pidfile_arg
         max_priority_arg
         minimal_fees_arg
         minimal_nanotez_per_gas_unit_arg
         minimal_nanotez_per_byte_arg
         no_waiting_for_endorsements_arg)
      (prefixes [ "run" ; "with" ; "local" ; "node" ]
       @@ param
         ~name:"context_path"
         ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
         directory_parameter
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun (pidfile, max_priority, minimal_fees, minimal_nanotez_per_gas_unit,
            minimal_nanotez_per_byte, no_waiting_for_endorsements)
        node_path delegates cctxt ->
        init_signal () ;
        may_lock_pidfile pidfile >>=? fun () ->
        Tezos_signer_backends.Encrypted.decrypt_list
          cctxt (List.map fst delegates) >>=? fun () ->
        Client_daemon.Baker.run cctxt
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          ?max_priority
          ~await_endorsements:(not no_waiting_for_endorsements)
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
      (args2 pidfile_arg endorsement_delay_arg)
      (prefixes [ "run" ]
       @@ seq_of_param Client_keys.Public_key_hash.alias_param)
      (fun (pidfile, endorsement_delay) delegates cctxt ->
         init_signal () ;
         may_lock_pidfile pidfile >>=? fun () ->
         Tezos_signer_backends.Encrypted.decrypt_list
           cctxt (List.map fst delegates) >>=? fun () ->
         Client_daemon.Endorser.run cctxt
           ~delay:endorsement_delay
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
      (args2 pidfile_arg preserved_levels_arg)
      (prefixes [ "run" ]
       @@ stop)
      (fun (pidfile, preserved_levels) cctxt ->
         init_signal () ;
         may_lock_pidfile pidfile >>=? fun () ->
         Client_daemon.Accuser.run ~preserved_levels cctxt) ;
  ]
