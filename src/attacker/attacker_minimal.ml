(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Format
open Lwt
open Tezos_p2p

module Proto = Client_embedded_proto_bootstrap
module Ed25519 = Proto.Local_environment.Environment.Ed25519

(* the genesis block and network *)
let genesis_block_hashed = Block_hash.of_b48check
    "grHGHkVfgJb5gPaRd5AtQsa65g9GyLcXgQsHbSnQ5SD5DEp2ctqck"
let network = Store.Net genesis_block_hashed

(* the bootstrap accounts and actions like signing to do with them *)
let source_account = List.nth Proto.Bootstrap_storage.accounts 4
let destination_account = List.nth Proto.Bootstrap_storage.accounts 0
let wrong_account = List.nth Proto.Bootstrap_storage.accounts 1
let another_account = List.nth Proto.Bootstrap_storage.accounts 2
let signed = Ed25519.append_signature source_account.secret_key
let signed_wrong = Ed25519.append_signature wrong_account.secret_key

(* forge a block from a list of operations *)
let block_forged ?prev ops =
  let from_int64 x =
    [ MBytes.of_string Proto.Constants_repr.version_number ;
      Proto.Fitness_repr.int64_to_bytes x ] in
  let pred = match prev with None -> genesis_block_hashed | Some x -> x in
  let block ops = Store.{ net_id = network ;
                      predecessor = pred ;
                      timestamp = Time.now () ;
                      fitness = from_int64 1L;
                      operations = ops } in
  let open Proto in
  let generate_proof_of_work_nonce () =
    Sodium.Random.Bigbytes.generate
      Proto.Tezos_context.Constants.proof_of_work_nonce_size in
  let generate_seed_nonce () =
    match Proto.Nonce_storage.of_bytes @@
      Sodium.Random.Bigbytes.generate
        Proto.Tezos_context.Constants.nonce_length with
    | Error _ -> assert false
    | Ok nonce -> nonce in
  Block_repr.forge_header (block ops)
    Block_repr.{
      mining_slot = Raw_level_repr.of_int32_exn 1l, 0l ;
      seed_nonce_hash = Proto.Nonce_storage.hash (generate_seed_nonce ());
      proof_of_work_nonce = generate_proof_of_work_nonce () ;
    }

(* forge a transaction *)
let tx_forged ?dest amount fee =
  let open Proto.Operation_repr in
  let open Proto.Tez_repr in
  let open Proto.Contract_repr in
  let trgt
    = match dest with
      None -> destination_account
    | Some dest -> dest in
  let src = source_account in
  let tx = Transaction
      { amount = of_cents_exn amount ;
        parameters = None ;
        destination = default_contract trgt.public_key_hash ; } in
  let op = Sourced_operations
    ( Manager_operations
        { source = default_contract src.public_key_hash ;
          public_key = Some src.public_key ;
          fee = of_cents_exn fee ;
          counter = 1l ;
          operations = [tx] ; }) in
  forge { net_id = network } op

(* forge a list of proposals, california eat your heart out *)
let props_forged period props =
  let open Proto.Operation_repr in
  let src = source_account in
  let props = Proposals {
      period = period ;
      proposals = props } in
  let op = Sourced_operations (Delegate_operations {
      source = src.public_key ;
      operations = [props] }) in
  forge { net_id = network } op

(* "forge" a ballot *)
let ballot_forged period prop vote =
  let open Proto.Operation_repr in
  let src = source_account in
  let ballot = Ballot {
      period = period ;
      proposal = prop ;
      ballot = vote
      } in
  let op = Sourced_operations (Delegate_operations {
      source = src.public_key ;
      operations = [ballot] }) in
  forge { net_id = network } op

(* connect to the network, run an action and then disconnect *)
let try_action addr port action =
  let limits : P2p.limits = {
      max_message_size = 1 lsl 16 ;
      peer_answer_timeout = 10. ;
      expected_connections = 1;
      min_connections = 1 ;
      max_connections = 1 ;
      blacklist_time = 0. ;
    } in
  let config : P2p.config = {
      incoming_port = None ;
      discovery_port = None ;
      known_peers = [(addr, port)] ;
      peers_file = Filename.temp_file "peers_file" ".txt";
      closed_network = true ;
    } in
  bootstrap ~config ~limits >>= fun net ->
  let peer =
    match peers net with
    | [peer] -> peer
    | _ -> Pervasives.failwith "" in
  action net peer >>= fun () -> shutdown net

let replicate n x =
  let rec replicate_acc acc n x =
    if n <= 0 then acc else replicate_acc (x :: acc) (n-1) x in
  replicate_acc [] n x

let request_block_times block_hash n net peer =
  let open Block_hash in
  let () = printf "requesting %a block %a times\n"
    pp_short block_hash pp_print_int n in
  let block_hashes = replicate n block_hash in
  send net peer (Get_blocks block_hashes)

let request_op_times op_signed n net peer =
  let open Operation_hash in
  let op_hash = hash_bytes [op_signed] in
  let () = printf "sending %a transaction\n" pp_short op_hash in
  send net peer (Operation op_signed) >>= fun () ->
  let () = printf "requesting %a transaction %a times\n"
    pp_short op_hash pp_print_int n in
  let op_hashes = replicate n op_hash in
  send net peer (Get_operations op_hashes)

let send_block_size n net peer =
  let bytes = MBytes.create n in
  let open Block_hash in
  let () = printf "propagating fake %a byte block %a\n"
    pp_print_int n pp_short (hash_bytes [bytes]) in
  send net peer (Block bytes)

let send_protocol_size n net peer =
  let bytes = MBytes.create n in
  let open Protocol_hash in
  let () = printf "propagating fake %a byte protocol %a\n"
    pp_print_int n pp_short (hash_bytes [bytes]) in
  send net peer (Protocol bytes)

let send_operation_size n net peer =
  let op_faked = MBytes.create n in
  let op_hashed = Operation_hash.hash_bytes [op_faked] in
  let () = printf "propagating fake %a byte operation %a\n"
      pp_print_int n Operation_hash.pp_short op_hashed in
  send net peer (Operation op_faked) >>= fun () ->
  let block = signed (block_forged [op_hashed]) in
  let block_hashed = Block_hash.hash_bytes [block] in
  let () = printf "propagating block %a with operation\n"
      Block_hash.pp_short block_hashed in
  send net peer (Block block)

let send_operation_bad_signature () net peer =
  let open Operation_hash in
  let signed_wrong_op = signed_wrong (tx_forged 5L 1L) in
  let hashed_wrong_op = hash_bytes [signed_wrong_op] in
  let () = printf "propagating operation %a with wrong signature\n"
    pp_short hashed_wrong_op in
  send net peer (Operation signed_wrong_op) >>= fun () ->
  let block = signed (block_forged [hashed_wrong_op]) in
  let block_hashed = Block_hash.hash_bytes [block] in
  let () = printf "propagating block %a with operation\n"
      Block_hash.pp_short block_hashed in
  send net peer (Block block)

let send_block_bad_signature () net peer =
  let open Block_hash in
  let signed_wrong_block = signed_wrong (block_forged []) in
  let () = printf "propagating block %a with wrong signature\n"
      pp_short (hash_bytes [signed_wrong_block]) in
  send net peer (Block signed_wrong_block)

let double_spend () net peer =
  let spend account =
    let op_signed = signed (tx_forged ~dest:account 199999999L 1L) in
    let op_hashed = Operation_hash.hash_bytes [op_signed] in
    let block_signed = signed (block_forged [op_hashed]) in
    let block_hashed = Block_hash.hash_bytes [block_signed] in
    let () = printf "propagating operation %a\n"
        Operation_hash.pp_short op_hashed in
    send net peer (Operation op_signed) >>= fun () ->
    let () = printf "propagating block %a\n"
        Block_hash.pp_short block_hashed in
    send net peer (Block block_signed) in
  spend destination_account <&> spend another_account

let long_chain n net peer =
  let () = printf "propogating %a blocks\n"
    pp_print_int n in
  let prev_ref = ref genesis_block_hashed in
  let rec loop k = if k < 1 then return_unit else
      let block = signed (block_forged ~prev:!prev_ref []) in
      let () = prev_ref := Block_hash.hash_bytes [block] in
      send net peer (Block block) >>= fun () -> loop (k-1) in
  loop n

let lots_transactions amount fee n net peer =
  let signed_op = signed (tx_forged amount fee) in
  let rec loop k = if k < 1 then return_unit else
      send net peer (Operation signed_op) >>= fun () -> loop (k-1) in
  let ops = replicate n (Operation_hash.hash_bytes [signed_op]) in
  let signed_block = signed (block_forged ops) in
  let () = printf "propogating %a transactions\n"
      pp_print_int n in
  loop n >>= fun () ->
  let () = printf "propagating block %a with wrong signature\n"
    Block_hash.pp_short (Block_hash.hash_bytes [signed_block]) in
  send net peer (Block signed_block)

let main () =
  let addr = Ipaddr.V4 Ipaddr.V4.localhost in
  let port = 9732 in
  let run_action action = try_action addr port action in
  let run_cmd_unit lwt = Arg.Unit (fun () -> Lwt_main.run (lwt ())) in
  let run_cmd_int_suffix lwt = Arg.String (fun str ->
      let last = str.[String.length str - 1] in
      let init = String.sub str 0 (String.length str - 1) in
      let n =
        if last == 'k' || last == 'K'
        then int_of_string init * 1 lsl 10
        else if last == 'm' || last == 'M'
        then int_of_string init * 1 lsl 20
        else if last == 'g' || last == 'G'
        then int_of_string init * 1 lsl 30
        else int_of_string str in
      Lwt_main.run (lwt n)) in
  let cmds =
    [( "-1",
       run_cmd_int_suffix (run_action << request_block_times genesis_block_hashed),
       "[N {,K,M,G}] Attempt to request to download N {,kilo,mega,giga}blocks.")
    ;( "-2",
       run_cmd_int_suffix (run_action << request_op_times (signed (tx_forged 5L 1L))),
       "[N {,K,M,G}] Attempt to request to download N {,kilo,mega,giga}ops.")
    ;( "-3",
       run_cmd_int_suffix (run_action << send_block_size),
       "[N {,K,M,G}] Attempt to propagate an N {,kilo,mega,giga}byte fake block.")
    ;( "-4",
       run_cmd_int_suffix (run_action << send_operation_size),
       "[N {,K,M,G}] Attempt to propagate an N {,kilo,mega,giga}byte fake operation.")
    ;( "-5",
       run_cmd_int_suffix (run_action << send_protocol_size),
       "[N {,K,M,G}] Attempt to propagate an N {,kilo,mega,giga}byte fake protocol.")
    ;( "-6",
       run_cmd_unit (run_action << send_operation_bad_signature),
       "Attempt to propagate a transaction with a bad signature.")
    ;( "-7",
       run_cmd_unit (run_action << send_block_bad_signature),
       "Attempt to propagate a block with a bad signature.")
    ;( "-8",
       run_cmd_unit (run_action << double_spend),
       "Attempt to send the same transaction in two blocks")
    ; ( "-9",
        run_cmd_int_suffix (run_action << long_chain),
        "[N {,K,M,G}] Attempt to send a chain of N {,kilo,mega,giga}blocks")
    ; ( "-10",
        run_cmd_int_suffix (run_action << lots_transactions 0L 0L),
        "[N {,K,M,G}] Attempt to send N {,kilo,mega,giga}ops")
    ] in
  Arg.parse cmds print_endline "Tezos Evil Client"
