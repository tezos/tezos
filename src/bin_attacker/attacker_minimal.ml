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

open Format
include Logging.Make(struct let name = "attacker" end)

module Proto = Client_embedded_proto_alpha

(* the genesis block and network *)
let genesis_block_hashed = Block_hash.of_b58check
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"
let network = Store.Net genesis_block_hashed
let network = Store.Chain_id.Id genesis_block_hashed

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
  let block ops = Store.Block_header.{ chain_id = network ;
                                       predecessor = pred ;
                                       timestamp = Systime_os.now () ;
                                       fitness = from_int64 1L;
                                       operations = ops } in
  let open Proto in
  let generate_proof_of_work_nonce () =
    Rand.generate
      Proto.Alpha_context.Constants.proof_of_work_nonce_size in
  let generate_seed_nonce () =
    match Proto.Nonce_storage.of_bytes @@
      Rand.generate
        Proto.Alpha_context.Constants.nonce_length with
    | Error _ -> assert false
    | Ok nonce -> nonce in
  Block_repr.forge_header (block ops)
    Block_repr.{
      baking_slot = {level = Raw_level_repr.of_int32_exn 1l ; priority = 0l } ;
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
  forge { chain_id = network } op

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
  forge { chain_id = network } op

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
  forge { chain_id = network } op

let identity = P2p_identity.generate Crypto_box.default_target

(* connect to the network, run an action and then disconnect *)
let try_action addr port action =
  let socket = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  let uaddr = Ipaddr_unix.V6.to_inet_addr addr in
  Lwt_unix.connect socket (Lwt_unix.ADDR_INET (uaddr, port)) >>= fun () ->
  let io_sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 14) () in
  let conn = P2p_io_scheduler.register io_sched socket in
  P2p_connection.authenticate
    ~proof_of_work_target:Crypto_box.default_target
    ~incoming:false
    conn
    (addr, port)
    identity Distributed_db.Raw.supported_versions >>=? fun (_, auth_fd) ->
  P2p_connection.accept auth_fd Distributed_db.Raw.encoding >>= function
  | Error _ -> failwith "Connection rejected by peer."
  | Ok conn ->
      action conn >>=? fun () ->
      P2p_connection.close conn >>= fun () ->
      return_unit

let replicate n x =
  let rec replicate_acc acc n x =
    if n <= 0 then acc else replicate_acc (x :: acc) (n-1) x in
  replicate_acc [] n x

let send conn (msg : Distributed_db.Message.t) =
  P2p_connection.write conn (P2p.Raw.Message msg)

let request_block_times block_hash n conn =
  let open Block_hash in
  lwt_log_notice
    "requesting %a block %d times"
    pp_short block_hash n >>= fun () ->
  let block_hashes = replicate n block_hash in
  send conn (Get_block_headers (network, block_hashes))

let request_op_times op_signed n conn =
  let open Operation_hash in
  let op_hash = hash_bytes [op_signed] in
  lwt_log_notice "sending %a transaction" pp_short op_hash >>= fun () ->
  send conn (Operation op_signed) >>=? fun () ->
  lwt_log_notice
    "requesting %a transaction %d times"
    pp_short op_hash n  >>= fun () ->
  let op_hashes = replicate n op_hash in
  send conn (Get_operations op_hashes)

let send_block_size n conn =
  let bytes = MBytes.create n in
  let open Block_hash in
  lwt_log_notice
    "propagating fake %d byte block %a" n pp_short (hash_bytes [bytes]) >>= fun () ->
  send conn (Block bytes)

let send_protocol_size n conn =
  let bytes = MBytes.create n in
  let open Protocol_hash in
  lwt_log_notice
    "propagating fake %d byte protocol %a"
    n pp_short (hash_bytes [bytes]) >>= fun () ->
  send conn (Protocol bytes)

let send_operation_size n conn =
  let op_faked = MBytes.create n in
  let op_hashed = Operation_hash.hash_bytes [op_faked] in
  lwt_log_notice
    "propagating fake %d byte operation %a"
    n Operation_hash.pp_short op_hashed >>= fun () ->
  send conn (Operation op_faked) >>=? fun () ->
  let block = signed (block_forged [op_hashed]) in
  let block_hashed = Block_hash.hash_bytes [block] in
  lwt_log_notice
    "propagating block %a with operation"
    Block_hash.pp_short block_hashed >>= fun () ->
  send conn (Block block)

let send_operation_bad_signature () conn =
  let open Operation_hash in
  let signed_wrong_op = signed_wrong (tx_forged 5L 1L) in
  let hashed_wrong_op = hash_bytes [signed_wrong_op] in
  lwt_log_notice
    "propagating operation %a with wrong signature"
    pp_short hashed_wrong_op >>= fun () ->
  send conn (Operation signed_wrong_op) >>=? fun () ->
  let block = signed (block_forged [hashed_wrong_op]) in
  let block_hashed = Block_hash.hash_bytes [block] in
  lwt_log_notice
    "propagating block %a with operation"
    Block_hash.pp_short block_hashed >>= fun () ->
  send conn (Block block)

let send_block_bad_signature () conn =
  let open Block_hash in
  let signed_wrong_block = signed_wrong (block_forged []) in
  lwt_log_notice
    "propagating block %a with wrong signature"
    pp_short (hash_bytes [signed_wrong_block]) >>= fun () ->
  send conn (Block signed_wrong_block)

let double_spend () conn =
  let spend account =
    let op_signed = signed (tx_forged ~dest:account 199999999L 1L) in
    let op_hashed = Operation_hash.hash_bytes [op_signed] in
    let block_signed = signed (block_forged [op_hashed]) in
    let block_hashed = Block_hash.hash_bytes [block_signed] in
    lwt_log_notice
      "propagating operation %a"
      Operation_hash.pp_short op_hashed >>= fun () ->
    send conn (Operation op_signed) >>=? fun () ->
    lwt_log_notice
      "propagating block %a"
      Block_hash.pp_short block_hashed >>= fun () ->
    send conn (Block block_signed) in
  spend destination_account >>=? fun () ->
  spend another_account

let long_chain n conn =
  lwt_log_notice "propogating %d blocks" n >>= fun () ->
  let prev_ref = ref genesis_block_hashed in
  let rec loop k =
    if k < 1 then
      return_unit
    else
      let block = signed (block_forged ~prev:!prev_ref []) in
      prev_ref := Block_hash.hash_bytes [block] ;
      send conn (Block block) >>=? fun () ->
      loop (k-1) in
  loop n

let lots_transactions amount fee n conn =
  let signed_op = signed (tx_forged amount fee) in
  let rec loop k =
    if k < 1 then
      return_unit
    else
      send conn (Operation signed_op) >>=? fun () ->
      loop (k-1) in
  let ops = replicate n (Operation_hash.hash_bytes [signed_op]) in
  let signed_block = signed (block_forged ops) in
  lwt_log_notice "propogating %d transactions" n >>= fun () ->
  loop n >>=? fun () ->
  lwt_log_notice
    "propagating block %a with wrong signature"
    Block_hash.pp_short (Block_hash.hash_bytes [signed_block]) >>= fun () ->
  send conn (Block signed_block)

let main () =
  let addr = Ipaddr.V6.localhost in
  let port = 9732 in
  let run_action action = try_action addr port action in
  let run_cmd_unit lwt =
    Arg.Unit begin fun () ->
      Lwt_main.run begin
        lwt () >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
            lwt_log_error "Error: %a" pp_print_error err >>= fun () ->
            Lwt.return_unit
      end
    end in
  let run_cmd_int_suffix lwt =
    Arg.String begin fun str ->
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
      Lwt_main.run begin
        lwt n >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
            lwt_log_error "Error: %a" pp_print_error err >>= fun () ->
            Lwt.return_unit
      end
    end in
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
