(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_embedded_proto_alpha
open Client_alpha
open Tezos_context
open Error_monad
open Hash

let () =
  Random.self_init () ;
  Unix.chdir (Filename.dirname Sys.executable_name)

let cctxt =
  let log channel msg = match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return ()
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return ()
    | _ -> Lwt.return () in
  Client_commands.make_context log

let should_fail f t =
  t >>= function
  | Ok _ -> failwith "Expected error found success."
  | Error error ->
      if not (List.exists f error) then
        failwith "@[<v 2>Unexpected error@ %a@]" pp_print_error error
      else
        return ()

let fork_node () =
  let init_timeout = 4 in
  let data_dir =
    Printf.sprintf
      "%s/tezos_node_%6X"
      (Filename.get_temp_dir_name ())
      (Random.int 0xFF_FF_FF) in
  let log_file_name, log_file = Filename.open_temp_file "tezos_node_" ".log" in
  let log_fd = Unix.descr_of_out_channel log_file in
  let null_fd = Unix.(openfile "/dev/null" [O_RDONLY] 0o644) in
  let pid =
    Unix.create_process
      Filename.(concat (dirname (Sys.getcwd ())) "tezos-node")
      [| "tezos-node" ;
         "run" ;
         "--data-dir"; data_dir ;
         "--sandbox"; "./sandbox.json";
         "--rpc-addr"; "[::]:8732" |]
      null_fd log_fd log_fd in
  Printf.printf "Created node, pid: %d, log: %s\n%!" pid log_file_name ;
  at_exit
    (fun () ->
       Unix.kill pid Sys.sigkill;
       ignore (Sys.command (Printf.sprintf "rm -fr \"%s\"" data_dir))) ;
  Printf.printf "Waiting %d seconds for its initialisation\n%!" init_timeout;
  Unix.sleep init_timeout ;
  match Unix.waitpid [Unix.WNOHANG] pid with
  | 0, _ -> ()
  | pid, Unix.WEXITED x -> Printf.eprintf "Wait: %d, exit %d\n%!" pid x
  | pid, Unix.WSIGNALED x -> Printf.eprintf "Wait: %d, signaled %d\n%!" pid x
  | pid, Unix.WSTOPPED x -> Printf.eprintf "Wait: %d, stopped %d \n%!" pid x

type account = {
  name : string ;
  secret_key : Sodium.secret Sodium.Sign.key ;
  public_key : Sodium.public Sodium.Sign.key ;
  public_key_hash : public_key_hash ;
  contract : Contract.t ;
}

let genesis_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z"

let bootstrap1_pk =
  Environment.Ed25519.Public_key.of_b58check
    "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
let bootstrap2_pk =
  Environment.Ed25519.Public_key.of_b58check
    "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9"
let bootstrap3_pk =
  Environment.Ed25519.Public_key.of_b58check
    "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV"
let bootstrap4_pk =
  Environment.Ed25519.Public_key.of_b58check
    "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"
let bootstrap5_pk =
  Environment.Ed25519.Public_key.of_b58check
    "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n"
let bootstrap1_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi"
let bootstrap2_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDbym9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc"
let bootstrap3_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWBypUSbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC"
let bootstrap4_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyPJdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL"
let bootstrap5_sk =
  Environment.Ed25519.Secret_key.of_b58check
    "edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcCyM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ"

let switch_protocol () =
  let fitness =
    Client_embedded_proto_alpha.Fitness_repr.from_int64 0L in
  Client_genesis.Client_proto_main.mine cctxt `Genesis
    (Activate Client_alpha.Client_proto_main.protocol)
    fitness genesis_sk

let bootstrap_accounts () =
  let cpt = ref 0 in
  Lwt.return
    (List.map
       (fun (public_key, secret_key) ->
          incr cpt ;
          let name = Printf.sprintf "bootstrap%d" !cpt in
          let public_key_hash = Environment.Ed25519.Public_key.hash public_key in
          { name ; contract = Contract.default_contract public_key_hash;
            public_key_hash ; public_key ; secret_key })
       [ bootstrap1_pk, bootstrap1_sk;
         bootstrap2_pk, bootstrap2_sk;
         bootstrap3_pk, bootstrap3_sk;
         bootstrap4_pk, bootstrap4_sk;
         bootstrap5_pk, bootstrap5_sk; ])

let create_account name =
  let secret_key, public_key = Sodium.Sign.random_keypair () in
  let public_key_hash = Environment.Ed25519.Public_key.hash public_key in
  let contract = Contract.default_contract public_key_hash in
  Lwt.return { name ; contract ; public_key_hash ; public_key ; secret_key }

let transfer ?(block = `Prevalidation) ?(fee = 5L) ~src ~target amount =
  let fee =
    let fee = Tez.of_cents fee in
    Assert.is_some ~msg:__LOC__ fee ;
    match fee with
    | Some x -> x
    | None -> assert false in (* will be captured by the previous assert *)
  let amount =
    let amount = Tez.of_cents amount in
    Assert.is_some ~msg:__LOC__ amount ;
    match amount with
    | Some x -> x
    | None -> assert false in (* will be captured by the previous assert *)
  Client_proto_context.transfer cctxt
    block
    ~source:src.contract
    ~src_pk:src.public_key
    ~src_sk:src.secret_key
    ~destination:target.contract
    ~amount ~fee ()

let check_balance ?(block = `Prevalidation) account expected =
  Client_proto_rpcs.Context.Contract.balance cctxt
    block account.contract >>=? fun balance ->
  let balance = Tez.to_cents balance in
  Assert.equal_int64 ~msg:__LOC__ expected balance ;
  return ()

let mine contract =
  let block = `Head 0 in
  Client_proto_rpcs.Context.level cctxt block >>=? fun level ->
  let seed_nonce = Client_mining_forge.generate_seed_nonce () in
  Client_mining_forge.forge_block cctxt
    ~timestamp:(Time.now ()) ~seed_nonce ~src_sk:contract.secret_key
    block contract.public_key_hash >>=? fun block_hash ->
  return ()

let ecoproto_error f = function
  | Register_client_embedded_proto_alpha.Ecoproto_error errors ->
      List.exists f errors
  | _ -> false

let main () =
  fork_node () ;
  switch_protocol () >>=? fun () ->
  bootstrap_accounts () >>= fun bootstrap_accounts ->
  let bootstrap = List.hd bootstrap_accounts in
  create_account "foo" >>= fun foo ->
  create_account "bar" >>= fun bar ->
  transfer ~src:bootstrap ~target:foo 1000_00L >>=? fun contracts ->
  Assert.equal_int ~msg:__LOC__ 0 (List.length contracts) ;
  transfer ~src:bootstrap ~target:bar 2000_00L >>=? fun contracts ->
  Assert.equal_int ~msg:__LOC__ 0 (List.length contracts) ;
  check_balance foo 1000_00L >>=? fun () ->
  transfer ~src:bar ~target:foo 999_95L >>=? fun contracts ->
  Assert.equal_int ~msg:__LOC__ 0 (List.length contracts) ;
  check_balance foo 1999_95L >>=? fun () ->
  check_balance bar 1000_00L >>=? fun () ->
  should_fail
    (ecoproto_error (function Contract.Balance_too_low _ -> true | _ -> false))
  @@ transfer ~src:bar ~target:foo 1000_00L >>=? fun () ->
  mine bootstrap

let tests =
  [ "main", (fun _ -> main () >>= fun _ -> Lwt.return_unit) ]

let () =
  Test.run "basic." tests
