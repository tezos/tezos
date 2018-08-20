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

(** The activation operation creates an implicit contract from a
    registered commitment present in the context. It is parametrized by
    a public key hash (pkh) and a secret.

    The commitments are composed of :
    - a blinded pkh that can be revealed by the secret ;
    - an amount.

    The commitments and the secrets are generated from
    /scripts/create_genesis/create_genenis.py and should be coherent.
*)

open Proto_alpha
open Alpha_context
open Test_utils
open Test_tez

(* Generated commitments and secrets  *)

let commitments =
  List.map (fun (bpkh, a) ->
      Commitment_repr.{
        blinded_public_key_hash=Blinded_public_key_hash.of_b58check_exn bpkh ;
        amount = Tez_repr.of_mutez_exn (Int64.of_string a)}
    )
    [ ( "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ) ;
      ( "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ) ;
      ( "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428349" ) ;
      ( "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ) ;
      ( "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ) ;
      ( "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ) ;
      ( "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ) ;
      ( "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ) ;
      ( "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ) ;
      ( "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ) ;
    ]

type secret_account = {
  account : public_key_hash ;
  activation_code : Blinded_public_key_hash.activation_code ;
  amount : Tez.t ;
}

let secrets () =
  (* Exported from proto_alpha client - TODO : remove when relocated to lib_crypto *)
  let read_key mnemonic email password =
    match Bip39.of_words mnemonic with
    | None -> assert false
    | Some t ->
        (* TODO: unicode normalization (NFKD)... *)
        let passphrase = MBytes.(concat "" [
            of_string email ;
            of_string password ;
          ]) in
        let sk = Bip39.to_seed ~passphrase t in
        let sk = MBytes.sub sk 0 32 in
        let sk : Signature.Secret_key.t =
          Ed25519 (Data_encoding.Binary.of_bytes_exn Ed25519.Secret_key.encoding sk) in
        let pk = Signature.Secret_key.to_public_key sk in
        let pkh = Signature.Public_key.hash pk in
        (pkh, pk, sk)
  in
  List.map (fun (mnemonic, secret, amount, pkh, password, email) ->
      let (pkh', pk, sk) = read_key mnemonic email password in
      let pkh = Signature.Public_key_hash.of_b58check_exn pkh in
      assert (Signature.Public_key_hash.equal pkh pkh');
      let account = Account.{ pkh ; pk ; sk } in
      Account.add_account account ;
      { account = account.pkh ;
        activation_code = Blinded_public_key_hash.activation_code_of_hex secret ;
        amount = Option.unopt_exn (Invalid_argument "tez conversion")
            (Tez.of_mutez (Int64.of_string amount))
      })
    [
      (["envelope"; "hospital"; "mind"; "sunset"; "cancel"; "muscle"; "leisure";
        "thumb"; "wine"; "market"; "exit"; "lucky"; "style"; "picnic"; "success"],
       "0f39ed0b656509c2ecec4771712d9cddefe2afac",
       "23932454669343",
       "tz1MawerETND6bqJqx8GV3YHUrvMBCDasRBF",
       "z0eZHQQGKt",
       "cjgfoqmk.wpxnvnup@tezos.example.org"
      );
      (["flag"; "quote"; "will"; "valley"; "mouse"; "chat"; "hold"; "prosper";
        "silk"; "tent"; "cruel"; "cause"; "demise"; "bottom"; "practice"],
       "41f98b15efc63fa893d61d7d6eee4a2ce9427ac4",
       "72954577464032",
       "tz1X4maqF9tC1Yn4jULjHRAyzjAtc25Z68TX",
       "MHErskWPE6",
       "oklmcktr.ztljnpzc@tezos.example.org"
      );
      (["library"; "away"; "inside"; "paper"; "wise"; "focus"; "sweet"; "expose";
        "require"; "change"; "stove"; "planet"; "zone"; "reflect"; "finger"],
       "411dfef031eeecc506de71c9df9f8e44297cf5ba",
       "217487035428349",
       "tz1SWBY7rWMutEuWS54Pt33MkzAS6eWkUuTc",
       "0AO6BzQNfN",
       "ctgnkvqm.kvtiybky@tezos.example.org"
      );
      (["cruel"; "fluid"; "damage"; "demand"; "mimic"; "above"; "village"; "alpha";
        "vendor"; "staff"; "absent"; "uniform"; "fire"; "asthma"; "milk"],
       "08d7d355bc3391d12d140780b39717d9f46fcf87",
       "4092742372031",
       "tz1amUjiZaevaxQy5wKn4SSRvVoERCip3nZS",
       "9kbZ7fR6im",
       "bnyxxzqr.tdszcvqb@tezos.example.org"
      ) ;
      (["opera"; "divorce"; "easy"; "myself"; "idea"; "aim"; "dash"; "scout";
        "case"; "resource"; "vote"; "humor"; "ticket"; "client"; "edge"],
       "9b7cad042fba557618bdc4b62837c5f125b50e56",
       "17590039016550",
       "tz1Zaee3QBtD4ErY1SzqUvyYTrENrExu6yQM",
       "suxT5H09yY",
       "iilkhohu.otnyuvna@tezos.example.org"
      ) ;
      (["token"; "similar"; "ginger"; "tongue"; "gun"; "sort"; "piano"; "month";
        "hotel"; "vote"; "undo"; "success"; "hobby"; "shell"; "cart"],
       "124c0ca217f11ffc6c7b76a743d867c8932e5afd",
       "26322312350555",
       "tz1geDUUhfXK1EMj7VQdRjug1MoFe6gHWnCU",
       "4odVdLykaa",
       "kwhlglvr.slriitzy@tezos.example.org"
      ) ;
      (["shield"; "warrior"; "gorilla"; "birth"; "steak"; "neither"; "feel";
        "only"; "liberty"; "float"; "oven"; "extend"; "pulse"; "suffer"; "vapor"],
       "ac7a2125beea68caf5266a647f24dce9fea018a7",
       "244951387881443",
       "tz1h3nY7jcZciJgAwRhWcrEwqfVp7VQoffur",
       "A6yeMqBFG8",
       "lvrmlbyj.yczltcxn@tezos.example.org"
      ) ;
      (["waste"; "open"; "scan"; "tip"; "subway"; "dance"; "rent"; "copper";
        "garlic"; "laundry"; "defense"; "clerk"; "another"; "staff"; "liar"],
       "2b3e94be133a960fa0ef87f6c0922c19f9d87ca2",
       "80065050465525",
       "tz1VzL4Xrb3fL3ckvqCWy6bdGMzU2w9eoRqs",
       "oVZqpq60sk",
       "rfodmrha.zzdndvyk@tezos.example.org"
      ) ;
      (["fiber"; "next"; "property"; "cradle"; "silk"; "obey"; "gossip";
        "push"; "key"; "second"; "across"; "minimum"; "nice"; "boil"; "age"],
       "dac31640199f2babc157aadc0021cd71128ca9ea",
       "3569618927693",
       "tz1RUHg536oRKhPLFfttcB5gSWAhh4E9TWjX",
       "FfytQTTVbu",
       "owecikdy.gxnyttya@tezos.example.org"
      ) ;
      (["print"; "labor"; "budget"; "speak"; "poem"; "diet"; "chunk"; "eternal";
        "book"; "saddle"; "pioneer"; "ankle"; "happy"; "only"; "exclude"],
       "bb841227f250a066eb8429e56937ad504d7b34dd",
       "9034781424478",
       "tz1M1LFbgctcPWxstrao9aLr2ECW1fV4pH5u",
       "zknAl3lrX2",
       "ettilrvh.zsrqrbud@tezos.example.org"
      ) ;
    ]

let activation_init () =
  Context.init ~commitments 1 >>=? fun (b, cs) ->
  secrets () |> fun ss ->
  return (b, cs, ss)

let simple_init_with_commitments () =
  activation_init () >>=? fun (blk, _contracts, _secrets) ->
  Block.bake blk >>=? fun _ ->
  return_unit

(** A single activation *)
let single_activation () =
  activation_init () >>=? fun (blk, _contracts, secrets) ->
  let { account ; activation_code ; amount=expected_amount ; _ } as _first_one = List.hd secrets in

  (* Contract does not exist *)
  Assert.balance_is ~loc:__LOC__ (B blk) (Contract.implicit_contract account) Tez.zero >>=? fun () ->

  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->

  (* Contract does exist *)
  Assert.balance_is ~loc:__LOC__ (B blk) (Contract.implicit_contract account) expected_amount

(** 10 activations, one per bake *)
let multi_activation_1 () =
  activation_init () >>=? fun (blk, _contracts, secrets) ->

  Error_monad.fold_left_s (fun blk { account ; activation_code ; amount = expected_amount ; _ } ->
      Op.activation (B blk) account activation_code >>=? fun operation ->
      Block.bake ~operation blk >>=? fun blk ->

      Assert.balance_is ~loc:__LOC__ (B blk) (Contract.implicit_contract account) expected_amount >>=? fun () ->

      return blk
    ) blk secrets >>=? fun _ ->
  return_unit

(** All in one bake *)
let multi_activation_2 () =
  activation_init () >>=? fun (blk, _contracts, secrets) ->

  Error_monad.fold_left_s (fun ops { account ; activation_code ; _ } ->
      Op.activation (B blk) account activation_code >>=? fun op ->
      return (op::ops)
    ) [] secrets >>=? fun ops ->

  Block.bake ~operations:ops blk >>=? fun blk ->

  Error_monad.iter_s (fun { account ; amount = expected_amount ; _ } ->
      (* Contract does exist *)
      Assert.balance_is ~loc:__LOC__ (B blk) (Contract.implicit_contract account) expected_amount
    ) secrets

(** Transfer with activated account *)
let activation_and_transfer () =
  activation_init () >>=? fun (blk, contracts, secrets) ->
  let { account ; activation_code ; _ } as _first_one = List.hd secrets in
  let bootstrap_contract = List.hd contracts in
  let first_contract = Contract.implicit_contract account in

  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->

  Context.Contract.balance (B blk) bootstrap_contract >>=? fun amount ->
  Tez.(/?) amount 2L >>?= fun half_amount ->
  Context.Contract.balance (B blk) first_contract >>=? fun activated_amount_before ->

  Op.transaction (B blk) bootstrap_contract first_contract half_amount >>=? fun operation ->
  Block.bake ~operation blk >>=? fun blk ->

  Assert.balance_was_credited ~loc:__LOC__ (B blk) (Contract.implicit_contract account) activated_amount_before half_amount

(** Transfer to an unactivated account and then activating it *)
let transfer_to_unactivated_then_activate () =
  activation_init () >>=? fun (blk, contracts, secrets) ->
  let { account ; activation_code ; amount } as _first_one = List.hd secrets in
  let bootstrap_contract = List.hd contracts in
  let unactivated_commitment_contract = Contract.implicit_contract account in

  Context.Contract.balance (B blk) bootstrap_contract >>=? fun b_amount ->
  Tez.(/?) b_amount 2L >>?= fun b_half_amount ->

  Incremental.begin_construction blk >>=? fun inc ->
  Op.transaction (I inc) bootstrap_contract unactivated_commitment_contract b_half_amount >>=? fun op ->
  Incremental.add_operation inc op >>=? fun inc ->
  Op.activation (I inc) account activation_code >>=? fun op' ->
  Incremental.add_operation inc op' >>=? fun inc ->
  Incremental.finalize_block inc >>=? fun blk2 ->

  Assert.balance_was_credited ~loc:__LOC__ (B blk2) (Contract.implicit_contract account) amount b_half_amount

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Invalid pkh activation : expected to fail as the context does not
    contain any commitment *)
let invalid_activation_with_no_commitments () =
  Context.init 1 >>=? fun (blk, _) ->
  let secrets = secrets () in
  let { account ; activation_code ; _ } as _first_one = List.hd secrets in

  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_activation _ -> true
    | _ -> false
  end

(** Wrong activation : wrong secret given in the operation *)
let invalid_activation_wrong_secret () =
  activation_init () >>=? fun (blk, _, secrets) ->
  let { account ; _ } as _first_one = List.nth secrets 0 in
  let { activation_code ; _ } as _second_one = List.nth secrets 1 in

  Op.activation (B blk) account activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res  begin function
    | Apply.Invalid_activation _ -> true
    | _ -> false
  end

(** Invalid pkh activation : expected to fail as the context does not
    contain an associated commitment *)
let invalid_activation_inexistent_pkh () =
  activation_init () >>=? fun (blk, _, secrets) ->
  let { activation_code ; _ } as _first_one = List.hd secrets in
  let inexistent_pkh = Signature.Public_key_hash.of_b58check_exn
      "tz1PeQHGKPWSpNoozvxgqLN9TFsj6rDqNV3o" in

  Op.activation (B blk) inexistent_pkh activation_code >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_activation _ -> true
    | _ -> false
  end

(** Invalid pkh activation : expected to fail as the commitment has
    already been claimed *)
let invalid_double_activation () =
  activation_init () >>=? fun (blk, _, secrets) ->
  let { account ; activation_code ; _ } as _first_one = List.hd secrets in
  Incremental.begin_construction blk >>=? fun inc ->

  Op.activation (I inc) account activation_code >>=? fun op ->
  Incremental.add_operation inc op >>=? fun inc ->
  Op.activation (I inc) account activation_code >>=? fun op' ->
  Incremental.add_operation inc op' >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_activation _ -> true
    | _ -> false
  end

(** Transfer from an unactivated commitment account *)
let invalid_transfer_from_unactived_account () =
  activation_init () >>=? fun (blk, contracts, secrets) ->
  let { account ; _ } as _first_one = List.hd secrets in
  let bootstrap_contract = List.hd contracts in
  let unactivated_commitment_contract = Contract.implicit_contract account in

  (* No activation *)

  Op.transaction (B blk) unactivated_commitment_contract bootstrap_contract Tez.one >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->

  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Empty_implicit_contract pkh -> if pkh = account then true else false
    | _ -> false
  end

let tests = [
  Test.tztest "init with commitments" `Quick simple_init_with_commitments ;
  Test.tztest "single activation" `Quick single_activation ;
  Test.tztest "multi-activation one-by-one" `Quick multi_activation_1 ;
  Test.tztest "multi-activation all at a time" `Quick multi_activation_2 ;
  Test.tztest "activation and transfer" `Quick activation_and_transfer ;
  Test.tztest "transfer to unactivated account then activate" `Quick transfer_to_unactivated_then_activate ;
  Test.tztest "invalid activation with no commitments" `Quick invalid_activation_with_no_commitments ;
  Test.tztest "invalid activation with commitments" `Quick invalid_activation_inexistent_pkh ;
  Test.tztest "invalid double activation" `Quick invalid_double_activation ;
  Test.tztest "wrong activation code" `Quick invalid_activation_wrong_secret ;
  Test.tztest "invalid transfer from unactivated account" `Quick invalid_transfer_from_unactived_account
]
