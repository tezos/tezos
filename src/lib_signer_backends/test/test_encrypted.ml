(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

let loops = 10

let passwords = List.map MBytes.of_string [
    "ahThie5H"; "aVah7eid"; "Hihohh1n"; "mui0Hoox"; "Piu7pual"; "paik6aiW";
    "caeS5me5"; "boh5dauL"; "zaiK1Oht"; "Oogh4hah"; "kiY5ohlo"; "booth0Ei";
    "xa2Aidao"; "aju6oXu4"; "gooruGh9"; "ahy4Daih"; "chosh0Wu"; "Cheij6za";
    "quee9ooL"; "Sohs9are"; "Pae3gay7"; "Naif5iel"; " eir6Aed1"; "aa6Aesai";
    "";
  ]

let nb_passwds = List.length passwords

let fake_ctx () = object
  val mutable i = 0;
  val mutable distributed = false;
  inherit Client_context.simple_printer (fun _ _ -> Lwt.return_unit)
  method prompt : type a. (a, string tzresult) Client_context.lwt_format -> a =
    Format.kasprintf (fun _ -> return "")
  method prompt_password : type a. (a, MBytes.t tzresult) Client_context.lwt_format -> a =
    Format.kasprintf begin fun _ -> (* return Bigstring.empty *)
      match distributed with
      | false ->
          distributed <- true ;
          return (List.nth passwords 0)
      | true ->
          i <- if i = nb_passwds - 1 then 0 else succ i ;
          distributed <- false ;
          return (List.nth passwords i)
    end
end

let make_sk_uris =
  List.map begin fun path ->
    Client_keys.make_sk_uri (Uri.make ~scheme:"encrypted" ~path ())
  end

let ed25519_sks = [
  "edsk3kMNLNdzLPbbABASDLARft8JRZ3Wpwibn8SMAb4KmuWSMJmAFd";
  "edsk3Kqr8VHRx9kmR8Pj5qRGcmvQH34cForiMaMz1Ahhq5DkZp7FxJ";
  "edsk2mBu4w9sMGhryvvXK53dXgpcNdZWi8pJQ1QL2rAgRPrE5y12az" ]

let ed25519_sks_encrypted = make_sk_uris [
    "edesk1oGXxunJ5FTGpQ6o1xdop8VGKdT36Fj7LwWF9HLjzEqaCC4V6tdRVN1jaeJTfCHS8bYf7U2YhMK2yW6jSUy" ;
    "edesk1s4xEifbUdUkghHHimbNUuyQ4eidDVJdc8JtPRUon758hBqZNZsQxadUDFRSRiUdLoFqBG35HAiLKafyczw" ;
    "edesk1zY5jEs4QXrF9tXxFq1mfW9PkatdRxCKQ2Q598y5LLz65nQj4eWxefYFp8YLerya1haRdGe5NWckHDb5ApM" ;
  ]

let secp256k1_sks = [
  "spsk24attf9uuQ7PUKFHxTm6E3TMqB6SPkFiMbXPBur7JNrvupW2xg";
  "spsk2H32XfWL7MkW58r76q6Yu5tJg77YGgVyjwq7EvLUHhn4JmAtEG";
  "spsk3KQ56REAUGc6Gn87xCRnWyPwR2Un667vegQVuU16ZcgNyLCooh" ]

let secp256k1_sks_encrypted = make_sk_uris [
    "spesk2CXQHDbzrcNatRzmg83Dto6kX6BWwpP2zGs4Zks9LDsXzaX6mAYRj5ZrrdgyZQap4DS9YRRLNSpaVC2TSsk" ;
    "spesk1upiFp23osWSUTgHcx8DCVpTrMr9xtdqVQkQDWj5sFG7vqcWLDaNv9AKKcF27Nb266YfuAGF2hEbcyAxHmK" ;
    "spesk1w7d68hzTWJusk5Xn5oz8EgDXbotDW9BXb5ksFjr8Jd94Kxnu5yKAhgRszojhMUoJ1EEt5BtPpGpkgCjELq" ;
  ]

let p256_sks = [
  "p2sk2YQcwF5h7qgRztocEMrfizUwZaM41f4v7zWneiig2Y5AxajqYC";
  "p2sk2XiSoQC9tvejVBDJyvkbHUq2kvcQHdJJ2wM8rii228DkjKV2b5";
  "p2sk3ZsfsEaxDNn74orv91Ruu35fomzF373aT9ForA4fDo54c47o6H" ]

let p256_sks_encrypted = make_sk_uris [
    "p2esk2JMFpR9yaSpgsaKQYLqFnv16t4gowJ4cgjj7D7iMfoaJz2vZuH7Tdi11MrX6FC2yhfs2nvy5VRxAvzH1STE" ;
    "p2esk1nfobVL73mY5Y18W8Ltb3Vm6Nf5Th7trN3yA3ucyyP4AH93XfyRatkh9AxxaDtnju1EtArykjroEQHDT97k" ;
    "p2esk2Ge1jrVak7NhxksimzaQjRCTLx5vxUZ4Akgq3spGQLx6N41h6aKXeEYDgxN5eztnPwD6QiCHCfVAKXLPNm8" ;
  ]

let sk_testable =
  Alcotest.testable
    Signature.Secret_key.pp
    Signature.Secret_key.equal

let test_vectors () =
  let open Encrypted in
  iter_s begin fun (sks, encrypted_sks) ->
    let ctx = fake_ctx () in
    let sks = List.map Signature.Secret_key.of_b58check_exn sks in
    map_s (decrypt ctx) encrypted_sks >>=? fun decs ->
    assert (decs = sks) ;
    return_unit
  end [
    ed25519_sks, ed25519_sks_encrypted ;
    secp256k1_sks, secp256k1_sks_encrypted ;
    p256_sks, p256_sks_encrypted ;
  ]

let test_random algo =
  let open Encrypted in
  let ctx = fake_ctx () in
  let decrypt_ctx = (ctx :> Client_context.prompter) in
  let rec inner i =
    if i >= loops then return_unit
    else
      let _, _, sk = Signature.generate_key ~algo () in
      encrypt ctx sk >>=? fun sk_uri ->
      decrypt decrypt_ctx sk_uri >>=? fun decrypted_sk ->
      Alcotest.check sk_testable "test_encrypt: decrypt" sk decrypted_sk ;
      inner (succ i)
  in inner 0

let test_random _switch () =
  iter_s test_random Signature.[Ed25519 ; Secp256k1 ; P256] >>= function
  | Ok _ -> Lwt.return_unit
  | Error _ -> Lwt.fail_with "test_random"

let test_vectors _switch () =
  test_vectors () >>= function
  | Ok _ -> Lwt.return_unit
  | Error _ -> Lwt.fail_with "test_vectors"

let tests = [
  Alcotest_lwt.test_case "random_roundtrip" `Quick test_random ;
  Alcotest_lwt.test_case "vectors_decrypt" `Quick test_vectors ;
]

let () =
  Alcotest.run "tezos-signer-backends" [
    "encrypted", tests
  ]
