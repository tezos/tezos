open Libsecp256k1

module Num = struct
  open Internal
  open Num
  let basic () =
    let z = zero () in
    Alcotest.(check bool "Num.is_zero" true (is_zero z))

  let runtest =
    [ "basic", `Quick, basic ;
    ]
end

module Scalar = struct
  open Internal
  open Scalar
  let basic () =
    let z = zero () in
    Alcotest.(check bool "Scalar.is_zero" true (is_zero z)) ;
    (* set_int z 1 ; *)
    let z = const ~d0:1L () in
    Alcotest.(check bool "Scalar.is_zero" false (is_zero z)) ;
    Alcotest.(check bool "Scalar.is_even" false (is_even z)) ;
    Alcotest.(check bool "Scalar.is_one" true (is_one z))

  let runtest =
    [ "basic", `Quick, basic ;
    ]
end

module External = struct
  open External
  let buffer_of_hex s =
    Cstruct.to_bigarray (Hex.to_cstruct (`Hex s))

  let ctx = Context.create ()

  let cstruct_testable =
    Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

  let assert_eq_cstruct a b =
    let a = Cstruct.of_bigarray a in
    let b = Cstruct.of_bigarray b in
    assert (Alcotest.equal cstruct_testable a b)

  let test_signature_of_string () =
    let sign_orig = buffer_of_hex
        "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589" in
    let signature = Sign.read_der_exn ctx sign_orig in
    let sign = Sign.to_bytes ~der:true ctx signature in
    assert_eq_cstruct sign_orig sign

  let test_valid_signature _ =
    let msg = buffer_of_hex
        "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
    let signature = Sign.read_der_exn ctx
        (buffer_of_hex "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589") in
    let pk = Key.read_pk_exn ctx
        (buffer_of_hex "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40") in
    assert (Sign.verify_exn ctx ~signature ~pk ~msg)

  let test_invalid_signature _  =
    let msg = buffer_of_hex
        "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A91" in
    let signature = Sign.read_der_exn ctx
        (buffer_of_hex "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589") in
    let pk = Key.read_pk_exn ctx
        (buffer_of_hex "040a629506e1b65cd9d2e0ba9c75df9c4fed0db16dc9625ed14397f0afc836fae595dc53f8b0efe61e703075bd9b143bac75ec0e19f82a2208caeb32be53414c40") in
    assert (not (Sign.verify_exn ctx ~signature ~pk ~msg))

  let test_public_module _ =
    let pubtrue =
      buffer_of_hex "04c591a8ff19ac9c4e4e5793673b83123437e975285e7b442f4ee2654dffca5e2d2103ed494718c697ac9aebcfd19612e224db46661011863ed2fc54e71861e2a6" in
    let pub = Key.read_pk_exn ctx pubtrue in
    let pub_serialized = Key.to_bytes ~compress:false ctx pub in
    assert_eq_cstruct pubtrue pub_serialized

  let test_pubkey_creation _ =
    let seckey = buffer_of_hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530" in
    let pubtrue = buffer_of_hex "04c591a8ff19ac9c4e4e5793673b83123437e975285e7b442f4ee2654dffca5e2d2103ed494718c697ac9aebcfd19612e224db46661011863ed2fc54e71861e2a6" in
    let seckey = Key.read_sk_exn ctx seckey in
    let pubkey = Key.neuterize_exn ctx seckey in
    let buf_pk_comp = Cstruct.create 33 in
    let buf_pk_uncomp = Cstruct.create 65 in
    let nb_written = Key.write ~compress:true ctx buf_pk_comp.buffer pubkey in
    assert (nb_written = 33) ;
    let nb_written = Key.write ~compress:false ctx buf_pk_uncomp.buffer pubkey in
    assert (nb_written = 65) ;
    let nb_written = Key.write ~compress:true ctx buf_pk_uncomp.buffer ~pos:32 pubkey in
    assert (nb_written = 33) ;
    let pubkey_serialized = Key.to_bytes ~compress:false ctx pubkey in
    assert_eq_cstruct pubtrue pubkey_serialized

  let test_sign _ =
    let msg =  buffer_of_hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
    let sk = Key.read_sk_exn ctx (buffer_of_hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530") in
    let validsign = Sign.read_der_exn ctx (buffer_of_hex "30440220182a108e1448dc8f1fb467d06a0f3bb8ea0533584cb954ef8da112f1d60e39a202201c66f36da211c087f3af88b50edf4f9bdaa6cf5fd6817e74dca34db12390c6e9") in
    let sign = Sign.sign_exn ctx ~sk msg in
    assert (Sign.equal sign validsign)

  let test_recover _ =
    let msg = buffer_of_hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
    let seckey = Key.read_sk_exn ctx (buffer_of_hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530") in
    let pubkey = Key.neuterize_exn ctx seckey in
    let recoverable_sign = Sign.sign_recoverable_exn ctx ~sk:seckey msg in
    let usual_sign = Sign.to_plain ctx recoverable_sign in
    assert (Sign.verify_exn ctx ~pk:pubkey ~signature:usual_sign ~msg);
    let recoverable_bytes = Sign.to_bytes ctx recoverable_sign in
    let usual_sign' = Sign.read_exn ctx recoverable_bytes in
    assert (Sign.equal usual_sign' usual_sign) ;
    let recoverable_sign' = Sign.read_recoverable_exn ctx recoverable_bytes in
    assert (Sign.equal recoverable_sign' recoverable_sign);
    match Sign.recover ctx ~signature:recoverable_sign msg with
    | Error _ -> assert false
    | Ok recovered -> assert (Key.equal recovered pubkey)

  let runtest = [
    "signature_of_string", `Quick, test_signature_of_string ;
    "valid_signature", `Quick, test_valid_signature ;
    "invalid_signature", `Quick, test_invalid_signature ;
    "public_module", `Quick, test_public_module ;
    "pubkey_creation", `Quick, test_pubkey_creation ;
    "sign", `Quick, test_sign ;
    "recover", `Quick, test_recover ;
  ]
end

let () =
  Alcotest.run "secp256k1" [
    "Num", Num.runtest ;
    "Scalar", Scalar.runtest ;
    "External", External.runtest ;
  ]
