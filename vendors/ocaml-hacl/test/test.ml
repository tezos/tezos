(* Copyright 2018 Vincent Bernardoff, Marco Stronati.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Hacl

let bigstring = Alcotest.testable
    (fun ppf t -> Cstruct.(hexdump_pp ppf (of_bigarray t)))
    Bigstring.equal

let msg = Bigstring.of_string "Voulez-vous coucher avec moi, ce soir ?"
let msglen = Bigstring.length msg

let of_hex hex =
  Cstruct.(to_bigarray (of_hex hex))

let randmsg = of_hex "12c0c5a283401a81163dfd645e57ef6ff58b2f877c4e2d4add10345ec80bef3ffc720060c82e4288a20eccf99d64f18223edb30069fa76de9fe9ae8f875f3a3f75f91dd625652632869766839075e88afc852918da3445bca6d428a4f55d98366065fc70e0306fc6c84ec9e8d1325cc63ba09d5803383d0be40bd7ace7e7551615e4267f94630a0ad62cf798b4a7648390547a3616f42d8b8e58d7223f3c07826670209601be0ef2ea60e662c34b21113680141bead22e8b31015d7fe1a6617101036f03050d8b6854989bdfc13efaa6b2e1960c291f91da346911b1d46f20242bb1eb16f4104f9d684ed0dfca8e13e46b47ba9c39513f5e0746dd828f43da416e10341f3b169691ee823a53500f1ef00c6a52c3f4ecb42f68e1894785d4d192079cc8e53be8bb4ca1e000553504d6132e95490a4b477baaddca598f8947b20fbf732ac608830fb4b11c3cd1e19257e8cb00a22a8fc54ad6e47960086cd5ed24451c1f2ac2cda4514e6e1118ffabd74e7aae3514f3e5d40443ed94bdbbf7af5fa737d2da3b19cac58ca24539313a545164c20c4fae74d01fcb535d4414885ee50cdbb5ff1fcd465fc0c0a0c0f0ebc62687569bd5d36774a6a9c8d9e05b33ac30f13fdd7906aebd27dfd2ee19616a6f3694f2539b89b9ce6d73396816202700f50617f26a7134a6819fe808775bff75df240102fb0352f67eb97e022f66d40403"
let randmsg_len = Bigstring.length randmsg

let sha256 () =
  let open Hash.SHA256 in
  let resp = of_hex "bd4860cc3f39995c47f94205a86c9e22e2fc8ab91c88c5293b704d454991f757" in
  let randresp = of_hex "9f043732d7117fa402d24e7343108976524b097390b0b160df42b0fa5bc6425c" in
  let st = init () in
  Printf.printf "Init done\n" ;
  update st msg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bigstring.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check bigstring "sha256" resp d) ;
  let d = digest msg in
  print_endline "Direct hash done." ;
  Alcotest.(check bigstring "sha256" resp d) ;

  let st = init () in
  Printf.printf "Init done\n" ;
  update st randmsg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bigstring.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check bigstring "sha256" randresp d) ;
  let d = digest randmsg in
  print_endline "Direct hash done." ;
  Alcotest.(check bigstring "sha256" randresp d)

let sha256_seq () =
  let open Hash.SHA256 in
  let bothresp = of_hex "97e13d5a675bf308eb52ce5eb7c4926940bcf9304668e08240f7c8c73f927953" in
  let st = init () in
  Printf.printf "Init done\n" ;
  update st msg ;
  update st randmsg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bigstring.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check bigstring "sha256_seq" bothresp d)

let sha512 () =
  let resp = of_hex "7941f442d956f124d77ee1d1f0ba3db100751090462cdce4aed5fcd240529097bc666bf9c424becde760910df652c7aefec50b02d7f6efe666f79e5242fb755b" in
  let digest = Hash.SHA512.digest msg in
  Alcotest.(check bigstring "sha512" resp digest)

let hmac_sha256 () =
  let key  = Bigstring.of_string "key" in
  let msg = Bigstring.of_string "The quick brown fox jumps over the lazy dog" in
  let resp = of_hex "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8" in
  let digest = Hash.SHA256.HMAC.digest ~key ~msg in
  Alcotest.(check bigstring "hmac_sha256" resp digest)

let hmac_sha512 () =
  let vectors = [
    Bigstring.of_string "key",
    Bigstring.of_string "The quick brown fox jumps over the lazy dog",
    of_hex "b42af09057bac1e2d41708e48a902e09b5ff7f12ab428a4fe86653c73dd248fb82f948a549f7b791a5b41915ee4d1ec3935357e4e2317250d0372afa2ebeeb3a" ;
    Bigstring.empty,
    Bigstring.empty,
    of_hex "b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47" ;
  ] in
  List.iter begin fun (key, msg, resp) ->
    let digest = Hash.SHA512.HMAC.digest ~key ~msg in
    Alcotest.(check bigstring "hmac_sha512" resp digest)
  end vectors

let hash = [
  "hmac_sha256", `Quick, hmac_sha256 ;
  "hmac_sha512", `Quick, hmac_sha512 ;

  "sha256", `Quick, sha256 ;
  "sha256_seq", `Quick, sha256_seq ;

  "sha512", `Quick, sha512 ;
]

let secretbox () =
  let open Secretbox in
  let key = genkey () in
  let nonce = Nonce.gen () in
  let orig_msg = Bigstring.create (msglen + zerobytes) in
  Bigstring.fill orig_msg '\x00' ;
  Bigstring.blit msg 0 orig_msg zerobytes msglen ;
  let cmsg = Bigstring.create (msglen + zerobytes) in
  box ~key ~nonce ~msg:orig_msg ~cmsg ;
  let decrypted_msg = Bigstring.create (msglen + zerobytes) in
  assert (box_open ~key ~nonce ~cmsg ~msg:decrypted_msg) ;
  Alcotest.check bigstring "secretbox_decrypt" orig_msg decrypted_msg ;
  (* in place *)
  box ~key ~nonce ~msg:orig_msg ~cmsg:orig_msg ;
  assert (box_open ~key ~nonce ~cmsg:orig_msg ~msg:orig_msg) ;
  Alcotest.check bigstring "secretbox_decrypt_inplace" decrypted_msg orig_msg

let secretbox = [
  "secretbox", `Quick, secretbox ;
]

let box () =
  let open Box in
  let pk, sk = keypair () in
  let k = dh pk sk in
  let nonce = Nonce.gen () in
  let msg_orig = Bigstring.create (msglen + zerobytes) in
  Bigstring.fill msg_orig '\x00' ;
  Bigstring.blit msg 0 msg_orig zerobytes msglen ;
  let cmsg = Bigstring.create (msglen + zerobytes) in
  Bigstring.fill cmsg '\x00' ;
  let decrypted_msg = Bigstring.create (msglen + zerobytes) in
  box ~k ~nonce ~msg:msg_orig ~cmsg ;
  assert (box_open ~k ~nonce ~cmsg ~msg:decrypted_msg) ;
  Alcotest.check bigstring "box" msg_orig decrypted_msg ;
  (* in place *)
  assert (box_open ~k ~nonce ~cmsg ~msg:cmsg) ;
  Alcotest.check bigstring "box" msg_orig cmsg

let box = [
  "box", `Quick, box ;
]

let keypair () =
  let seed = Hacl.Rand.gen 32 in
  let sk = Sign.unsafe_sk_of_bytes seed in
  let pk = Sign.neuterize sk in
  let sk' = Sign.unsafe_sk_of_bytes seed in
  let pk' = Sign.neuterize sk' in
  Alcotest.(check bool "Sign.of_seed" true (Sign.equal pk pk')) ;
  Alcotest.(check bool "Sign.of_seed" true (Sign.equal sk sk')) ;
  let pk_bytes = Sign.unsafe_to_bytes pk in
  let pk_bytes_length = Bigstring.length pk_bytes in
  Alcotest.(check int "Sign.to_bytes" Sign.pkbytes pk_bytes_length)

let sign () =
  let pk, sk = Sign.keypair () in
  let signature = Bigstring.create Sign.bytes in
  Sign.sign ~sk ~msg ~signature ;
  assert (Sign.verify ~pk ~msg ~signature)

let public () =
  let pk, sk = Sign.keypair () in
  let pk' = Sign.unsafe_to_bytes pk in
  let ppk = Sign.(unsafe_to_bytes (neuterize pk)) in
  let psk = Sign.(unsafe_to_bytes (neuterize sk)) in
  Alcotest.check bigstring "public" pk' ppk ;
  Alcotest.check bigstring "public" pk' psk

let sign = [
  "keypair", `Quick, keypair ;
  "sign", `Quick, sign ;
  "public", `Quick, public ;
]

let () =
  Alcotest.run "hacl" [
    "hash", hash ;
    "secretbox", secretbox ;
    "box", box ;
    "sign", sign ;
  ]
