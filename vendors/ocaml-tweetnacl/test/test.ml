open Tweetnacl

let msg = "Voulez-vous coucher avec moi, ce soir ?" |> Cstruct.of_string
let msglen = Cstruct.len msg

let sha512 () =
  let resp = `Hex "7941f442d956f124d77ee1d1f0ba3db100751090462cdce4aed5fcd240529097bc666bf9c424becde760910df652c7aefec50b02d7f6efe666f79e5242fb755b" in
  let digest = Hash.sha512 msg in
  assert (resp = (Hex.of_cstruct digest))

let keypair () =
  let seed = Rand.gen 32 in
  let pk, sk = Sign.keypair ~seed () in
  let pk', sk' = Sign.keypair ~seed () in
  assert (Sign.equal pk pk') ;
  assert (Sign.equal sk sk')

let sign () =
  let pk, sk = Sign.keypair () in
  let signed_msg = Sign.sign ~key:sk msg in
  match Sign.verify ~key:pk signed_msg with
  | None -> failwith "Impossible to verify"
  | Some verified_msg ->
    assert (Hex.of_cstruct msg =
            Hex.of_cstruct (Cstruct.sub verified_msg Sign.bytes msglen))

let sign_detached () =
  let pk, sk = Sign.keypair () in
  let signature = Sign.detached ~key:sk msg in
  match Sign.verify_detached ~key:pk ~signature msg with
  | false -> failwith "Impossible to verify"
  | true -> ()

let sign_extended () =
  let pk, sk = Sign.keypair () in
  let ek = Sign.extended sk in
  let signed_msg = Sign.sign_extended ~key:ek msg in
  match Sign.verify ~key:pk signed_msg with
  | None -> failwith "Impossible to verify"
  | Some verified_msg ->
    assert (Hex.of_cstruct msg =
            Hex.of_cstruct (Cstruct.sub verified_msg Sign.bytes msglen))

let sign_extended_detached () =
  let pk, sk = Sign.keypair () in
  let ek = Sign.extended sk in
  let signature = Sign.detached_extended ~key:ek msg in
  match Sign.verify_detached ~key:pk ~signature msg with
  | false -> failwith "Impossible to verify"
  | true -> ()

let public () =
  let pk, sk = Sign.keypair () in
  let pk' = Sign.to_cstruct pk in
  let ek = Sign.extended sk in
  let ppk = Sign.(public pk |> to_cstruct) in
  let psk = Sign.(public sk |> to_cstruct) in
  let pek = Sign.(public ek |> to_cstruct) in
  assert (Cstruct.equal pk' ppk) ;
  assert (Cstruct.equal pk' psk) ;
  assert (Cstruct.equal pk' pek)

let base () =
  let pk, sk = Sign.keypair () in
  let ek = Sign.(extended sk |> to_cstruct) in
  let z = Z.of_bits Cstruct.(sub ek 0 32 |> to_string) in
  let pk' = Sign.base z in
  assert (Sign.equal pk pk')

let comm () =
  let pk1, _ = Sign.keypair () in
  let pk2, _ = Sign.keypair () in
  let pk3 = Sign.add pk1 pk2 in
  let pk3' = Sign.add pk2 pk1 in
  assert (Sign.equal pk3 pk3')

let assoc () =
  let pk1, _ = Sign.keypair () in
  let pk2, _ = Sign.keypair () in
  let pk3, _ = Sign.keypair () in
  let sum12 = Sign.add pk1 pk2 in
  let sum23 = Sign.add pk2 pk3 in
  let a = Sign.add sum12 pk3 in
  let b = Sign.add pk1 sum23 in
  assert (Sign.equal a b)

let arith () =
  let pk, _sk = Sign.keypair () in
  let pk2 = Sign.mult pk (Z.of_int 3) in
  let pk2' = Sign.(add (add pk pk) pk) in
  assert (Sign.equal pk2 pk2')

let arith2 () =
  let a = Sign.base (Z.of_int 3) in
  let b = Sign.mult a (Z.of_int 2) in
  let b' = Sign.base (Z.of_int 6) in
  assert (Sign.equal b b')

let hash = [
  "sha512", `Quick, sha512 ;
]

let secretbox () =
  let open Secretbox in
  let key = genkey () in
  let nonce = Nonce.gen () in
  let cmsg = box ~key ~nonce ~msg in
  assert (Cstruct.len cmsg = msglen + boxzerobytes) ;
  begin match box_open ~key ~nonce ~cmsg with
    | None -> assert false
    | Some msg' -> assert Cstruct.(equal msg msg')
  end

let secretbox_noalloc () =
  let open Secretbox in
  let buflen = msglen + zerobytes in
  let buf = Cstruct.create buflen in
  Cstruct.blit msg 0 buf zerobytes msglen ;
  let key = genkey () in
  let nonce = Nonce.gen () in
  box_noalloc ~key ~nonce ~msg:buf ;
  let res = box_open_noalloc ~key ~nonce ~cmsg:buf in
  assert res ;
  assert Cstruct.(equal msg (sub buf zerobytes msglen))

let secretbox = [
  "secretbox", `Quick, secretbox ;
  "secretbox_noalloc", `Quick, secretbox_noalloc ;
]

let box () =
  let open Box in
  let pk, sk = keypair () in
  let ck = combine pk sk in
  let nonce = Nonce.gen () in
  let cmsg = box ~pk ~sk ~nonce ~msg in
  assert (Cstruct.len cmsg = msglen + boxzerobytes) ;
  begin match box_open ~pk ~sk ~nonce ~cmsg with
    | None -> assert false
    | Some msg' -> assert Cstruct.(equal msg msg')
  end ;
  let cmsg = box_combined ~k:ck ~nonce ~msg in
  begin match box_open_combined ~k:ck ~nonce ~cmsg with
    | None -> assert false
    | Some msg' -> assert Cstruct.(equal msg msg')
  end

let box_noalloc () =
  let open Box in
  let buflen = msglen + zerobytes in
  let buf = Cstruct.create buflen in
  Cstruct.blit msg 0 buf zerobytes msglen ;
  let pk, sk = keypair () in
  let ck = combine pk sk in
  let nonce = Nonce.gen () in
  box_noalloc ~pk ~sk ~nonce ~msg:buf ;
  let res = box_open_noalloc ~pk ~sk ~nonce ~cmsg:buf in
  assert res ;
  assert Cstruct.(equal msg (sub buf zerobytes msglen)) ;
  box_combined_noalloc ~k:ck ~nonce ~msg:buf ;
  let res = box_open_combined_noalloc ~k:ck ~nonce ~cmsg:buf in
  assert res ;
  assert Cstruct.(equal msg (sub buf zerobytes msglen))

let box = [
  "box", `Quick, box ;
  "box_noalloc", `Quick, box_noalloc ;
]

let sign = [
  "keypair", `Quick, keypair ;
  "sign", `Quick, sign ;
  "sign_detached", `Quick, sign_detached ;
  "sign_extended", `Quick, sign_extended ;
  "sign_extended_detached", `Quick, sign_extended_detached ;
  "public", `Quick, public ;
  "base", `Quick, base ;
  "comm", `Quick, comm ;
  "assoc", `Quick, assoc ;
  "arith", `Quick, arith ;
  "arith2", `Quick, arith2 ;
]

let () =
  Alcotest.run "tweetnacl" [
    "hash", hash ;
    "secretbox", secretbox ;
    "box", box ;
    "sign", sign ;
  ]
