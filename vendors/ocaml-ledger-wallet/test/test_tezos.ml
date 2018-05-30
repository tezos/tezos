open Ledgerwallet_tezos

let vendor_id = 0x2C97
let product_id = 0x0001

let test_open_close () =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
  Hidapi.close h

let test_ping () =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
  Ledgerwallet.Transport.ping h ;
  Hidapi.close h

let hard x =
  Int32.logor x 0x8000_0000l

let path = [
  hard 44l ; hard 1729l
]

let curves = [Ed25519; Secp256k1; Secp256r1]

let msg = Cstruct.of_string "Voulez-vous coucher avec moi, ce soir ?"
let msg_ba = Cstruct.to_bigarray msg

let test_getpk h curve =
  let pk = get_public_key h curve path in
  Alcotest.(check int "pklen"
              (if curve = Ed25519 then 33 else 65) (Cstruct.len pk))

let test_getpk () =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
  List.iter (test_getpk h) curves ;
  Hidapi.close h

let test_sign h curve =
  let open Alcotest in
  let pk = get_public_key h curve path in
  let signature = sign h curve path msg in
  match curve with
  | Ed25519 ->
      let pk = Tweetnacl.Sign.(pk_of_cstruct_exn (Cstruct.sub pk 1 pkbytes)) in
      check bool "sign Ed25519" true
        (Tweetnacl.Sign.verify_detached ~key:pk ~signature msg)
  | Secp256k1 -> begin
      let pk = Cstruct.to_bigarray pk in
      let signature = Cstruct.to_bigarray signature in
      match Uecc.(pk_of_bytes secp256k1 pk) with
      | None -> assert false
      | Some pk ->
          check bool "sign Secp256k1" true (Uecc.verify pk ~msg:msg_ba ~signature)
    end
  | Secp256r1 -> begin
      let pk = Cstruct.to_bigarray pk in
      let signature = Cstruct.to_bigarray signature in
      match Uecc.(pk_of_bytes secp256r1 pk) with
      | None -> assert false
      | Some pk ->
          check bool "sign Secp256r1" true (Uecc.verify pk ~msg:msg_ba ~signature)
    end

let test_sign () =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
  (* List.iter (test_sign h) curves ; *)
  (* List.iter (test_sign h) [Secp256k1] ; *)
  Hidapi.close h

let basic = [
  "open_close", `Quick, test_open_close ;
  "ping", `Quick, test_ping ;
  "get_public_key", `Quick, test_getpk ;
  "sign", `Quick, test_sign ;
]

let () =
  Alcotest.run "ledgerwallet.tezos" [
    "basic", basic ;
  ]
