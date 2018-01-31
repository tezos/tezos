(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  peer_id : P2p_peer.Id.t ;
  public_key : Crypto_box.public_key ;
  secret_key : Crypto_box.secret_key ;
  proof_of_work_stamp : Crypto_box.nonce ;
}

let encoding =
  let open Data_encoding in
  conv
    (fun { public_key ; secret_key ; proof_of_work_stamp ; _ } ->
       (public_key, secret_key, proof_of_work_stamp))
    (fun (public_key, secret_key, proof_of_work_stamp) ->
       let peer_id = Tezos_crypto.Crypto_box.hash public_key in
       { peer_id ; public_key ; secret_key ; proof_of_work_stamp })
    (obj3
       (req "public_key" Crypto_box.public_key_encoding)
       (req "secret_key" Crypto_box.secret_key_encoding)
       (req "proof_of_work_stamp" Crypto_box.nonce_encoding))

let generate ?max target =
  let secret_key, public_key, peer_id = Crypto_box.random_keypair () in
  let proof_of_work_stamp =
    Crypto_box.generate_proof_of_work ?max public_key target in
  { peer_id ; public_key ; secret_key ; proof_of_work_stamp }

let animation = [|
  "|.....|" ;
  "|o....|" ;
  "|oo...|" ;
  "|ooo..|" ;
  "|.ooo.|" ;
  "|..ooo|" ;
  "|...oo|" ;
  "|....o|" ;
  "|.....|" ;
  "|.....|" ;
  "|.....|" ;
  "|.....|" ;
|]

let init = String.make (String.length animation.(0)) '\ '
let clean = String.make (String.length animation.(0)) '\b'
let animation = Array.map (fun x -> clean ^ x) animation
let animation_size = Array.length animation
let duration = 1200 / animation_size

let generate_with_animation ppf target =
  Format.fprintf ppf "%s%!" init ;
  let count = ref 10000 in
  let rec loop n =
    let start = Mtime_clock.counter () in
    Format.fprintf ppf "%s%!" animation.(n mod animation_size);
    try generate ~max:!count target
    with Not_found ->
      let time = Mtime.Span.to_ms (Mtime_clock.count start) in
      count :=
        if time <= 0. then
          !count * 10
        else
          !count * duration / int_of_float time ;
      loop (n+1)
  in
  let id = loop 0 in
  Format.fprintf ppf "%s%s\n%!" clean init ;
  id

let generate target = generate target
