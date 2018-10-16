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

type t = {
  peer_id : P2p_peer.Id.t ;
  public_key : Crypto_box.public_key ;
  secret_key : Crypto_box.secret_key ;
  proof_of_work_stamp : Crypto_box.nonce ;
}

let encoding =
  let open Data_encoding in
  conv
    (fun { peer_id ; public_key ; secret_key ; proof_of_work_stamp } ->
       (Some peer_id, public_key, secret_key, proof_of_work_stamp))
    (fun (peer_id_opt, public_key, secret_key, proof_of_work_stamp) ->
       let peer_id =
         match peer_id_opt with
         | Some peer_id -> peer_id
         | None -> Tezos_crypto.Crypto_box.hash public_key in
       { peer_id ; public_key ; secret_key ; proof_of_work_stamp })
    (obj4
       (opt "peer_id" P2p_peer_id.encoding)
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
