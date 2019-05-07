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

(* pvss tests here *)

module Pvss = Pvss_secp256k1
module Sp = Secp256k1_group

module Setup : sig

  val shares : Pvss.Encrypted_share.t list
  val commitments: Pvss.Commitment.t list
  val proof: Pvss.proof

  val secret_scalar : Sp.Group.Scalar.t
  val secret: Pvss.Secret_key.t
  val public_secret: Pvss.Public_key.t

  val other_shares : Pvss.Encrypted_share.t list
  val other_commitments: Pvss.Commitment.t list
  val other_proof: Pvss.proof
  val other_secret: Pvss.Secret_key.t

  type keypair = {secret_key: Pvss.Secret_key.t; public_key: Pvss.Public_key.t}
  val public_keys : Pvss.Public_key.t list
  val keypairs : keypair list
  val reveals : (Pvss.Encrypted_share.t * (Pvss.Clear_share.t * Pvss.proof)) list

  val convert_encoding : 'a Data_encoding.t -> 'b Data_encoding.t -> 'a -> 'b
  val group_encoding : Sp.Group.t Data_encoding.t

end  = struct

  type keypair = {secret_key: Pvss.Secret_key.t; public_key: Pvss.Public_key.t}

  let group_encoding = Data_encoding.(conv Sp.Group.to_bits Sp.Group.of_bits_exn string)
  let scalar_encoding = Data_encoding.(conv Sp.Group.Scalar.to_bits Sp.Group.Scalar.of_bits_exn string)

  let convert_encoding de1 de2 x =
    Data_encoding.Binary.of_bytes_exn de2
      (Data_encoding.Binary.to_bytes_exn de1 x)


  (** Random value of Z in the range [0,2^256] *)
  let rand_Z () =
    [Random.int64 Int64.max_int |> Z.of_int64 |> Z.to_bits]
    |> Blake2B.hash_string |> Blake2B.to_string |> Z.of_bits

  (** Generates n random keypairs *)
  let random_keypairs n =
    List.init n
      (fun _ -> let s = Sp.Group.Scalar.of_Z (rand_Z ()) in
        let secret_key = convert_encoding scalar_encoding Pvss.Secret_key.encoding s in
        { secret_key ; public_key = Pvss.Secret_key.to_public_key secret_key })

  let t = 5
  let n = 8

  let random_scalar () =
    Sp.Group.Scalar.of_Z (rand_Z ())

  let secret_of_scalar s =
    convert_encoding scalar_encoding Pvss.Secret_key.encoding s

  let secret_scalar = random_scalar ()
  let secret = secret_of_scalar secret_scalar
  let public_secret = Pvss.Secret_key.to_public_key secret
  let other_secret= secret_of_scalar (random_scalar ())


  let keypairs = random_keypairs n
  let public_keys = List.map (fun { public_key ; _ } -> public_key) keypairs

  let ((shares, commitments, proof),
       (other_shares, other_commitments, other_proof)) =
    (
      Pvss.dealer_shares_and_proof ~secret ~t ~public_keys,
      Pvss.dealer_shares_and_proof ~secret:other_secret ~t ~public_keys
    )

  let reveals = List.map2 (
      fun share keypair ->
        (share, Pvss.reveal_share share
           ~secret_key:keypair.secret_key ~public_key:keypair.public_key))
      shares keypairs
end

let test_dealer_proof () =
  let shr = (Setup.shares, Setup.other_shares)
  and cmt = (Setup.commitments, Setup.other_commitments)
  and prf = (Setup.proof, Setup.other_proof) in

  begin
    for i = 0 to 1 do
      for j = 0 to 1 do
        for k = 0 to 1 do
          let pick = function 0 -> fst | _ -> snd in
          assert ((Pvss.check_dealer_proof
                     (pick i shr)
                     (pick j cmt)
                     ~proof:(pick k prf) ~public_keys:Setup.public_keys) = (i = j && j = k))
        done
      done
    done
  end

let test_share_reveal () =

  (* check reveal shares *)
  let shares_valid = List.map2 (fun (share, (reveal, proof)) public_key ->
      Pvss.check_revealed_share share reveal ~public_key:public_key proof)
      Setup.reveals Setup.public_keys in

  List.iteri (fun i b -> print_endline (string_of_int i); assert b)
    shares_valid

let test_reconstruct () =
  let indices = [0;1;2;3;4] in
  let reconstructed =  Pvss.reconstruct
      (List.map
         (fun n -> let (_, (r, _)) = List.nth Setup.reveals n in r) indices
      )
      indices
  in
  assert (Sp.Group.((=))
            (Setup.convert_encoding
               Pvss.Public_key.encoding Setup.group_encoding reconstructed)
            (Setup.convert_encoding
               Pvss.Public_key.encoding Setup.group_encoding Setup.public_secret))


let tests = [
  "dealer_proof", `Quick, test_dealer_proof ;
  "reveal", `Quick, test_share_reveal ;
  "recontruct", `Quick, test_reconstruct
]

let () =
  Alcotest.run "test-pvss" [
    "pvss", tests
  ]
