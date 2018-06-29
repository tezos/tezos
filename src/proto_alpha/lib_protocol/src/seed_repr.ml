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

(* Tezos Protocol Implementation - Random number generation *)

type seed = B of State_hash.t
type t = T of State_hash.t
type sequence = S of State_hash.t
type nonce = MBytes.t

let nonce_encoding = Data_encoding.Fixed.bytes Constants_repr.nonce_length

let init = "Laissez-faire les proprietaires."
let zero_bytes = MBytes.of_string (String.make Nonce_hash.size '\000')

let state_hash_encoding =
  let open Data_encoding in
  conv
    State_hash.to_bytes
    State_hash.of_bytes_exn
    (Fixed.bytes Nonce_hash.size)

let seed_encoding =
  let open Data_encoding in
  conv
    (fun (B b) -> b)
    (fun b -> B b)
    state_hash_encoding

let empty = B (State_hash.hash_bytes [MBytes.of_string init])

let nonce (B state) nonce =
  B (State_hash.hash_bytes ( [State_hash.to_bytes state; nonce] ))

let initialize_new (B state) append =
  T (State_hash.hash_bytes
       (State_hash.to_bytes state :: zero_bytes :: append ))

let xor_higher_bits i b =
  let higher = MBytes.get_int32 b 0 in
  let r = Int32.logxor higher i in
  let res = MBytes.copy b in
  MBytes.set_int32 res 0 r;
  res

let sequence (T state) n =
  State_hash.to_bytes state
  |> xor_higher_bits n
  |> (fun b -> S (State_hash.hash_bytes [b]))

let take (S state) =
  let b = State_hash.to_bytes state in
  let h = State_hash.hash_bytes [b] in
  (State_hash.to_bytes h, S h)

let take_int32 s bound =
  if Compare.Int32.(bound <= 0l)
  then invalid_arg "Seed_repr.take_int32" (* FIXME *)
  else
    let rec loop s =
      let bytes, s = take s in
      let r = Int32.abs (MBytes.get_int32 bytes 0) in
      let drop_if_over =
        Int32.sub Int32.max_int (Int32.rem Int32.max_int bound) in
      if Compare.Int32.(r >= drop_if_over)
      then loop s
      else
        let v = Int32.rem r bound in
        v, s
    in
    loop s

type error += Unexpected_nonce_length (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"unexpected_nonce_length"
    ~title:"Unexpected nonce length"
    ~description:"Nonce length is incorrect."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Nonce length is not %i bytes long as it should."
          Constants_repr.nonce_length)
    Data_encoding.empty
    (function Unexpected_nonce_length -> Some () | _ -> None)
    (fun () -> Unexpected_nonce_length)

let make_nonce nonce =
  if Compare.Int.(MBytes.length nonce <> Constants_repr.nonce_length)
  then error Unexpected_nonce_length
  else ok nonce

let hash nonce = Nonce_hash.hash_bytes [nonce]

let check_hash nonce hash =
  Compare.Int.(MBytes.length nonce = Constants_repr.nonce_length)
  && Nonce_hash.equal (Nonce_hash.hash_bytes [nonce]) hash

let nonce_hash_key_part = Nonce_hash.to_path

let initial_nonce_0 = zero_bytes

let initial_nonce_hash_0 =
  hash initial_nonce_0

let deterministic_seed seed = nonce seed zero_bytes

let initial_seeds n =
  let rec loop acc elt i =
    if Compare.Int.(i = 1) then
      List.rev (elt :: acc)
    else
      loop
        (elt :: acc)
        (deterministic_seed elt)
        (i-1) in
  loop [] (B (State_hash.hash_bytes [])) n
