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

(** Block header *)

type t = {
  shell: Block_header.shell_header ;
  protocol_data: protocol_data ;
}

and protocol_data = {
  contents: contents ;
  signature: Signature.t ;
}

and contents = {
  priority: int ;
  seed_nonce_hash: Nonce_hash.t option ;
  proof_of_work_nonce: MBytes.t ;
}

type block_header = t

type raw = Block_header.t
type shell_header = Block_header.shell_header

let raw_encoding = Block_header.encoding
let shell_header_encoding = Block_header.shell_header_encoding

let contents_encoding =
  let open Data_encoding in
  def "block_header.alpha.unsigned_contents" @@
  conv
    (fun { priority ; seed_nonce_hash ; proof_of_work_nonce } ->
       (priority, proof_of_work_nonce, seed_nonce_hash))
    (fun (priority, proof_of_work_nonce, seed_nonce_hash) ->
       { priority ; seed_nonce_hash ; proof_of_work_nonce })
    (obj3
       (req "priority" uint16)
       (req "proof_of_work_nonce"
          (Fixed.bytes Constants_repr.proof_of_work_nonce_size))
       (opt "seed_nonce_hash" Nonce_hash.encoding))

let protocol_data_encoding =
  let open Data_encoding in
  def "block_header.alpha.signed_contents" @@
  conv
    (fun { contents ; signature } -> (contents, signature))
    (fun (contents, signature) -> { contents ; signature })
    (merge_objs
       contents_encoding
       (obj1 (req "signature" Signature.encoding)))

let raw { shell ; protocol_data ; } =
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      protocol_data in
  { Block_header.shell ; protocol_data }

let unsigned_encoding =
  let open Data_encoding in
  merge_objs
    Block_header.shell_header_encoding
    contents_encoding

let encoding =
  let open Data_encoding in
  def "block_header.alpha.full_header" @@
  conv
    (fun { shell ; protocol_data } ->
       (shell, protocol_data))
    (fun (shell, protocol_data) ->
       { shell ; protocol_data })
    (merge_objs
       Block_header.shell_header_encoding
       protocol_data_encoding)

(** Constants *)

let max_header_length =
  let fake_shell = {
    Block_header.level = 0l ;
    proto_level = 0 ;
    predecessor = Block_hash.zero ;
    timestamp = Time.of_seconds 0L ;
    validation_passes = 0 ;
    operations_hash = Operation_list_list_hash.zero ;
    fitness = Fitness_repr.from_int64 0L ;
    context = Context_hash.zero ;
  }
  and fake_contents =
    { priority = 0 ;
      proof_of_work_nonce =
        MBytes.create Constants_repr.proof_of_work_nonce_size ;
      seed_nonce_hash = Some Nonce_hash.zero
    } in
  Data_encoding.Binary.length
    encoding
    { shell = fake_shell ;
      protocol_data = {
        contents = fake_contents ;
        signature = Signature.zero ;
      }
    }

(** Header parsing entry point  *)

let hash_raw = Block_header.hash
let hash { shell ; protocol_data } =
  Block_header.hash
    { shell ;
      protocol_data =
        Data_encoding.Binary.to_bytes_exn
          protocol_data_encoding
          protocol_data }
