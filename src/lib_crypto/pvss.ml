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

module H = Blake2B

(** Polynomial ring (ℤ/qℤ)[X] *)
module PZ_q (Z_q : Znz.ZN)  : sig
  type t
  module Z_q  : Znz.ZN

  (** Evaluates the polynomial p at point x *)
  val eval  : p:t -> x:Z_q.t -> Z_q.t

  (** Builds the polynomial from a list of coefficient, ordered by power.
      That is, of_list [a₀; a₁; a₂; …] = a₀ + a₁ x + a₂ x² + … *)
  val of_list : Z_q.t list -> t

end with type Z_q.t = Z_q.t = struct
  module Z_q = Z_q
  type t  = Z_q.t list
  let eval ~p ~x = List.fold_right (fun c y -> Z_q.(y * x + c)) p Z_q.zero
  let of_list l = l
end

(** Functor type for an Cyclic group *)
module type CYCLIC_GROUP = sig
  type t
  include S.B58_DATA with type t := t
  include S.ENCODER with type t := t
  val name      : string
  module Z_m    : Znz.ZN
  val e         : t
  val g1        : t
  val g2        : t
  val ( * )     : t -> t -> t
  val (=)       : t -> t -> bool
  val pow       : t -> Z_m.t -> t
  val to_bits   : t -> String.t
  val of_bits   : String.t -> t option
end

(** Type of a module that handles proofs for the discrete logarithm
    equality equation. *)
module type DLEQ = sig

  (** A DLEQ equation. *)
  type equation

  (** A non-interactive zero-knowledge proof-of-knowledge of an
      exponent solving the equation. *)
  type proof

  val proof_encoding : proof Data_encoding.t


  (** Group element. *)
  type element

  (** Exponent, i.e. an integer modulo the group's order. *)
  type exponent


  (** Sets up a equation of the form
      ∀ i, ∃ x(i), b₁ˣ⁽ⁱ⁾ = h₁ᵢ and b₂ᵢˣ⁽ⁱ⁾ = h₂ᵢ. The arguments
      are given as b₁, h₁ᵢ, b₂ᵢ, h₂ᵢ *)
  val setup_equation :
    element -> element list -> element list -> element list -> equation

  (** Creates a zero-knowledge proof of knowledge of the exponent list *)
  val make_proof  : equation -> exponent list -> proof

  (** Checkes the proof created by make_proof for a given equation *)
  val check_proof : equation -> proof -> bool
end

(** Functor for creating a module handling proofs for the discrete logarithm
    equality in cyclic group G *)
module MakeDleq (G : CYCLIC_GROUP) :
  DLEQ with type element = G.t and type exponent = G.Z_m.t =
struct

  type element = G.t
  type exponent = G.Z_m.t
  type equation = element * (element list) * (element list) * (element list)
  type proof = exponent  * (exponent list)

  let proof_encoding = Data_encoding.(
      tup2 G.Z_m.encoding (list G.Z_m.encoding))

  (* Fiat-Shamir heuristic to derive a random element of ℤ/mℤ from the
       hash of a list of group elements *)
  let fiat_shamir ?(exponents=[]) elements =
    String.concat "||" (
      "tezosftw" :: (List.map G.to_bits elements) @ (List.map G.Z_m.to_bits exponents)
    ) |> (fun x -> H.hash_string [x]) |> H.to_string |> G.Z_m.of_bits_exn

  let setup_equation b1 h1_n b2_n h2_n = (b1, h1_n, b2_n, h2_n)
  let make_proof (b1, h1_n, b2_n, h2_n) x_n =
    (* First, draw blinding factors. Normally these should be picked randomly. To maximize
       reproducibility and avoid weak random number generation, we generate the blinding
       factor deterministically from the problem parameters and the secret x_n.
       TODO: review with cryptographer
    *)
    let
      pseudo_seed = fiat_shamir (b1::(List.concat [h1_n; b2_n; h2_n])) ~exponents:x_n in
    let
      w_n  = List.mapi (fun i __ -> fiat_shamir [] ~exponents:[pseudo_seed; G.Z_m.of_int i]) h1_n in let
      a1_n = List.map  (G.pow b1)   w_n and
      a2_n = List.map2 G.pow b2_n w_n in let
      (* Pick the challenge, c, following the Fiat-Shamir heuristic. *)
      c = fiat_shamir (List.concat [h1_n; h2_n; a1_n; a2_n]) in let
      (* rᵢ = wᵢ - c * xᵢ *)
      r_n = List.map2 (fun w x -> G.Z_m.(w - c * x)) w_n x_n in
    (c, r_n)

  let check_proof (b1, h1_n, b2_n, h2_n) (c, r_n) =
    (* First check that the lists have the same sizes. *)
    let same_sizes = List.(
        Compare.Int.((length h1_n) = (length b2_n) && (length b2_n) = (length h2_n) &&
                     (length h2_n) = (length r_n))) in

    if not same_sizes then false
    else
      let
        a1_n = List.map2 G.( * )
          (List.map (G.pow b1) r_n)
          (List.map (fun h1 -> G.pow h1 c) h1_n)
      and
        a2_n = List.map2 G.( * )
          (List.map2 G.pow b2_n r_n)
          (List.map (fun h2 -> G.pow h2 c) h2_n)
      in
      G.Z_m.(c = fiat_shamir (List.concat [h1_n; h2_n; a1_n; a2_n]))
end

module type PVSS = sig

  module type ENCODED = sig
    type t
    include S.B58_DATA with type t := t
    include S.ENCODER with type t := t
  end

  module Commitment : ENCODED
  module Encrypted_share : ENCODED
  module Clear_share : ENCODED

  module Public_key : ENCODED
  module Secret_key : sig
    include ENCODED
    val to_public_key : t -> Public_key.t
  end

  type proof

  val proof_encoding : proof Data_encoding.t

  val dealer_shares_and_proof:
    secret:Secret_key.t -> t:int -> public_keys:Public_key.t list ->
    (Encrypted_share.t list * Commitment.t list * proof)

  val check_dealer_proof:
    Encrypted_share.t list -> Commitment.t list -> proof:proof ->
    public_keys:Public_key.t list -> bool

  val reveal_share : Encrypted_share.t -> secret_key:Secret_key.t
    -> public_key:Public_key.t -> Clear_share.t * proof

  val check_revealed_share:
    Encrypted_share.t -> Clear_share.t -> public_key:Public_key.t -> proof
    -> bool
  val reconstruct: Clear_share.t list -> int list -> Public_key.t
end

module MakePvss (G : CYCLIC_GROUP) : PVSS = struct

  module type ENCODED = sig
    type t
    include S.B58_DATA with type t := t
    include S.ENCODER with type t := t
  end

  (* Module to make discrete logarithm equality proofs *)
  module Dleq = MakeDleq (G)
  type proof = Dleq.proof

  (* Polynomials over ℤ/mℤ *)
  module PZ_m = PZ_q (G.Z_m)

  (* A public key is a group element *)
  module Public_key = G

  module Secret_key = struct
    include G.Z_m
    let to_public_key x = G.(pow g2 x)
  end

  module Encrypted_share = G
  module Clear_share = G
  module Commitment = G

  let proof_encoding = Dleq.proof_encoding

  (* generate a "random": polynomial of degree t to hide secret `secret` *)
  let random_polynomial secret t =
    (* the t-1 coefficients are computed deterministically from
       the secret and mapped to G.Z_m *)

    let nonce = [String.concat "||" [G.Z_m.to_bits secret]]
                |> H.hash_string |> H.to_string in

    (* TODO: guard against buffer overlow *)
    let rec make_coefs = function
      | 0 -> []
      | k -> let h =
               ( H.hash_string [string_of_int k; "||"; nonce])
               |> H.to_string |> G.Z_m.of_bits_exn in
          h :: make_coefs (k-1) in
    let coefs = secret :: (make_coefs (t-1)) in

    (*    let coefs = secret :: List_Utils.list_init ~f:G.Z_m.random ~n:(t-1) in *)
    let poly = PZ_m.of_list coefs
    in (coefs, poly)

  (* Hides secret s in a random polynomial of degree t, publishes t commitments
     to the polynomial coefficients and n encrypted shares for the holders of
     the public keys *)
  let dealer_shares_and_proof ~secret ~t ~public_keys =
    let coefs, poly = random_polynomial secret t in
    let
      (*  Cⱼ represents the commitment to the coefficients of the polynomial
          Cⱼ = g₁^(aⱼ) for j in 0 to t-1  *)

      cC_j = List.map G.(pow g1) coefs and

      (* pᵢ = p(i) for i in 1…n, with i ∈ ℤ/mℤ: points of the polynomial. *)
      p_i = List.mapi (fun i _ ->
        PZ_m.eval ~p:poly ~x:(i+1 |> G.Z_m.of_int)) public_keys in let

      (* yᵢ = pkᵢᵖ⁽ⁱ⁾ for i ∈ 1…n: the value of p(i) encrypted with pkᵢ,
         the public key of the party receiving the iᵗʰ party. The public
         keys use the g₂ generator of G. Thus pkᵢ = g₂ˢᵏⁱ *)
      y_i = List.map2 G.pow public_keys p_i and

      (* xᵢ = g₁ᵖ⁽ⁱ⁾ for in in 1…n: commitment to polynomial points *)
      x_i = List.map G.(pow g1) p_i in let

      equation = Dleq.setup_equation G.g1 x_i public_keys y_i in let
      proof = Dleq.make_proof equation p_i
    in (y_i, cC_j, proof)

  let check_dealer_proof y_i cC_j ~proof ~public_keys =

    (* Reconstruct Xᵢ from Cⱼ *)
    let x_i =
      (* prod_C_j_to_the__i_to_the_j = i ↦ Πⱼ₌₀ᵗ⁻¹ Cⱼ^(iʲ) *)
      let prod_C_j_to_the__i_to_the_j i =
        List.mapi (fun j cC ->G.pow cC (G.Z_m.pow i (Z.of_int j)))
          cC_j |> (List.fold_left G.( * ) G.e)
      in
      List.mapi (fun i _ ->
          prod_C_j_to_the__i_to_the_j (i+1 |> G.Z_m.of_int)) y_i
    in let
      equation = Dleq.setup_equation G.g1 x_i public_keys y_i in
    Dleq.check_proof equation proof

  (* reveal a share *)
  let reveal_share y ~secret_key ~public_key  =
    match G.Z_m.inv secret_key with
    | None -> failwith "Invalid secret key"
    | Some inverse_key ->
        let reveal = G.(pow y inverse_key) in
        (* y = g₂^(private_key) and public_key = reveal^(private_key) *)
        let equation = Dleq.setup_equation G.g2 [public_key] [reveal] [y] in
        let proof = Dleq.make_proof equation [secret_key] in
        (reveal, proof)

  (* check the validity of a revealed share *)
  let check_revealed_share share reveal ~public_key proof =
    let equation = Dleq.setup_equation G.g2 [public_key] [reveal] [share] in
    Dleq.check_proof equation proof

  (* reconstruct the secret *)
  let reconstruct reveals int_indices =
    (* check that there enough reveals *)
    let indices = List.map (fun x -> G.Z_m.of_int (1+x)) int_indices in
    let lagrange i =
      List.fold_left G.Z_m.( * ) G.Z_m.one (
        List.map (
          fun j ->
            if G.Z_m.(j = i) then G.Z_m.one else
              match G.Z_m.(inv (j - i)) with
              | None -> failwith "Unexpected error inverting scalar."
              | Some inverse -> G.Z_m.(j * inverse)
        ) indices)
    in let lagrange = List.map lagrange indices in
    List.fold_left G.( * ) G.e (List.map2 G.pow reveals lagrange)

end
