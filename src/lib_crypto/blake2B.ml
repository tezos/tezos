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

open Error_monad

(*-- Type specific Hash builder ---------------------------------------------*)

module type Name = sig
  val name : string
  val title : string
  val size : int option
end

module type PrefixedName = sig
  include Name
  val b58check_prefix : string
end

module Make_minimal (K : Name) = struct

  open Blake2
  type t = Blake2b.hash

  include K

  let size =
    match K.size with
    | None -> 32
    | Some x -> x

  let of_string_opt s =
    if String.length s <> size then
      None
    else
      Some (Blake2b.Hash (MBytes.of_string s))
  let of_string s =
    match of_string_opt s with
    | None ->
        generic_error "%s.of_string: wrong string size (%d)"
          K.name (String.length s)
    | Some h -> Ok h
  let of_string_exn s =
    match of_string_opt s with
    | None ->
        Format.kasprintf invalid_arg
          "%s.of_string: wrong string size (%d)"
          K.name (String.length s)
    | Some h -> h
  let to_string (Blake2b.Hash h) = MBytes.to_string h

  let of_hex s = of_string (Hex.to_string s)
  let of_hex_opt s = of_string_opt (Hex.to_string s)
  let of_hex_exn s = of_string_exn (Hex.to_string s)
  let to_hex s = Hex.of_string (to_string s)

  let pp ppf h =
    let `Hex h = to_hex h in
    Format.pp_print_string ppf h
  let pp_short ppf h =
    let `Hex h = to_hex h in
    Format.pp_print_string ppf (String.sub h 0 8)

  let of_bytes_opt b =
    if MBytes.length b <> size then
      None
    else
      Some (Blake2b.Hash b)
  let of_bytes_exn b =
    match of_bytes_opt b with
    | None ->
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            K.name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h -> h
  let of_bytes s =
    match of_bytes_opt s with
    | Some x -> Ok x
    | None ->
        generic_error "Failed to deserialize a hash (%s)" K.name
  let to_bytes (Blake2b.Hash h) = h

  (* let read src off = of_bytes_exn @@ MBytes.sub src off size *)
  (* let write dst off h = MBytes.blit (to_bytes h) 0 dst off size *)

  let hash_bytes ?key l =
    let state = Blake2b.init ?key size in
    List.iter (fun b -> Blake2b.update state b) l ;
    Blake2b.final state

  let hash_string ?key l =
    let key = Option.map ~f:Bigstring.of_string key in
    let state = Blake2b.init ?key size in
    List.iter (fun s -> Blake2b.update state (MBytes.of_string s)) l ;
    Blake2b.final state

  let path_length = 6

  (** Converts [key] to hex thus doubling its size then splits it into a list of
      length [path_length] where each element is one byte, or two characters,
      except the last one which contains the rest. *)
  let to_path key l =
    let `Hex key = to_hex key in
    String.sub key 0 2 :: String.sub key 2 2 ::
    String.sub key 4 2 :: String.sub key 6 2 ::
    String.sub key 8 2 :: String.sub key 10 (size * 2 - 10) :: l
  let of_path path =
    let path = String.concat "" path in
    of_hex_opt (`Hex path)
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn (`Hex path)

  let prefix_path p =
    let `Hex p = Hex.of_string p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (min (len - 10) (size * 2 - 10)) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

  let zero = of_hex_exn (`Hex (String.make (size * 2) '0'))

  include Compare.Make(struct
      type nonrec t = t
      let compare (Blake2b.Hash h1) (Blake2b.Hash h2) = MBytes.compare h1 h2
    end)

end

module Make (R : sig
    val register_encoding:
      prefix: string ->
      length:int ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base58.data) ->
      'a Base58.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal(K)

  (* Serializers *)

  let raw_encoding =
    let open Data_encoding in
    conv to_bytes of_bytes_exn (Fixed.bytes size)

  let hash =
    if Compare.Int.(size >= 8) then
      fun h -> Int64.to_int (MBytes.get_int64 (to_bytes h) 0)
    else if Compare.Int.(size >= 4) then
      fun h -> Int32.to_int (MBytes.get_int32 (to_bytes h) 0)
    else
      fun h ->
        let r = ref 0 in
        let h = to_bytes h in
        for i = 0 to size - 1 do
          r := MBytes.get_uint8 h i + 8 * !r
        done ;
        !r

  type Base58.data += Data of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length: size
      ~wrap: (fun s -> Data s)
      ~of_raw: of_string_opt
      ~to_raw: to_string

  include Helpers.Make(struct
      type nonrec t = t
      let title = title
      let name = name
      let b58check_encoding = b58check_encoding
      let raw_encoding = raw_encoding
      let compare = compare
      let equal = equal
      let hash = hash
    end)

end

module Generic_Merkle_tree (H : sig
    type t
    type elt
    val empty : t
    val leaf : elt -> t
    val node : t -> t -> t
  end) = struct

  let rec step a n =
    let m = (n+1) / 2 in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      a.(0)
    else if m mod 2 = 0 then
      step a m
    else begin
      a.(m+1) <- a.(m) ;
      step a (m+1)
    end

  let empty = H.empty

  let compute xs =
    match xs with
    | [] -> H.empty
    | [x] -> H.leaf x
    | _ :: _ :: _ ->
        let last = TzList.last_exn xs in
        let n = List.length xs in
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x -> a.(i) <- H.leaf x) xs ;
        step a n

  type path =
    | Left of path * H.t
    | Right of H.t * path
    | Op

  let rec step_path a n p j =
    let m = (n+1) / 2 in
    let p = if j mod 2 = 0 then Left (p, a.(j+1)) else Right (a.(j-1), p) in
    for i = 0 to m - 1 do
      a.(i) <- H.node a.(2*i) a.(2*i+1)
    done ;
    a.(m) <- H.node a.(n) a.(n) ;
    if m = 1 then
      p
    else if m mod 2 = 0 then
      step_path a m p (j/2)
    else begin
      a.(m+1) <- a.(m) ;
      step_path a (m+1) p (j/2)
    end

  let compute_path xs i =
    match xs with
    | [] -> invalid_arg "compute_path"
    | [_] -> Op
    | _ :: _ :: _ ->
        let last = TzList.last_exn xs in
        let n = List.length xs in
        if i < 0 || n <= i then invalid_arg "compute_path" ;
        let a = Array.make (n+1) (H.leaf last) in
        List.iteri (fun i x -> a.(i) <- H.leaf x) xs ;
        step_path a n Op i

  let rec check_path p h =
    match p with
    | Op ->
        H.leaf h, 1, 0
    | Left (p, r) ->
        let l, s, pos = check_path p h in
        H.node l r, s * 2, pos
    | Right (l, p) ->
        let r, s, pos = check_path p h in
        H.node l r, s * 2, pos + s

  let check_path p h =
    let h, _, pos = check_path p h in
    h, pos

end

let rec log2 x = if x <= 1 then 0 else 1 + log2 ((x+1) / 2)

module Make_merkle_tree
    (R : sig
       val register_encoding:
         prefix: string ->
         length:int ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base58.data) ->
         'a Base58.encoding
     end)
    (K : PrefixedName)
    (Contents: sig
       type t
       val to_bytes: t -> MBytes.t
     end) = struct

  include Make (R) (K)

  type elt = Contents.t
  let elt_bytes = Contents.to_bytes

  let empty = hash_bytes []

  include Generic_Merkle_tree(struct
      type nonrec t = t
      type nonrec elt = elt
      let empty = empty
      let leaf x = hash_bytes [Contents.to_bytes x]
      let node x y = hash_bytes [to_bytes x; to_bytes y]
    end)

  let path_encoding =
    let open Data_encoding in
    mu "path"
      (fun path_encoding ->
         union [
           case (Tag 240)
             ~title:"Left"
             (obj2
                (req "path" path_encoding)
                (req "right" encoding))
             (function Left (p, r) -> Some (p, r) | _ -> None)
             (fun (p, r) -> Left (p, r)) ;
           case (Tag 15)
             ~title:"Right"
             (obj2
                (req "left" encoding)
                (req "path" path_encoding))
             (function Right (r, p) -> Some (r, p) | _ -> None)
             (fun (r, p) -> Right (r, p)) ;
           case (Tag 0)
             ~title:"Op"
             unit
             (function Op -> Some () | _ -> None)
             (fun () -> Op)
         ])

  let bounded_path_encoding ?max_length () =
    match max_length with
    | None -> path_encoding
    | Some max_length ->
        let max_depth = log2 max_length in
        Data_encoding.check_size (max_depth * (size + 1) + 1) path_encoding

end

include
  Make_minimal (struct
    let name = "Generic_hash"
    let title = ""
    let size = None
  end)

let pp ppf h =
  let `Hex h = to_hex h in
  Format.pp_print_string ppf h
let pp_short ppf h =
  let `Hex h = to_hex h in
  Format.pp_print_string ppf (String.sub h 0 8)
