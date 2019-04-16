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

module MakeRaw(H : sig
    type t
    val name: string
    val of_bytes_opt: MBytes.t -> t option
    val to_string: t -> string
    val of_string_opt: string -> t option
  end) = struct

  let of_bytes_exn s =
    match H.of_bytes_opt s with
    | None ->
        Format.kasprintf invalid_arg "of_bytes_exn (%s)" H.name
    | Some pk -> pk
  let of_bytes s =
    match H.of_bytes_opt s with
    | None -> generic_error "of_bytes (%s)" H.name
    | Some pk -> Ok pk

  let of_string_exn s =
    match H.of_string_opt s with
    | None ->
        Format.kasprintf invalid_arg "of_string_exn (%s)" H.name
    | Some pk -> pk
  let of_string s =
    match H.of_string_opt s with
    | None -> generic_error "of_string (%s)" H.name
    | Some pk -> Ok pk

  let to_hex s = Hex.of_string (H.to_string s)
  let of_hex_opt s = H.of_string_opt (Hex.to_string s)
  let of_hex_exn s =
    match H.of_string_opt (Hex.to_string s) with
    | Some x -> x
    | None ->
        Format.kasprintf invalid_arg "of_hex_exn (%s)" H.name
  let of_hex s =
    match of_hex_opt s with
    | None -> generic_error "of_hex (%s)" H.name
    | Some pk -> ok pk
end

module MakeB58(H : sig
    type t
    val name: string
    val b58check_encoding: t Base58.encoding
  end) = struct

  let of_b58check_opt s =
    Base58.simple_decode H.b58check_encoding s
  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" H.name
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        generic_error
          "Failed to read a b58check_encoding data (%s): %S"
          H.name s

  let to_b58check s = Base58.simple_encode H.b58check_encoding s
  let to_short_b58check s =
    String.sub
      (to_b58check s) 0
      (10 + String.length (Base58.prefix H.b58check_encoding))

end

module MakeEncoder(H : sig
    type t
    val title: string
    val name: string
    val to_b58check: t -> string
    val to_short_b58check: t -> string
    val of_b58check: string -> t tzresult
    val of_b58check_exn: string -> t
    val of_b58check_opt: string -> t option
    val raw_encoding: t Data_encoding.t
  end) = struct

  let pp ppf t =
    Format.pp_print_string ppf (H.to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (H.to_short_b58check t)

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (obj1 (req H.name H.raw_encoding))
      ~json:
        (def H.name
           ~title: (H.title ^ " (Base58Check-encoded)") @@
         conv
           H.to_b58check
           (Data_encoding.Json.wrap_error H.of_b58check_exn)
           string)

  let rpc_arg =
    RPC_arg.make
      ~name: H.name
      ~descr: (Format.asprintf "%s (Base58Check-encoded)" H.name)
      ~destruct:
        (fun s ->
           match H.of_b58check_opt s with
           | None ->
               Error (Format.asprintf
                        "failed to decode Base58Check-encoded data (%s): %S"
                        H.name s)
           | Some v -> Ok v)
      ~construct: H.to_b58check
      ()

  let param ?(name=H.name) ?(desc=H.title) t =
    Clic.param ~name ~desc
      (Clic.parameter (fun _ str -> Lwt.return (H.of_b58check str))) t

end

module MakeIterator(H : sig
    type t
    val encoding: t Data_encoding.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val hash: t -> int
  end) = struct

  module Set = struct
    include Set.Make(struct type t = H.t let compare = H.compare end)
    exception Found of elt
    let random_elt s =
      let n = Random.int (cardinal s) in
      try
        ignore
          (fold (fun x i -> if i = n then raise (Found x) ; i+1) s 0 : int) ;
        assert false
      with Found x -> x
    let encoding =
      Data_encoding.conv
        elements
        (fun l -> List.fold_left (fun m x -> add x m) empty l)
        Data_encoding.(list H.encoding)
  end

  module Table = struct
    include Hashtbl.Make(struct
        type t = H.t
        let hash = H.hash
        let equal = H.equal
      end)
    let encoding arg_encoding =
      Data_encoding.conv
        (fun h -> fold (fun k v l -> (k, v) :: l) h [])
        (fun l ->
           let h = create (List.length l) in
           List.iter (fun (k,v) -> add h k v) l ;
           h)
        Data_encoding.(list (tup2 H.encoding arg_encoding))
  end

  module Map = struct
    include Map.Make(struct type t = H.t let compare = H.compare end)
    let encoding arg_encoding =
      Data_encoding.conv
        bindings
        (fun l -> List.fold_left (fun m (k,v) -> add k v m) empty l)
        Data_encoding.(list (tup2 H.encoding arg_encoding))
  end

  module WeakRingTable = struct
    include WeakRingTable.Make(struct
        type t = H.t
        let hash = H.hash
        let equal = H.equal
      end)
    let encoding arg_encoding =
      Data_encoding.conv
        (fun h -> fold (fun k v l -> (k, v) :: l) h [])
        (fun l ->
           let h = create (List.length l) in
           List.iter (fun (k,v) -> add h k v) l ;
           h)
        Data_encoding.(list (tup2 H.encoding arg_encoding))
  end


end

module Make(H : sig
    type t
    val title: string
    val name: string
    val b58check_encoding: t Base58.encoding
    val raw_encoding: t Data_encoding.t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val hash: t -> int
  end) = struct

  include MakeB58(H)
  include MakeEncoder(struct
      include H
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
    end)
  include MakeIterator(struct
      include H
      let encoding = encoding
    end)

end
