open Uecc
open Vectors

let bigstring_of_hex hex_str =
  Cstruct.(to_bigarray (of_hex hex_str))

let msgs =
  List.map bigstring_of_hex msgs

let keys =
  List.map begin fun (sk, pk) ->
    match
      sk_of_bytes secp256r1 (bigstring_of_hex sk),
      pk_of_bytes secp256r1 (bigstring_of_hex pk) with
    | Some (sk, pk), Some pk' when pk = pk' -> sk, pk
    | _ -> failwith "invalid key"
  end keys

let sigs =
  List.map begin fun block ->
    List.map begin fun (r, s) ->
      let r = bigstring_of_hex r in
      let s = bigstring_of_hex s in
      Bigstring.concat "" [r; s]
    end block
  end sigs

let () =
  List.iter2 begin fun (_sk, pk) sigs ->
    List.iter2 begin fun msg signature ->
      assert (verify pk ~msg ~signature)
    end msgs sigs
  end keys sigs
