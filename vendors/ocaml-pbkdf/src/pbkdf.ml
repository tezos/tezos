open Nocrypto
open Uncommon

module type S = sig
  val pbkdf1 : password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int -> Cstruct.t
  val pbkdf2 : password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int32 -> Cstruct.t
end

module Make (H: Hash.S) : S = struct
  let pbkdf1 ~password ~salt ~count ~dk_len =
    if Cstruct.len salt <> 8 then invalid_arg "salt should be 8 bytes"
    else if count <= 0 then invalid_arg "count must be a positive integer"
    else if dk_len <= 0 then invalid_arg "derived key length must be a positive integer"
    else if dk_len > H.digest_size then invalid_arg "derived key too long"
    else
      let rec loop t = function
          0 -> t
        | i -> loop (H.digest t) (i - 1)
      in
      Cstruct.sub (loop (Cstruct.append password salt) count) 0 dk_len

  let pbkdf2 ~password ~salt ~count ~dk_len =
    if count <= 0 then invalid_arg "count must be a positive integer"
    else if dk_len <= 0l then invalid_arg "derived key length must be a positive integer"
    else
      let h_len = H.digest_size
      and dk_len = Int32.to_int dk_len in
      let l = cdiv dk_len h_len in
      let r = dk_len - (l - 1) * h_len in
      let block i =
        let rec f u xor = function
            0 -> xor
          | j -> let u = H.hmac ~key:password u in
            f u (Cs.xor xor u) (j - 1)
        in
        let int_i = Cstruct.create 4 in
        Cstruct.BE.set_uint32 int_i 0 (Int32.of_int i);
        let u_1 = H.hmac ~key:password (Cstruct.append salt int_i) in
        f u_1 u_1 (count - 1)
      in
      let rec loop blocks = function
          0 -> blocks
        | i -> loop ((block i)::blocks) (i - 1)
      in
      Cstruct.concat (loop [Cstruct.sub (block l) 0 r] (l - 1))
end

let pbkdf1 ~hash ~password ~salt ~count ~dk_len =
  let module H = (val (Hash.module_of hash)) in
  let module PBKDF = Make (H) in
  PBKDF.pbkdf1 ~password ~salt ~count ~dk_len

let pbkdf2 ~prf ~password ~salt ~count ~dk_len =
  let module H = (val (Hash.module_of prf)) in
  let module PBKDF = Make (H) in
  PBKDF.pbkdf2 ~password ~salt ~count ~dk_len
