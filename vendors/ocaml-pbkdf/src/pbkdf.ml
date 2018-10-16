let xorbuf a b =
  let alen = Bigstring.length a in
  if Bigstring.length b <> alen then
    invalid_arg "xor: both buffers must be of same size" ;
  for i = 0 to alen - 1 do
    Bigstring.(set a i Char.(chr (code (get a i) lxor code (get b i))))
  done ;
  a

let cdiv x y =
  (* This is lifted from Nocrypto.Uncommon.(//)
     (formerly known as [cdiv]). It is part of the documented, publically
     exposed _internal_ utility library not for public consumption, hence
     the API break that prompted this copy-pasted function. *)
  if y < 1 then raise Division_by_zero else
  if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]

module type S = sig
  val pbkdf2 : password:Bigstring.t -> salt:Bigstring.t -> count:int -> dk_len:int32 -> Bigstring.t
end

module Make (H: Hacl.Hash.S) : S = struct
  let pbkdf2 ~password ~salt ~count ~dk_len =
    if count <= 0 then invalid_arg "count must be a positive integer" ;
    if dk_len <= 0l then invalid_arg "derived key length must be a positive integer" ;
    let h_len = H.bytes
    and dk_len = Int32.to_int dk_len in
    let l = cdiv dk_len h_len in
    let r = dk_len - (l - 1) * h_len in
    let block i =
      let rec f u xor = function
          0 -> xor
        | j -> let u = H.HMAC.digest ~key:password ~msg:u in
            f u (xorbuf xor u) (j - 1)
      in
      let int_i = Bigstring.create 4 in
      EndianBigstring.BigEndian.set_int32 int_i 0 (Int32.of_int i);
      let u_1 = H.HMAC.digest ~key:password ~msg:(Bigstring.concat "" [salt; int_i]) in
      f u_1 u_1 (count - 1)
    in
    let rec loop blocks = function
        0 -> blocks
      | i -> loop ((block i)::blocks) (i - 1)
    in
    Bigstring.concat "" (loop [Bigstring.sub (block l) 0 r] (l - 1))
end

module SHA256 = Make(Hacl.Hash.SHA256)
module SHA512 = Make(Hacl.Hash.SHA512)
