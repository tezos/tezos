(** {{:https://tools.ietf.org/html/rfc2898}RFC 2898} specifies two password-based
    key derivation functions (PBKDF1 and PBKDF2), which are abstracted over
    a specific hash/pseudorandom function. *)
module type S = sig
  (** [pbkdf2 password salt count dk_len] is [dk], the derived key of [dk_len] octets.
      @raise Invalid_argument when either [count] or [dk_len] are not valid *)
  val pbkdf2 : password:Bigstring.t -> salt:Bigstring.t -> count:int -> dk_len:int32 -> Bigstring.t
end

(** Given a Hash/pseudorandom function, get the PBKDF *)
module Make (H: Hacl.Hash.S) : S

module SHA256 : S
module SHA512 : S
