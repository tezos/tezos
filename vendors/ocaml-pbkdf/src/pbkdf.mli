(** {{:https://tools.ietf.org/html/rfc2898}RFC 2898} specifies two password-based
    key derivation functions (PBKDF1 and PBKDF2), which are abstracted over
    a specific hash/pseudorandom function. *)
module type S = sig
  (** [pbkdf1 password salt count dk_len] is [dk], the derived key of [dk_len] octets.
      The [salt] must be eight octets, [count] the iteration count.
      @raise Invalid_argument when either [salt] is not eight octets long or either
      [count] or [dk_len] are not valid. *)
  val pbkdf1 : password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int -> Cstruct.t

  (** [pbkdf2 password salt count dk_len] is [dk], the derived key of [dk_len] octets.
      @raise Invalid_argument when either [count] or [dk_len] are not valid *)
  val pbkdf2 : password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int32 -> Cstruct.t
end

(** Given a Hash/pseudorandom function, get the PBKDF *)
module Make (H: Nocrypto.Hash.S) : S

(** convenience [pbkdf1 hash password salt count dk_len] where the [hash] has to be provided explicitly *)
val pbkdf1 : hash:[`MD5 | `SHA1] -> password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int -> Cstruct.t

(** convenience [pbkdf2 prf password salt count dk_len] where the [prf] has to be provided explicitly *)
val pbkdf2 : prf:Nocrypto.Hash.hash -> password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int32 -> Cstruct.t
