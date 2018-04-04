type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Context : sig
  type flag =
    | Verify
    | Sign
    (** which parts of the context to initialize. *)

  type t
  (** Opaque data structure that holds context information
      (precomputed tables etc.).

      Do not create a new context object for each operation, as
      construction is far slower than all other API calls (~100 times
      slower than an ECDSA verification).

      A constructed context can safely be used from multiple threads
      simultaneously, but API call that take a non-const pointer to a
      context need exclusive access to it. In particular this is the
      case for secp256k1_context_destroy and
      secp256k1_context_randomize.

      Regarding randomization, either do it once at creation time (in
      which case you do not need any locking for the other calls), or
      use a read-write lock. *)

  val create : flag list -> t
  (** Create a secp256k1 context object. *)

  val clone : t -> t
  (** Copies a secp256k1 context object. *)

  val randomize : t -> buffer -> bool
  (** While secp256k1 code is written to be constant-time no matter
      what secret values are, it's possible that a future compiler may
      output code which isn't, and also that the CPU may not emit the
      same radio frequencies or draw the same amount power for all
      values.

      This function provides a seed which is combined into the
      blinding value: that blinding value is added before each
      multiplication (and removed afterwards) so that it does not
      affect function results, but shields against attacks which rely
      on any input-dependent behaviour.

      You should call this after secp256k1_context_create or
      secp256k1_context_clone, and may call this repeatedly
      afterwards. *)

end

module Key : sig
  type secret
  type public
  type _ t = private
    | Sk : buffer -> secret t
    | Pk : buffer -> public t

  val to_buffer : _ t -> buffer
  val length : _ t -> int
  val equal : 'a t -> 'a t -> bool
  val copy : 'a t -> 'a t

  (** {2 Aritmetic operations } *)

  val negate : Context.t -> 'a t -> 'a t
  val add_tweak : Context.t -> 'a t -> ?pos:int -> buffer -> 'a t
  val mul_tweak : Context.t -> 'a t -> ?pos:int -> buffer -> 'a t
  val neuterize : Context.t -> _ t -> public t option
  val neuterize_exn : Context.t -> _ t -> public t
  val combine : Context.t -> _ t list -> public t option
  val combine_exn : Context.t -> _ t list -> public t

  (** {2 Input/Output} *)

  val read_sk : Context.t -> ?pos:int -> buffer -> (secret t, string) result
  val read_sk_exn : Context.t -> ?pos:int -> buffer -> secret t
  val read_pk : Context.t -> ?pos:int -> buffer -> (public t, string) result
  val read_pk_exn : Context.t -> ?pos:int -> buffer -> public t
  val write : ?compress:bool -> Context.t -> ?pos:int -> buffer -> _ t -> int
  val to_bytes : ?compress:bool -> Context.t -> _ t -> buffer
end

module Sign : sig

  (** {2 Message} *)

  type msg

  val msg_of_bytes : ?pos:int -> buffer -> msg option
  val msg_of_bytes_exn : ?pos:int -> buffer -> msg
  val write_msg_exn : ?pos:int -> buffer -> msg -> int
  val write_msg : ?pos:int -> buffer -> msg -> (int, string) result
  val msg_to_bytes : msg -> buffer

  (** {2 Signature} *)

  type plain
  type recoverable
  type _ t = private
    | P : buffer -> plain t
    | R : buffer -> recoverable t

  val equal : 'a t -> 'a t -> bool
  val to_plain : Context.t -> recoverable t -> plain t

  (** {3 Input/Output} *)

  val read : Context.t -> ?pos:int -> buffer -> (plain t, string) result
  val read_exn : Context.t -> ?pos:int -> buffer -> plain t
  val read_der : Context.t -> ?pos:int -> buffer -> (plain t, string) result
  val read_der_exn : Context.t -> ?pos:int -> buffer -> plain t
  val read_recoverable : Context.t -> recid:int -> ?pos:int -> buffer -> (recoverable t, string) result
  val read_recoverable_exn : Context.t -> recid:int -> ?pos:int -> buffer -> recoverable t

  val write_exn : ?der:bool -> Context.t -> ?pos:int -> buffer -> _ t -> int
  val write : ?der:bool -> Context.t -> ?pos:int -> buffer -> _ t -> (int, string) result
  val to_bytes : ?der:bool -> Context.t -> _ t -> buffer
  val to_bytes_recid : Context.t -> recoverable t -> buffer * int

  (** {3 Sign} *)

  (** {4 Creation} *)

  val sign : Context.t -> sk:Key.secret Key.t -> msg:msg -> (plain t, string) result
  val sign_exn : Context.t -> sk:Key.secret Key.t -> msg:msg -> plain t
  val sign_recoverable : Context.t -> sk:Key.secret Key.t -> msg -> (recoverable t, string) result
  val sign_recoverable_exn : Context.t -> sk:Key.secret Key.t -> msg -> recoverable t

  (** {4 Direct write in buffers} *)

  val write_sign : Context.t -> sk:Key.secret Key.t -> msg:msg -> ?pos:int -> buffer -> (int, string) result
  val write_sign_exn : Context.t -> sk:Key.secret Key.t -> msg:msg -> ?pos:int -> buffer -> int
  val write_sign_recoverable : Context.t -> sk:Key.secret Key.t -> msg:msg -> ?pos:int -> buffer -> (int, string) result
  val write_sign_recoverable_exn : Context.t -> sk:Key.secret Key.t -> msg:msg -> ?pos:int -> buffer -> int

  (** {4 Verification} *)

  val verify_exn : Context.t -> pk:Key.public Key.t -> msg:msg -> signature:_ t -> bool
  val verify : Context.t -> pk:Key.public Key.t -> msg:msg -> signature:_ t -> (bool, string) result

  (** {4 Recovery} *)

  val recover_exn : Context.t -> signature:recoverable t -> msg:msg -> Key.public Key.t
  val recover : Context.t -> signature:recoverable t -> msg:msg -> (Key.public Key.t, string) result
end
