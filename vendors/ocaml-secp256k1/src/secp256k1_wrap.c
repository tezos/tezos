#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "secp256k1.h"
#include "secp256k1_recovery.h"

/* Accessing the secp256k1_context * part of an OCaml custom block */
#define Context_val(v) (*((secp256k1_context **) Data_custom_val(v)))

static void context_destroy(value ctx) {
    secp256k1_context_destroy (Context_val(ctx));
}

static struct custom_operations secp256k1_context_ops = {
    .identifier = "secp256k1_context",
    .finalize = context_destroy,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default
};

static value alloc_context (secp256k1_context *ctx) {
    value ml_ctx = alloc_custom(&secp256k1_context_ops, sizeof(secp256k1_context *), 0, 1);
    Context_val(ml_ctx) = ctx;
    return ml_ctx;
}

CAMLprim value caml_secp256k1_context_create (value flags) {
    CAMLparam1(flags);
    secp256k1_context *ctx = secp256k1_context_create (Int_val(flags));
    if (!ctx) caml_failwith("context_create");
    CAMLreturn(alloc_context(ctx));
}

CAMLprim value caml_secp256k1_context_randomize (value ctx, value seed) {
    return Val_bool(secp256k1_context_randomize(Context_val(ctx),
                                                Caml_ba_data_val(seed)));
}

CAMLprim value caml_secp256k1_context_clone (value ctx) {
    CAMLparam1(ctx);
    secp256k1_context *new = secp256k1_context_clone (Context_val(ctx));
    if (!new) caml_failwith("context_clone");
    CAMLreturn(alloc_context(new));
}

CAMLprim value caml_secp256k1_ec_seckey_verify (value ctx, value sk) {
    return Val_bool(secp256k1_ec_seckey_verify(Context_val(ctx),
                                               Caml_ba_data_val(sk)));
}

CAMLprim value caml_secp256k1_ec_privkey_negate(value ctx, value sk) {
    int ret = secp256k1_ec_privkey_negate(Context_val(ctx),
                                          Caml_ba_data_val(sk));
    return Val_unit;
}

CAMLprim value caml_secp256k1_ec_privkey_tweak_add(value ctx, value sk, value tweak) {
    return Val_bool(secp256k1_ec_privkey_tweak_add(Context_val(ctx),
                                                   Caml_ba_data_val(sk),
                                                   Caml_ba_data_val(tweak)));
}

CAMLprim value caml_secp256k1_ec_privkey_tweak_mul(value ctx, value sk, value tweak) {
    return Val_bool(secp256k1_ec_privkey_tweak_mul(Context_val(ctx),
                                                   Caml_ba_data_val(sk),
                                                   Caml_ba_data_val(tweak)));
}

CAMLprim value caml_secp256k1_ec_pubkey_create (value ctx, value buf, value sk) {
    return Val_bool(secp256k1_ec_pubkey_create (Context_val(ctx),
                                                Caml_ba_data_val(buf),
                                                Caml_ba_data_val(sk)));
}

CAMLprim value caml_secp256k1_ec_pubkey_serialize (value ctx, value buf, value pk) {
    size_t size = Caml_ba_array_val(buf)->dim[0];
    unsigned int flags =
        size == 33 ? SECP256K1_EC_COMPRESSED : SECP256K1_EC_UNCOMPRESSED;

    secp256k1_ec_pubkey_serialize(Context_val(ctx),
                                  Caml_ba_data_val(buf),
                                  &size,
                                  Caml_ba_data_val(pk),
                                  flags);

    return Val_int(size);
}

CAMLprim value caml_secp256k1_ec_pubkey_parse(value ctx, value buf, value pk) {
    return Val_bool(secp256k1_ec_pubkey_parse(Context_val(ctx),
                                              Caml_ba_data_val(buf),
                                              Caml_ba_data_val(pk),
                                              Caml_ba_array_val(pk)->dim[0]));
}

CAMLprim value caml_secp256k1_ec_pubkey_negate(value ctx, value pk) {
    int ret = secp256k1_ec_pubkey_negate(Context_val(ctx),
                                         Caml_ba_data_val(pk));
    return Val_unit;
}

CAMLprim value caml_secp256k1_ec_pubkey_tweak_add(value ctx, value pk, value tweak) {
    return Val_bool(secp256k1_ec_pubkey_tweak_add(Context_val(ctx),
                                                  Caml_ba_data_val(pk),
                                                  Caml_ba_data_val(tweak)));
}

CAMLprim value caml_secp256k1_ec_pubkey_tweak_mul(value ctx, value pk, value tweak) {
    return Val_bool(secp256k1_ec_pubkey_tweak_mul(Context_val(ctx),
                                                  Caml_ba_data_val(pk),
                                                  Caml_ba_data_val(tweak)));
}

CAMLprim value caml_secp256k1_ec_pubkey_combine(value ctx, value out, value pks) {
    int size = 0;
    const secp256k1_pubkey* cpks[1024] = {0};

    while(Field(pks, 1) != Val_unit) {
        cpks[size] = Caml_ba_data_val(Field(pks, 0));
        size++;
        pks = Field(pks, 1);
    }

    return Val_int(secp256k1_ec_pubkey_combine(Context_val(ctx),
                                               Caml_ba_data_val(out),
                                               cpks,
                                               size));
}

CAMLprim value caml_secp256k1_ecdsa_signature_parse_compact (value ctx, value buf, value sig) {
    return Val_bool(secp256k1_ecdsa_signature_parse_compact (Context_val(ctx),
                                                             Caml_ba_data_val(buf),
                                                             Caml_ba_data_val(sig)));
}

CAMLprim value caml_secp256k1_ecdsa_signature_parse_der (value ctx, value buf, value sig) {
    return Val_bool(secp256k1_ecdsa_signature_parse_der (Context_val(ctx),
                                                         Caml_ba_data_val(buf),
                                                         Caml_ba_data_val(sig),
                                                         Caml_ba_array_val(sig)->dim[0]));
}

CAMLprim value caml_secp256k1_ecdsa_signature_normalize(value ctx, value buf, value sig) {
    return Val_bool(secp256k1_ecdsa_signature_normalize (Context_val(ctx),
                                                         Caml_ba_data_val(buf),
                                                         Caml_ba_data_val(sig)));
}

CAMLprim value caml_secp256k1_ecdsa_verify (value ctx, value pubkey, value msg, value signature) {
    return Val_bool(secp256k1_ecdsa_verify (Context_val(ctx),
                                            Caml_ba_data_val(signature),
                                            Caml_ba_data_val(msg),
                                            Caml_ba_data_val(pubkey)));
}


CAMLprim value caml_secp256k1_ecdsa_sign (value ctx, value buf, value seckey, value msg) {
    return Val_bool(secp256k1_ecdsa_sign (Context_val(ctx),
                                          Caml_ba_data_val(buf),
                                          Caml_ba_data_val(msg),
                                          Caml_ba_data_val(seckey),
                                          NULL, NULL));
}

CAMLprim value caml_secp256k1_ecdsa_signature_serialize_der(value ctx, value buf, value signature) {
    size_t size = Caml_ba_array_val(buf)->dim[0];
    int ret = secp256k1_ecdsa_signature_serialize_der(Context_val(ctx),
                                                      Caml_ba_data_val(buf),
                                                      &size,
                                                      Caml_ba_data_val(signature));

    return (ret == 0 ? Val_int(ret) : Val_int(size));
}

CAMLprim value caml_secp256k1_ecdsa_signature_serialize_compact(value ctx, value buf, value signature) {
    secp256k1_ecdsa_signature_serialize_compact(Context_val(ctx),
                                                Caml_ba_data_val(buf),
                                                Caml_ba_data_val(signature));
    return Val_unit;
}

CAMLprim value caml_secp256k1_ecdsa_recoverable_signature_parse_compact (value ctx, value buf, value signature, value recid) {
    return Val_bool(secp256k1_ecdsa_recoverable_signature_parse_compact (Context_val(ctx),
                                                                         Caml_ba_data_val(buf),
                                                                         Caml_ba_data_val(signature),
                                                                         Int_val(recid)));
}

CAMLprim value caml_secp256k1_ecdsa_sign_recoverable (value ctx, value buf, value seckey, value msg) {
    return Val_bool(secp256k1_ecdsa_sign_recoverable (Context_val(ctx),
                                                      Caml_ba_data_val(buf),
                                                      Caml_ba_data_val(msg),
                                                      Caml_ba_data_val(seckey),
                                                      NULL, NULL));
}

CAMLprim value caml_secp256k1_ecdsa_recoverable_signature_serialize_compact(value ctx, value buf, value signature) {
    int recid;
    secp256k1_ecdsa_recoverable_signature_serialize_compact(Context_val(ctx),
                                                            Caml_ba_data_val(buf),
                                                            &recid,
                                                            Caml_ba_data_val(signature));
    return Val_int(recid);
}

CAMLprim value caml_secp256k1_ecdsa_recoverable_signature_convert(value ctx, value buf, value signature) {
    secp256k1_ecdsa_recoverable_signature_convert(Context_val(ctx),
                                                   Caml_ba_data_val(buf),
                                                   Caml_ba_data_val(signature));
    return Val_unit;
}

CAMLprim value caml_secp256k1_ecdsa_recover(value ctx, value buf, value signature, value msg) {
    return Val_bool(secp256k1_ecdsa_recover(Context_val(ctx),
                                            Caml_ba_data_val(buf),
                                            Caml_ba_data_val(signature),
                                            Caml_ba_data_val(msg)));
}
