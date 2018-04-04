/**********************************************************************
 * Copyright (c) 2013-2015 Pieter Wuille                              *
 * Distributed under the MIT software license, see the accompanying   *
 * file COPYING or http://www.opensource.org/licenses/mit-license.php.*
 **********************************************************************/

#include "secp256k1.h"

#include "util.h"
#include "num_impl.h"
#include "field_impl.h"
#include "scalar_impl.h"
#include "group_impl.h"
#include "ecmult_impl.h"
#include "ecmult_const_impl.h"
#include "ecmult_gen_impl.h"
#include "ecdsa_impl.h"
#include "eckey_impl.h"
#include "hash_impl.h"
#include "scratch_impl.h"

#define ARG_CHECK(cond) do { \
    if (EXPECT(!(cond), 0)) { \
        secp256k1_callback_call(&ctx->illegal_callback, #cond); \
        return 0; \
    } \
} while(0)

static void default_illegal_callback_fn(const char* str, void* data) {
    (void)data;
    fprintf(stderr, "[libsecp256k1] illegal argument: %s\n", str);
    abort();
}

static const secp256k1_callback default_illegal_callback = {
    default_illegal_callback_fn,
    NULL
};

static void default_error_callback_fn(const char* str, void* data) {
    (void)data;
    fprintf(stderr, "[libsecp256k1] internal consistency check failed: %s\n", str);
    abort();
}

static const secp256k1_callback default_error_callback = {
    default_error_callback_fn,
    NULL
};


struct secp256k1_context_struct {
    secp256k1_ecmult_context ecmult_ctx;
    secp256k1_ecmult_gen_context ecmult_gen_ctx;
    secp256k1_callback illegal_callback;
    secp256k1_callback error_callback;
};

secp256k1_context* secp256k1_context_create(unsigned int flags) {
    secp256k1_context* ret = (secp256k1_context*)checked_malloc(&default_error_callback, sizeof(secp256k1_context));
    ret->illegal_callback = default_illegal_callback;
    ret->error_callback = default_error_callback;

    if (EXPECT((flags & SECP256K1_FLAGS_TYPE_MASK) != SECP256K1_FLAGS_TYPE_CONTEXT, 0)) {
            secp256k1_callback_call(&ret->illegal_callback,
                                    "Invalid flags");
            free(ret);
            return NULL;
    }

    secp256k1_ecmult_context_init(&ret->ecmult_ctx);
    secp256k1_ecmult_gen_context_init(&ret->ecmult_gen_ctx);

    if (flags & SECP256K1_FLAGS_BIT_CONTEXT_SIGN) {
        secp256k1_ecmult_gen_context_build(&ret->ecmult_gen_ctx, &ret->error_callback);
    }
    if (flags & SECP256K1_FLAGS_BIT_CONTEXT_VERIFY) {
        secp256k1_ecmult_context_build(&ret->ecmult_ctx, &ret->error_callback);
    }

    return ret;
}

secp256k1_context* secp256k1_context_clone(const secp256k1_context* ctx) {
    secp256k1_context* ret = (secp256k1_context*)checked_malloc(&ctx->error_callback, sizeof(secp256k1_context));
    ret->illegal_callback = ctx->illegal_callback;
    ret->error_callback = ctx->error_callback;
    secp256k1_ecmult_context_clone(&ret->ecmult_ctx, &ctx->ecmult_ctx, &ctx->error_callback);
    secp256k1_ecmult_gen_context_clone(&ret->ecmult_gen_ctx, &ctx->ecmult_gen_ctx, &ctx->error_callback);
    return ret;
}

void secp256k1_context_destroy(secp256k1_context* ctx) {
    if (ctx != NULL) {
        secp256k1_ecmult_context_clear(&ctx->ecmult_ctx);
        secp256k1_ecmult_gen_context_clear(&ctx->ecmult_gen_ctx);

        free(ctx);
    }
}

void secp256k1_context_set_illegal_callback(secp256k1_context* ctx, void (*fun)(const char* message, void* data), const void* data) {
    if (fun == NULL) {
        fun = default_illegal_callback_fn;
    }
    ctx->illegal_callback.fn = fun;
    ctx->illegal_callback.data = data;
}

void secp256k1_context_set_error_callback(secp256k1_context* ctx, void (*fun)(const char* message, void* data), const void* data) {
    if (fun == NULL) {
        fun = default_error_callback_fn;
    }
    ctx->error_callback.fn = fun;
    ctx->error_callback.data = data;
}

secp256k1_scratch_space* secp256k1_scratch_space_create(const secp256k1_context* ctx, size_t init_size, size_t max_size) {
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(max_size >= init_size);

    return secp256k1_scratch_create(&ctx->error_callback, init_size, max_size);
}

void secp256k1_scratch_space_destroy(secp256k1_scratch_space* scratch) {
    secp256k1_scratch_destroy(scratch);
}

static int secp256k1_pubkey_load(const secp256k1_context* ctx, secp256k1_ge* ge, const secp256k1_pubkey* pubkey) {
    if (sizeof(secp256k1_ge_storage) == 64) {
        /* When the secp256k1_ge_storage type is exactly 64 byte, use its
         * representation inside secp256k1_pubkey, as conversion is very fast.
         * Note that secp256k1_pubkey_save must use the same representation. */
        secp256k1_ge_storage s;
        memcpy(&s, &pubkey->data[0], sizeof(s));
        secp256k1_ge_from_storage(ge, &s);
    } else {
        /* Otherwise, fall back to 32-byte big endian for X and Y. */
        secp256k1_fe x, y;
        secp256k1_fe_set_b32(&x, pubkey->data);
        secp256k1_fe_set_b32(&y, pubkey->data + 32);
        secp256k1_ge_set_xy(ge, &x, &y);
    }
    ARG_CHECK(!secp256k1_fe_is_zero(&ge->x));
    return 1;
}

static void secp256k1_pubkey_save(secp256k1_pubkey* pubkey, secp256k1_ge* ge) {
    if (sizeof(secp256k1_ge_storage) == 64) {
        secp256k1_ge_storage s;
        secp256k1_ge_to_storage(&s, ge);
        memcpy(&pubkey->data[0], &s, sizeof(s));
    } else {
        VERIFY_CHECK(!secp256k1_ge_is_infinity(ge));
        secp256k1_fe_normalize_var(&ge->x);
        secp256k1_fe_normalize_var(&ge->y);
        secp256k1_fe_get_b32(pubkey->data, &ge->x);
        secp256k1_fe_get_b32(pubkey->data + 32, &ge->y);
    }
}

int secp256k1_ec_pubkey_parse(const secp256k1_context* ctx, secp256k1_pubkey* pubkey, const unsigned char *input, size_t inputlen) {
    secp256k1_ge Q;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(pubkey != NULL);
    memset(pubkey, 0, sizeof(*pubkey));
    ARG_CHECK(input != NULL);
    if (!secp256k1_eckey_pubkey_parse(&Q, input, inputlen)) {
        return 0;
    }
    secp256k1_pubkey_save(pubkey, &Q);
    secp256k1_ge_clear(&Q);
    return 1;
}

int secp256k1_ec_pubkey_serialize(const secp256k1_context* ctx, unsigned char *output, size_t *outputlen, const secp256k1_pubkey* pubkey, unsigned int flags) {
    secp256k1_ge Q;
    size_t len;
    int ret = 0;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(outputlen != NULL);
    ARG_CHECK(*outputlen >= ((flags & SECP256K1_FLAGS_BIT_COMPRESSION) ? 33 : 65));
    len = *outputlen;
    *outputlen = 0;
    ARG_CHECK(output != NULL);
    memset(output, 0, len);
    ARG_CHECK(pubkey != NULL);
    ARG_CHECK((flags & SECP256K1_FLAGS_TYPE_MASK) == SECP256K1_FLAGS_TYPE_COMPRESSION);
    if (secp256k1_pubkey_load(ctx, &Q, pubkey)) {
        ret = secp256k1_eckey_pubkey_serialize(&Q, output, &len, flags & SECP256K1_FLAGS_BIT_COMPRESSION);
        if (ret) {
            *outputlen = len;
        }
    }
    return ret;
}

static void secp256k1_ecdsa_signature_load(const secp256k1_context* ctx, secp256k1_scalar* r, secp256k1_scalar* s, const secp256k1_ecdsa_signature* sig) {
    (void)ctx;
    if (sizeof(secp256k1_scalar) == 32) {
        /* When the secp256k1_scalar type is exactly 32 byte, use its
         * representation inside secp256k1_ecdsa_signature, as conversion is very fast.
         * Note that secp256k1_ecdsa_signature_save must use the same representation. */
        memcpy(r, &sig->data[0], 32);
        memcpy(s, &sig->data[32], 32);
    } else {
        secp256k1_scalar_set_b32(r, &sig->data[0], NULL);
        secp256k1_scalar_set_b32(s, &sig->data[32], NULL);
    }
}

static void secp256k1_ecdsa_signature_save(secp256k1_ecdsa_signature* sig, const secp256k1_scalar* r, const secp256k1_scalar* s) {
    if (sizeof(secp256k1_scalar) == 32) {
        memcpy(&sig->data[0], r, 32);
        memcpy(&sig->data[32], s, 32);
    } else {
        secp256k1_scalar_get_b32(&sig->data[0], r);
        secp256k1_scalar_get_b32(&sig->data[32], s);
    }
}

int secp256k1_ecdsa_signature_parse_der(const secp256k1_context* ctx, secp256k1_ecdsa_signature* sig, const unsigned char *input, size_t inputlen) {
    secp256k1_scalar r, s;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(sig != NULL);
    ARG_CHECK(input != NULL);

    if (secp256k1_ecdsa_sig_parse(&r, &s, input, inputlen)) {
        secp256k1_ecdsa_signature_save(sig, &r, &s);
        return 1;
    } else {
        memset(sig, 0, sizeof(*sig));
        return 0;
    }
}

int secp256k1_ecdsa_signature_parse_compact(const secp256k1_context* ctx, secp256k1_ecdsa_signature* sig, const unsigned char *input64) {
    secp256k1_scalar r, s;
    int ret = 1;
    int overflow = 0;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(sig != NULL);
    ARG_CHECK(input64 != NULL);

    secp256k1_scalar_set_b32(&r, &input64[0], &overflow);
    ret &= !overflow;
    secp256k1_scalar_set_b32(&s, &input64[32], &overflow);
    ret &= !overflow;
    if (ret) {
        secp256k1_ecdsa_signature_save(sig, &r, &s);
    } else {
        memset(sig, 0, sizeof(*sig));
    }
    return ret;
}

int secp256k1_ecdsa_signature_serialize_der(const secp256k1_context* ctx, unsigned char *output, size_t *outputlen, const secp256k1_ecdsa_signature* sig) {
    secp256k1_scalar r, s;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(output != NULL);
    ARG_CHECK(outputlen != NULL);
    ARG_CHECK(sig != NULL);

    secp256k1_ecdsa_signature_load(ctx, &r, &s, sig);
    return secp256k1_ecdsa_sig_serialize(output, outputlen, &r, &s);
}

int secp256k1_ecdsa_signature_serialize_compact(const secp256k1_context* ctx, unsigned char *output64, const secp256k1_ecdsa_signature* sig) {
    secp256k1_scalar r, s;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(output64 != NULL);
    ARG_CHECK(sig != NULL);

    secp256k1_ecdsa_signature_load(ctx, &r, &s, sig);
    secp256k1_scalar_get_b32(&output64[0], &r);
    secp256k1_scalar_get_b32(&output64[32], &s);
    return 1;
}

int secp256k1_ecdsa_signature_normalize(const secp256k1_context* ctx, secp256k1_ecdsa_signature *sigout, const secp256k1_ecdsa_signature *sigin) {
    secp256k1_scalar r, s;
    int ret = 0;

    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(sigin != NULL);

    secp256k1_ecdsa_signature_load(ctx, &r, &s, sigin);
    ret = secp256k1_scalar_is_high(&s);
    if (sigout != NULL) {
        if (ret) {
            secp256k1_scalar_negate(&s, &s);
        }
        secp256k1_ecdsa_signature_save(sigout, &r, &s);
    }

    return ret;
}

int secp256k1_ecdsa_verify(const secp256k1_context* ctx, const secp256k1_ecdsa_signature *sig, const unsigned char *msg32, const secp256k1_pubkey *pubkey) {
    secp256k1_ge q;
    secp256k1_scalar r, s;
    secp256k1_scalar m;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(secp256k1_ecmult_context_is_built(&ctx->ecmult_ctx));
    ARG_CHECK(msg32 != NULL);
    ARG_CHECK(sig != NULL);
    ARG_CHECK(pubkey != NULL);

    secp256k1_scalar_set_b32(&m, msg32, NULL);
    secp256k1_ecdsa_signature_load(ctx, &r, &s, sig);
    return (!secp256k1_scalar_is_high(&s) &&
            secp256k1_pubkey_load(ctx, &q, pubkey) &&
            secp256k1_ecdsa_sig_verify(&ctx->ecmult_ctx, &r, &s, &q, &m));
}

static SECP256K1_INLINE void buffer_append(unsigned char *buf, unsigned int *offset, const void *data, unsigned int len) {
    memcpy(buf + *offset, data, len);
    *offset += len;
}

static int nonce_function_rfc6979(unsigned char *nonce32, const unsigned char *msg32, const unsigned char *key32, const unsigned char *algo16, void *data, unsigned int counter) {
   unsigned char keydata[112];
   unsigned int offset = 0;
   secp256k1_rfc6979_hmac_sha256 rng;
   unsigned int i;
   /* We feed a byte array to the PRNG as input, consisting of:
    * - the private key (32 bytes) and message (32 bytes), see RFC 6979 3.2d.
    * - optionally 32 extra bytes of data, see RFC 6979 3.6 Additional Data.
    * - optionally 16 extra bytes with the algorithm name.
    * Because the arguments have distinct fixed lengths it is not possible for
    *  different argument mixtures to emulate each other and result in the same
    *  nonces.
    */
   buffer_append(keydata, &offset, key32, 32);
   buffer_append(keydata, &offset, msg32, 32);
   if (data != NULL) {
       buffer_append(keydata, &offset, data, 32);
   }
   if (algo16 != NULL) {
       buffer_append(keydata, &offset, algo16, 16);
   }
   secp256k1_rfc6979_hmac_sha256_initialize(&rng, keydata, offset);
   memset(keydata, 0, sizeof(keydata));
   for (i = 0; i <= counter; i++) {
       secp256k1_rfc6979_hmac_sha256_generate(&rng, nonce32, 32);
   }
   secp256k1_rfc6979_hmac_sha256_finalize(&rng);
   return 1;
}

const secp256k1_nonce_function secp256k1_nonce_function_rfc6979 = nonce_function_rfc6979;
const secp256k1_nonce_function secp256k1_nonce_function_default = nonce_function_rfc6979;

int secp256k1_ecdsa_sign(const secp256k1_context* ctx, secp256k1_ecdsa_signature *signature, const unsigned char *msg32, const unsigned char *seckey, secp256k1_nonce_function noncefp, const void* noncedata) {
    secp256k1_scalar r, s;
    secp256k1_scalar sec, non, msg;
    int ret = 0;
    int overflow = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(secp256k1_ecmult_gen_context_is_built(&ctx->ecmult_gen_ctx));
    ARG_CHECK(msg32 != NULL);
    ARG_CHECK(signature != NULL);
    ARG_CHECK(seckey != NULL);
    if (noncefp == NULL) {
        noncefp = secp256k1_nonce_function_default;
    }

    secp256k1_scalar_set_b32(&sec, seckey, &overflow);
    /* Fail if the secret key is invalid. */
    if (!overflow && !secp256k1_scalar_is_zero(&sec)) {
        unsigned char nonce32[32];
        unsigned int count = 0;
        secp256k1_scalar_set_b32(&msg, msg32, NULL);
        while (1) {
            ret = noncefp(nonce32, msg32, seckey, NULL, (void*)noncedata, count);
            if (!ret) {
                break;
            }
            secp256k1_scalar_set_b32(&non, nonce32, &overflow);
            if (!overflow && !secp256k1_scalar_is_zero(&non)) {
                if (secp256k1_ecdsa_sig_sign(&ctx->ecmult_gen_ctx, &r, &s, &sec, &msg, &non, NULL)) {
                    break;
                }
            }
            count++;
        }
        memset(nonce32, 0, 32);
        secp256k1_scalar_clear(&msg);
        secp256k1_scalar_clear(&non);
        secp256k1_scalar_clear(&sec);
    }
    if (ret) {
        secp256k1_ecdsa_signature_save(signature, &r, &s);
    } else {
        memset(signature, 0, sizeof(*signature));
    }
    return ret;
}

int secp256k1_ec_seckey_verify(const secp256k1_context* ctx, const unsigned char *seckey) {
    secp256k1_scalar sec;
    int ret;
    int overflow;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(seckey != NULL);

    secp256k1_scalar_set_b32(&sec, seckey, &overflow);
    ret = !overflow && !secp256k1_scalar_is_zero(&sec);
    secp256k1_scalar_clear(&sec);
    return ret;
}

int secp256k1_ec_pubkey_create(const secp256k1_context* ctx, secp256k1_pubkey *pubkey, const unsigned char *seckey) {
    secp256k1_gej pj;
    secp256k1_ge p;
    secp256k1_scalar sec;
    int overflow;
    int ret = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(pubkey != NULL);
    memset(pubkey, 0, sizeof(*pubkey));
    ARG_CHECK(secp256k1_ecmult_gen_context_is_built(&ctx->ecmult_gen_ctx));
    ARG_CHECK(seckey != NULL);

    secp256k1_scalar_set_b32(&sec, seckey, &overflow);
    ret = (!overflow) & (!secp256k1_scalar_is_zero(&sec));
    if (ret) {
        secp256k1_ecmult_gen(&ctx->ecmult_gen_ctx, &pj, &sec);
        secp256k1_ge_set_gej(&p, &pj);
        secp256k1_pubkey_save(pubkey, &p);
    }
    secp256k1_scalar_clear(&sec);
    return ret;
}

int secp256k1_ec_privkey_negate(const secp256k1_context* ctx, unsigned char *seckey) {
    secp256k1_scalar sec;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(seckey != NULL);

    secp256k1_scalar_set_b32(&sec, seckey, NULL);
    secp256k1_scalar_negate(&sec, &sec);
    secp256k1_scalar_get_b32(seckey, &sec);

    return 1;
}

int secp256k1_ec_pubkey_negate(const secp256k1_context* ctx, secp256k1_pubkey *pubkey) {
    int ret = 0;
    secp256k1_ge p;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(pubkey != NULL);

    ret = secp256k1_pubkey_load(ctx, &p, pubkey);
    memset(pubkey, 0, sizeof(*pubkey));
    if (ret) {
        secp256k1_ge_neg(&p, &p);
        secp256k1_pubkey_save(pubkey, &p);
    }
    return ret;
}

int secp256k1_ec_privkey_tweak_add(const secp256k1_context* ctx, unsigned char *seckey, const unsigned char *tweak) {
    secp256k1_scalar term;
    secp256k1_scalar sec;
    int ret = 0;
    int overflow = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(seckey != NULL);
    ARG_CHECK(tweak != NULL);

    secp256k1_scalar_set_b32(&term, tweak, &overflow);
    secp256k1_scalar_set_b32(&sec, seckey, NULL);

    ret = !overflow && secp256k1_eckey_privkey_tweak_add(&sec, &term);
    memset(seckey, 0, 32);
    if (ret) {
        secp256k1_scalar_get_b32(seckey, &sec);
    }

    secp256k1_scalar_clear(&sec);
    secp256k1_scalar_clear(&term);
    return ret;
}

int secp256k1_ec_pubkey_tweak_add(const secp256k1_context* ctx, secp256k1_pubkey *pubkey, const unsigned char *tweak) {
    secp256k1_ge p;
    secp256k1_scalar term;
    int ret = 0;
    int overflow = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(secp256k1_ecmult_context_is_built(&ctx->ecmult_ctx));
    ARG_CHECK(pubkey != NULL);
    ARG_CHECK(tweak != NULL);

    secp256k1_scalar_set_b32(&term, tweak, &overflow);
    ret = !overflow && secp256k1_pubkey_load(ctx, &p, pubkey);
    memset(pubkey, 0, sizeof(*pubkey));
    if (ret) {
        if (secp256k1_eckey_pubkey_tweak_add(&ctx->ecmult_ctx, &p, &term)) {
            secp256k1_pubkey_save(pubkey, &p);
        } else {
            ret = 0;
        }
    }

    return ret;
}

int secp256k1_ec_privkey_tweak_mul(const secp256k1_context* ctx, unsigned char *seckey, const unsigned char *tweak) {
    secp256k1_scalar factor;
    secp256k1_scalar sec;
    int ret = 0;
    int overflow = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(seckey != NULL);
    ARG_CHECK(tweak != NULL);

    secp256k1_scalar_set_b32(&factor, tweak, &overflow);
    secp256k1_scalar_set_b32(&sec, seckey, NULL);
    ret = !overflow && secp256k1_eckey_privkey_tweak_mul(&sec, &factor);
    memset(seckey, 0, 32);
    if (ret) {
        secp256k1_scalar_get_b32(seckey, &sec);
    }

    secp256k1_scalar_clear(&sec);
    secp256k1_scalar_clear(&factor);
    return ret;
}

int secp256k1_ec_pubkey_tweak_mul(const secp256k1_context* ctx, secp256k1_pubkey *pubkey, const unsigned char *tweak) {
    secp256k1_ge p;
    secp256k1_scalar factor;
    int ret = 0;
    int overflow = 0;
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(secp256k1_ecmult_context_is_built(&ctx->ecmult_ctx));
    ARG_CHECK(pubkey != NULL);
    ARG_CHECK(tweak != NULL);

    secp256k1_scalar_set_b32(&factor, tweak, &overflow);
    ret = !overflow && secp256k1_pubkey_load(ctx, &p, pubkey);
    memset(pubkey, 0, sizeof(*pubkey));
    if (ret) {
        if (secp256k1_eckey_pubkey_tweak_mul(&ctx->ecmult_ctx, &p, &factor)) {
            secp256k1_pubkey_save(pubkey, &p);
        } else {
            ret = 0;
        }
    }

    return ret;
}

int secp256k1_context_randomize(secp256k1_context* ctx, const unsigned char *seed32) {
    VERIFY_CHECK(ctx != NULL);
    ARG_CHECK(secp256k1_ecmult_gen_context_is_built(&ctx->ecmult_gen_ctx));
    secp256k1_ecmult_gen_blind(&ctx->ecmult_gen_ctx, seed32);
    return 1;
}

int secp256k1_ec_pubkey_combine(const secp256k1_context* ctx, secp256k1_pubkey *pubnonce, const secp256k1_pubkey * const *pubnonces, size_t n) {
    size_t i;
    secp256k1_gej Qj;
    secp256k1_ge Q;

    ARG_CHECK(pubnonce != NULL);
    memset(pubnonce, 0, sizeof(*pubnonce));
    ARG_CHECK(n >= 1);
    ARG_CHECK(pubnonces != NULL);

    secp256k1_gej_set_infinity(&Qj);

    for (i = 0; i < n; i++) {
        secp256k1_pubkey_load(ctx, &Q, pubnonces[i]);
        secp256k1_gej_add_ge(&Qj, &Qj, &Q);
    }
    if (secp256k1_gej_is_infinity(&Qj)) {
        return 0;
    }
    secp256k1_ge_set_gej(&Q, &Qj);
    secp256k1_pubkey_save(pubnonce, &Q);
    return 1;
}

#ifdef ENABLE_MODULE_ECDH
# include "ecdh.h"
#endif

#ifdef ENABLE_MODULE_RECOVERY
# include "recovery.h"
#endif

/* START OF CUSTOM CODE */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim value sizeof_secp256k1_num(value unit) {
    return Val_int(sizeof(secp256k1_num));
}

CAMLprim value ml_secp256k1_num_copy(value r, value a) {
    secp256k1_num_copy(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_get_bin(value r, value rlen, value a) {
    secp256k1_num_get_bin(Caml_ba_data_val(r), Int_val(rlen), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_set_bin(value r, value a, value alen) {
    secp256k1_num_set_bin(Caml_ba_data_val(r), Caml_ba_data_val(a), Int_val(alen));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mod_inverse(value r, value a, value m) {
    secp256k1_num_mod_inverse(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(m));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_jacobi(value a, value b) {
    return Val_int(secp256k1_num_jacobi(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_cmp(value a, value b) {
    return Val_int(secp256k1_num_cmp(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_eq(value a, value b) {
    return Val_bool(secp256k1_num_eq(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_num_add(value r, value a, value b) {
    secp256k1_num_add(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_sub(value r, value a, value b) {
    secp256k1_num_sub(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mul(value r, value a, value b) {
    secp256k1_num_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_mod(value r, value m) {
    secp256k1_num_mod(Caml_ba_data_val(r), Caml_ba_data_val(m));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_shift(value r, value bits) {
    secp256k1_num_shift(Caml_ba_data_val(r), Int_val(bits));
    return Val_unit;
}

CAMLprim value ml_secp256k1_num_is_zero(value a) {
    return Val_bool(secp256k1_num_is_zero(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_is_one(value a) {
    return Val_bool(secp256k1_num_is_one(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_is_neg(value a) {
    return Val_bool(secp256k1_num_is_neg(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_num_negate(value r) {
    secp256k1_num_negate(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_const (value r,
                                          value d7, value d6, value d5, value d4,
                                          value d3, value d2, value d1, value d0) {
    secp256k1_scalar s = SECP256K1_SCALAR_CONST(Int64_val(d7), Int64_val(d6), Int64_val(d5), Int64_val(d4),
                                                Int64_val(d3), Int64_val(d2), Int64_val(d1), Int64_val(d0));
    memcpy(Caml_ba_data_val(r), &s, sizeof(secp256k1_scalar));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_scalar_const(argv[0], argv[1], argv[2], argv[3],
                                     argv[4], argv[5], argv[6], argv[7],
                                     argv[8]);
}

CAMLprim value ml_secp256k1_scalar_clear(value r) {
    secp256k1_scalar_clear(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_get_bits(value a, value offset, value count) {
    return Val_int(secp256k1_scalar_get_bits(Caml_ba_data_val(a), Int_val(offset), Int_val(count)));
}

CAMLprim value ml_secp256k1_scalar_get_bits_var(value a, value offset, value count) {
    return Val_int(secp256k1_scalar_get_bits_var(Caml_ba_data_val(a), Int_val(offset), Int_val(count)));
}

CAMLprim value ml_secp256k1_scalar_set_b32(value r, value bin) {
    int overflow;
    secp256k1_scalar_set_b32(Caml_ba_data_val(r), Caml_ba_data_val(bin), &overflow);
    return Val_bool(overflow);
}

CAMLprim value ml_secp256k1_scalar_set_int(value r, value v) {
    secp256k1_scalar_set_int(Caml_ba_data_val(r), Int_val(v));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_get_b32(value bin, value a) {
    secp256k1_scalar_get_b32(Caml_ba_data_val(bin), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_add(value r, value a, value b) {
    return Val_int(secp256k1_scalar_add(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_scalar_cadd_bit(value r, value bit, value flag) {
    secp256k1_scalar_cadd_bit(Caml_ba_data_val(r), Int_val(bit), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_mul(value r, value a, value b) {
    secp256k1_scalar_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_shr_int(value r, value n) {
    return Val_int(secp256k1_scalar_shr_int(Caml_ba_data_val(r), Int_val(n)));
}

CAMLprim value ml_secp256k1_scalar_sqr(value r, value a) {
    secp256k1_scalar_sqr(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_inverse(value r, value a) {
    secp256k1_scalar_inverse(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_inverse_var(value r, value a) {
    secp256k1_scalar_inverse_var(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_negate(value r, value a) {
    secp256k1_scalar_negate(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_is_zero(value r) {
    return Val_bool(secp256k1_scalar_is_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_one(value r) {
    return Val_bool(secp256k1_scalar_is_one(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_even(value r) {
    return Val_bool(secp256k1_scalar_is_even(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_is_high(value r) {
    return Val_bool(secp256k1_scalar_is_high(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_scalar_cond_negate(value r, value flag) {
    int ret = secp256k1_scalar_cond_negate(Caml_ba_data_val(r), Bool_val(flag));
    return (ret == -1 ? Val_true : Val_false);
}

CAMLprim value ml_secp256k1_scalar_get_num(value r, value a) {
    secp256k1_scalar_get_num(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_order_get_num(value r) {
    secp256k1_scalar_order_get_num(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_scalar_eq(value a, value b) {
    return Val_bool(secp256k1_scalar_eq(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_mul_shift_var(value r, value a, value b, value shift) {
    secp256k1_scalar_mul_shift_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Int_val(shift));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_const (value r,
                                      value d7, value d6, value d5, value d4,
                                      value d3, value d2, value d1, value d0) {
    secp256k1_fe fe = SECP256K1_FE_CONST(Int64_val(d7), Int64_val(d6), Int64_val(d5), Int64_val(d4),
                                         Int64_val(d3), Int64_val(d2), Int64_val(d1), Int64_val(d0));
    memcpy(Caml_ba_data_val(r), &fe, sizeof(secp256k1_fe));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_fe_const(argv[0], argv[1], argv[2], argv[3],
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8]);
}

CAMLprim value ml_secp256k1_fe_storage_const (value r,
                                              value d7, value d6, value d5, value d4,
                                              value d3, value d2, value d1, value d0) {
    secp256k1_fe_storage fes = SECP256K1_FE_STORAGE_CONST(Int64_val(d7), Int64_val(d6), Int64_val(d5), Int64_val(d4),
                                                          Int64_val(d3), Int64_val(d2), Int64_val(d1), Int64_val(d0));
    memcpy(Caml_ba_data_val(r), &fes, sizeof(secp256k1_fe_storage));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_storage_const_bytecode (value * argv, int argn)
{
    return ml_secp256k1_fe_storage_const(argv[0], argv[1], argv[2], argv[3],
                                         argv[4], argv[5], argv[6], argv[7],
                                         argv[8]);
}

CAMLprim value ml_secp256k1_fe_normalize(value r) {
    secp256k1_fe_normalize(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalize_weak(value r) {
    secp256k1_fe_normalize_weak(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalize_var(value r) {
    secp256k1_fe_normalize_var(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_normalizes_to_zero(value r) {
    return Val_bool(secp256k1_fe_normalizes_to_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_normalizes_to_zero_var(value r) {
    return Val_bool(secp256k1_fe_normalizes_to_zero_var(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_set_int(value r, value a) {
    secp256k1_fe_set_int(Caml_ba_data_val(r), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_clear(value r) {
    secp256k1_fe_clear(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_is_zero(value r) {
    return Val_bool(secp256k1_fe_is_zero(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_is_odd(value r) {
    return Val_bool(secp256k1_fe_is_odd(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_equal(value a, value b) {
    return Val_bool(secp256k1_fe_equal(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_equal_var(value a, value b) {
    return Val_bool(secp256k1_fe_equal_var(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_cmp_var(value a, value b) {
    return Val_int(secp256k1_fe_cmp_var(Caml_ba_data_val(a), Caml_ba_data_val(b)));
}

CAMLprim value ml_secp256k1_fe_set_b32(value r, value a) {
    return Val_bool(secp256k1_fe_set_b32(Caml_ba_data_val(r), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_fe_get_b32(value a, value r) {
    secp256k1_fe_get_b32(Caml_ba_data_val(a), Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_negate(value r, value a, value m) {
    secp256k1_fe_negate(Caml_ba_data_val(r), Caml_ba_data_val(a), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_mul_int(value r, value a) {
    secp256k1_fe_mul_int(Caml_ba_data_val(r), Int_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_add(value r, value a) {
    secp256k1_fe_add(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_mul(value r, value a, value b) {
    secp256k1_fe_mul(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_sqr(value r, value a) {
    secp256k1_fe_sqr(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_sqrt(value r, value a) {
    return Val_bool(secp256k1_fe_sqrt(Caml_ba_data_val(r), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_fe_is_quad_var(value r) {
    return Val_bool(secp256k1_fe_is_quad_var(Caml_ba_data_val(r)));
}

CAMLprim value ml_secp256k1_fe_inv(value r, value a) {
    secp256k1_fe_inv(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_inv_var(value r, value a) {
    secp256k1_fe_inv_var(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_inv_all_var(value r, value a, value len) {
    secp256k1_fe_inv_all_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Long_val(len));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_to_storage(value r, value a) {
    secp256k1_fe_to_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_from_storage(value r, value a) {
    secp256k1_fe_from_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_storage_cmov(value r, value a, value flag) {
    secp256k1_fe_storage_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_fe_cmov(value r, value a, value flag) {
    secp256k1_fe_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_of_fields (value r, value x, value y, value infinity) {
    secp256k1_ge *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    g->infinity = Bool_val(infinity);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_of_fields (value r, value x, value y, value z, value infinity) {
    secp256k1_gej *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    memcpy(&g->z, Caml_ba_data_val(z), sizeof(secp256k1_fe));
    g->infinity = Bool_val(infinity);
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_storage_of_fields (value r, value x, value y) {
    secp256k1_ge_storage *g = Caml_ba_data_val(r);
    memcpy(&g->x, Caml_ba_data_val(x), sizeof(secp256k1_fe));
    memcpy(&g->y, Caml_ba_data_val(y), sizeof(secp256k1_fe));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_xy(value r, value x, value y) {
    secp256k1_ge_set_xy(Caml_ba_data_val(r), Caml_ba_data_val(x), Caml_ba_data_val(y));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_xquad(value r, value x) {
    return Val_bool(secp256k1_ge_set_xquad(Caml_ba_data_val(r), Caml_ba_data_val(x)));
}

CAMLprim value ml_secp256k1_ge_set_xo_var(value r, value x, value odd) {
    return Val_bool(secp256k1_ge_set_xo_var(Caml_ba_data_val(r), Caml_ba_data_val(x), Int_val(odd)));
}

CAMLprim value ml_secp256k1_ge_is_infinity(value a) {
    return Val_bool(secp256k1_ge_is_infinity(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_ge_is_valid_var(value a) {
    return Val_bool(secp256k1_ge_is_valid_var(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_ge_neg(value r, value a) {
    secp256k1_ge_neg(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_set_gej(value r, value a) {
    secp256k1_ge_set_gej(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_set_infinity(value r) {
    secp256k1_gej_set_infinity(Caml_ba_data_val(r));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_set_ge(value r, value a) {
    secp256k1_gej_set_ge(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_eq_x_var(value x, value a) {
    return Val_int(secp256k1_gej_eq_x_var(Caml_ba_data_val(x), Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_neg(value r, value a) {
    secp256k1_gej_neg(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_is_infinity(value a) {
    return Val_bool(secp256k1_gej_is_infinity(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_has_quad_y_var(value a) {
    return Val_bool(secp256k1_gej_has_quad_y_var(Caml_ba_data_val(a)));
}

CAMLprim value ml_secp256k1_gej_double_nonzero(value r, value a, value rzr) {
    secp256k1_gej_double_nonzero(Caml_ba_data_val(r), Caml_ba_data_val(a), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_double_var(value r, value a, value rzr) {
    secp256k1_gej_double_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_var(value r, value a, value b, value rzr) {
    secp256k1_gej_add_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_ge(value r, value a, value b) {
    secp256k1_gej_add_ge(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_ge_var(value r, value a, value b, value rzr) {
    secp256k1_gej_add_ge_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Is_block(rzr) ? Caml_ba_data_val(Field(rzr, 0)) : NULL);
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_add_zinv_var(value r, value a, value b, value bzinv) {
    secp256k1_gej_add_ge_var(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(b), Caml_ba_data_val(bzinv));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_clear(value a) {
    secp256k1_gej_clear(Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_clear(value a) {
    secp256k1_ge_clear(Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_to_storage(value r, value a) {
    secp256k1_ge_to_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_from_storage(value r, value a) {
    secp256k1_ge_from_storage(Caml_ba_data_val(r), Caml_ba_data_val(a));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ge_storage_cmov(value r, value a, value flag) {
    secp256k1_ge_storage_cmov(Caml_ba_data_val(r), Caml_ba_data_val(a), Bool_val(flag));
    return Val_unit;
}

CAMLprim value ml_secp256k1_gej_rescale(value r, value b) {
    secp256k1_gej_rescale(Caml_ba_data_val(r), Caml_ba_data_val(b));
    return Val_unit;
}

CAMLprim value ml_secp256k1_ecmult_const(value r, value a, value q) {
    secp256k1_ecmult_const(Caml_ba_data_val(r), Caml_ba_data_val(a), Caml_ba_data_val(q));
    return Val_unit;
}

CAMLprim value ml_secp256k1_eckey_pubkey_parse(value elem, value pub, value size) {
    return Val_bool(secp256k1_eckey_pubkey_parse(Caml_ba_data_val(elem), Caml_ba_data_val(pub), Long_val(size)));
}

CAMLprim value ml_secp256k1_eckey_pubkey_serialize(value elem, value pub, value size, value compressed) {
    size_t sz = Long_val(size);
    return (secp256k1_eckey_pubkey_serialize(Caml_ba_data_val(elem), Caml_ba_data_val(pub), &sz, Bool_val(compressed)) ? Val_long(sz) : Val_long(0));
}
