/* --------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  --------------------------------------------------------------------------- */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include "uECC.h"

#define Curve_val(v) (*((uECC_Curve *) Data_custom_val(v)))

#define Gen_custom_block(SNAME, CNAME, MNAME)                           \
    static int compare_##SNAME(value a, value b) {                      \
        CNAME aa = MNAME(a), bb = MNAME(b);                             \
        return (aa == bb ? 0 : (aa < bb ? -1 : 1));                     \
    }                                                                   \
                                                                        \
    static struct custom_operations uecc_##SNAME##_ops = {		\
        .identifier = "uecc_" #SNAME,					\
        .finalize = custom_finalize_default,                            \
        .compare = compare_##SNAME,                                     \
        .compare_ext = custom_compare_ext_default,                      \
        .hash = custom_hash_default,                                    \
        .serialize = custom_serialize_default,                          \
        .deserialize = custom_deserialize_default                       \
    };                                                                  \
                                                                        \
    static value alloc_##SNAME (CNAME a) {                              \
        value custom = alloc_custom(&uecc_##SNAME##_ops, sizeof(CNAME), 0, 1); \
        MNAME(custom) = a;                                              \
        return custom;                                                  \
    }

Gen_custom_block(curve, uECC_Curve, Curve_val)

CAMLprim value uECC_curve_stub(value kind) {
    CAMLparam1(kind);
    CAMLlocal1(ret);

    uECC_Curve c;
    switch(Int_val(kind)) {
    case 0:
        c = uECC_secp160r1();
        break;
    case 1:
        c = uECC_secp192r1();
        break;
    case 2:
        c = uECC_secp224r1();
        break;
    case 3:
        c = uECC_secp256r1();
        break;
    case 4:
        c = uECC_secp256k1();
        break;
    }

    ret = alloc_curve(c);
    CAMLreturn(ret);
}

CAMLprim value uECC_curve_private_key_size_stub(value curve) {
    return Val_int(uECC_curve_private_key_size(Curve_val(curve)));
}

CAMLprim value uECC_curve_public_key_size_stub(value curve) {
    return Val_int(uECC_curve_public_key_size(Curve_val(curve)));
}

CAMLprim value uECC_make_key_stub(value pk, value sk, value curve) {
    return Val_bool(uECC_make_key(Caml_ba_data_val(pk),
                                  Caml_ba_data_val(sk),
                                  Curve_val(curve)));
}

CAMLprim value uECC_valid_public_key_stub(value pk, value curve) {
    return Val_bool(uECC_valid_public_key(Caml_ba_data_val(pk),
                                          Curve_val(curve)));
}

CAMLprim value uECC_compute_public_key_stub(value sk, value pk, value curve) {
    return Val_bool(uECC_compute_public_key(Caml_ba_data_val(sk),
                                            Caml_ba_data_val(pk),
                                            Curve_val(curve)));
}

CAMLprim value uECC_compress_stub(value pk, value cpk, value curve) {
    uECC_compress(Caml_ba_data_val(pk),
                  Caml_ba_data_val(cpk),
                  Curve_val(curve));
    return Val_unit;
}

CAMLprim value uECC_decompress_stub(value cpk, value pk, value curve) {
    uECC_decompress(Caml_ba_data_val(cpk),
                    Caml_ba_data_val(pk),
                    Curve_val(curve));
    return Val_unit;
}

CAMLprim value uECC_shared_secret_stub(value pk, value sk, value secret, value curve) {
    return Val_bool(uECC_shared_secret(Caml_ba_data_val(pk),
                                       Caml_ba_data_val(sk),
                                       Caml_ba_data_val(secret),
                                       Curve_val(curve)));
}

CAMLprim value uECC_sign_stub(value sk, value msg, value signature, value curve) {
    return Val_bool(uECC_sign(Caml_ba_data_val(sk),
                              Caml_ba_data_val(msg),
                              Caml_ba_array_val(msg)->dim[0],
                              Caml_ba_data_val(signature),
                              Curve_val(curve)));
}

CAMLprim value uECC_verify_stub(value pk, value msg, value signature, value curve) {
    return Val_bool(uECC_verify(Caml_ba_data_val(pk),
                                Caml_ba_data_val(msg),
                                Caml_ba_array_val(msg)->dim[0],
                                Caml_ba_data_val(signature),
                                Curve_val(curve)));
}

/* --------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  --------------------------------------------------------------------------- */
