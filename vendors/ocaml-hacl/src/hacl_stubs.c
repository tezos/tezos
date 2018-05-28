/* Copyright 2018 Vincent Bernardoff, Marco Stronati.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "Hacl_Unverified_Random.h"
CAMLprim value ml_randombytes(value buf) {
    randombytes(Caml_ba_data_val(buf),
                Caml_ba_array_val(buf)->dim[0]);
    return Val_unit;
}

#include "Hacl_HMAC_SHA2_256.h"
CAMLprim value ml_Hacl_HMAC_SHA2_256_hmac(value mac, value key, value data) {
    Hacl_HMAC_SHA2_256_hmac(Caml_ba_data_val(mac),
                            Caml_ba_data_val(key),
                            Caml_ba_array_val(key)->dim[0],
                            Caml_ba_data_val(data),
                            Caml_ba_array_val(data)->dim[0]);
    return Val_unit;
}

#include "Hacl_SHA2_256.h"
CAMLprim value ml_Hacl_SHA2_256_init(value state) {
    Hacl_SHA2_256_init(Caml_ba_data_val(state));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_256_update(value state, value data) {
    Hacl_SHA2_256_update(Caml_ba_data_val(state),
                         Caml_ba_data_val(data));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_256_update_last(value state, value data, value datalen) {
    Hacl_SHA2_256_update_last(Caml_ba_data_val(state),
                              Caml_ba_data_val(data),
                              Int_val(datalen));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_256_finish(value state, value hash) {
    Hacl_SHA2_256_finish(Caml_ba_data_val(state),
                         Caml_ba_data_val(hash));
    return Val_unit;
}

#include "Hacl_SHA2_512.h"
CAMLprim value ml_Hacl_SHA2_512_init(value state) {
    Hacl_SHA2_512_init(Caml_ba_data_val(state));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_512_update(value state, value data) {
    Hacl_SHA2_512_update(Caml_ba_data_val(state),
                         Caml_ba_data_val(data));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_512_update_last(value state, value data, value datalen) {
    Hacl_SHA2_512_update_last(Caml_ba_data_val(state),
                              Caml_ba_data_val(data),
                              Int_val(datalen));
    return Val_unit;
}

CAMLprim value ml_Hacl_SHA2_512_finish(value state, value hash) {
    Hacl_SHA2_512_finish(Caml_ba_data_val(state),
                         Caml_ba_data_val(hash));
    return Val_unit;
}

#include "Hacl_Curve25519.h"
CAMLprim value ml_Hacl_Curve25519_crypto_scalarmult(value pk, value sk, value basepoint) {
    Hacl_Curve25519_crypto_scalarmult(Caml_ba_data_val(pk),
                                      Caml_ba_data_val(sk),
                                      Caml_ba_data_val(basepoint));
    return Val_unit;
}

#include "NaCl.h"
CAMLprim value ml_NaCl_crypto_secretbox_easy(value c, value m, value n, value k) {
    NaCl_crypto_secretbox_easy(Caml_ba_data_val(c),
			       Caml_ba_data_val(m),
			       Caml_ba_array_val(m)->dim[0] - 32,
			       Caml_ba_data_val(n),
			       Caml_ba_data_val(k));
    return Val_unit;
}

CAMLprim value ml_NaCl_crypto_secretbox_open_detached(value m, value c, value mac,
						      value n, value k) {
    return Val_int(NaCl_crypto_secretbox_open_detached(Caml_ba_data_val(m),
						       Caml_ba_data_val(c),
						       Caml_ba_data_val(mac),
						       Caml_ba_array_val(c)->dim[0] - 32,
						       Caml_ba_data_val(n),
						       Caml_ba_data_val(k)));
}

CAMLprim value ml_NaCl_crypto_box_beforenm(value k, value pk, value sk) {
    NaCl_crypto_box_beforenm(Caml_ba_data_val(k),
                             Caml_ba_data_val(pk),
                             Caml_ba_data_val(sk));
    return Val_unit;
}

CAMLprim value ml_NaCl_crypto_box_easy_afternm(value c, value m, value n, value k) {
    NaCl_crypto_box_easy_afternm(Caml_ba_data_val(c),
				 Caml_ba_data_val(m),
				 Caml_ba_array_val(m)->dim[0] - 32,
				 Caml_ba_data_val(n),
				 Caml_ba_data_val(k));
    return Val_unit;
}

CAMLprim value ml_NaCl_crypto_box_open_easy_afternm(value m, value c, value n, value k) {
    return Val_int(NaCl_crypto_box_open_easy_afternm(Caml_ba_data_val(m),
						     Caml_ba_data_val(c),
						     Caml_ba_array_val(c)->dim[0] - 32,
						     Caml_ba_data_val(n),
						     Caml_ba_data_val(k)));
}

#include "Hacl_Ed25519.h"
CAMLprim value ml_Hacl_Ed25519_secret_to_public(value pk, value sk) {
    Hacl_Ed25519_secret_to_public(Caml_ba_data_val(pk),
                                  Caml_ba_data_val(sk));
    return Val_unit;
}

CAMLprim value ml_Hacl_Ed25519_sign(value sig, value sk, value m) {
    Hacl_Ed25519_sign(Caml_ba_data_val(sig),
                      Caml_ba_data_val(sk),
                      Caml_ba_data_val(m),
                      Caml_ba_array_val(m)->dim[0]);
    return Val_unit;
}

CAMLprim value ml_Hacl_Ed25519_verify(value pk, value m, value sig) {
    return Val_bool(Hacl_Ed25519_verify(Caml_ba_data_val(pk),
                                        Caml_ba_data_val(m),
                                        Caml_ba_array_val(m)->dim[0],
                                        Caml_ba_data_val(sig)));
}
