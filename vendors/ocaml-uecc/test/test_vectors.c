#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/obj_mac.h>
#include <openssl/bn.h>
#include <openssl/ec.h>
#include <openssl/sha.h>
#include <openssl/opensslv.h>

#define CURVE_P256 NID_X9_62_prime256v1
BIO* err_bio;

unsigned char msg[10][32];
const unsigned char seed[] = "Tadam!!";
const char sks[][65] =
    { "D63FF7D5D8FB7334287CB397F824B3567178BB635CD2FAA8A34D0B1BC65FDAF2",
      "9D585160C3AD171D7F4925F359C2B4C8992730DBAEE4D4D10B1E0E489CCA3404",
      "62FAA069EE7286D027747656EC736F29D20E5BF827F1D531B1A9DE215AF876F3",
      "2FE21C081FFA7CBD631E6F20B05B870D64A252A3B7E1A125C3D07E7B6BBFF41B",
      "BC8CB5B05C306B5561CCBAAFE3777C267A8CDEFA6D00B5CE2E65578DEA030F3C",
      "610B1C48263CFEA3AEF9C3EF7DE9D39899155BAD1EC66F50165453429032ED9C",
      "36149C23680AD42DE8C516DE685A52411086EB9556244A83393FC10CEE12F7C5",
      "0866AD7FBCE585B740799D508824A547E877CC5F7D64E121FF13016AAF74B734",
      "8D05026AF08E9FFDDB2C485E1A0F1D093C34C5D77962BF31F1437CF0D44CE9D5",
      "B495924045946A563F436F5408A68FA21B9782DFC56F44ED90B12130CF9D926A"
    };

const char pks[][67] =
    { "02CA5364C4302C38E93F8A4850E61A8FE6C27E386D4541B898F4E74BE5E6DD0256",
      "03D6008C2A656DD414C6869558A1E262F38BD5A142039FE84E750335C543B376B9",
      "03812447C00050CA921B05A6097C3F29F4ADFF23C4DD6062CBB114DD5B917D1995",
      "03BBF4A1B5DF7E66E1FFC67AD2778F5E3A78717027FEBFB940C0C3CFC73F052583",
      "0276285EE239631F904401C2C2A22CEFDF7590546ED3AA4E2B2759F16DD7709D6B",
      "03BD396999184DBCA1CBA0596A9BA2E973BFA1AF610F8458A1280DDCB9EAE9EA0E",
      "023F35281E1AC1EA0589BA9D7FE7C21BA331D2A7A12B3EF2EFE20BEC6639283769",
      "0345FFE8A13964727E2D27FD5471D8899CB3C3EE6EDDE81F2C8E19E2EA1FE98160",
      "03DF08C9BE891DA811A86914C58B0BE798769FC756A2BD80878B33D9E7373D99A3",
      "02823ACE5698ABE2F45C6C9BCB6920AA5183D0ECE663B6B93C213A02F4766CE6A5"
    };

void init_msgs() {
    SHA256(seed, 32, msg[0]);
    for (int i=1; i<10; i++) {
        SHA256(msg[i-1], 32, msg[i]);
    }
}

void test_encoding() {
    char* hex;
    EC_GROUP *g = EC_GROUP_new_by_curve_name(CURVE_P256);
    EC_KEY *k = EC_KEY_new_by_curve_name(CURVE_P256);
    if (!EC_KEY_generate_key(k)) goto error;
    const EC_POINT *pk = EC_KEY_get0_public_key(k);
    EC_POINT *pk2 = EC_POINT_new(g);
    hex = EC_POINT_point2hex(g, pk, POINT_CONVERSION_COMPRESSED, NULL);
    printf("Test: %s\n", hex);
    if (hex == NULL) goto error;
    if (EC_POINT_hex2point(g, hex, pk2, NULL) == NULL) goto error;
    OPENSSL_free(hex);
    if (EC_POINT_cmp(g, pk, pk2, NULL)) goto error;
    return;

 error:
    ERR_print_errors(err_bio);
    exit(EXIT_FAILURE);
}

void vectors_of_key(EC_GROUP *g, EC_KEY *k) {
    const BIGNUM *sk = EC_KEY_get0_private_key(k);
    const EC_POINT *pk = EC_KEY_get0_public_key(k);
    if (sk == NULL) goto error;
    if (pk == NULL) goto error;

    char *sk_hex = BN_bn2hex(sk);
    if (sk_hex == NULL) goto error;

    char *pk_hex = EC_POINT_point2hex(g, pk, POINT_CONVERSION_COMPRESSED, NULL);
    if (pk_hex == NULL) goto error;

    printf("\"%s\", \"%s\";\n\n", sk_hex, pk_hex);

    ECDSA_SIG *sig;
    const BIGNUM *r, *s;
    int valid;
    for (int i=0; i<10; i++) {
        sig = ECDSA_do_sign(msg[i], 32, k);
        valid = ECDSA_do_verify(msg[i], 32, sig, k);
        if (valid == -1) goto error;
        assert(valid);
        if (sig == NULL || r == NULL || s == NULL) goto error;
        ECDSA_SIG_get0(sig, &r, &s);
        printf("\"%s\", \"%s\";\n", BN_bn2hex(r), BN_bn2hex(s));
    }
    printf("\n");

    OPENSSL_free(sk_hex);
    OPENSSL_free(pk_hex);
    return;

 error:
    ERR_print_errors(err_bio);
    exit(EXIT_FAILURE);
}

int main(int argc, char** argv) {
    EC_GROUP *group;
    BIGNUM *sk;
    EC_POINT *pk;
    EC_KEY *k;

    OPENSSL_init();
    err_bio = BIO_new_fd(STDERR_FILENO, false);

    /* for (int i=0; i<10; i++) test_encoding(); */
    init_msgs();

    for (int i=0; i<10; i++) {
        printf("\"");
        for (int j=0; j<32; j++)
            printf("%02x", msg[i][j]);
        printf("\";\n");
    }

    group = EC_GROUP_new_by_curve_name(CURVE_P256);
    if (group == NULL) goto error;

    k = EC_KEY_new_by_curve_name(CURVE_P256);
    if (k == NULL) goto error;

    for (int i=0; i<sizeof(sks)/sizeof(sks[0]); i++) {
        /* if (!EC_KEY_generate_key(k)) goto error; */
        sk = BN_new();
        pk = EC_POINT_new(group);
        if (sk == NULL) goto error;
        if (!BN_hex2bn(&sk, sks[i])) goto error;
        if (EC_POINT_hex2point(group, pks[i], pk, NULL) == NULL) goto error;
        if (!EC_KEY_set_private_key(k, sk)) goto error;
        if (!EC_KEY_set_public_key(k, pk)) goto error;
        if (!EC_KEY_check_key(k)) goto error;
        vectors_of_key(group, k);
    }
    return EXIT_SUCCESS;

 error:
    ERR_print_errors(err_bio);
    return EXIT_FAILURE;
}
