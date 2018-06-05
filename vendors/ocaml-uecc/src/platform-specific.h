/* Copyright 2015, Kenneth MacKay. Licensed under the BSD 2-clause license. */

#ifndef _UECC_PLATFORM_SPECIFIC_H_
#define _UECC_PLATFORM_SPECIFIC_H_

#include "types.h"

#if (defined(_WIN32) || defined(_WIN64))
/* Windows */

// use pragma syntax to prevent tweaking the linker script for getting CryptXYZ function
#pragma comment(lib, "crypt32.lib")
#pragma comment(lib, "advapi32.lib")

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <wincrypt.h>

static int default_RNG(uint8_t *dest, unsigned size) {
    HCRYPTPROV prov;
    if (!CryptAcquireContext(&prov, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT)) {
        return 0;
    }

    CryptGenRandom(prov, size, (BYTE *)dest);
    CryptReleaseContext(prov, 0);
    return 1;
}
#define default_RNG_defined 1

#elif (defined(__linux__) && (__GLIBC__ > 2 || __GLIBC_MINOR__ > 24)) || (defined __sun)
/* Linux and Solaris */

#include <sys/random.h>
static int default_RNG(uint8_t* dest, unsigned size) {
    ssize_t nb_written = getrandom(dest, size, 0);
    return ((nb_written == size) ? 1 : 0);
}
#define default_RNG_defined 1

#elif defined (__linux__) /* No glibc */
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>
static int default_RNG(uint8_t* dest, unsigned size) {
    int ret;
    ret = syscall(SYS_getrandom, dest, size, 0);
    if (ret != size)
        return 0;
    return 1;
}
#define default_RNG_defined 1

#elif defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#include <sys/param.h>
#if defined(BSD)
/* OSX and BSDs */

#include <stdlib.h>
static int default_RNG(uint8_t* dest, unsigned size) {
    arc4random_buf(dest, size);
    return 1;
}
#define default_RNG_defined 1
#endif /* defined(BSD) */

#endif /* platform */

#endif /* _UECC_PLATFORM_SPECIFIC_H_ */
