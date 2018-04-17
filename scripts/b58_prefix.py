#! /usr/bin/env python

import sys
import bitcoin

alphabet = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'


def b58dec(word):
    x = 0
    for c in word:
        x *= 58
        x += alphabet.find(c)
    return x

def asciidec(val):
    word = []
    while val > 0:
        word.append(val % 256)
        val /= 256
    return word[-1::-1]

if __name__ == '__main__':

    prefix = sys.argv[1]
    length = int(sys.argv[2])
    target = b58dec(prefix)

    shift = 8*(length+4)

    for m in range(1,1000):
        lo = target * 58**m
        lo = (lo >> shift) + (0 if lo == ((lo >> shift) << shift) else 1)
        hi = (target + 1) * 58**m - (1 << shift) +1
        hi = hi >> shift
        if hi >= lo:
            # test
            for bt in '\x00\xff':
                s = bitcoin.bin_to_b58check(bt * length, magicbyte=lo)
                assert s.startswith(prefix)
                assert len(s) == m + len(prefix)

            print m + len(prefix), lo, asciidec(lo)
            exit(0)
