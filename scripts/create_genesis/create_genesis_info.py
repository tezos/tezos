#! /usr/bin/env python

import sys
import os
import json
import bitcoin
import binascii
import numpy as np
import pysodium
from pyblake2 import blake2b

def tez_to_int(amount):
    return int(round(amount * 1e6, 0))

def allocate_with_subthreshold( w ):
    from_ether = sum(x[u'tezzies'] for x in w[u'ethDeposits'])
    def discount_level( level ):
        if level >= 2000:
            return 0
        return 6000 - (level/400)*250
    from_bitcoin = sum(x[u'satoshis'] * discount_level(x[u'crowdfund_level']) for x in w[u'utxos'])
    return from_ether + from_bitcoin * 1e-8

def get_wallets(path):
    wallets = {}
    for fn in os.listdir(path):
        # ignore misc files
        if not fn.startswith("tz1"):
            continue
        w = json.load(open(os.path.join(path, fn), "r"))
        # if not u'allocated_tezzies' in w.keys():
        #    continue
        wallets[fn.split(".")[0]] = allocate_with_subthreshold(w)
    return wallets

def secret_code(pkh, blind):
    return blake2b(pkh, 20, key=blind).digest()

def genesis_commitments(wallets, blind):
    commitments = []
    for pkh_b58, amount in wallets.iteritems():
        # Public key hash corresponding to this Tezos address.
        pkh = bitcoin.b58check_to_bin(pkh_b58)[2:]
        # The redemption code is unique to the public key hash and deterministically
        # constructed using a secret blinding value.
        secret = secret_code(pkh, blind)
        # The redemption code is used to blind the pkh
        blinded_pkh = blake2b(pkh, 20, key=secret).digest()
        commitment = {
            'half_pkh': binascii.hexlify(pkh[:10]),
            'blinded_pkh': binascii.hexlify(blinded_pkh),
            'amount': tez_to_int(amount)
        }
        commitments.append(commitment)
    return commitments

# Generate dummy genesis information for a centralized alphanet faucet
def make_dummy_wallets(n, blind):
    # Not a realistic shape, but for an alphanet faucet it's better to
    # have less variance.
    amounts = np.random.pareto(10.0, n)
    amounts = amounts / sum(amounts) * 760e6
    wallets = {}
    secret_seeds = {}
    for i in range(0, n):
        seed = blake2b(str(i), 32, key=blind).digest()
        pk, sk = pysodium.crypto_sign_seed_keypair(seed)
        pkh = blake2b(pk, 20).digest()
        pkh_b58 = bitcoin.bin_to_b58check(pkh, magicbyte=434591)
        amount = tez_to_int(amounts[i])
        wallets[pkh_b58] = amount
        secret = secret_code(pkh, blind)
        secret_seeds[pkh_b58] = (bitcoin.bin_to_b58check(seed, magicbyte=219101703), amount, binascii.hexlify(secret))
    return wallets, secret_seeds

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage: python create_genesis_info.py /path/to/json blind [dummy]"
        exit(1)
    blind = sys.argv[2]
    if len(sys.argv) == 4 and sys.argv[3] == "dummy":
        wallets, secret_seeds = make_dummy_wallets(30000, blind)
        with open('secret_seeds.json', 'w') as f:
            json.dump([(pkh, sk, a, r) for pkh, (sk, a, r) in secret_seeds.iteritems()], f, indent=1)
    else:
        wallets = get_wallets( sys.argv[1] )

    commitments = genesis_commitments(wallets, blind)

    print "let commitments = ["
    print ";\n".join([ '"%s", "%s", %d' %
                       ( commitment['half_pkh'],
                         commitment['blinded_pkh'],
                         commitment['amount'] )
                       for commitment in commitments if commitment['amount'] > 0])
    print "]"
