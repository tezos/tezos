#! /usr/bin/env python

import sys
import os
import json
import bitcoin
import binascii
import numpy as np
import pysodium
from pyblake2 import blake2b
import unicodedata
import pysodium
from hashlib import sha256
import random
import string

def get_keys(mnemonic, email, password):
    salt = unicodedata.normalize(
    "NFKD", (email + password).decode("utf8")).encode("utf8")
    seed = bitcoin.mnemonic_to_seed(mnemonic, salt)
    pk, sk = pysodium.crypto_sign_seed_keypair(seed[0:32])
    pkh = blake2b(pk,20).digest()
    pkhb58 = bitcoin.bin_to_b58check(pkh, magicbyte=434591)
    return (sk, pk, pkh, pkhb58)

def random_email():
    rnd = lambda n: ''.join(random.choice(string.ascii_lowercase) for _ in range(n))
    return '%s.%s@tezos.example.org' % (rnd(8),rnd(8))

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
            'blinded_pkh': bitcoin.bin_to_b58check(blinded_pkh, magicbyte=16921055),
            'amount': amount
        }
        commitments.append(commitment)
    return commitments

# Generate dummy genesis information for a centralized alphanet faucet
def make_dummy_wallets(n, blind):
    # Not a realistic shape, but for an alphanet faucet it's better to
    # have less variance.
    amounts = np.random.pareto(10.0, n)
    amounts = amounts / sum(amounts) * 700e6
    wallets = {}
    secrets = {}
    for i in range(0, n):
        entropy = blake2b(str(i), 20, key=blind).digest()
        mnemonic = bitcoin.mnemonic.entropy_to_words(entropy)
        password = ''.join(random.choice(string.letters + string.digits) for _ in range(10))
        email    = random_email()
        sk, pk, pkh, pkh_b58 = get_keys(' '.join(mnemonic), email, password)
        amount = tez_to_int(amounts[i])
        wallets[pkh_b58] = amount
        secret = secret_code(pkh, blind)
        secrets[pkh_b58] = (mnemonic, email, password, amount, binascii.hexlify(secret))
    return wallets, secrets

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage: python create_genesis_info.py /path/to/json blind [dummy]"
        exit(1)
    blind = sys.argv[2]
    if len(sys.argv) == 4 and sys.argv[3] == "dummy":
        wallets, secrets = make_dummy_wallets(30000, blind)
        with open('secret_seeds.json', 'w') as f:
            json.dump([ { "pkh" : pkh,
                          "mnemonic" : mnemonic,
                          "email" : email,
                          "password" : password,
                          "amount" : str(amount),
                          "activation_code" : secret }
                        for pkh, (mnemonic, email, password, amount, secret) in secrets.iteritems()], f, indent=1)
    else:
        wallets = get_wallets( sys.argv[1] )

    commitments = genesis_commitments(wallets, blind)

    with open('commitments.json', 'w') as f:
        json.dump({
        "bootstrap_accounts": [
            [ "edsk4X12XaKRPHgDkgvMe4UWEiygx8AVrt9rpktmhu1uT2GCPU4dp7",
              "12000000000000" ],
            [ "edsk46ypB8PztxMDPMdVnEgjQmJhca7zMJvTMDrdwJaJ4mgm4qNmwE",
              "12000000000000" ],
            [ "edsk4JsBpWJH5cDtanNADY2D5Ygma1dUtxko8qaM2Af8FHGU52yLcW",
              "12000000000000" ],
            [ "edsk3b5GrQdRF1Pt3ccRjvyoNHTFrSXUKZufg2zQYhBumqS8kMfeGC",
              "12000000000000" ],
            [ "edsk3T8CRr8YK2vnjsZK2vDzCjpcWpMEUXMAzjeR1GWjmyhGaDHTNV",
              "12000000000000" ]
        ],
            "commitments": [
                (commitment['blinded_pkh'], str(commitment['amount']))
                for commitment in commitments if commitment['amount'] > 0
            ],
            "no_rewards_cycles": 7,
            "security_deposit_ramp_up_cycles": 64
        }, f, indent=1)
