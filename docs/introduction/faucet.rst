.. _faucet:

Get testnet coins
=================

If you wish to play with the Alphanet or the Zeronet, you must first
grab a wallet from the faucet:

https://faucet.tzalpha.net/

This will provide you with a JSON file, named like
``tz1__xxxxxxxxx__.json``.  Once your node is synchronized, you should
run the following command to activate your wallet, where ``my_account``
is a local name you choose, and ``tz1__xxxxxxxxx__.json`` is the name
of the file you grab:

::

    $ tezos-client activate account my_account with tz1__xxxxxxxxx__.json
    Operation successfully injected in the node.
    Operation hash is 'ooGoVS5cikbTHEimTzYhQWrYqY2LeJYmfkbzoiW8KQ59jtGQaXr'.
    Waiting for the operation to be included...
    Operation found in block: BKihN2QgSAu2etftNvs8FWWhwTvZiY8P3e7H3jgdj2MCpKZXXRs
    Account my_account (tz1__xxxxxxxxx__) created with ꜩ23,454.

Or, if you use the ``alphanet.sh`` (resp. ``zeronet.sh``) script, you
should prefix the file with ``container:`` in order to copy it into
the docker image:

::

    $ ./alphanet.sh client activate account my_account with container:tz1__xxxxxxxxx__.json

Please preserve the JSON file, after each reset of the Alphanet (or
Zeronet), you will have to reactivate the wallet.

Please drink carefully and don't abuse the faucet: it only contains
30.000 wallets for a total amount of ꜩ760.000.000.
