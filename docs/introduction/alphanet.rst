.. _alphanet:

Participating in the Alphanet
=============================

Welcome to the Tezos alphanet, which is a pre-release network for the
Tezos blockchain. Currently, the chain is reset every few weeks.

For news and support about the alphanet, please join IRC (``#tezos`` on
freenode). Please, report bugs related to the alphanet on the IRC
channel before filling Github issues.

For more information about the project in general, see:

https://www.tezos.com/

How to join the alphanet ?
--------------------------

We provide two ways of joining the alphanet :

-  use ``docker`` and prebuilt binaries (recommended way, tested on
   windows/mac/linux)
-  manual compilation and installation (linux and mac only)

The ``alphanet.sh`` script
~~~~~~~~~~~~~~~~~~~~~~~~~~

The recommended way for running an up-to-date Tezos node connected to
the alphanet is to use ``scripts/alphanet.sh``. Its only requirement is
a working installation of `Docker <https://www.docker.com/>`_.

First, you need to download the script:

::

    wget https://raw.githubusercontent.com/tezos/tezos/alphanet/scripts/alphanet.sh
    chmod +x alphanet.sh

You are now one step away from a working node:

::

    ./alphanet.sh start

This will launch a docker container running the various daemons that
form a working tezos node. The first launch might take a few minutes to
synchronize the chain.

On first launch the script will also create a cryptographic identity
(nicknamed ``my_identity``) and provide you with free tezzies on a fresh
account (nicknamed ``my_account``). You might check your balance with:

::

    ./alphanet.sh client get balance for my_account

On some circumstances the account creation might fail. If so, see
section “Known issues” below on how to force the account creation.

See ``./alphanet.sh --help`` for more informations about the script. In
particular see ``./alphanet.sh client --help`` and
``scripts/README.master`` for more information about the client.

Every call to ``alphanet.sh`` will check for updates of the node and
will fail if your node is not up-to-date. For updating the node, simply
run:

::

    ./alphanet.sh restart

If you prefer to temporarily disable automatic updates, you just have to
set an environment variable:

::

    export TEZOS_ALPHANET_DO_NOT_PULL=yes

Compilation from sources
~~~~~~~~~~~~~~~~~~~~~~~~

The ``alphanet`` branch in the tezos git repository will always contain
the up-to-date sources of the tezos-node required for running the
alphanet. See ``docs/README.master`` on how to compile it.

Once built, you might launch the a node by running:

::

    ./tezos-node identity generate 24.
    ./tezos-node run --rpc-addr localhost

By default this instance will store its data in ``$HOME/.tezos-node``
and will listen to incoming peers on port 9732. It will also listen to
RPC requests on port 8732 (only from ``localhost``). You might find more
options by running ``./tezos-node config --help``.

If you want to stake (see below for more details), you will also have to
run:

::

    ./tezos-client launch daemon

That’s all. For the rest of the document, to execute the example
commands, you will have to replace ``./alphanet.sh client`` by
``./tezos-client``.

How to observe the network ?
----------------------------

The alphanet script provides a basic command ``./alphanet.sh head`` that
allows you to see if your own node is synchronized.

The Tezos client also offers a lot of commands to introspect the state
of the node, and also to list and call the RPCs of the nodes.

Enthusiastic Tezos adopter fredcy has also developed a nice block
explorer for the alphanet. See https://github.com/fredcy/tezos-client.

In an upcoming version, we will also provide an opt-in tool for node
runners that will allow us to provide a global monitoring panel of the
alphanet.

How to obtain free Tez from the faucet contract ?
-------------------------------------------------

The alphanet contains an ad-hoc faucet contract, that will generate new
tezzies for you to test. Obviously, this contract will not be available
outside of the test network.

First, if you don’t have any cryptographic identity yet, you need to
generate one (replace ``my_identity`` with any name that suits you
best):

::

    ./alphanet.sh client gen keys "my_identity"

Then, you have to generate a new “free” account (replace ``my_account``
with any name that suits you best and ``my_identity`` by the name used
in the previous command):

::

    ./alphanet.sh client originate free account "my_account" for "my_identity"

That’s all. You might check your balance:

::

    ./alphanet.sh client get balance for "my_account"

If you want MORE tezzies, you need to generate as many free accounts as
you need (you should receive ꜩ100.000 per account) and then transfer the
tezzies into a single account. For instance:

::

    ./alphanet.sh client originate free account "my_alt_account" for "my_identity"
    ./alphanet.sh client transfer 100,000.00 from "my_alt_account" to "my_account" -fee 0.00
    ./alphanet.sh client forget contract "my_alt_account"

Note that the test network is kind enough to accept transactions without
fees…

How to play with smart-contracts ?
----------------------------------

An advanced documentation of the smart contract language is in

``/docs/language.md``

Some test contracts are in

``/tests/contracts/``

For details and examples, see:

http://www.michelson-lang.com/

How to stake on the alphanet ?
------------------------------

By default, the faucet of the alphanet (the one behind
``./alphanet.sh originate free account "my_account" for "my_identity"``)
creates contracts which are managed by ``my_identity`` but whose staking
rights are delegated to the baker of the block including the
origination. That way we are sure that staking rights are attributed to
an active baker.

But, nonetheless, you might claim your staking rights!

The following command returns the current delegate of a contract:

::

    ./alphanet.sh client get delegate for "my_account"

If it is one the following, it is indeed one of our “bootstrap”
contracts!

-  ``tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc``
-  ``tz1irovm9SKduvL3npv8kDM54PSWY5VJXoyz``
-  ``tz1UsgSSdRwwhYrqq7iVp2jMbYvNsGbWTozp``
-  ``tz1TwYbKYYJxw7AyubY4A9BUm2BMCPq7moaC``
-  ``tz1QWft73Zhj5VSA1sCuEi9HhDDJqywE6BtC``

You might change the delegate of a contract with a single command:

::

    ./alphanet.sh client set delegate for "my_account" to "my_identity"

You now have staking rights!

Well, almost.

You should wait.

A little bit.

At most two cycles. Which, on the alphanet is 128 blocks (something
around 2 hours). On the mainnet, this will be between 2 weeks and a
month.

But, to enforce your right a last step is required. When baking or
endorsing a block, a bond is taken out of the default account associated
to the public key of the delegate. Hence, in order to stake, you must be
provisioning for bond deposit.

::

    ./alphanet.sh client transfer 50,000.00 from "my_account" to "my_identity"

On the alphanet, a bond is ꜩ1000. Hence, with the previous command you
provisioned 50 bonds. If you want more, see section “How to obtain free
Tez from the faucet contract ?”.

Now, you are settled. The ``alphanet`` docker image runs a baker daemon
and a endorser daemon, by default for all your keys.

To know if you staked, just run:

::

    ./alphanet.sh baker log
    ./alphanet.sh endorser log

You should see lines such as:

::

    Injected block BLxzbB7PBW1axq for bootstrap5 after BLSrg4dXzL2aqq  (level 1381, slot 0, fitness 00::0000000000005441, operations 21)

Or:

::

    Injected endorsement for block 'BLSrg4dXzL2aqq'  (level 1381, slot 3, contract bootstrap5) 'oo524wKiEWBoPD'

On the alphanet, rewards for staking are credited after 24 hours. The
reward for baking or endorsing a block is ꜩ150. The safety bond is
returned together with the reward.

To know when you will be allowed to stake in the current cycle, you
might try the following RPCs, where you replaced ``tz1iFY8ads...`` by
the appropriate value:

::

    $ ./alphanet.sh client list known identities
    my_identity: tz1iFY8aDskx9QGbgBy68SNAGgkc7AE2iG9H (public key known) (secret key known)
    $ ./alphanet.sh client rpc call /blocks/head/proto/helpers/rights/baking/delegate/tz1iFY8aDskx9QGbgBy68SNAGgkc7AE2iG9H with '{}'
    { "ok":
        [ { "level": 1400.000000, "priority": 2.000000,
            "timestamp": "2017-05-19T03:21:52Z" },
          ...  ] }

Known issues
------------

Missing account ``my_account``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The chain synchronization has not been optimized yet and the
``alphanet.sh`` script might mis-detect the end of the synchronization
step. If so, it will try to create your free account in an outdated
context and your new account will never be included in the chain.

To fix this, just wait for your node to be synchronized: for that run
the following command, in the middle of a (raw) json object, it should
display the date of the last block (which should not be too far in the
past):

::

    ./alphanet.sh head

Please note that the printed date is GMT, don’t forget the time shift.

Then, you need to remove from the client state the non-existent contract
and regenerate a new one:

::

    ./alphanet.sh client forget contract "my_account"
    ./alphanet.sh client originate free account "my_account" for "my_identity"
