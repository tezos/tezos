.. _alphanet:

Alphanet
========

Welcome to the Tezos alphanet, which is a pre-release network for the
Tezos blockchain. Currently, the chain is reset every few weeks.

For news and support about the alphanet, please join IRC (*#tezos* on
freenode). Please, report bugs related to the alphanet on the IRC
channel before filling `GitLab issues
<https://gitlab.com/tezos/tezos/issues>`__.

For more information about the project in general, see:

https://tezos.com/

How to join the alphanet
------------------------

We provide two ways of joining the alphanet :

-  use ``docker`` and prebuilt binaries (recommended way, tested on
   windows/mac/linux)
-  manual compilation and installation (linux and mac only)

The ``alphanet.sh`` script
~~~~~~~~~~~~~~~~~~~~~~~~~~

The recommended way for running an up-to-date Tezos node connected to
the alphanet is to use ``scripts/alphanet.sh``. Its only requirement is
a working installation of `Docker <https://www.docker.com/>`__.

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

Please refer to the :ref:`instructions<howto>`.

For the rest of the document, to execute the example commands, you
will have to replace ``./alphanet.sh client`` by ``./tezos-client``.

How to observe the network
--------------------------

The alphanet script provides a basic command ``./alphanet.sh head`` that
allows you to see if your own node is synchronized.

The Tezos client also offers a lot of commands to introspect the state
of the node, and also to list and call the RPCs of the nodes.

Enthusiastic Tezos adopter fredcy has also developed a nice block
explorer for the alphanet. See https://github.com/fredcy/tezos-client.

In an upcoming version, we will also provide an opt-in tool for node
runners that will allow us to provide a global monitoring panel of the
alphanet.

.. _faucet:

How to obtain free Tezzies
--------------------------

You must first grab a wallet from the `faucet
<https://faucet.tzalpha.net>`__.

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

Or, if you use the ``alphanet.sh`` script, you should prefix the file
with ``container:`` in order to copy it into the docker image:

::

    $ ./alphanet.sh client activate account my_account with container:tz1__xxxxxxxxx__.json

Please preserve the JSON file, after each reset of the Alphanet (or
Zeronet), you will have to reactivate the wallet.

Please drink carefully and don't abuse the faucet: it only contains
30.000 wallets for a total amount of ꜩ760.000.000.

How to play with smart-contracts
--------------------------------

An advanced documentation of the smart contract language is in

``/docs/language.md``

Some test contracts are in

``/tests/contracts/``

For details and examples, see:

https://www.michelson-lang.com/

How to bake on the alphanet
---------------------------

Baking 101
~~~~~~~~~~

In order to understand how baking works, please refer to :ref:`this
section <proof-of-stake>`. The following is a **TL;DR** to help you
get started.

In Tezos there are two kinds of accounts: *implicit* and *originated*
accounts. Originated accounts can have michelson code, in which case
they are also called *contracts*.

- An *implicit account* is identified by a public key hash and is
  automatically created when a public key hash is the recipient of a
  transfer. The genesis block will contain one account per
  contributor.

- An *originated account* or *contract* is *originated* with an
  operation sent to the blockchain. It has a *manager* and an optional
  *delegate* account.

The baking itself is done by *implicit accounts*. By default, they
don't participate in baking unless they are *registred* as
delegates. In order to register an *implicit account* as a delegate,
use:

::

   ./alphanet.sh client register key <mgr> as delegate

Once registered, an *implicit account* can participate in baking for
its own balance plus the balance of *originated accounts* and
*contracts* that are delegated to it.

Originated accounts and contracts thus participate in baking only via
their delegate. If they don't have a delegate set (the default, unless
you specify one), they don't participate in baking.

Implicit accounts cannot have a delegate. In order to delegate funds,
they need to be transfered to an *originated account* beforehand, and
a delegate must be set.

To summarize:

- *Implicit accounts* only can be registered as *delegates* and
  actually bake.

- Funds in *implicit accounts* which are not registered as *delegates*
  do not participate in baking.

- *Originated accounts* and *contracts* do not participate in baking
  unless they have a delegate account set **and** this delegate is
  actually baking.

In order to be entitled to bake, *delegates* need enough tezzies
(delegated or otherwise) to have a least one *roll*. Baking rights are
randomly chosen around rolls, which are blocks of 10K tezzies.

When you obtain Alphanet coins from :ref:`the faucet<faucet>`, if you
are lucky to obtain more than one roll, you can bake using this
identity (after it is registered as a delegate). Otherwise, you need
to ask the faucet for more coins. When you obtain more than 10K
tezzies into one or multiple identities, you are ready to start
baking!

Before we see how to bake with your delegate, you might want to know
how to delegate your tezzies. Although not necessary (you can just
bake with your delegate if it has enough tezzies), it is useful if you
want for example to avoid transfering all your coins in one
account.

Delegating your coins
~~~~~~~~~~~~~~~~~~~~~

As explained above, *implicit accounts* cannot have a delegate, so the
first step is to *originate* an *account* and transfer your tezzies
there. During the origination step, we will set the delegate.

::

   ./alphanet.sh client originate account <new> for <implicit> transfering <qty> from <implicit> --delegate <implicit>


Where ``<new>`` must be a contract alias that you choose,
``<implicit>`` is the alias of one *implicit account* that you own, and
``<qty>`` is the amount in tezzies that you want to transfer.

This will originate an *account*, transfer ``<qty>`` tezzies from
``<implicit>`` in it, and set that ``<implicit>`` is the manager and
the delegate for this freshly minted account.

If you already own contracts that are delegatable and want to change
the delegate to ``<implicit>``, use the following command:

::

    ./alphanet.sh client set delegate for <account> to <implicit>


You need to wait at most two cycles which, on the alphanet is 128
blocks (something about 2 hours). On the mainnet, this will be between
2 weeks and a month.

From now on, the funds in ``<new>`` will be delegated to
``<implicit>``. In the next section, we will learn how to bake with
your delegate.

Baking with your delegate
~~~~~~~~~~~~~~~~~~~~~~~~~

If you have read and followed the doc up until this point, you now
have a delegate, which have enough tezzies (delegated or otherwise) to
bake.

When baking or endorsing a block, a *security deposit* (or *bond*) is
taken out of the default account associated to the public key of the
delegate. Hence, in order to bake, your delegate must have enough
funds to be able to pay security deposits for baking and endorsing.

Check out the Alphanet *constants* for this:

::

    ./alphanet.sh client rpc call /blocks/head/proto/constants

Check for the ``endorsement_security_deposit`` and
``block_security_deposit`` keys of the JSON record. The value is in
*µtez*, one millionth of a tezzie. In the alphanet, the current value
is set to *512tz* per block and *64tz* per endorsement. If you run out
of funds, you will not be able to bake.

Now, you are settled. The ``alphanet`` docker image runs a baker
daemon and a endorser daemon, by default for all your keys.

To know if you baked, just run:

::

    ./alphanet.sh baker log
    ./alphanet.sh endorser log

You should see lines such as:

::

    Injected block BLxzbB7PBW1axq for bootstrap5 after BLSrg4dXzL2aqq  (level 1381, slot 0, fitness 00::0000000000005441, operations 21)

Or:

::

    Injected endorsement for block 'BLSrg4dXzL2aqq'  (level 1381, slot 3, contract bootstrap5) 'oo524wKiEWBoPD'

On the alphanet, rewards for staking are credited after 5 cycles (~10
hours). The reward for baking a block is ꜩ16 and ``ꜩ2 /
<block_priority>`` for endorsing a block. The safety bond is returned
together with the reward.

To know when you will be allowed to bake in the current cycle, you
might try the following RPCs, where you replaced ``tz1iFY8ads...`` by
the appropriate value:

::

    $ ./alphanet.sh client list known identities
    my_identity: tz1iFY8aDskx9QGbgBy68SNAGgkc7AE2iG9H (public key known) (secret key known)
    $ ./alphanet.sh client rpc call /blocks/head/proto/helpers/rights/baking/delegate/tz1iFY8aDskx9QGbgBy68SNAGgkc7AE2iG9H with '{}'
    [ { "level": 1400.000000, "priority": 2.000000,"timestamp": "2017-05-19T03:21:52Z" }, ...  ]

.. include:: alphanet_changes.rst
