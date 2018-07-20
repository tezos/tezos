.. _howtorun:

How to run Tezos
================

In this section we discuss how to take part in the protocol that runs
the network.
There are two main ways to participate in the consensus, delegating
your coins and running a delegate.
To learn more about the protocol refer to :ref:`this section <proof-of-stake>`.


Delegating your coins
---------------------

If you don't want to deal with the complexity of running your own
delegate, you can always take part in the protocol by delegating your
coins to one.

Implicit accounts cannot have a delegate, so the first step is to
originate an account, transfer your tezzies there and set a delegate.
Notice that an originated account is a special case of a contract
without code, so it is still necessary to pay for its small storage
(see `originated_account`).

::

   tezos-client originate account alice_del for alice \
                                  transfering 1000 from alice \
                                  --delegate bob

As done before, we originate a contract *alice_del* with manager
*alice* and we fund it with 1kꜩ.
The interesting part is setting the delegate to *bob*, when
originating a contract the delegate is not set by default.
If you already own contracts that are delegatable you can change
the delegate with the command ``set delegate``.

Notice that only implicit accounts can be delegates, so your delegate
must by a *tz1* address.

Funds in implicit accounts which are not registered as delegates
do not participate in baking.


Running a delegate
------------------

We now explain how to run a delegate, which means running the 3
daemons for baking, endorsing and accusing.

In order to run a delegate you first need to register as one using
your implicit account:

::

   tezos-client register key bob as delegate

Once registered, a delegate can participate in the network
proportionally to how many rolls it is delegated, that is
its own balance plus the balance of originated accounts and
contracts that are delegated to it.

Rolls
~~~~~

In the network, rights for baking and endorsing are randomly assigned
to delegates proportionally to the number of rolls they have been
delegated.
A roll is just a block of 10K tezzies.

To know when you will be allowed to bake in the current cycle, you
might try the following RPCs (use ``tezos-client list known
addresses`` to check the address of your account *bob*).

::

   tezos-client rpc post /chains/main/blocks/head/helpers/rights/baking/delegate/tz1_xxxxxxxxxxx with {}
   { "ok":
        [ { "level": 1400.000000, "priority": 2.000000,
            "timestamp": "2017-05-19T03:21:52Z" },
          ...  ] }

When you obtain Alphanet coins from :ref:`the faucet<faucet>`, if you
are lucky to obtain more than one roll, you can register a delegate
using this identity.
Otherwise, you need to ask the faucet for more accounts, orginate a
account for each one and delegate them to the first.


Deposits
~~~~~~~~

When baking or endorsing a block, a *security deposit* (or *bond*) is
frozen for ``preserved_cycles`` cycles from the account of the
delegate.
Hence a delegate must have enough funds to be able to pay security
deposits for all the blocks it can potentially bake/endorse during
``preserved_cycles``.
The current deposits are *512tz* for baked block and *64tz* for
endorsement.

Being delegated coins doesn't mean that a delegate can spend them,
they only add up to its rolls count.
All the deposits that are necessary to run the deamons can only be put
down from the delegate account.


Baker
~~~~~

The baker is a daemon that once connected to an account, computes the
baking rights for that account, collects transactions from the mempool
and bakes a block at priority zero.
Note that the baker is the only program that needs direct access to
the node data directory for performance reasons.

Let's launch the daemon pointing to the standard node directory and
baking for user *bob*:

::

   tezos-baker-alpha run with local node ~/.tezos-node bob

Endorser
~~~~~~~~

The endorser is a daemon that once connected to an account, computes
the endorsing rights for that account and, upon reception of a new
block, verifies the validity of the block and emits an endorsement
operation.
It can endorse for a specific account or if omitted it endorses for
all accounts.

::

   tezos-endorser-alpha run


Accuser
~~~~~~~

The accuser is a daemon that monitors all blocks received on all
chains and looks for:

* bakers who signed two blocks at the same level
* endorsers who injected more than one endorsement operation for the
  same baking slot (more details :ref:`here<proof-of-stake>`)

Upon finding such irregularity, it will emit respectively a
double-baking or double-endorsing denunciation operation, which will
cause the offender to loose its security deposit.

::

   tezos-accuser-alpha run

Remember that having two bakers or endorsers running connected to the
same account could lead to double baking/endorsing and the loss of all
your bonds.
If you are worried about availability of your node when is its turn to
bake/endorse there are other ways than duplicating your credentials.
**Never** use the same account on two daemons.


Rewards
~~~~~~~

After ``preserved_cycles``, not only the delegate takes back control of
its frozen deposits but it also receives the rewards for its hard work
which amount to 16ꜩ to bake a block and ``ꜩ2 / <block_priority>`` for
endorsing a block.


Docker
~~~~~~

The docker image runs the daemons by default for all your keys.
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
