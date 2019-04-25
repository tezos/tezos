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
originate an account, transfer your tez there and set a delegate.
Notice that an originated account is a special case of a contract
without code, so it is still necessary to pay for its small storage
(see :ref:`Originated Account <originated-accounts>`).

::

   tezos-client originate account alice_del for alice \
                                  transferring 1000 from alice \
                                  --delegate bob

As done before, we originate a contract *alice_del* with manager
*alice* and we fund it with 1kꜩ. The interesting part here is setting the
delegate to *bob*, as the delegate is not set by default when originating a
contract. If you already own contracts that are delegatable you can change the
delegate with the command ``set delegate``.


Notice that, by default, an originated account is not *delegatable*,
which means that you can't change the delegate once the contract is
originated, even if you initially set a delegate.
To be able to change the delegate latter, add the
``--delegatable`` flag.

Notice that only implicit accounts can be delegates, so your delegate
must be a *tz1* address.

Funds in implicit accounts which are not registered as delegates
do not participate in baking.


Running a delegate
------------------

A delegate is responsible for baking blocks, endorsing blocks and
accusing other delegates in case they try to double bake or double
endorse.

In the network, rights for baking and endorsing are randomly assigned
to delegates proportionally to the number of rolls they have been
delegated.
A roll is just a block of 10kꜩ and all computations with rolls are
rounded to the nearest lower integer e.g. if you have 16kꜩ it amounts
to 1 roll.

When you obtain coins from :ref:`the faucet<faucet>`, if you
are lucky to obtain more than one roll, you can register a delegate
using this identity.
Otherwise, you need to ask the faucet for more accounts, originate an
account for each one and delegate them to the first.

Deposits and over-delegation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When baking or endorsing a block, a *security deposit* (or *bond*) is
frozen for ``preserved_cycles`` cycles from the account of the
delegate.
Hence a delegate must have enough funds to be able to pay security
deposits for all the blocks it can potentially bake/endorse during
``preserved_cycles``.
The current deposits are *512ꜩ* for baked block and *64ꜩ* for
endorsement.
Note that being delegated coins doesn't mean that a delegate can spend
them, they only add up to its rolls count while all the deposits must
come from the delegate's account.

If a delegate runs out of funds to deposit it won't be able to bake or
endorse. Other than being a missed opportunity for them, this has also
negative consequences on the network.
Missing baking slots slows the network, as it is necessary to wait one
minute for the baker at priority 2 to bake, while missing endorsements
reduce the fitness of the chain, making it more susceptible to forks.
Running out of funds can happen if a delegate is *over-delegated*,
that is if the amount of rolls it was delegate is disproportionate
with respect to its available funds.
It is the responsibility of every delegator to make sure a delegate is
not already over-delegated (a delegate cannot refuse a delegation) and
each delegate should plan carefully its deposits.

.. _expected_rights:

Expected rights, deposits and rewards
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's assume we have 1 roll, we want to estimate our chances to bake
or endorse in order to prepare the funds for our deposits.
Our chances depend on how many rolls are currently active in the
network, once we know that we can estimate how many blocks and
endorsements we could be assigned in a cycle.
The number of active rolls can be computed with two RPCs, first we
list all the active delegates with ``delegates?active``, then we sum
all their ``stacking_balance`` and we simply divide by the size of a
roll, 10kꜩ.
At the time of writing, on Betanet the number of active rolls is ~30k
so for each block we know that the chance that we get selected for
baking is ``1/30k`` while for endorsing is 32 times that.
Given that every draw is with replacement, the distribution that
describes our chances of being selected is the binomial with
probability of success ``p=1/30k``.
The distribution has another parameter ``n`` for the number of times
we draw, in our case in a cycle the draws for baking are ``n_b =
4096`` while for endorsing are ``n_e = 4096 * 32``.
Moreover we could extend ``n`` to cover ``preserved_cycles = 5``.
Once we have ``p`` and ``n``, the expected number of times that we
might get selected is ``p * n`` (the mean of the distribution).
Over many cycles our chances will fall around the mean, in some cycles
we might get unlucky and be assigned fewer rights, but in some cycles we might
get lucky and be assigned more rights!
Clearly we would like to plan ahead and have enough deposits to cover
also the "lucky" cycles so we need to compute a sort of "maximum"
number of rights that is safe for `most cases`.
We can compute this maximum using the inverse of Cumulative
Distribution Function of the Binomial where `most cases` is a value of
confidence that we can put to 95%.
There a simple `Python
script <https://gitlab.com/paracetamolo/utils/blob/master/estimated-rights.py>`_
that does the computation for us and returns the deposits and rewards,
expected and maximum, for a cycle and for `preserved_cycles`.

::

   prob success 3.333333e-05
   confidence   0.95
   ----------one-cycle--------------------
   blocks
    mean 0.14
    max  1.00
   endorsements
    mean 4.37
    max  8.00
   deposits
    mean 69.91 + 279.62
    max  512.00 + 512.00
   rewards
    mean 2.18 + 8.74
    max  16.00 + 16.00
   ----------preserved-cycles-------------
   blocks
    mean 0.68
    max  2.00
   endorsements
    mean 21.85
    max  30.00
   deposits
    mean 349.53 + 1398.10
    max  1024.00 + 1920.00
   rewards
    mean 10.92 + 43.69
    max  32.00 + 60.00

As a rule of thumb if we want to have a very high confidence that we
won't miss any opportunity we should have around ~3kꜩ for deposits,
on the other hand the expected returns will probably be around ~10ꜩ per cycle.

After ``preserved_cycles``, not only does the delegate take back control of
its frozen deposits, but it also receives the rewards for its hard work
which amount to 16ꜩ to bake a block and ``2ꜩ / <block_priority>`` for
endorsing a block.
Additionally a baker also receives the fees of the operations it
included in its blocks.
While fees are unfrozen after ``preserved_cycles`` like deposits and
rewards, they participate in the staking balance of the delegate
immediately after the block has been baked.


Register and check your rights
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to run a delegate you first need to register as one using
your implicit account:

::

   tezos-client register key bob as delegate

Once registered, you need to wait ``preserved_cycles + 2 = 7`` cycles
for your rights to be considered.

There is a simple rpc that can be used to check your rights for every
cycle, up to 5 cycles in the future.

::

   tezos-client rpc get /chains/main/blocks/head/helpers/baking_rights\?cycle=300\&delegate=tz1_xxxxxxxxxxx\&max_priority=2

Sometimes a delegate skips its turn so it is worth considering also
baking rights at priority 2 like in the example above.
There is no priority for endorsements, every missed endorsement is
lost.

Inactive delegates
~~~~~~~~~~~~~~~~~~

If a delegate doesn't show any sign of activity for `preserved_cycles`
it is marked **inactive** and its rights are removed.
This mechanism is important to remove inactive delegates and reallocate
their rights to the active ones so that the network is always working
smoothly.
Normally even a baker with one single roll should perform enough
operations during 5 cycles to remain active.
If for some reason your delegate is marked inactive you can reactivate
it simply by re-registering again like above.

Baker
~~~~~

The baker is a daemon that once connected to an account, computes the
baking rights for that account, collects transactions from the mempool
and bakes a block.
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
If you are worried about availability of your node when it is its turn to
bake/endorse, there are other ways than duplicating your credentials.
**Never** use the same account on two daemons.


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
