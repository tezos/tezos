.. _proto_003:

Protocol 003_PsddFKi3
=====================

Description of the patch
------------------------

Fix to prevent account creation spam
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While creating accounts currently requires a .257 tez burn, there is
currently no cost to create implicit accounts, despite them occupying
space in the context.
This patch adjusts the cost to .257 tez for both regular (KT1) and
implicit (tz1) accounts.

Error handling for nonce revelation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In cycle 48, a baker who lost their deposits and rewards due to double
baking also did not inject nonce revelation. The protocol reached an
error condition after trying to take away rewards from an account for
which rewards had already been slashed. As a result, no new blocks
could be accepted unless the nonces were revealed. The patch ensures
correct handling of this scenario.

Add RPCs for voting
~~~~~~~~~~~~~~~~~~~

This patch introduces RPCs to query ballot status, functionality
needed by bakers to interact with proposals to amend the protocol.
They are the following:

::

   Sum of ballots cast so far during a voting period.
   GET /chains/<chain_id>/blocks/<block_id>/votes/ballots

   Ballots cast so far during a voting period.
   GET /chains/<chain_id>/blocks/<block_id>/votes/ballot_list

   Current period kind: proposal, testing_vote, testing, promotion_vote.
   GET /chains/<chain_id>/blocks/<block_id>/votes/current_period_kind

   Current expected quorum.
   GET /chains/<chain_id>/blocks/<block_id>/votes/current_quorum

   List of delegates with their voting weight, in number of rolls.
   GET /chains/<chain_id>/blocks/<block_id>/votes/listings

   List of proposals with number of supporters.
   GET /chains/<chain_id>/blocks/<block_id>/votes/proposals

   Current proposal under evaluation.
   GET /chains/<chain_id>/blocks/<block_id>/votes/current_proposal

Correct accounting for approval voting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The current protocol does not properly count baking rolls during the
approval voting phase. This is corrected in this version.


How to apply the patch
----------------------

If compiling from source, please pull the latest code from the mainnet
branch. From a clone of the git repository, a minimal update command
is ``git checkout mainnet && git pull && eval $(opam env) && make
build-deps && make``.
Tezos binaries (node, client, baker, endorser, etc) should not be
running while you re-compile.
If using the script ``mainnet.sh`` based on docker provided at
https://gitlab.com/tezos/tezos, simply do ``./mainnet.sh restart`` as
every call to mainnet.sh checks for updates and updates if necessary.

The node will automatically switch over to the new protocol at block
height **204762** expected to occur after 2018-11-26T17:30:00 UTC.

After updating, all processes (the node, baker, endorser, and accuser)
should be restarted. The updated node handles multiple protocols but
notice that there are several, protocol-specific, versions of every
other processes.
In order not to miss a block or an endorsement, you may run versions
002_PsYLVpVv and 003_PsddFKi3 of the baker and endorser in parallel.
Versions 003_PsddFKi3 of the baking and endorsement processes will not
start baking or endorsing until the target block height. Version
002_PsYLVpVv will stop by themselves and it will be safe to stop them
at that time. This has been tested and the processes do not attempt to
bake at the same height.

However, as an extra precaution, specially if you do not use a
hardware key or a remote-signer with a high water-mark, you may
consider waiting until the target block height to shut down the old
process and start the new one.)

FAQ
---

Q. Who should  apply this patch?

A. Anyone running a node needs to update. If you are using a wallet
   that connects to a third party node, you do not need to apply a
   patch, but you can inquire with the wallet developers to make sure
   they are running a patched node. If you are delegating your tez you
   may wish to inquire with your baker that he is running the patched
   node in order not to miss any reward.

Q. What are the risks and impact of account creation spam?

A. Over time, account creation spam can make it uneconomical to run a
   node due to the amount of disk space required. This would make it
   harder for people to participate in the ecosystem.

Q. What happens if I apply the patch early?

A. The patch will automatically activate at a set block-height.
   Specifically, block height 204762 (approximately Monday Nov 26 1730
   UTC)

Q. What happens if I don't apply the patch?

A. Your node will continue tracking a branch with a known bug which
   does not represent the consensus among network participants.

Q. Why not use the governance mechanism to correct these issues?

A. The governance mechanism is a slow, deliberative, procedure for
   deciding on the evolution of the code. It is not a substitute for
   security patches which require quick deployment.

Q. Why not mandate minimal transaction fees in the protocol?

A. Transaction fees solve a slightly different problem, but they can
   help. If bakers wish to filter out transaction with low fees, they
   can run the process by passing the flag:

   ::

      --minimal-fees (default 0.0001)
      --minimal-nanotez-per-byte (default 1000)
      --minimal-nanotez-per-gaz-unit (default 100)

   1 mutez is equivalent to 1000 nanotez. The patch does include
   default minimal fees in the mempool, but individual bakers can
   choose to override these.
