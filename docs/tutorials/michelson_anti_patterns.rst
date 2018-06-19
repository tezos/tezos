Michelson Anti-Patterns
=======================

Even though Michelson is designed to make it easy to write secure
contracts and difficult to write vulnerable ones, it is still possible
to write buggy contracts that leak data and funds. This is a list of
mistakes that you can make when writing or interacting with contracts on
the Tezos blockchain and alternative ways to write code that avoid these
problems.

Note: We are currently reworking the concurrency model of Michelson (how
and when sub-transactions are made), so that some of these patterns will
be prevented by the language itself.

Refunding to a list of contracts
--------------------------------

One common pattern in contracts is to refund a group of people’s funds
at once. This is problematic if you accepted arbitrary contracts as a
malicious user can do cause various issues for you.

Possible issues:
~~~~~~~~~~~~~~~~

-  One contract swallows all the gas through a series of callbacks
-  One contract writes transactions until the block is full
-  Reentrancy bugs. Michelson intentionally makes these difficult to
   write, but it is still possible if you try.
-  A contract calls the \`FAIL\` instruction, stopping all computation.

Alternatives/Solutions:
~~~~~~~~~~~~~~~~~~~~~~~

-  Create a default account from people’s keys. Default accounts cannot
   execute code, avoiding the bugs above. Have people submit keys rather
   than contracts.
-  Have people pull their funds individually. Each user can break their
   own withdrawal only. **This does not protect against reentrancy
   bugs.**

Avoid batch operations when users can increase the size of the batch
--------------------------------------------------------------------

Contracts that rely on linear or super-linear operations are vulnerable
to malicious users supplying values until the contract cannot finish
without running into fuel limits. This can deadlock your contract.

.. _possible-issues-1:

Possible issues:
~~~~~~~~~~~~~~~~

-  Malicious users can force your contract into a pathological worst
   case, stopping it from finishing with available gas. Note that in the
   absence of hard gas limits, this can still be disabling as node
   operators may not want to run contracts that take more than a certain
   amount of gas.
-  You may hit the slow case of an amortized algorithm or data structure
   at an inopportune time, using up all of your contract’s available
   gas.

.. _alternativessolutions-1:

Alternatives/Solutions:
~~~~~~~~~~~~~~~~~~~~~~~

-  Avoid data structures and algorithms that rely on amortized
   operations, especially when users may add data.
-  Restrict the amount of data your contract can store to a level that
   will not overwhelm the available gas.
-  Write your contract so that it may pause and resume batch operations.
   This would complicate these sequences and require constant checking
   of available gas, but it prevents various attacks.

\*Do not assume an attack will be prohibitively expensive\*
Cryptocurrencies have extreme price fluctuations frequently and an
extremely motivated attacker may decide that an enormous expense is
justified. Remember, an attack that disables a contract is not just
targeted at the authors, but also the users of that contract.

Signatures alone do not prevent replay attacks
----------------------------------------------

If your contract uses signatures to authenticate messages, beware of
replay attacks. If a user ever signs a piece of data, you *must* make
sure that that piece of data is never again a valid message to the
contract. If you do not do this, anyone else can call your contract with
the same input and piggyback on the earlier approval.

.. _possible-issues-2:

Possible issues:
~~~~~~~~~~~~~~~~

-  A previously approved action can be replayed.

.. _alternativessolutions-2:

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Use an internal counter to make the data you ask users to sign
   unique. This counter should be per key so that users can find out
   what they need to approve. This should be paired with a signed hash
   of your contract to prevent cross-contract replays.
-  Use the ``SENDER`` instruction to verify that the expected sender is
   the source of the message.

Do not assume users will use a unique key for every smart contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users should always use a different key for every contract with which
they interact. If this is not the case, a message the user signed for
another contract can be sent to your contract. An internal counter alone
does not protect against this attack. It *must* be paired with a hash of
your contract. You must verify the source of the message.

Storing/transferring private data
---------------------------------

Once data is published to anyone, including broadcasting a transaction,
that data is public. Never transmit secret information via any part of
the blockchain ecosystem. As soon as you have broadcast a transaction
including that piece of information, anyone can see it. Furthermore,
malicious nodes in the system can manipulate unsigned transactions by
delaying, modifying, or reordering them.

.. _possible-issues-3:

Possible Issues
~~~~~~~~~~~~~~~

-  If data is not signed, it can be modified
-  Transactions can be delayed
-  Secret information will become public

.. _alternativessolutions-3:

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Do not store private information on the blockchain or broadcast it in
   transactions.
-  Sign all transactions that contain information that, if manipulated,
   could be abused.
-  Use counters to enforce transaction orders.

This will at least create a logical clock on messages sent to your
contract.

Not setting all state before a transfer
---------------------------------------

Reentrancy is a potential issue on the blockchain. When a contract makes
a transfer to another contract, that contract can execute its own code,
and can make arbitrary further transfers, including back to the original
contract. If state has not been updated before the transfer is made, a
contract can call back in and execute actions based on old state.

.. _possible-issues-4:

Possible Issues
~~~~~~~~~~~~~~~

-  Multiple withdrawals/actions
-  Generating illegal state if state is updated twice later

.. _alternativessolutions-4:

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Forbid reentrancy by means of a flag in your storage, unless you have
   a good reason to allow users to reenter your contract, this is likely
   the best option.
-  Only make transfers to trusted contracts or default accounts. Default
   accounts cannot execute code, so it is always safe to transfer to
   them. Before trusting a contract, make sure that its behavior cannot
   be modified and that you have an extremely high degree of confidence
   in it.

Do not store funds for others in spendable contracts
----------------------------------------------------

Tezos allows contracts to be marked as spendable. Managers of spendable
contracts can make transfers using the funds stored inside the contract.
This can subvert guarantees about the contract’s behavior that are
visible in the code.

.. _possible-issues-5:

Possible Issues
~~~~~~~~~~~~~~~

-  The funds of a contract can be removed.
-  A contract may not be able to meet its obligations

.. _alternativessolutions-5:

Alternatives/Solutions
~~~~~~~~~~~~~~~~~~~~~~

-  Do not store funds in spendable contracts that you do not control.
