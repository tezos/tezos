.. _voting:

The Voting Process
==================

The design of the Tezos Node allows the consensus protocol to be
amended, that is replaced by another set of OCaml files which
implement the API of a valid protocol.

In the current protocol the amendment procedure is guided by a voting
procedure where delegates can propose, select and test a candidate
protocol before activating it.
Delegates take part in the amendment procedure with an influence
proportional to their stake, one roll one vote.

The procedure consists of four periods, each of 32768 blocks (or
~three weeks), for a total of approximately three months.

Other than this page, there is an excellent overview from `Jacob
Arluck on medium.
<https://medium.com/tezos/amending-tezos-b77949d97e1e>`_

Periods
-------

The voting procedure works as follows:

- `Proposal period`: delegates can submit protocol amendment proposals using
  the `proposals` operation. At the end of a proposal period, the proposal with
  most supporters is selected and we move to a testing_vote period.
  If there are no proposals, or a tie between proposals, a new proposal
  period starts. Each delegate can submit a maximum of 20 proposals,
  including duplicates.
- `Testing_vote period`: delegates can cast one vote to test or not the winning
  proposal using the `ballot` operation.
  At the end of a testing_vote period if participation reaches the quorum
  and the proposal has a super-majority in favor, we proceed to a testing
  period. Otherwise we go back to a proposal period.
- `Testing period`: a test chain is forked for 48 hours to test a
  correct migration of the context.
  At the end of a testing period we move to a promotion_vote period.
- `Promotion_vote period`: delegates can cast one vote to promote or not the
  tested proposal using the `ballot` operation.
  At the end of a promotion_vote period if participation reaches the quorum
  and the tested proposal has a super-majority in favor, it is activated as
  the new protocol. Otherwise we go back to a proposal period.

It is important to note that the stake of each delegated is computed
at the beginning of each period.

Super-majority and Quorum
-------------------------

Both voting periods work in the same way, only the subject of the
vote differs.
During a vote a delegate can cast a single Yea, Nay or Pass vote.
A vote is successful if it has super-majority and the participation
reaches the current quorum.

`Super-majority` means the Yeas are more than 8/10 of Yeas+Nays votes.
The `participation` is the ratio of all received votes, including
passes, with respect to the number of possible votes. The `quorum`
starts at 80% and at each vote it is updated using the old quorum and
the current participation with the following coefficients::

  newQ = oldQ * 8/10 + participation * 2/10

More details can be found in the file
``src/proto_alpha/lib_protocol/src/amendment.ml``.

Operations
----------

There are two operations used by the delegates: ``proposals`` and ``ballot``.
A proposal operation can only be submitted during a proposal period.

::

   Proposals : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposals: Protocol_hash.t list ; }

Source is the public key hash of the delegate, period is the unique
identifier of each voting period and proposals is a non-empty list of
maximum 20 protocol hashes.
The operation can be submitted more than once but only as long as the
cumulative length of the proposals lists is less than 20.

A ballot operation can only be submitted during one of the voting
periods, and only once per period.

::

   Ballot : {
     source: Signature.Public_key_hash.t ;
     period: Voting_period_repr.t ;
     proposal: Protocol_hash.t ;
     ballot: Vote_repr.ballot ; }

Source and period are the same as above, while proposal is the
currently selected proposal and ballot is one of ``Yea``, ``Nay`` or
``Pass``.
The pass vote allows a delegate to not influence a vote but still
allowing it to reach quorum.

More details can be found, as for all operations, in
``src/proto_alpha/lib_protocol/src/operation_repr.ml``.
The binary format is described by ``tezos-client describe unsigned
operation``.

Client Commands
---------------

Tezos' client provides a command to show the status of a voting period.
It displays different informations for different kind of periods, as
in the following samples::

  $ tezos-client show voting period
  Current period: "proposal"
  Blocks remaining until end of period: 59
  Current proposals:
  PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp 400

  $ tezos-client show voting period
  Current period: "testing_vote"
  Blocks remaining until end of period: 63
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp
  Ballots: { "yay": 400, "nay": 0, "pass": 0 }
  Current participation 20.00%, necessary quorum 80.00%
  Current in favor 400, needed supermajority 320

  $ tezos-client show voting period
  Current period: "testing"
  Blocks remaining until end of period: 64
  Current proposal: PsNa6jTtsRfbGaNSoYXNTNM5A7c3Lji22Yf2ZhpFUjQFC17iZVp

It should be noted that the number 400 above is a number of rolls.
The proposal has a total of 400 rolls, which may come from several
delegates. The same applies for the ballots, there are 400 rolls in
favor of testing protocol PsNa6jTt.

Submit proposals
~~~~~~~~~~~~~~~~

During a proposal period, the list of proposals can be submitted with::

    tezos-client submit proposals for <delegate> <proposal1> <proposal2> ...

Remember that each delegate can submit a maximum of 20 protocol
hashes including duplicates.
Moreover each proposal is accepted only if it meets one of the
following two conditions:

- the protocol hash was already proposed on the network. In this case
  we can submit an additional proposal that "upvotes" an existing one
  and our rolls are added to the ones already supporting the proposal.
- the protocol is known by the node. In particular the first proposer
  of a protocol should be able to successfully inject the protocol in
  its node which performs some checks, compiles and loads the
  protocol.

Submit ballots
~~~~~~~~~~~~~~

During a voting period, being it a testing vote or a promotion vote,
ballots can be submitted once with::

    tezos-client submit ballot for <delegate> <proposal> <yay|nay|pass>

Other resources
~~~~~~~~~~~~~~~

For more details on the client commands refer to the manual at
:ref:`client_manual`.

For vote related RPCs check the :ref:`rpc_index` under the prefix
``vote/``.

For Ledger support refer to Obsidian Systems' `documentation
<https://github.com/obsidiansystems/ledger-app-tezos#proposals-and-voting>`_.
