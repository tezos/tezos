.. _voting:

The Voting Process
==================

The design of the Tezos Node allows the consensus protocol to be
amended, that is replaced by another set of OCaml files which
implements the API of a valid protocol.

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

More details can be found in the file ``amendment.ml``.

Operations
----------

There two operations used by the delegates: ``proposals`` and ``ballot``.
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
``operation_repr.ml``. The binary format is described by
``tezos-client describe unsigned operation``.
