Glossary
========

This glossary is divided in two sections, the first one concerns Tezos, and
the second one concerns the Alpha protocol, which is the current economic
protocol.

Tezos
-----

Context
    The state of the blockchain. The context is defined by the
    Economic Protocol and typically includes information such as “this
    account is credited with this many tez” and “this is the code for that smart
    contract.”

    The Context is modified by Operations. For example, an
    operation can transfer tez from one account to another, which modifies the
    part of the context that tracks account credit.

Block
    A block is a collection of several Operations. The operations are located
    in the payload of the block which is specific to the Economic Protocol.

    Along with the payload, the block includes a header which contains metadata.
    The metadata is agnostic to the economic protocol. It consists of generic
    information such as the block predecessor, a time stamp, etc.

Economic protocol
    The economic protocol is the application that runs on top of the blockchain
    proper. It defines a Context (the state of the application), some
    Operations (how the state evolves).

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. The procedure for an upgrade is defined within
    the economic protocol itself so it can be upgraded as well.

Fitness
    See score.

Node
    A peer in the P2P network. It maintains a local state and propagates blocks
    and operations.

Operation
    Operations transform the context, this is what makes the state of the chain
    change. Operations are grouped into blocks so that the chain progresses in
    batches.

Score (a.k.a. Fitness, a.k.aa.k.a. Weight)
    The score is a metric used to compare contexts. For example, when several
    blocks claim to be heads of the chain, their context's scores are compared.
    The highest scoring block is selected as the head of the chain.

Shell
    The shell is a software component of the node. It is parameterized by a
    specific economic protocol. It serves as the bridge between the P2P layer
    (handling communication between nodes) and the economic protocol layer
    (handling the context, operation application, scoring, etc.).

Weight
    See score.

Protocol alpha
--------------

Accuser
    When a node attempts to inject several incompatible blocks (or when it tries
    to abuse the network in another similar way), another node can make an
    accusation: show evidence of attempted abuse. The node making the accusation
    is the accuser.

    The accuser is awarded some funds from the baking deposit of the accused.

    Using the tools provided by Nomadic Labs, accusation is handled by a
    separate binary.

Account
    An account is a unique identifier within protocol alpha. There are different
    kinds of accounts (see Originated account and Implicit account).

    In the Context, each account is associated with a balance (an amount of
    tez available), a manager (another account that manages this one; possibly
    itself), and other information.

Baker
    When a node creates a new block, it is the baker of this block.
    Baking rights are distributed to different accounts based on their
    available balance. Only a node that handles an account with baking rights
    is allowed to bake; blocks created by another node are invalid.

    Using the tools provided by Nomadic Labs, baking is handled by a
    separate binary.

Baking/Endorsement rights
    A delegate is allowed to bake/endorse a block if he holds the
    baking/endorsement right for that block. At the start of a Cycle,
    baking and endorsement rights are computed for all the block heights in the
    cycle, based on the proportion of Rolls owned by each accounts.

    For each block height, there are several accounts that are allowed to bake.
    These different accounts are given different Priorities.

    For each block height, there are several accounts that are allowed to
    endorse. There can be multiple endorsements per block. Endorsements increase
    the Score of the block.

Contract
    Originated account which is associated to a Michelson script.
    Contracts are sometimes referred to as smart contracts.

Cycle
    A cycle is a set of consecutive blocks. E.g., cycle 12 started at block
    height 49152 and ended at block height 53248.

    Cycles are used as a unit of “time” in the block chain. For example, the
    different phases in the amendment voting procedures are defined based on
    cycles.

Delegate
    An Implicit account to which an account has delegated their baking and
    endorsement rights. The Baking rights and Endorsement rights are
    calculated based on the total balance of tez that an account has been
    delegated to.

Delegation
    An operation in which an originated account's manager lends its account
    balance to a delegate. This increases the delegate's rolls and consequently
    its Baking rights. The delegate does not control the funds from the
    account.

Double baking
    When a baker signs two different blocks at the same height, it is called
    double baking. Double baking is detrimental to the network and might be
    indicative of an attempt to double spend. As such, it is punished by the
    network: an accuser can provide proof of the double baking to be awarded
    part of the baker's deposit.

Endorser
    When a block is created and propagated on the network, nodes that have
    Endorsement rights for the matching block height can emit an endorsement
    operation. The accounts that emit the block are the endorsers of the block.
    The endorsement operations can be included in the next block to increase
    the block's Score.

    Using the tools provided by Nomadic Labs, endorsement is handled by a
    separate binary.

Gas
    A measure of the number of elementary operations performed during the
    execution of a contract. Gas is used to measure how much computing power is
    used to execute a contract.

Implicit account / Manager
    An account that is linked to a public key. An implicit account cannot
    include a Contract and cannot be delegated. An implicit contract can be
    set as the manager for other accounts, in which case it can perform
    operations on these accounts.

Michelson
    The built-in language used in smart contracts.

Operations
    In protocol Alpha, the main operations are transactions (to transfer funds
    or to execute contracts), accusations, activations, delegations,
    endorsements, originations and transactions.

Originated account
    An account that can contain a contract or be delegated. They are
    created with an explicit origination operation.

Origination
    An operation to create an originated account.

Liquidity
    A high-level programming language that compiles to Michelson.

Priority
    A rank of different baking rights. Each rank corresponds to a time span. A
    baker with baking rights at a given priority is only allowed to bake during
    the priority's corresponding time span. Baking outside of one's designated
    priority, results in an invalid block.

Roll
    An amount of tez (e.g., 10000ꜩ) serving as a unit to determine delegates'
    baking rights in a cycle. A delegate with twice as many rolls as another
    will be given twice as many rights to bake.

Transaction
    An operation to transfer tez between two accounts, or to run the code of a
    contract.

