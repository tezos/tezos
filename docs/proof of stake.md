# Proof-of-stake in Tezos: Michelson

## Introduction

This write-up intends to provide a more detailed description of the proof-of-stake mechanism in Michelson (Tezos' seed protocol) than is available in the white paper. It explains the rationale between the design decisions, highlights some tradeoffs and calls attention to potential weaknesses and concerns.

## Outline

Our proof-of-stake mechanism is based on a blockchain. We attempt to replicate Bitcoin's consensus properties without relying on proof-of-work. Instead of being discovered by miners through brute force search, the blocks are *forged* and *signed* by the participants in the network. We attempt to assign the right to sign the blocks randomly in proportion to the amount of tokens held. This isn’t necessarily because the party receiving the signing right has “something at stake” but rather because it acts as a reasonable Sybil prevention mechanism. This approach presents a few challenges:

1. What source of randomness can we use to assign signing rights?
2. What happens if the responsible party doesn’t sign a block ?
3. The old quip goes: “Time is nature's way to keep everything from happening all at once”. How do we go about that?
4. Since signatures are costless, how do we incentivize against forks that reorganize the blockchain?
5. Since blocks are cheap to create (it is the goal), how do we prevent DOS attacks that force nodes to waste time evaluating spurious forks?

If we can reasonably patch all of these issues, we may have a workable proof-of-stake scheme. It may not strictly dominate proof-of-work in all aspects, but it will better for some threat models and worse for others. Whether or not it is overall "better" or "worse" is ultimately an empirical question.

## A warning about complexity

Before we get started, I should disclose that the complexity of our approach gives me pause. Proof-of-work looks dead simple and yet has proven out to be a trove of gotchas (such as selfish-mining). With a complex system like the one I am about to describe, it is much more harduous to foresee all the potential problems and attacks. With hindsight, I realize I would be more comfortable with a mechanism offering a more patent security model. A verifiable reduction to PBFT would be ideal. Tendermint claims to do this, but I haven’t evaluated it and they have systematically failed to demonstrate reliable consensus in demonstrations. This may be an implementation issue or an issue with their design at large.

Fortunately, Tezos can upgrade its consensus mechanism, so all that is needed is for the current consensus mechanism to be sufficiently robust to cope long enough for a cleaner mechanism to replace it. A successful launch will give us ample funding to develop such a solution.

## Overview of our approach

There is some circular dependencies in the various parts which make up Michelson's proof-of-stake mechanism. For instance, the procedure by which we derive a random seed for assigning forging rights depends on the system's ability to resist censorship, but the latter cannot exist without random seeds. As a result, the reader may want to jump between the different sections or two read in two passes so as to resolve the dependencies.

### Cycle

Briefly speaking, the algorithm proceeds according to a cycle composed of a set number of blocks. The length of a cycle, in blocks, is a tunable parameter which we have yet to decide upon. We intend for a cycle to last between a few days and a few months. Shorter cycles offer better protection against censorship, while longer cycles provide a stronger consensus.

### Forging

At the beginning of a cycle, a random seed is derived from information revealed in the penultimate cycle. For each block in the cycle, we randomly assign "forging" rights priorities to stakeholders. For instance, for the 5th block in the cycle, the protocol may randomly pick stakeholder A and gives them the top priority for forging that block, then randomly pick stakeholder B and give them the second highest priority, and so on and so forth. Thus, each block in the cycle is associated with a list of stakeholders ordered by priority. This priority is reflected in the form of timing rules. The stakeholder with the top forging priority may create the block one minute after the previous block. If he doesn't do so, the stakeholder with the second highest forging priority may create the block two minutes after the previous block, etc.

### Endorsements 

In addition to forging rights, the protocol randomly assigns endorsement rights. For each block, 16 endorsement rights are randomly assigned to stakeholders. An endorsement means that a select stakeholder signs a particular block. Those endorsements are then included in the block's children. The total number of signature in a chain is the criterion for selecting the longest chain. 

Here's a quick explanation why signatures are needed. Suppose the next block is to be forged in priority by stakeholder A, followed by stakeholder B, while the block after that gives the first priority to B and the second to A. B could be tempted to create both blocks, linking them to each other in a form of "selfish forging". Signatures mitigate this greatly because endorsements for the first block would refer to the block mined by A, and B needs to include as many of these endorsements as possible for its block to hold weight.

Stakeholders are incentivized to collaborate to sign the same block in two ways. If a signer signs a block which doesn't end up being part of the blockchain, then they will miss out on receiving a reward, and the more signers agree on which block to sign, the higher the reward. They also receive a higher reward for higher priority blocks, to nudge them away from trying to hold out.

### Bonds and rewards 

In proof-of-work, at a given time, a mining rig can only be used to mine children of a specific block. This financially incentivizes miners to commit to one particular branch. Cryptographic signatures on the other hand are very cheap, and thus a stakehoklder forging a block or signing an endorsment on our network could theoretically participate in two different branches. However, this can be detected by networks participants since in both cases the public key is the same.

Stakeholders maintain a pool of tokens which is used as collateral. Whenever a stakeholder forges a block or sign an endorsment, a portion of this pool is frozen for a period of time (we are targetting one year but may have a shorter cycle initially). The period of time is adjusted so that reimbursements fall on a predictable schedule in order to compress the size of the state needed. Reimbursements are made to the pool of tokens from which collateral is drawn. If a double signature is detected for the same block, but in a different branch, or the same endorsement, but for a different block, a miner can include a denunication in their block, which contains a proof of malfeasance. The bond is forfeited, and the miner receives a reward (of lower xvalue to the bond). 

This effectively protects only against double signatures when the branches fork within the same cycle. If a fork happened ahead of the current cycle, "blocks" and "endorsements" are not meaningfully the same, and a stakeholder may get away with a double signature. However, large stakeholders who take part in the consensus algorithm will typically have a large amount of bonds "at stake" in the chain, making it unwise for them to cause trouble in the consensus.

### Randomness & rolls

In order to "randomly" determine how to assign mining a signing rights at the beginning of a cycle, we use a cryptographically secure PRNG seeded with a value computed at the end of the penultimate cycle. The seed is computed as follow. During a first cycle, block forgers commit to a secret random number by including its hash in their blocks. During the following cycle, they reveal their commitment under penalty of forfeiting the bond deposited for the block. All the revelead commitments are hashed together and a seed is derived by solving a related discrete logarithm problem. 

To select a random stakeholder, we use a "follow-the-coin" strategy. This ensures that a malicious participant creating a fork by themselves cannot assign themselves signing and forging rights by shuffling around coins. Tracking every single coin would be inefficient. Instead, we track "rolls" of coin. A roll of coin represent a set of several coins bundled together. Sometimes transactions may break rolls, in which case the roll id goes into a LIFO queue and is reused as soon as enough loose coins are joined in a contract to form a roll. This means that a stake will alway be rounded down to the nearest number of rolls it represents.

### DDOS protection

Bitcoin's least appreciated strength is its resistance to DDOS attacks. When a peer advertises a better chain to a node, before beginning to even download the blocks, the amount of work can be assessed extremely quickly by looking at the block hashes. This doesn't mean further validation isn't warranted, the blocks may be invalid, but if they are, this is an extremely expensive attack to throw.

In contrast, in proof-of-stake, blocks are cheap to create and can take longer to validate. How do we prevent a scenario where peers flood us with invalid blocks advertising them as higher priority than the real chain. We take a variety of approaches.

First, we analyze blocks defensively, trying to reject invalid blocks as early as possible. This may not be as cheap as counting the number of 0s in a hash, andit may require downloading more data, but in the end, both are constant time operations.

Moreover, a valid, low priority, fork will have long time intervals between blocks. The network shell will simply refuse to validate blocks dated in the future. This prevents an adversary from forcing the evaluation of a trillion bock long forks for instance.

Furthermore, the network shell may blacklist IPs that relay invalid blocks. This is not ideal as it may lead to the blacklisting of TOR exit nodes and may not work well with IPv6. In the future, we could consider having peers purchase keys on the blockchain and require such keys to participate in the gossip network. This would make blacklisting / whitelisting work much better.

Last, but not least, we require the hash of each block to solve a small proof-of-work problem. It is calibrated so that creating a block should be roughly 1,000 times more expensive than validating it. The deadweight loss of this mild proof-of-work stamp is many order of magnitudes smaller than that of a regular proof-of-work consensus. It is not subject to a red queen effect: as hardware becomes better, it becomes cheaper to both generate and validate blocks.x

## Seed derivation

We can harden the seed derivation by making it computationally expensive to compute the seed. A simple approach would be to use many rounds of hashing, but this has the downside of requiring every party validating the ledger to perform the same computation. The solution to a preimage problem as in Bitcoin's proof-of-work is computationally hard but can be checked very cheaply. Unfortunately, those solutions aren't unique.

Instead we use a computationally hard problem where there exists a unique solution which can be checked inexpensively.

We first derive a 64 bit hash, x, out of the nonces revealed by the forgers. We then increment x until we find p, the smallest prime number greater or equal to x. The seed we seek is the discrete log of h(x) modulo p in base 2. This takes 32 bits of effort and about 1Gb of mmeory. A parameter which may be updated in further protocol changes.

The last block of a cycle must contain the solution to the problem.

### Informal checkpointing 

Requiring regular, centralized checkpoints would defeat the purpose of a decentralized ledger. However, occasional checkpoints do have their use. After several months, the hash of a particular block in the blockchain should be considered largely a settled matter and may be discovered through social consensus. Certain attacks against proof-of-stake involve purchasing old wallet keys from former stakeholders. Such an attack, by its nature would be largely visible. If such a threat did arise, given the exceptional circumstances, the network participants can simply decide to protect themselves by picking a checkpoint. It's ad hoc, it's inelegant, but it works and it should be enough to thwart such an attack and even discourage it in the first place.

### Delegation

Each contract has two keys. A "manager" key for spending the funds, and a "delegate" key for all operations related to participations in the proof-of-stake protocol or the governance process. This has several advantages.

First, it permits specialization. Not every party holding a contract on the network is interested in participating directly in the proof-of-stake protocol or in network governance.

Second, it permits to keep private spending keys offline.

Third, delegate keys will be sticky to some extent. This prevents parties from changing their keys willy nilly which would weaken the ability to detect double signatures.




* The hash of the block
* An Ed25519 signature of the block
* A shell header
* A protocol header

The shell header is common to all protocol versions and can be understood by the network shell:

* A timestamp
* The hash of the preceding block
* The fitness score of the current chain
* A merkle tree of operation hashes, the semantics of which are opaque to the network shellx

The protocol header (specific to Michelson)

* A nonce used for a minimal proof-of-work stamp for DDOS purposes
* The current block height
* A list of denounciations

"Operations" act on the ledger to change its state. They are typically transactions, or new contract originations, but certain operations play a specific role in the proof-of-stake mechanism. They are

 * Delegations
 * Bond payments
 * Endorsements
 * Divulgments
 
## Cycles

The proof-of-stake algorithm works according to a set cycle. The lenght of the cycle (in block) is a tunable parameter, typically we intend to set it between a few days and a few months.









