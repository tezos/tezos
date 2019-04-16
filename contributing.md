# Contributing

Thank you for considering contributing to Tezos. This is a large codebase and
due to the on-chain governance nature of Tezos we use some non-standard
workflows.

There are two classes of changes and both are treated differently:
* Changes to the shell implementation
* Changes to the protocol implementation

## Shell Implementation
The shell is not part of the consensus state machine and hence every full node
can have a different/custom version of the shell while still running the same
protocol. A good example is how Ethereum has Parity and Geth, both are different
shells but they are running the same protocol. 

Making changes to the shell is straightforward. Fork this repo, and then branch
from `master`. Once you have made your changes please submit a merge request on
gitlab against the `master` branch. Periodically those changes will get merged
into `zeronet`, `alphanet`, and `mainnet` by hand.

## Protocol Implementation
The protocol is the Tezos state machine and run in consensus. It is fully
deterministic and every node in the network has to arrive at the same output
given the same input. 

Making changes to the protocol is slightly different than one would expect. You
branch from the latest `master` branch, make changes to the protocol, and then
push them to your own branch in your fork. Please start from `proto_alpha`, and
then follow the naming convention of `proto_00*_***` by renaming the former
folder. You may create a merge request against the `master` branch, but it will
get merged only once the network has voted on your proposal and accepted it.
After it has gone through the Tezos governance process it will also get merged
into `zeronet` for testing and finally into `mainnet` for deployment.

[Here](http://tezos.gitlab.io/master/introduction/contributing.html) you can 
find more documentation on the development flow and [here](http://tezos.gitlab.io/master/whitedoc/voting.html)
you can find more information on the governance and voting process.

## Conclusion
Hacking on Tezos is awesome and everyone is welcome. If you discover
inconsistencies in this document please fix them to make it easier for the next
person. Furthermore, please don't hesitate to ask questions, we don't bite.

## Further
To get more details on the contribution process, check out the
[online contributing guidelines](http://tezos.gitlab.io/master/introduction/contributing.html).
