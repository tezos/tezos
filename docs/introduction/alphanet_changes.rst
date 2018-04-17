Alphanet changelog
==================

For the next reset
------------------

[Alpha]

- Do not allow revealing the same endorsement twice.

- Tez values now have 6 decimals instead of two. The syntax used by
  the client and Michelson use comma separators every three
  digits, before and after the dot. For instance, 3 million tez and 10
  µtez is written `3,000,000.000,01`. The syntax in JSON is the raw
  amount in µtez, either as a number without decimals or as a decimal
  string, for the same example we would get `"3000000000010"`.

[Node]

- Rewrite of the RPC library to handle content types, to enable binary
  RPCs and proper HTTP verbs. The next version will probably break the
  HTTP API.

- Now that we don't use the git backend anymore, we finally updated
  the context hashing function from SHA1 to Blake2B.

[Michelson]

- Set a maximum type size, as a simple solution to avoid some type
  checker abuses where types can grow exponentially.

- Annotations are now correctly handled by macros.

[Build]

- Split the code base into separate OPAM packages.


Reset 2017-11-20
------------------

[Alphanet]

- Limit the number of faucet operations at 5 per block.

[Client]

- Autocomplete scripts for bash.

- Smart contracts are now non spendable by default.

- Add a debug command to list invalid blocks.

[Node]

- Prevent potential stack overflow in validation.

- Fix concurrency issue where operations were cleared from
   memory before being used.

- Continue background work on the multipass validator:
   cleanup and document data structures, better logging
   of resource requests, enhance requests for the same piece
   of data to multiple peers, split the code in smaller
   simpler components.

- P2p: fix issue with data greater than 2^16 bytes

- Irmin: use an experimental LevelDB backend

[Build]

- Refactor the economic protocol amendment code. Protocols are
   now compiled to functors, taking the type signature of their
   runtime environment as parameter. This simplifies the
   dependencies, and will allow third party developers to
   instanciate economic protocols in other contexts than the node.

- Switch from Makefiles to jbuilder, yay!

- Rename (hopefully) all occurences of "mining" into "baking".

[Michelson]

 - Introduce Micheline, the (now independent) IR of Michelson.
   The parser and printer should now be used on their own, outside
   of the client or node.

- Implement a basic semantics of annotations.
   The typechecker now propagates annotations on types througout the
   code, and tagging instructions with an annotation allows the
   programmer to reannotate the element produced by the instruction.
   The emacs mode displays propagated annotations.

- Add a version of `ITER` that takes a static code block and expects
   a colletion on the initial stack, and works like a `LOOP`, pushing
   the element of the collection one at a time on the stack. This is
   like `REDUCE` but using a static code block instead of a dynamic
   lambda. In the same vein, `MAP` can take a code block.

- Add `LOOP_LEFT` that uses a different type for the accumulator and
   the return value. Continues while the top of the stack is `Left 'a`
   and stops on `Right 'b`.

- Change timestamps to be arbitrary  precision relative integers.

- Add `SIZE` on lists.

Reset 2017-11-17
----------------

[Node]

- P2p: fix issue with data greater then 2^16 bytes
- Irmin: restore usage `git-repack`... (mistakenly removed)

Reset 2017-10-13
----------------

[Client]

 - Fix missing nonce revelation at end of cycle.
 - New command line analyzer and better help pages.

[Node]

 - Various small fixes and error message enhancements.

[Alphanet]

 - Use older leveldb-1.18 as upgrade to the newer version made the
   node crash.

[Michelson]

 - Split the `key` type into `key` and `key_hash` to
   prevent an error raised when using an unrevealed key.

Reset 2017-09-21
----------------

[Node]

- fix a performance issue in roll storage

[Doc]

- improve scripts and documentations on how to run sandboxed node
  or a local private network

[Client]

- add an option `-log-requests`. All RPC requests and responses to the
  node are logged on `stderr`.

[Michelson]

 - Split the `key` type into `key` and `key_hash` to
   prevent an error raised when using an unrevealed key.

Reset 2017-08-10
----------------

This update includes changes in the on-disk state of the node and in
the format of blocks and operations. It thus requires a chain reset.

Main changes includes:

[Doc]

- The documentation previously available on the Slack channel is now
  available at:

    https://raw.githubusercontent.com/tezos/tezos/alphanet/README.md

- The `alphanet` branch of the github repository is now automaticaly
  synchronized with `alphanet` docker image. And the latest version of
  the `alphanet.sh` is available at:

    https://raw.githubusercontent.com/tezos/tezos/alphanet/scripts/alphanet.sh

  No need to update manually though, the script auto-update itself
  when running:

    ./alphanet.sh restart

  Or:

    ./alphanet.sh update_script


[Michelson]

- minor language enhancements, mostly resulting from the feedback of
  Milo's daily challenge:

    http://www.michelson-lang.com/

- the alphanet scripts now understands a container: prefix wherever a
  file: prefix is accepted, temporarily copying the file into the
  container, and the emacs-mode is aware of that

[Node]

- Operations now include a block hash in their header. Such an
  operation could only be included in a successor of this block.

- The economics protocol now refuses blocks that includes an operation
  forged more than 64 blocks in the past. As any constants set by the
  economic protocol, it is amendable by a vote.

- Header of blocks now includes a hash of the "context" that result
  from its validation. This is currently the SHA1 of the git commit,
  but this will be changed in a near future for a safer cryptographic
  hash.

- The node does not need anymore to maintain a full index of the
  operation to operate. This greatly reduce the memory and disk usage.

- The node now builds against `irmin.1.3` where some of our code and
  optimizations were upstreamed. We were previously stuck to
  irmin.0.12.


[CI]

- This is not directly visible in the alphanet, but our CI
  infrastrucre is now ready for open development.
  More about that soon (or later).

