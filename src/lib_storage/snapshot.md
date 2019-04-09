
(* The context dumping file format

   ==Overview

   The dumping file format aims at providing an efficient and simple format to
   input and output contexts.

   It supports the dumping of several contexts with simplification of sharing
   through the hashes provided by the context.

   The file must start with the ASCII sentence "V1Tezos-Context-dump" (quotes not
   included).

   Instructions given as "must" have to be respected to be valid. Instructions
   given as "should" are required for the file to be in the "normal form" that will
   ensure its unicity for a given context, they are however not checked by the
   import functionnality.

   ==Data encoding

   Several types are used in this format. Note that at any point in the file, the
   type of the next piece of data is known. Here is how the different pieces of
   data are encoded.

 * Integers are stored as big-endian 64-bit integers
   => usage of EndianBytes.BigEndian.{set,get}_int64 is advised
 * Strings, bytes and MBytes are stored as an integer indicating of the length of
   the data (in bytes), followed by the data itself
 * Lists are stored as an integer indicating the length of the list (in number of
   elements), followed by each element starting with the head.
 * Tuples are stored as each element subsequently.
 * Hashes (which should be seen as MBytes) are Blake2b hashes.

   ==The current context

   This file format is tightly linked to the notion of context
   (see src/lib_storage/context.mli).

   It is to be noted that some data may be shared between context, and not
   necessarily under the same path. But, we'd rather build our contexts
   incrementally, without having to use a temporary, fake index.

   So, when the file starts (whether for dumping or restoring). Assume there is a
   current context. Any path given in the file will be respective to that current
   context. Note that the current context changes after a Root instruction.

   If an element as already been inserted under a different path, it should not be reinserted.

   ==Storage instructions

   There are three storage instructions, they start with a 1-byte opcode that
   indicate the instruction, followed by the relevant data.

 * Blob: 'b' (hash : mbytes, path_rev : string list, data : mbytes)
   Blob adds the data under the right path in the current context.
   hash must match data.
   path_rev must be given reversed.

 * Dir: 'd' (hash : mbytes, path_rev : string list, keys : (key:string,khash:mbytes) list)
   Dir adds a directory under the right path in the current context.
   Each hash must correspond to a blob or directory already present in the index.
   hash must match the complete directory.
   path_rev must be given reversed.
   keys should be ordered by String.compare on its first element.

 * Root: 'r' (block_data : mbytes, date : int, author : string, message : string, parents : mbytes list)
   Root marks a context as complete, everything happening after the ROOT
   instruction will be in a different context, and no other modification will be
   performed on that context.
   block_header must contain the hash of the corresponding context.
   date, author and message must be the ones used by Tezos.
   parents must be the list of ancestors of the context (which would have a single element).
   parents should be ordered by hash decreasing (though if there are several parents, something really odd happened).

 * Proot: 'p' (pruned_block_data: mbytes)
   Adds a history block (only the chain data without context).
   For a snapshot to be valid, there should be Proot entries for at least a number of blocks
   preceding each Root according to the Root's operation ttl.

 * Loot: 'l' (protocol_data: mbytes)
   Adds the protocol data to the snapshots. It contains a pair (level * data) for each
   transition blocks of the exported block list. For a snapshot to be valid, there should be
   Loot entries.

 * End: 'e'
   This must be the end of the file.

   ==Storage order

   A Dir or Root instruction must be present after any value in it has already been
   inserted. Dir elements should be inserted in the order provided by String.compare.

   example:

   a tree of the form:

    root
   /   \
   a     b
   / \    |
   aa ab   ba

   Will be exported as:

   Blob root/a/aa
   Blob root/a/ab
   Dir root/a
   Blob root/b/ba
   Blob root/b
   Dir root
   Proot data
   Proot data
   Root
   ...
   End

   In case several trees are given, they should be exported in the order given as input.

*)

