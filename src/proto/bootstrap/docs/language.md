Tezos Contract Script Language Specification
============================================

The language is stack based, with high level data types and primitives
and scrict static type checking. Its design is insipired by Forth,
Scheme, ML and Cat.

This specification gives the complete instruction set, type system and
semantics of the language. It is meant as a precise reference manual,
not an easy introduction. Even though, some examples are provided at
the end of the document and can be read first or at the same time as
the specification.


Table of contents
-----------------

  * I - Type system
  * II - Semantics
  * III - Core instructions
  * IV - Data types
  * V - Operations
  * VI - Domain specific data types
  * VII - Domain specific operations
  * VIII - Concrete syntax
  * IX - Examples
  * X - Full grammar
  * XI - Reference implementation

I - Type system
---------------

The types `T` of values in the stack are written using notations

  * `bool`, `string`, `void`, `u?int{8|16|32|64}`,
    the core primitive types,
  * `identifier` for a primitive data-type,
  * `T identifier` for a parametric data-type with one parameter type `T`,
  * `identifier T_0 ... T_n` for a parametric data-type with several
    parameters,
  * `'a` for a type variable,
  * `_` for an anonymous type variable,
  * `[ P ]` for a code quotation whose program type is `P`,
  * `lambda T_arg T_ret` is a shortcut for `[ T_arg :: [] -> T_ret :: []]`.
  * other specific notations for compound types, described later.

Instructions, programs and primitives of the language are also typed,
their types `P` are written using the following notation, where `S`
the type of a stack.

    :: S before   ->   S after

A stack type `S` can be written

  * `[]` for the empty stack,
  * `T_top : S_rest` for the stack whose first value has type `Ttop` and queue `Srest`,
  * `'A` for a stack type variable,
  * `_` for an anonymous stack type variable.


II - Semantics
--------------

The instructions are specified as follows, giving their mnemonic, type
in the previously defined syntax, and small step semantics as a list
of rewriting rules of the form

    > pre state => result state

where the preconditions of all rules are to be read in order, the first
match selecting the behaviour of the instruction, so that the choice
is deterministic. Only the valid pre states are described, any other
cannot happen thanks to static typing.


The pre and post result states are described as

  * pairs `code / stack` for stack manipulation primitives,
  * triples `code / stack / memory` for primitives that also manipulate memory,
  * `[FAIL]` for a fatal failure state.

The notations used are

  * `;` to represent the concatenation of instructions or sequences,
  * `[]` for the empty code sequence,
  * `top : tail` for stack consing, as in types,
  * `identifier` for variable stack and code elements,
  * `<identifier>` for variable memory locations,
  * `_` for elements whose value does not affect the semantics.

The memory is described as a relation between locations and constants of
the form `variable = constant, ...`.

The constants are of one of the following forms.

  * integers with their sign and size, e.g. `(Uint8 3)`,
  * `Void`, the unique value of type `void`
  * booleans `True` and `False`,
  * string literals, as in `(String "contents")`,
  * structured constants of compound types described later.


III - Core instructions
-----------------------

### Control structures

   * `(I :: [ 'A -> 'B ]) ; (C :: [ 'B -> 'C ])`: Sequence operator.

        :: 'A   ->   'C

        > I ; C / SA   =>   C / SB   iff   I / SA   =>   [] / SB

   * `IF bt bf`: Conditional branching.

        :: bool : 'A   ->   'B
           iff   bt :: [ 'A -> 'B ]
                 bf :: [ 'A -> 'B ]

        > IF ; C / True : S    =>    bt ; C / S
        > IF ; C / False : S   =>    bf ; C / S

   * `LOOP body`: A generic loop.

        :: bool : 'A   ->   'A
           iff   body :: [ 'A -> bool : 'A ]

        > LOOP body ; C / True : S    =>    body ; LOOP body ; C / S
        > LOOP body ; C / False : S   =>    C / S

   * `DIP code`: Runs code protecting the top of the stack.

        :: 'b : 'A   ->   'b : 'C
           iff   code :: [ 'A -> 'C ]

        > DIP code ; C / x : S   =>   code ; PUSH x ; C / S

   * `DII+P code`: A sugar syntax for working deeper in the stack.

        > DII(\rest)P code ; C / S   =>   DIP (DI(\rest)P code) ; C / S

   * `LAMBDA 'a 'b code`: Push a function onto the stack.

        :: 'C   ->   lambda 'a 'b : 'C
           iff   code :: lambda 'a 'b

        > LAMBDA 'a 'b code ; C / S   =>   C / code : S

   * `EXEC`: Execute a function from the stack.

        :: 'a : lambda 'a 'b : 'C   ->   'b : 'C

        > EXEC ; C / a : f : S   =>   f ; C / a : S

### Stack operations

   * `DROP`: Drop the top element of the stack.

        :: _ : 'A   ->   'A

        > DROP ; C / _ : S   =>   C / S

   * `DUP`: Duplicate the top of the stack.

        :: 'a : 'A   ->   'a : 'a : 'A

        > DUP ; C / x : S   =>   C / x : x : S

   * `DUP n`: Duplicate the `n`th element of the stack.

        > DUP (n > 0) ; C / S   =>   DIP { DUP (n - 1) } ; SWAP ; C / S
        > DUP 0 ; C / S   =>   DUP ; C / S

     This variant of `DUP` with an optional argument is syntactic
     sugar for combining `DIP`, `SWAP` and `DUP` in order to access
     elements in the stack by their depth, `DUP 0` being equivqlent to
     a simple `DUP`.

   * `SWAP`: Exchange the top two elements of the stack.

        :: 'a : 'b : 'A   ->   'b : 'a : 'A

        > SWAP ; C / x : y : S   =>   C / y : x : S

   * `PUSH x`: Push a value onto the stack.


        :: 'A   ->   'a : 'A
           iff   x :: 'a

        > PUSH x ; C / S   =>   C / x : S

   * `DROP`: Drop the top element of the stack.

        :: _ : 'A   ->   'A

        > DROP ; C / _ : S   =>   C / S

   * `VOID`: Push a void value onto the stack.

        :: 'A   ->   void : 'A

        > VOID ; C / S   =>   C / () : S

### Generic comparison

Comparison only works on a class of types that we call comparable.  A
`COMPARE` operation is defined in an ad hoc way for each comparable
type, but the result of compare is always an `int64`, which can in turn
be checked in a generic manner using the following combinators. The
result of `COMPARE` is `0` if the compared values are equal, negative if
the first is less than the second, and positive otherwise.

   * `EQ`: Checks that the top if the stack EQuals zero.

        :: int64 : 'S   ->   bool : 'S

        > EQ ; C / Int64 (0) : S   =>   C / True : S
        > EQ ; C / _ : S           =>   C / False : S


   * `NEQ`: Checks that the top if the stack does Not EQual zero.

        :: int64 : 'S   ->   bool : 'S

        > NEQ ; C / Int64 (0) : S   =>   C / False : S
        > NEQ ; C / _ : S           =>   C / True : S

   * `LT`: Checks that the top if the stack is Less Than zero.

        :: int64 : 'S   ->   bool : 'S

        > LT ; C / Int64 (v) : S   =>   C / True : S   iff  v < 0
        > LT ; C / _ : S           =>   C / False : S

   * `GT`: Checks that the top if the stack is Greater Than zero.

        :: int64 : 'S   ->   bool : 'S

        > GT ; C / Int64 (v) : S   =>   C / True : S   iff  v > 0
        > GT ; C / _ : S           =>   C / False : S

   * `LE`: Checks that the top if the stack is Less Than of Equal to zero.

        :: int64 : 'S   ->   bool : 'S

        > LE ; C / Int64 (v) : S   =>   C / True : S   iff  v <= 0
        > LE ; C / _ : S           =>   C / False : S

   * `GE`: Checks that the top if the stack is Greater Than of Equal to zero.

        :: int64 : 'S   ->   bool : 'S

        > GE ; C / Int64 (v) : S   =>   C / True : S   iff  v >= 0
        > GE ; C / _ : S           =>   C / False : S

Syntactic sugar exists for merging `COMPARE` and comparison
combinators, and also for branching.

   * `CMP{EQ|NEQ|LT|GT|LE|GE}`

        > CMP(\op) ; C / S   =>   COMPARE ; (\op) ; C / S

   * `IF{EQ|NEQ|LT|GT|LE|GE} bt bf`

        > IFCMP(\op) ; C / S   =>   (\op) ; IF bt bf ; C / S

   * `IFCMP{EQ|NEQ|LT|GT|LE|GE} bt bf`

        > IFCMP(\op) ; C / S   =>   COMPARE ; IF(\op) bt bf ; C / S


IV - Data types
---------------

   * `bool`, `string`, `void`, `u?int{8|16|32|64}`:
     The core primitive types.

   * `list 'a`:
     A single, immutable, homogeneous linked list, whose elements are
     of type 'a, and that we note Nil for the empty list or
     (Cons head tail).

   * `pair 'a 'b`:
     A pair of values a and b of types 'a and 'b, that we write (Pair a b).

   * `option 'a`:
     Optional value that we note (None) or (Some v).

   * `or 'a 'b`:
     A union of two types, a value holding either a value a of type 'a
     or a value b of type 'b, that we write (Left a) or (Right b).

   * `set 'a`, `map 'a 'b`:
     Immutable map and sets.


V - Operations
--------------

### Operations on booleans

   * `OR`

        :: bool : bool : 'S   ->   bool : 'S

        > OR ; C / x : y : S   =>   C / (x | y) : S

   * `AND`

        :: bool : bool : 'S   ->   bool : 'S

        > AND ; C / x : y : S   =>   C / (x & y) : S

   * `XOR`

        :: bool : bool : 'S   ->   bool : 'S

        > XOR ; C / x : y : S   =>   C / (x ^ y) : S

   * `NOT`

        :: bool : 'S   ->   bool : 'S

        > NOT ; C / x : S   =>   C / ~x : S

### Operations on integers

Integers can be of size 1, 2, 4 or 8 bytes, signed or unsigned.
Integer Operations are homogeneous, so that performing computations
between values of different int types must be done via explicit casts.

For specifying arithmetics, we consider that integers are all stored
on 64 bits (the largest integer size) so that we can express the
operations, in particular casts, using usual bitwise masks. In this
context, the type indicator functions are defined as follows (which
can be read both as a constraint on the bitpatttern and as a
conversion operation).

     Uint64 (x) = Int64 (x) = x
     Uint32 (x) = x & 0x00000000FFFFFFFF
     Int32 (x) = x & 0x00000000FFFFFFFF
               | (x & 0x80000000 ? 0xFFFFFFFF00000000 : 0)
     Uint16 (x) = x & 0x000000000000FFFF
     Int16 (x) = x & 0x000000000000FFFF
               | (x & 0x8000 ? 0xFFFFFFFFFFFF0000 : 0)
     Uint8 (x) = x & 0x00000000000000FF
     Int8 (x) = x & 0x00000000000000FF
              | (x & 0x80 ? 0xFFFFFFFFFFFFFF00 : 0)

We also use the function `bits (t)` that retrieve the meaningful number
of bits for a given integer type (e.g. `bits (int8) = 8`).

   * `NEG`

        :: t : 'S   ->   t : 'S   where   t in int{8|16|32|64}

        > NEG ; C / t (x) : S   =>   C / t (-x) : S

     With cycling semantics for overflows (min (t) = -min (t)).

   * `ABS`

        :: t : 'S   ->   t : 'S   where   t in int{8|16|32|64}

        > ABS ; C / t (x) : S   =>   C / t (abs (x)) : S

     With cycling semantics for overflows (abs (min (t)) = min (t)).

   * `ADD`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > ADD ; C / t (x) : t (y) : S   =>   C / t (x + y) : S

     With cycling semantics for overflows.

   * `SUB`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > SUB ; C / t (x) : t (y) : S   =>   C / t (x + y) : S

     With cycling semantics for overflows.

   * `MUL`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > MUL ; C / t (x) : t (y) : S   =>   C / t (x + y) : S

     Unckeched for overflows.

   * `DIV`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > DIV ; C / t (x) : t (0) : S   =>   C / [FAIL]
        > DIV ; C / t (x) : t (y) : S   =>   C / t (x / y) : S

   * `MOD`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > MOD ; C / t (x) : t (0) : S   =>   C / [FAIL]
        > MOD ; C / t (x) : t (y) : S   =>   C / t (x % y) : S

   * `CAST t_to` where `t_to in u?int{8|16|32|64}`

        :: t_from : 'S   ->   t_to : 'S   where   t_from in u?int{8|16|32|64}

        > CAST t_to ; C / t_from (x) : S   =>   C / t_to (x) : S

Alternative operators are defined that check for overflows.

   * `CHECKED_NEG`

        :: t : 'S   ->   t : 'S   where   t in int{8|16|32|64}

        > CHECKED_NEG ; C / t (x) : S   =>   [FAIL] on overflow
        > CHECKED_NEG ; C / t (x) : S   =>   C / t (-x) : S

   * `CHECKED_ABS`

        :: t : 'S   ->   t : 'S   where   t in int{8|16|32|64}

        > CHECKED_ABS ; C / t (x) : S   =>   [FAIL] on overflow
        > CHECKED_ABS ; C / t (x) : S   =>   C / t (abs (x)) : S

   * `CHECKED_ADD`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > CHECKED_ADD ; C / t (x) : t (y) : S   =>   [FAIL] on overflow
        > CHECKED_ADD ; C / t (x) : t (y) : S   =>   C / t (x + y) : S

   * `CHECKED_SUB`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > CHECKED_SUB ; C / t (x) : t (y) : S   =>   [FAIL] on overflow
        > CHECKED_SUB ; C / t (x) : t (y) : S   =>   C / t (x - y) : S

   * `CHECKED_MUL`

        :: t : t : 'S   ->   t : 'S   where   t in u?int{8|16|32|64}

        > CHECKED_MUL ; C / t (x) : t (y) : S   =>   [FAIL] on overflow
        > CHECKED_MUL ; C / t (x) : t (y) : S   =>   C / t (x * y) : S

   * `CHECKED_CAST t_to` where `t_to in u?int{8|16|32|64}`

        :: t_from : 'S   ->   t_to : 'S   where   t_from in u?int{8|16|32|64}

        > CHECKED_CAST t_to ; C / t_from (x) : S   =>   C / t_to (x) : S
          iff   t_from (x) = t_to (x)
        > CHECKED_CAST t_to ; C / t_from (x) : S   =>   [FAIL]

Bitwise logical operators are also available on unsigned integers.

   * `OR`

        :: t : t : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > OR ; C / t (x) : t (y) : S   =>   C / t (x | y) : S

   * `AND`

        :: t : t : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > AND ; C / t (x) : t (y) : S   =>   C / t (x & y) : S

   * `XOR`

        :: t : t : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > XOR ; C / t (x) : t (y) : S   =>   C / t (x ^ y) : S

   * `NOT`

        :: t : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > NOT ; C / t (x) : S   =>   C / t (~x) : S

   * `LSL`

        :: t : uint8 (s) : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > LSL ; C / t (x) : uint8 (s) : S   =>   C / t (x << s) : S
          iff   s <= bits (t)
        > LSL ; C / t (x) : uint8 (s) : S   =>   [FAIL]

   * `LSR`

        :: t : uint8 (s) : 'S   ->   t : 'S   where   t in uint{8|16|32|64}

        > LSR ; C / t (x) : uint8 (s) : S   =>   C / t (x >>> s) : S
          iff   s <= bits (t)
        > LSR ; C / t (x) : uint8 (s) : S   =>   [FAIL]

   * `COMPARE`:
     Integer comparison (signed or unsigned according to the type)

        :: t : t : 'S   ->   int64 : 'S   where   t in uint{8|16|32|64}

### Operations on strings

Strings are mostly used for naming things without having to rely on
external ID databases. So what can be done is basically use string
constants as is, concatenate them and use them as keys.

   * `CONCAT`:
     String concatenation.

        :: string : string : 'S   -> string : 'S

   * `COMPARE`:
     Lexicographic comparison.

        :: string : string : 'S   ->   int64 : 'S

### Operations on pairs

   * `PAIR`:
     Build a pair from the stack's top two elements.

        :: 'a : 'b : 'S   ->   pair 'a 'b : 'S

        > PAIR ; C / a : b : S   =>   C / (Pair a b) : S

   * `P(A*AI)+R`:
     A syntactic sugar for building nested pairs in bulk.

        > PA{N}AI(\rest)R ; C / S   =>   DIP (PA{n-1}AIR) ; P(\rest)R ; C / S
        > PAIR ; C / S             =>   PAIR ; C / S
        > PR ; C / S               =>   C / S

   * `CAR`:
     Access the left part of a pair.

        :: pair 'a _ : 'S   ->   'a : 'S

        > Car ; C / (Pair a _) : S   =>   C / a : S

   * `CDR`:
     Access the left part of a pair.

        :: pair _ 'b : 'S   ->   'b : 'S

        > Car ; C / (Pair _ b) : S   =>   C / b : S

   * `C[AD]+R`:
     A sugary syntax for accessing fields in nested pairs.

        > CA(\rest)R ; C / S   =>   CAR ; C(\rest)R ; C / S
        > CD(\rest)R ; C / S   =>   CDR ; C(\rest)R ; C / S
        > CR ; C / S          =>   C / S

### Operations on sets

   * `EMPTY_SET 'elt`:
     Build a new, empty set for elements of a given type.

        :: 'S   ->   set 'elt : 'S

     The `'elt` type must be comparable (the `COMPARE` primitive must
     be defined over it).

   * `MEM`:
     Check for the presence of an element in a set.

        :: 'key : set 'elt : 'S   ->  bool : 'S

   * `UPDATE`:
     Inserts or removes an element in a set, replacing a previous value.

        :: 'elt : bool : set 'elt : 'S   ->   set 'elt : 'S

   * `REDUCE`:
     Apply a function on a set passing the result of each
     application to the next one and return the last.

        :: lambda (pair 'elt * 'b) 'b : set 'elt : 'b : 'S   ->   'b : 'S

### Operations on maps

   * `EMPTY_MAP 'key 'val`:
     Build a new, empty map.

     The `'key` type must be comparable (the `COMPARE` primitive must be
     defined over it).

        :: 'S -> map 'key 'val : 'S

   * `GET`:
     Access an element in a map, returns an optional value to be
     checked with `IF_SOME`.

        :: 'key : map 'key 'val : 'S   ->   option 'val : 'S

   * `MEM`:
     Check for the presence of an element in a map.

        :: 'key : map 'key 'val : 'S   ->  bool : 'S

   * `UPDATE`:
     Assign or remove an element in a map.

        :: 'key : option 'val : map 'key 'val : 'S   ->   map 'key 'val : 'S

   * `MAP`:
     Apply a function on a map and return the map of results under
     the same bindings.

        :: lambda (pair 'key 'val) 'b : map 'key 'val : 'S   ->   map 'key 'b : 'S

   * `REDUCE`:
     Apply a function on a map passing the result of each
     application to the next one and return the last.

        :: lambda (pair (pair 'key 'val) 'b) 'b : map 'key 'val : 'b : 'S   ->   'b : 'S

### Operations on optional values

   * `SOME`:
     Pack a present optional value.

        :: 'a : 'S   ->   'a? : 'S

        > SOME ; C / v :: S   =>   C / (Some v) :: S

   * `NONE 'a`:
     The absent optional value.

        :: 'S   ->   'a? : 'S

        > NONE ; C / v :: S   =>   C / None :: S

   * `IF_SOME bt bf`:
     Inspect an optional value.

        :: 'a? : 'S   ->   'b : 'S
           iff   bt :: [ 'a : 'S -> 'b : 'S]
                 bf :: [ 'S -> 'b : 'S]

        > IF_SOME ; C / (Some a) : S    =>    bt ; C / a : S
        > IF_SOME ; C / (None) : S   =>    bf ; C / S

### Operations on unions

   * `LEFT 'b`:
     Pack a value in a union (left case).

        :: 'a : 'S   ->   or 'a 'b : 'S

        > LEFT ; C / v :: S   =>   C / (Left v) :: S

   * `RIGHT 'a`:
     Pack a value in a union (right case).

        :: 'b : 'S   ->   or 'a 'b : 'S

        > RIGHT ; C / v :: S   =>   C / (Right v) :: S

   * `IF_LEFT bt bf`:
     Inspect an optional value.

        :: or 'a 'b : 'S   ->   'c : 'S
           iff   bt :: [ 'a : 'S -> 'c : 'S]
                 bf :: [ 'b : 'S -> 'c : 'S]

        > IF_LEFT ; C / (Left a) : S    =>    bt ; C / a : S
        > IF_LEFT ; C / (Right b) : S   =>    bf ; C / b : S

### Operations on lists

   * `CONS`:
     Prepend an element to a list.

        :: 'a : list 'a : 'S   ->   list 'a : 'S

        > CONS ; C / a : l : S   =>   C / (Cons a l) : S

   * `NIL 'a`:
     The empty list.

        :: 'S   ->   list 'a : 'S

        > NIL ; C / S   =>   C / Nil : S

   * `IF_CONS bt bf`:
     Inspect an optional value.

        :: list 'a : 'S   ->   'b : 'S
           iff   bt :: [ 'a : list 'a : 'S -> 'b : 'S]
                 bf :: [ 'S -> 'b : 'S]

        > IF_CONS ; C / (Cons a rest) : S   =>    bt ; C / a : rest : S
        > IF_CONS ; C / Nil : S   =>    bf ; C / S

   * `MAP`:
     Apply a function on a list from left to right and
     return the list of results in the same order.

        :: lambda 'a 'b : list 'a : 'S -> list 'b : 'S

   * `REDUCE`:
     Apply a function on a list from left to right
     passing the result of each application to the next one
     and return the last.

        :: lambda (pair 'a 'b) 'b : list 'a : 'b : 'S -> 'b : 'S


VI - Domain specific data types
-------------------------------

   * `timestamp`:
     Dates in the real world.

   * `tez`:
     A specific type for manipulating tokens.

   * `contract 'param 'result`:
     A contract, with the type of its code.

   * `key`:
     A public cryptography key.

   * `signature`:
     A cryptographic signature.


VII - Domain specific operations
--------------------------------

### Operations on timestamps

Timestamp immediates can be obtained by the `NOW` operation, or
retrieved from script parameters or globals. The only valid operations
are the addition of a (positive) number of seconds and the comparison.

   * `ADD`
     Increment / decrement a timestamp of the given number of seconds.

        :: timestamp : float : 'S -> timestamp : 'S

        > ADD ; C / t : period : S   =>   [FAIL]   iff   period < 0
        > ADD ; C / t : period : S   =>   C / (t + period seconds) : S

   * `ADD`
     Increment / decrement a timestamp of the given number of seconds.

        :: timestamp : uint{8|16|32|64} : 'S -> timestamp : 'S

        > ADD ; C / t : seconds : S   =>   [FAIL]   on overflow
        > ADD ; C / t : seconds : S   =>   C / (t + seconds) : S

   * `COMPARE`:
     Timestamp comparison.

        :: timestamp : timestamp : 'S   ->   int64 : 'S

### Operations on Tez

Operations on tez are limited to prevent overflow and mixing them with
other numerical types by mistake. They are also mandatorily checked
for under/overflows.

   * `ADD`:

        :: tez : tez : 'S   ->   tez : 'S

        > Add ; C / x : y : S   =>   [FAIL]   on overflow
        > Add ; C / x : y : S   =>   C / (x + y) : S

   * `SUB`:

        :: tez : tez : 'S   ->   tez : 'S

        > Sub ; C / x : y : S   =>   [FAIL]   iff   x < y
        > Sub ; C / x : y : S   =>   C / (x - y) : S

   * `MUL`

        :: tez : u?int{8|16|32|64} : 'S   ->   tez : 'S

        > Mul ; C / x : y : S   =>   [FAIL]   on overflow
        > Mul ; C / x : y : S   =>   C / (x * y) : S

   * `COMPARE`:

        :: tez : tez : 'S   ->   int64 : 'S

### Operations on contracts

   * `MANAGER`:
     Access the manager of a contract.

        :: contract 'p 'r : 'S   ->   key : 'S

   * `CREATE_CONTRACT`:
     Forge a new contract.


        :: key : key? : bool : bool : tez : lambda (pair (pair tez 'p) 'g) (pair 'r 'g) : 'g : 'S
           -> contract 'p 'r : 'S

     As with non code-emitted originations the
     contract code takes as argument the transfered amount plus an
     ad-hoc argument and returns an ad-hoc value. The code also takes
     the global data and returns it to be stored and retrieved on the
     next transaction. These data are initialized by another
     parameter. The calling convention for the code is as follows:
     (Pair (Pair amount arg) globals)) -> (Pair ret globals), as
     extrapolable from the instruction type. The first parameters are
     the manager, optional delegate, then spendable and delegatable
     flags and finally the initial amount taken from the currently
     executed contract. The contract is returned as a first class
     value to be called immediately or stored.

   * `CREATE_ACCOUNT`:
     Forge an account (a contract without code).

        :: key : key? : bool : tez : 'S   ->   contract void void : 'S

     Take as argument the manager, optional delegate, the delegatable
     flag and finally the initial amount taken from the currently
     executed contract.

   * `TRANSFER_TOKENS`:
     Forge and evaluate a transaction.

        :: 'p : tez : contract 'p 'r : 'g : []   ->   'r : 'g : []

     The parameter and return value must be consistent with the ones
     expected by the contract, void for an account. To preserve the
     global consistency of the system, the current contract's storage
     must be updated before passing the control to another script. For
     this, the script must put the partially updated storage on the
     stack ('g is the type of the contract's storage).  If a recursive
     call to the current contract happened, the updated storage is put
     on the stack next to the return value.  Nothing else can remain
     on the stack during a nested call. If some local values have to
     be kept for after the nested call, they have to be stored
     explicitly in a transient part of the storage. A trivial example
     of that is to reserve a boolean in the storage, initialized to
     false, reset to false at the end of each contract execution, and
     set to true during a nested call.  This thus gives an easy way
     for a contract to prevent recursive call (the contract just fails
     if the boolean is true).

   * `BALANCE`:
     Push the current amount of tez of the current contract.

        :: 'S   ->   tez :: 'S

   * `SOURCE 'p 'r`:
     Push the source contract of the current transaction.

        :: 'S   ->   contract 'p 'r :: 'S

   * `SELF`:
     Push the current contract.

        :: 'S   ->   contract 'p 'r :: 'S
           where   contract 'p 'r is the type of the current contract

   * `AMOUNT`:
     Push the amount of the current transaction.

        :: 'S   ->   tez :: 'S

### Special operations

   * `STEPS_TO_QUOTA`:
     Push the remaining steps before the contract execution must terminate.

        :: 'S   ->   uint32 :: 'S

   * `NOW`:
     Push the timestamp of the block whose validation triggered this
     execution (does not change during the execution of the contract).

        :: 'S   ->   timestamp :: 'S

   * `FAIL`:
     Explicitly abort the current transaction (and all of its parents).

        :: _   ->  _

        > FAIL ; _ / _   =>  [FAIL]

### Cryptographic primitives

   * `H`:
     Compute a cryptographic hash of the value contents using the
     Sha256 cryptographic algorithm.

        :: 'a : 'S   ->   string : 'S

   * `CHECK_SIGNATURE`
     Check that a sequence of bytes has been signed with a given key.

        :: key : pair signature string : 'S   ->   bool : 'S

   * `COMPARE`

        :: key : key : 'S   ->   int64 : 'S


VIII - Concrete syntax
----------------------

The structure of the concrete language is extremely simple. An
expression in the language can only be one of the three following
constructs.

  1. A constant.
  2. The application of a primitive to a sequence of expressions.
  3. A sequence of expressions.

As in Python or Haskell, the concrete syntax of the language is
indentation sensitive. The elements of a syntactical block, such as
all the elements of a sequence, or all the parameters of a primitive,
must be written with the exact same left margin in the program source
code. This is unlike in C-like languages, where blocks are delimited
with braces and the margin is ignored by the compiled. The exact
parsing policy is described just after.

### Constants

There are two kinds of constants:

  1. Integers in decimal (no prefix), hexadecimal (0x prefix), octal
     (0o prefix) or binary (0b prefix).
  2. Strings with usual escapes `\n`, `\t`, `\b`, `\r`, `\\`, `\"`.
     Strings are encoding agnostic sequences of bytes. Non printable
     characters can be escaped by 3 digits decimal codes `\ddd` or
     2 digit hexadecimal codes `\xHH`.

All domain specific constants are strings:

  - `tez` amounts are written using the same notation as JSON schemas
    and the command line client: thousands are optionally separated by
    comas, and centiles, if present, must be prefixed by a period.
     - in regexp form: `([0-9]{1,3}(,[0-9]{3})+)|[0-9]+(\.[0.9]{2})?`
     - `"1234567"`      means 123456700 tez centiles
     - `"1,234,567"`    means 123456700 tez centiles
     - `"1234567.89"`   means 123456789 tez centiles
     - `"1,234,567.00"` means 123456789 tez centiles
     - `"1234,567"`     is invalid
     - `"1,234,567."`   is invalid
     - `"1,234,567.0"`  is invalid
  - `timestamp`s are written using `RFC 339` notation.
  - `contract`s are the raw strings returned by JSON RPCs or the command
    line interface and cannot be forged by hand so their format is of
    no interest here.
  - `key`s are `Sha256` hashes of `ed25519` public keys encoded in
    `base48` format with the following custom alphabet:
    `"eXMNE9qvHPQDdcFx5J86rT7VRm2atAypGhgLfbS3CKjnksB4"`.
  - `signature`s are `ed25519` signatures as a series of hex-encoded bytes.

### Primitive applications

The simplest form requires to break the line after the primitive name
and after every argument. Argument must be indented by at least one
more space than the primitive, and all arguments must sit on the exact
same column.

    PRIM
      arg1
      arg2
      ...

If an argument of a primitive application is a primitive application
itself, its arguments must be pushed even further on the right, to
lift any ambiguity, as in the following example.

    PRIM1
      PRIM2
        arg1_prim2
        arg2_prim2
      arg2_prim1

It is possible to put successive arguments on a single line using
a semicolon as a separator:

    PRIM
      arg1; arg2
      arg3; arg4

It is also possible to add arguments on the same line as the primitive
as a lighter way to write simple expressions. An other representation
of the first example is:

    PRIM arg1 arg2 ...

It is possible to mix both notations as in:

    PRIM arg1 arg2
      arg3
      arg4

Or even:

    PRIM arg1 arg2
      arg3; arg4

Both equivalent to:

    PRIM
      arg1
      arg2
      arg3
      arg4

Trayling semicolons are ignored:

    PRIM
      arg1;
      arg2

Calling a primitive with a compound argument on a single line is
allowed by wrapping with parentheses. Another notation for the second
example is:

    PRIM1 (PRIM2 arg1_prim2 arg2_prim2) arg2_prim1

### Sequences

Successive instructions can be grouped as a single one by grouping
them inside braces, separated by semicolons. To prevent errors,
control flow primitives that take instructions as parameters require
sequences in the concrete syntax.

    IF { instr1_true ; instr2_true ; ... } { instr1_false ; instr2_false ; ... }

    IF
      { instr1_true ; instr2_true ; ... }
      { instr1_false ; instr2_false ; ... }

A sequence block can be split on several lines. In this situation, the
whole block, including the closing brace, must be indented with
respect to the first instruction.

    LAMBDA t_arg t_ret
      { instr1 ; instr2
        instr3 ; instr4 }

### Lexical conventions

Instructions are represented by uppercase identifiers, type
constructor are lowercase identifiers and constant constructors are
Capitalised.

  * Types, in lowercase, in prefixed notation as in this specification:

        string

        pair string (pair int8 tez)

        lambda int8 int16

    Of course, types can be split over multiple lines using the
    common indented notation.

        map
          string
          uint32

  * Constants are built using constructors (starting with a capital)
    followed by the actual value.

        Int8 1

    Compound constants such as lists, in order not to repeat the same
    constant constructor for each element, take the type(s) of inner
    values as first argument(s), and then the values without their
    constructors.

        List int8 1 2 3 4 5

        Pair int8 int16 1 2

    For constructors whose type cannot be completely deduced fron a
    single value, the free type variables must be specified. For this,
    some constant constructors take extra types arguments as follows.

        List int8

        None tez

        Left (Int8 3) int16

        Right int16 (Int8 3)

    When the type is already completely specified, by a parent
    constructor or as in the instruction PUSH, these annotations must
    be omitted.

        Pair int8 (list int16) 1 (List 2 3)

        Pair (option (pair void int8)) void
          None
          Void

        Pair (or int8 string) (or int8 string)
          Left 3
          Right "text"

  * Instructions, in uppercase:

        ADD

### Comments

A hash sign (`#`) anywhere outside of a string literal will make the
rest of the line (and itself) completely ignored, as in the following
example.

    PUSH (Int8 1) # pushes 1
    PUSH (Int8 2) # pushes 2
    ADD         # computes 2 + 1

IX - Examples
-------------

Contracts in the system are stored as a piece of code and a global
data storage. The type of the global data of the storage is fixed for
each contract at origination time. This is ensured statically by
checking on origination that the code preserves the type of the global
data. For this, the code of the contract is checked to be of the
following type lambda (pair (pair tez 'arg) 'global) -> (pair 'ret
'global) where 'global is the type of the original global store given
on origination. The contract also takes a parameter and an amount, and
returns a value, hence the complete calling convention above.

### Empty contract

Because of the calling convention, the empty sequence is not a valid
contract of type `(contract void void)`. The code for building a
contract of such a type must take a `void` argument, an amount in `tez`,
and transform a void global storage, and must thus be of type `(lambda
(pair (pair tez void) void) (pair void void))`.

Such a minimal contract is thus `{ CDR ; VOID ; PAIR }`.

### Reservoir contract

We want to create a contract that stores tez until a timestamp `T` or a
maximum amount `N` is reached. Whenever `N` is reached before `T`, all tokens
are reversed to an account `B` (and the contract is automatically
deleted). Any call to the contract's code performed after `T` will
otherwise transfer the tokens to another account `A`.

We want to build this contract in a reusable manner, so we do not
hard-code the parameters. Instead, we assume that the global data of
the contract are `(Pair (Pair T N) (Pair A B))`.

Hence, the global data of the contract has the following type

    'g =
      pair
        pair timestamp tez
        pair (contract void void) (contract void void)

Following the contract calling convention, the code is a lambda of type

    lambda
      pair (pair tez void) 'g
      pair void 'g

writen as

    lambda
      pair (pair tez void)
        pair
          pair timestamp tez
          pair (contract void void) (contract void void)
      pair void
        pair
          pair timestamp tez
          pair (contract void void) (contract void void)

its code is

    DUP ; CDAAR # T
    NOW
    COMPARE ; LE
    IF { DUP ; CDADR # N
         BALANCE
         COMPARE ; LE
         IF { } # nothing to do
            { DUP ; CDDDR # B
              BALANCE ; PUSH Void ; TRANSFER_TOKENS ; DROP } }
       { DUP ; CDDAR ; # A
         BALANCE ;
         PUSH Void ; TRANSFER_TOKENS ; DROP }
    CDR ; PUSH Void ; PAIR

### Reservoir contract (variant with broker and status)

We basically want the same contract as the previous one, but instead
of destroying it, we want to keep it alive, storing a flag `S` so that
we can afterwards if the tokens have been transfered to `A` or `B`. We also
want the broker `A` to get some fee `P` in any case.

We thus add variables `P` and `S`  to the global data of the contract,
which becomes `(Pair (S, Pair (T, Pair (Pair P N) (Pair A B))))`.  `P`
is the  fee for broker  `A`, `S` is the  state, as a  string `"open"`,
`"timeout"` or `"success"`.

At the beginning of the transaction:

     S is accessible via a CDAR
     T               via a CDDAR
     P               via a CDDDAAR
     N               via a CDDDADR
     A               via a CDDDDAR
     B               via a CDDDDDR

For the contract to stay alive, we test that all least `(Tez "1.00")` is
still available after each transaction. This value is given as an
example and must be updated according to the actual Tezos minmal
value for contract balance.

    DUP ; CDAR # S
    PUSH (String "open") ;
    COMPARE ; NEQ ;
    IF { FAIL ; CDR } # on "success", "timeout" or a bad init value
       { DUP ; CDDAR ; # T
         NOW ;
         COMPARE ; LT ;
         IF { # Before timeout
              # We compute ((1 + P) + N) tez for keeping the contract alive
              PUSH (Tez "1.00") ;
              DIP { DUP ; CDDDAAR } ; ADD ; # P
              DIP { DUP ; CDDDADR } ; ADD ; # N
              # We compare to the cumulated amount
              BALANCE ;
              COMPARE; LT ;
              IF { # Not enough cash, we accept the transaction
                   # and leave the global
                   CDR }
                 { # We transfer the fee to the broker
                   DUP ; CDDDAAR ; # P
                   DIP { DUP ; CDDDDAR } # A
                   PUSH Void ; TRANSFER_TOKENS ; DROP ;
                   # We transfer the rest to the destination
                   DUP ; CDDDADR ; # N
                   DIP { DUP ; CDDDDDR } # B
                   PUSH Void ; TRANSFER_TOKENS ; DROP ;
                   # We update the global
                   CDR ; CDR ; PUSH (String "success") ; PAIR } }
            { # After timeout
              # We try to transfer P tez to A
              PUSH (Tez "1.00") ; BALANCE ; SUB ; # available
              DIP { DUP ; CDDDAAR } ;# P
              COMPARE ; LT ; # available < P
              IF { PUSH (Tez "1.00") ; BALANCE ; SUB ; # available
                   DIP { DUP ; CDDDDAR } # A
                   PUSH Void ; TRANSFER_TOKENS ; DROP }
                 { DUP ; CDDDAAR ; # P
                   DIP { DUP ; CDDDDAR } # A
                   PUSH Void ; TRANSFER_TOKENS ; DROP }
              # We transfer the rest to B
              PUSH (Tez "1.00") ; BALANCE ; SUB ; # available
              DIP { DUP ; CDDDDDR } # B
              PUSH Void ; TRANSFER_TOKENS ; DROP ;
              # We update the global
              CDR ; CDR ; PUSH (String "timeout") ; PAIR } }
    # return Void
    PUSH Void ; PAIR

### Forward contract

We want to write a forward contract on dried peas. The contract takes
as global data the tons of peas `Q`, the expected delivery date `T`, the
contract agreement date `Z`, a strike `K`, a collateral `C` per ton of dried
peas, and the accounts of the buyer `B`, the seller `S` and the warehouse
`W`.

These parameters as grouped in the global storage as follows:

    Pair
      (pair uint32 (pair timestamp timestamp))
      pair
        pair tez tez
        pair (pair account account) account
      Pair (Pair Q (Pair T Z))
      Pair (Pair K C) (Pair (Pair B S) W)

The 24 hours after timestamp `Z` are for the buyer and seller to store
their collateral `(Q * C)`. For this, the contract takes a string as
parameter, matching `"buyer"` or `"seller"` indicating the party for which
the tokens are transfered. At the end of this day, each of them can
send a transaction to send its tokens back. For this, we need to store
who already paid and how much, as a `(pair tez tez)` where the left
component is the buyer and the right one the seller.

After the first day, nothing cam happen until `T`.

During the 24 hours after `T`, the buyer must pay `(Q * K)` to the
contract, minus the amount already sent.

After this day, if the buyer didn't pay enough then any transaction
will send all the tokens to the seller.

Otherwise, the  seller must deliver at  least `Q` tons of  dried peas to
the warehouse, in  the next 24 hours.  When the amount is  equal to or
exceeds `Q`, all the tokens are transfered to the seller and the contract
is destroyed. For  storing the quantity of peas  already delivered, we
add a counter  of type `uint32` in the global  storage. For knowing this
quantity, we accept messages from W with a partial amount of delivered
peas as argument.

After this day, any transaction will send all the tokens to the buyer
(not enough peas have been delivered in time).

Hence, the global storage is a pair, with the counters on the left,
and the constant parameters on the right, initially as follows.

    Pair
      pair unit32 (pair tez tez)
      pair
        pair uint32 (pair timestamp timestamp)
        pair
          pair tez tez
          pair (pair account account) account
      Pair 0 (Pair 0_00 0_00)
      Pair
        Pair (Pair Q (Pair T Z))
        Pair (Pair K C) (Pair (Pair B S) W)

The parameter of the transaction will be either a transfer from the
buyer or the seller or a delivery notification from the warehouse of
type `(or string uint32)`.

At the beginning of the transaction:

    Q is accessible via a CDDAAR
    T               via a CDDADAR
    Z               via a CDDADDR
    K               via a CDDDAAR
    C               via a CDDDADR
    B               via a CDDDDAAR
    S               via a CDDDDADR
    W               via a CDDDDDR
    the delivery counter via a CDAAR
    the amount versed by the buyer via a CDADAR
    the amount versed by the seller via a CDADDR
    the argument via a CADR

The contract returns a void value, and we assume that it is created
with the minimum amount, set to `(Tez "1.00")`.

The code of the contract is thus as follows.

    DUP ; CDDADDR ; # Z
    PUSH (Uint64 86400) ; SWAP ; ADD ; # one day in second
    NOW ; COMPARE ; LT ;
    IF { # Before Z + 24
         DUP ; CADR ; # we must receive (Left "buyer") or (Left "seller")
         IF_LEFT
           { DUP ; PUSH (String "buyer") ; COMPARE ; EQ ;
             IF { DROP ;
                  DUP ; CDADAR ; # amount already versed by the buyer
                  DIP { DUP ; CAAR } ; ADD ; # transaction
                  #  then we rebuild the globals
                  DIP { DUP ; CDADDR } ; PAIR ; # seller amount
                  PUSH (Uint32 0) ; PAIR ; # delivery counter at 0
                  DIP { CDDR } ; PAIR ; # parameters
                  # and return Void
                  PUSH Void ; PAIR }
                { PUSH (String "seller") ; COMPARE ; EQ ;
                  IF { DUP ; CDADDR ; # amount already versed by the seller
                       DIP { DUP ; CAAR } ; ADD ; # transaction
                       #  then we rebuild the globals
                       DIP { DUP ; CDADAR } ; SWAP ; PAIR ; # buyer amount
                       PUSH (Uint32 0) ; PAIR ; # delivery counter at 0
                       DIP { CDDR } ; PAIR ; # parameters
                       # and return Void
                       PUSH Void ; PAIR }
                     { FAIL ; CDR ; PUSH Void ; PAIR }}} # (Left _)
           { FAIL ; DROP ; CDR ; PUSH Void ; PAIR }} # (Right _)
       { # After Z + 24
         # test if the required amount is reached
         DUP ; CDDAAR ; # Q
         DIP { DUP ; CDDDADR } ; MUL ; # C
         PUSH (Uint8 2) ; MUL ;
         PUSH (Tez "1.00") ; ADD ;
         BALANCE ; COMPARE ; LT ; # balance < 2 * (Q * C) + 1
         IF { # refund the parties
              DUP ; CDADAR ; # amount versed by the buyer
              DIP { DUP ; CDDDDAAR } # B
              PUSH Void ; TRANSFER_TOKENS ; DROP
              DUP ; CDADDR ; # amount versed by the seller
              DIP { DUP ; CDDDDADR } # S
              PUSH Void ; TRANSFER_TOKENS ; DROP
              BALANCE ; # bonus to the warehouse to destroy the account
              DIP { DUP ; CDDDDDR } # W
              PUSH Void ; TRANSFER_TOKENS ; DROP
              # return void, don't change the global
              # since the contract will be destroyed
              CDR ; PUSH Void ; PAIR }
            { # otherwise continue
              DUP ; CDDADAR # T
              NOW ; COMPARE ; LT
              IF { FAIL ; CDR ; PUSH Void ; PAIR } # Between Z + 24 and T
                 { # after T
                   DUP ; CDDADAR # T
                   PUSH (Uint64 86400) ; ADD # one day in second
                   NOW ; COMPARE ; LT
                   IF { # Between T and T + 24
                        # we only accept transactions from the buyer
                        DUP ; CADR ; # we must receive (Left "buyer")
                        IF_LEFT
                          { PUSH (String "buyer") ; COMPARE ; EQ ;
                            IF { DUP ; CDADAR ; # amount already versed by the buyer
                                 DIP { DUP ; CAAR } ; ADD ; # transaction
                                 # The amount must not exceed Q * K
                                 DUP ;
                                 DIIP { DUP ; CDDAAR ; # Q
                                        DIP { DUP ; CDDDAAR } ; MUL ; } ; # K
                                 DIP { COMPARE ; GT ; # new amount > Q * K
                                       IF { FAIL } { } } ; # abort or continue
                                 #  then we rebuild the globals
                                 DIP { DUP ; CDADDR } ; PAIR ; # seller amount
                                 PUSH (Uint32 0) ; PAIR ; # delivery counter at 0
                                 DIP { CDDR } ; PAIR ; # parameters
                                 # and return Void
                                 PUSH Void ; PAIR }
                               { FAIL ; CDR ; PUSH Void ; PAIR }} # (Left _)
                          { FAIL ; DROP ; CDR ; PUSH Void ; PAIR }} # (Right _)
                      { # After T + 24
                        # test if the required payment is reached
                        DUP ; CDDAAR ; # Q
                        DIP { DUP ; CDDDAAR } ; MUL ; # K
                        DIP { DUP ; CDADAR } ; # amount already versed by the buyer
                        COMPARE ; NEQ ;
                        IF { # not reached, pay the seller and destroy the contract
                             BALANCE ;
                             DIP { DUP ; CDDDDADR } # S
                             PUSH Void ; TRANSFER_TOKENS ; DROP ;
                             # and return Void
                             CDR ; PUSH Void ; PAIR }
                           { # otherwise continue
                             DUP ; CDDADAR # T
                             PUSH (Uint64 86400) ; ADD ;
                             PUSH (Uint64 86400) ; ADD ; # two days in second
                             NOW ; COMPARE ; LT
                             IF { # Between T + 24 and T + 48
                                  # We accept only delivery notifications, from W
                                  DUP ; CDDDDDR ; MANAGER ; # W
                                  SOURCE void void ; MANAGER ;
                                  COMPARE ; NEQ ;
                                  IF { FAIL } {} # fail if not the warehouse
                                  DUP ; CADR ; # we must receive (Right amount)
                                  IF_LEFT
                                    { FAIL ; DROP ; CDR ; PUSH Void ; PAIR } # (Left _)
                                    { # We increment the counter
                                      DIP { DUP ; CDAAR } ; ADD ;
                                      # And rebuild the globals in advance
                                      DIP { DUP ; CDADR } ; PAIR ;
                                      DIP CDDR ; PAIR ;
                                      PUSH Void ; PAIR ;
                                      # We test if enough have been delivered
                                      DUP ; CDAAR ;
                                      DIP { DUP ; CDDAAR } ;
                                      COMPARE ; LT ; # counter < Q
                                      IF { } # wait for more
                                         { # Transfer all the money to the seller
                                           BALANCE ; # and destroy the contract
                                           DIP { DUP ; CDDDDADR } # S
                                           PUSH Void ; TRANSFER_TOKENS ; DROP }}}
                                { # after T + 48, transfer everything to the buyer
                                  BALANCE ; # and destroy the contract
                                  DIP { DUP ; CDDDDAAR } # B
                                  PUSH Void ; TRANSFER_TOKENS ; DROP ;
                                  # and return void
                                  CDR ; PUSH Void ; PAIR }}}}}}

X - Full grammar
----------------

    <tagged data> ::=
      | <string constant>
      | Int8 <int constant>
      | Int16 <int constant>
      | Int32 <int constant>
      | Int64 <int constant>
      | Uint8 <int constant>
      | Uint16 <int constant>
      | Uint32 <int constant>
      | Uint64 <int constant>
      | Void
      | True
      | False
      | Timestamp <timestamp string constant>
      | Signature <signature string constant>
      | Tez <tez string constant>
      | Key <key string constant>
      | Left <tagged data> <type>
      | Right <type> <tagged data>
      | Or <type> <type> <untagged data>
      | Some <tagged data>
      | Some <type> <untagged data>
      | None <type>
      | Option <type> <untagged data>
      | Pair <tagged data> <tagged data>
      | Pair <type> <type> <untagged data> <untagged data>
      | List <type> <untagged data> ...
      | Set <comparable type> <untagged data> ...
      | Map <comparable type> <type> (Item <untagged data> <untagged data>) ...
      | Contract <type> <type> <contract string constant>
      | Lambda <type> <type> { <instruction> ... }
    <untagged data> ::=
      | <int constant>
      | <string constant>
      | <timestamp string constant>
      | <signature string constant>
      | <key string constant>
      | <tez string constant>
      | <contract string constant>
      | Void
      | True
      | False
      | Pair <untagged data> <untagged data>
      | Left <untagged data>
      | Right <untagged data>
      | Some <untagged data>
      | None
      | List <untagged data> ...
      | Set <untagged data> ...
      | Map (Item <untagged data> <untagged data>) ...
    <instruction> ::=
      | { <instruction> ... }
      | DROP
      | DUP
      | SWAP
      | PUSH <tagged data>
      | SOME
      | NONE <type>
      | IF_NONE { <instruction> ... } { <instruction> ... }
      | PAIR
      | CAR
      | CDR
      | LEFT <type>
      | RIGHT <type>
      | IF_LEFT { <instruction> ... } { <instruction> ... }
      | NIL <type>
      | CONS
      | IF_CONS { <instruction> ... } { <instruction> ... }
      | EMPTY_SET <type>
      | EMPTY_MAP <comparable type> <type>
      | MAP
      | REDUCE
      | MEM
      | GET
      | UPDATE
      | IF { <instruction> ... } { <instruction> ... }
      | LOOP { <instruction> ... }
      | LAMBDA <type> <type> { <instruction> ... }
      | EXEC
      | DIP { <instruction> ... }
      | FAIL
      | NOP
      | CONCAT
      | ADD
      | SUB
      | MUL
      | DIV
      | ABS
      | NEG
      | MOD
      | LSL
      | LSR
      | OR
      | AND
      | XOR
      | NOT
      | COMPARE
      | EQ
      | NEQ
      | LT
      | GT
      | LE
      | GE
      | CAST
      | CHECKED_ABS
      | CHECKED_NEG
      | CHECKED_ADD
      | CHECKED_SUB
      | CHECKED_MUL
      | CHECKED_CAST
      | FLOOR
      | CEIL
      | INF
      | NAN
      | ISNAN
      | NANAN
      | MANAGER
      | TRANSFER_TOKENS
      | CREATE_ACCOUNT
      | CREATE_CONTRACT
      | NOW
      | AMOUNT
      | BALANCE
      | CHECK_SIGNATURE
      | H
      | STEPS_TO_QUOTA
      | SOURCE <type> <type>
    <type> ::=
      | int8
      | int16
      | int32
      | int64
      | uint8
      | uint16
      | uint32
      | uint64
      | void
      | string
      | tez
      | bool
      | key
      | timestamp
      | signature
      | option <type>
      | list <type>
      | set <comparable type>
      | contract <type> <type>
      | pair <type> <type>
      | union <type> <type>
      | lambda <type> <type>
      | map <comparable type> <type>
    <comparable type> ::=
      | int8
      | int16
      | int32
      | int64
      | uint8
      | uint16
      | uint32
      | uint64
      | string
      | tez
      | bool
      | key
      | timestamp

XI - Reference implementation
-----------------------------

The language is implemented in OCaml as follows:

  * The lower internal representation is written as a GADT whose type
    parameters encode exactly the typing rules given in this
    specification. In other words, if a program written in this
    representation is accepted by OCaml's typechecker, it is
    mandatorily type-safe. This of course also valid for programs not
    handwritten but generated by OCaml code, so we are sure that any
    manipulated code is type-safe.

    In the end, what remains to be checked is the encoding of the
    typing rules as OCaml types, which boils down to half a line of
    code for each instruction. Everything else is left to the
    venerable and well trusted OCaml.

  * The interpreter is basically the direct transcription of the
    rewriting rules presented above. It takes an instruction, a stack
    and transforms it. OCaml's typechecker ensures that the
    transformation respects the pre and post stack types declared by
    the GADT case for each instruction.

    The only things that remain to we reviewed are value dependent
    choices, such as that we did not swap true and false when
    interpreting the If instruction.

  * The input, untyped internal representation is an OCaml ADT with
    the only 5 grammar constructions: `String`, `Int`, `Seq` and
    `Prim`.  It is the target language for the parser, since not all
    parsable programs are well typed, and thus could simply not be
    constructed using the GADT.

  * The typechecker is a simple function that recognizes the abstract
    grammar described in section X by pattern matching, producing the
    well-typed, corresponding GADT expressions. It is mostly a
    checker, not a full inferer, and thus takes some annotations
    (basically the inpout and output of the program, of lambdas and of
    uninitialized maps and sets). It works by performing a
    symbolic evaluation of the program, transforming a symbolic
    stack. It only needs one pass over the whole program.

    Here again, OCaml does most of the checking, the structure of the
    function is very simple, what we have to check is that we
    transform a `Prim ("If", ...)` into an `If`, a `Prim ("Dup", ...)`
    into a `Dup`, etc.
