Michelson: the language of Smart Contracts in Tezos
===================================================

The language is stack based, with high level data types and primitives
and strict static type checking. Its design cherry picks traits from
several language families. Vigilant readers will notice direct
references to Forth, Scheme, ML and Cat.

A Michelson program is a series of instructions that are run in
sequence: each instruction receives as input the stack resulting of the
previous instruction, and rewrites it for the next one. The stack
contains both immediate values and heap allocated structures. All values
are immutable and garbage collected.

A Michelson program receives as input a single element stack containing
an input value and the contents of a storage space. It must return a
single element stack containing an output value and the new contents of
the storage space. Alternatively, a Michelson program can fail,
explicitly using a specific opcode, or because something went wrong that
could not be caught by the type system (e.g. division by zero, gas
exhaustion).

The types of the input, output and storage are fixed and monomorphic,
and the program is typechecked before being introduced into the system.
No smart contract execution can fail because an instruction has been
executed on a stack of unexpected length or contents.

This specification gives the complete instruction set, type system and
semantics of the language. It is meant as a precise reference manual,
not an easy introduction. Even though, some examples are provided at the
end of the document and can be read first or at the same time as the
specification.

Table of contents
-----------------

-  I - Semantics
-  II - Type system
-  III - Core data types
-  IV - Core instructions
-  V - Operations
-  VI - Domain specific data types
-  VII - Domain specific operations
-  VIII - Macros
-  IX - Concrete syntax
-  X - JSON syntax
-  XI - Examples
-  XII - Full grammar
-  XIII - Reference implementation

I - Semantics
-------------

This specification gives a detailed formal semantics of the Michelson
language. It explains in a symbolic way the computation performed by the
Michelson interpreter on a given program and initial stack to produce
the corresponding resulting stack. The Michelson interpreter is a pure
function: it only builds a result stack from the elements of an initial
one, without affecting its environment. This semantics is then naturally
given in what is called a big step form: a symbolic definition of a
recursive reference interpreter. This definition takes the form of a
list of rules that cover all the possible inputs of the interpreter
(program and stack), and describe the computation of the corresponding
resulting stacks.

Rules form and selection
~~~~~~~~~~~~~~~~~~~~~~~~

The rules have the main following form.

::

    > (syntax pattern) / (initial stack pattern)  =>  (result stack pattern)
        iff (conditions)
        where (recursions)
	and (more recursions)

The left hand side of the ``=>`` sign is used for selecting the rule.
Given a program and an initial stack, one (and only one) rule can be
selected using the following process. First, the toplevel structure of
the program must match the syntax pattern. This is quite simple since
there is only a few non trivial patterns to deal with instruction
sequences, and the rest is made of trivial pattern that match one
specific instruction. Then, the initial stack must match the initial
stack pattern. Finally, some rules add extra conditions over the values
in the stack that follow the ``iff`` keyword. Sometimes, several rules
may apply in a given context. In this case, the one that appears first
in this specification is to be selected. If no rule applies, the result
is equivalent to the one for the explicit ``FAIL`` instruction. This
case does not happen on well-typed programs, as explained in the next
section.

The right hand side describes the result of the interpreter if the rule
applies. It consists in a stack pattern, whose part are either
constants, or elements of the context (program and initial stack) that
have been named on the left hand side of the ``=>`` sign.

Recursive rules (big step form)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, the result of interpreting a program is derived from the
result of interpreting another one (as in conditionals or function
calls). In these cases, the rule contains a clause of the following
form.

::

    where (intermediate program) / (intermediate stack)  =>  (partial result)

This means that this rules applies in case interpreting the intermediate
state on the left gives the pattern on the right.

The left hand sign of the ``=>`` sign is constructed from elements of
the initial state or other partial results, and the right hand side
identify parts that can be used to build the result stack of the rule.

If the partial result pattern does not actually match the result of the
interpretation, then the result of the whole rule is equivalent to the
one for the explicit ``FAIL`` instruction. Again, this case does not
happen on well-typed programs, as explained in the next section.

Format of patterns
~~~~~~~~~~~~~~~~~~

Code patterns are of one of the following syntactical forms.

-  ``INSTR`` (an uppercase identifier) is a simple instruction (e.g.
   ``DROP``);
-  ``INSTR (arg) ...`` is a compound instruction, whose arguments can be
   code, data or type patterns (e.g. ``PUSH nat 3``) ;
-  ``{ (instr) ; ... }`` is a possibly empty sequence of instructions,
   (e.g. ``IF { SWAP ; DROP } { DROP }``), nested sequences can drop the
   braces ;
-  ``name`` is a pattern that matches any program and names a part of
   the matched program that can be used to build the result ;
-  ``_`` is a pattern that matches any instruction.

Stack patterns are of one of the following syntactical forms.

-  ``[FAIL]`` is the special failed state ;
-  ``[]`` is the empty stack ;
-  ``(top) : (rest)`` is a stack whose top element is matched by the
   data pattern ``(top)`` on the left, and whose remaining elements are
   matched by the stack pattern ``(rest)`` on the right (e.g.
   ``x : y : rest``) ;
-  ``name`` is a pattern that matches any stack and names it in order to
   use it to build the result ;
-  ``_`` is a pattern that matches any stack.

Data patterns are of one of the following syntactical forms.

-  integer/natural number literals, (e.g. ``3``) ;
-  string literals, (e.g. ``"contents"``) ;
-  ``Tag`` (capitalized) is a symbolic constant, (e.g. ``Unit``,
   ``True``, ``False``) ;
-  ``(Tag (arg) ...)`` tagged constructed data, (e.g. ``(Pair 3 4)``) ;
-  a code pattern for first class code values ;
-  ``name`` to name a value in order to use it to build the result ;
-  ``_`` to match any value.

The domain of instruction names, symbolic constants and data
constructors is fixed by this specification. Michelson does not let the
programmer introduce its own types.

Be aware that the syntax used in the specification may differ a bit from
the concrete syntax, which is presented in Section IX. In particular,
some instructions are annotated with types that are not present in the
concrete language because they are synthesized by the typechecker.

Shortcuts
~~~~~~~~~

Sometimes, it is easier to think (and shorter to write) in terms of
program rewriting than in terms of big step semantics. When it is the
case, and when both are equivalents, we write rules of the form:

::

    p / S  =>  S''
    where   p' / S'  =>  S''

using the following shortcut:

::

    p / S  =>  p' / S'

The concrete language also has some syntax sugar to group some common
sequences of operations as one. This is described in this specification
using a simple regular expression style recursive instruction rewriting.

II - Introduction to the type system and notations
--------------------------------------------------

This specification describes a type system for Michelson. To make things
clear, in particular to readers that are not accustomed to reading
formal programming language specifications, it does not give a
typechecking or inference algorithm. It only gives an intentional
definition of what we consider to be well-typed programs. For each
syntactical form, it describes the stacks that are considered well-typed
inputs, and the resulting outputs.

The type system is sound, meaning that if a program can be given a type,
then if run on a well-typed input stack, the interpreter will never
apply an interpretation rule on a stack of unexpected length or
contents. Also, it will never reach a state where it cannot select an
appropriate rule to continue the execution. Well-typed programs do not
block, and do not go wrong.

Type notations
~~~~~~~~~~~~~~

The specification introduces notations for the types of values, terms
and stacks. Apart from a subset of value types that appear in the form
of type annotations in some places throughout the language, it is
important to understand that this type language only exists in the
specification.

A stack type can be written:

-  ``[]`` for the empty stack ;
-  ``(top) : (rest)`` for the stack whose first value has type ``(top)``
   and queue has stack type ``(rest)``.

Instructions, programs and primitives of the language are also typed,
their types are written:

::

    (type of stack before) -> (type of stack after)

The types of values in the stack are written:

-  ``identifier`` for a primitive data-type (e.g. ``bool``),
-  ``identifier (arg)`` for a parametric data-type with one parameter
   type ``(arg)`` (e.g. ``list nat``),
-  ``identifier (arg) ...`` for a parametric data-type with several
   parameters (e.g. ``map string int``),
-  ``[ (type of stack before) -> (type of stack after) ]`` for a code
   quotation, (e.g. ``[ int : int : [] -> int : [] ]``),
-  ``lambda (arg) (ret)`` is a shortcut for
   ``[ (arg) : [] -> (ret) : [] ]``.

Meta type variables
~~~~~~~~~~~~~~~~~~~

The typing rules introduce meta type variables. To be clear, this has
nothing to do with polymorphism, which Michelson does not have. These
variables only live at the specification level, and are used to express
the consistency between the parts of the program. For instance, the
typing rule for the ``IF`` construct introduces meta variables to
express that both branches must have the same type.

Here are the notations for meta type variables:

-  ``'a`` for a type variable,
-  ``'A`` for a stack type variable,
-  ``_`` for an anonymous type or stack type variable.

Typing rules
~~~~~~~~~~~~

The system is syntax directed, which means here that it defines a single
typing rule for each syntax construct. A typing rule restricts the type
of input stacks that are authorized for this syntax construct, links the
output type to the input type, and links both of them to the
subexpressions when needed, using meta type variables.

Typing rules are of the form:

::

    (syntax pattern)
    :: (type of stack before) -> (type of stack after) [rule-name]
       iff (premises)

Where premises are typing requirements over subprograms or values in the
stack, both of the form ``(x) :: (type)``, meaning that value ``(x)``
must have type ``(type)``.

A program is shown well-typed if one can find an instance of a rule that
applies to the toplevel program expression, with all meta type variables
replaced by non variable type expressions, and of which all type
requirements in the premises can be proven well-typed in the same
manner. For the reader unfamiliar with formal type systems, this is
called building a typing derivation.

Here is an example typing derivation on a small program that computes
``(x+5)*10`` for a given input ``x``, obtained by instantiating the
typing rules for instructions ``PUSH``, ``ADD`` and for the sequence, as
found in the next sections. When instantiating, we replace the ``iff``
with ``by``.

::

    { PUSH nat 5 ; ADD ; PUSH nat 10 ; SWAP ; MUL }
    :: [ nat : [] -> nat : [] ]
       by { PUSH nat 5 ; ADD }
          :: [ nat : [] -> nat : [] ]
             by PUSH nat 5
                :: [ nat : [] -> nat : nat : [] ]
                   by 5 :: nat
            and ADD
                :: [ nat : nat : [] -> nat : [] ]
      and { PUSH nat 10 ; SWAP ; MUL }
          :: [ nat : [] -> nat : [] ]
             by PUSH nat 10
                :: [ nat : [] -> nat : nat : [] ]
                   by 10 :: nat
            and { SWAP ; MUL }
                :: [ nat : nat : [] -> nat : [] ]
                   by SWAP
                      :: [ nat : nat : [] -> nat : nat : [] ]
                  and MUL
                      :: [ nat : nat : [] -> nat : [] ]

Producing such a typing derivation can be done in a number of manners,
such as unification or abstract interpretation. In the implementation of
Michelson, this is done by performing a recursive symbolic evaluation of
the program on an abstract stack representing the input type provided by
the programmer, and checking that the resulting symbolic stack is
consistent with the expected result, also provided by the programmer.

Annotations
~~~~~~~~~~~

Most instructions in the language can optionally take an annotation.
Annotations allow you to better track data, on the stack and within
pairs and unions.

If added on the components of a type, the annotation will be propagated
by the typechecker throughout access instructions.

Annotating an instruction that produces a value on the stack will
rewrite the annotation on the toplevel of its type.

Trying to annotate an instruction that does not produce a value will
result in a typechecking error.

At join points in the program (``IF``, ``IF_LEFT``, ``IF_CONS``,
``IF_NONE``, ``LOOP``), annotations must be compatible. Annotations are
compatible if both elements are annotated with the same annotation or if
at least one of the values/types is unannotated.

Stack visualization tools like the Michelson’s Emacs mode print
annotations associated with each type in the program, as propagated by
the typechecker. This is useful as a debugging aid.

Side note
~~~~~~~~~

As with most type systems, it is incomplete. There are programs that
cannot be given a type in this type system, yet that would not go wrong
if executed. This is a necessary compromise to make the type system
usable. Also, it is important to remember that the implementation of
Michelson does not accept as many programs as the type system describes
as well-typed. This is because the implementation uses a simple single
pass typechecking algorithm, and does not handle any form of
polymorphism.

III - Core data types and notations
-----------------------------------

-  ``string``, ``nat``, ``int``: The core primitive constant types.

-  ``bool``: The type for booleans whose values are ``True`` and
   ``False``

-  ``unit``: The type whose only value is ``Unit``, to use as a
   placeholder when some result or parameter is non necessary. For
   instance, when the only goal of a contract is to update its storage.

-  ``list (t)``: A single, immutable, homogeneous linked list, whose
   elements are of type ``(t)``, and that we note ``{}`` for the empty
   list or ``{ first ; ... }``. In the semantics, we use chevrons to
   denote a subsequence of elements. For instance ``{ head ; <tail> }``.

-  ``pair (l) (r)``: A pair of values ``a`` and ``b`` of types ``(l)``
   and ``(r)``, that we write ``(Pair a b)``.

-  ``option (t)``: Optional value of type ``(t)`` that we note ``None``
   or ``(Some v)``.

-  ``or (l) (r)``: A union of two types: a value holding either a value
   ``a`` of type ``(l)`` or a value ``b`` of type ``(r)``, that we write
   ``(Left a)`` or ``(Right b)``.

-  ``set (t)``: Immutable sets of values of type ``(t)`` that we note as
   lists ``{ item ; ... }``, of course with their elements unique, and
   sorted.

-  ``map (k) (t)``: Immutable maps from keys of type ``(k)`` of values
   of type ``(t)`` that we note ``{ Elt key value ; ... }``, with keys
   sorted.
-  ``big_map (k) (t)``: Lazily deserialized maps from keys of type
   ``(k)`` of values of type ``(t)`` that we note ``{ Elt key value ; ... }``,
   with keys sorted.  These maps should be used if you intend to store
   large amounts of data in a map. They have higher gas costs than
   standard maps as data is lazily deserialized.  You are limited to a
   single ``big_map`` per program, which must appear on the left hand
   side of a pair in the contract's storage.

IV - Core instructions
----------------------

Control structures
~~~~~~~~~~~~~~~~~~

-  ``FAIL``: Explicitly abort the current program.

   :: \_ -> \_

   This special instruction is callable in any context, since it does
   not use its input stack (first rule below), and makes the output
   useless since all subsequent instruction will simply ignore their
   usual semantics to propagate the failure up to the main result
   (second rule below). Its type is thus completely generic.

::

    > FAIL / _  =>  [FAIL]
    > _ / [FAIL]  =>  [FAIL]

-  ``{ I ; C }``: Sequence.

::

    :: 'A   ->   'C
       iff   I :: [ 'A -> 'B ]
             C :: [ 'B -> 'C ]

    > I ; C / SA  =>  SC
        where   I / SA  =>  SB
        and   C / SB  =>  SC

-  ``IF bt bf``: Conditional branching.

::

    :: bool : 'A   ->   'B
       iff   bt :: [ 'A -> 'B ]
             bf :: [ 'A -> 'B ]

    > IF bt bf / True : S  =>  bt / S
    > IF bt bf / False : S  =>  bf / S

-  ``LOOP body``: A generic loop.

::

    :: bool : 'A   ->   'A
       iff   body :: [ 'A -> bool : 'A ]

    > LOOP body / True : S  =>  body ; LOOP body / S
    > LOOP body / False : S  =>  S

-  ``LOOP_LEFT body``: A loop with an accumulator

::

    :: (or 'a 'b) : 'A   ->   'A
       iff   body :: [ 'a : 'A -> (or 'a 'b) : 'A ]

    > LOOP_LEFT body / (Left a) : S  =>  body ; LOOP_LEFT body / a : S
    > LOOP_LEFT body / (Right b) : S  =>  b : S

-  ``DIP code``: Runs code protecting the top of the stack.

::

    :: 'b : 'A   ->   'b : 'C
       iff   code :: [ 'A -> 'C ]

    > DIP code / x : S  =>  x : S'
        where    code / S  =>  S'

-  ``EXEC``: Execute a function from the stack.

::

    :: 'a : lambda 'a 'b : 'C   ->   'b : 'C

    > EXEC / a : f : S  =>  r : S
        where f / a : []  =>  r : []

Stack operations
~~~~~~~~~~~~~~~~

-  ``DROP``: Drop the top element of the stack.

::

    :: _ : 'A   ->   'A

    > DROP / _ : S  =>  S

-  ``DUP``: Duplicate the top of the stack.

::

    :: 'a : 'A   ->   'a : 'a : 'A

    > DUP / x : S  =>  x : x : S

-  ``SWAP``: Exchange the top two elements of the stack.

::

    :: 'a : 'b : 'A   ->   'b : 'a : 'A

    > SWAP / x : y : S  =>  y : x : S

-  ``PUSH 'a x``: Push a constant value of a given type onto the stack.

::

    :: 'A   ->   'a : 'A
       iff   x :: 'a

    > PUSH 'a x / S  =>  x : S

-  ``UNIT``: Push a unit value onto the stack.

::

    :: 'A   ->   unit : 'A

    > UNIT / S  =>  Unit : S

-  ``LAMBDA 'a 'b code``: Push a lambda with given parameter and return
   types onto the stack.

::

    :: 'A ->  (lambda 'a 'b) : 'A

    > LAMBDA _ _ code / S  =>  code : S

Generic comparison
~~~~~~~~~~~~~~~~~~

Comparison only works on a class of types that we call comparable. A
``COMPARE`` operation is defined in an ad hoc way for each comparable
type, but the result of compare is always an ``int``, which can in turn
be checked in a generic manner using the following combinators. The
result of ``COMPARE`` is ``0`` if the top two elements of the stack are
equal, negative if the first element in the stack is less than the
second, and positive otherwise.

-  ``EQ``: Checks that the top of the stack EQuals zero.

::

    :: int : 'S   ->   bool : 'S

    > EQ / 0 : S  =>  True : S
    > EQ / v : S  =>  False : S
        iff v <> 0

-  ``NEQ``: Checks that the top of the stack does Not EQual zero.

::

    :: int : 'S   ->   bool : 'S

    > NEQ / 0 : S  =>  False : S
    > NEQ / v : S  =>  True : S
        iff v <> 0

-  ``LT``: Checks that the top of the stack is Less Than zero.

::

    :: int : 'S   ->   bool : 'S

    > LT / v : S  =>  True : S
        iff  v < 0
    > LT / v : S  =>  False : S
        iff v >= 0

-  ``GT``: Checks that the top of the stack is Greater Than zero.

::

    :: int : 'S   ->   bool : 'S

    > GT / v : S  =>  C / True : S
        iff  v > 0
    > GT / v : S  =>  C / False : S
        iff v <= 0

-  ``LE``: Checks that the top of the stack is Less Than of Equal to
   zero.

::

    :: int : 'S   ->   bool : 'S

    > LE / v : S  =>  True : S
        iff  v <= 0
    > LE / v : S  =>  False : S
        iff v > 0

-  ``GE``: Checks that the top of the stack is Greater Than of Equal to
   zero.

::

    :: int : 'S   ->   bool : 'S

    > GE / v : S  =>  True : S
        iff  v >= 0
    > GE / v : S  =>  False : S
        iff v < 0

V - Operations
--------------

Operations on booleans
~~~~~~~~~~~~~~~~~~~~~~

-  ``OR``

::

    :: bool : bool : 'S   ->   bool : 'S

    > OR / x : y : S  =>  (x | y) : S

-  ``AND``

::

    :: bool : bool : 'S   ->   bool : 'S

    > AND / x : y : S  =>  (x & y) : S

-  ``XOR``

::

    :: bool : bool : 'S   ->   bool : 'S

    > XOR / x : y : S  =>  (x ^ y) : S

-  ``NOT``

::

    :: bool : 'S   ->   bool : 'S

    > NOT / x : S  =>  ~x : S

Operations on integers and natural numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Integers and naturals are arbitrary-precision, meaning the only size
limit is fuel.

-  ``NEG``

::

    :: int : 'S   ->   int : 'S
    :: nat : 'S   ->   int : 'S

    > NEG / x : S  =>  -x : S

-  ``ABS``

::

    :: int : 'S   ->   nat : 'S

    > ABS / x : S  =>  abs (x) : S

-  ``ADD``

::

    :: int : int : 'S   ->   int : 'S
    :: int : nat : 'S   ->   int : 'S
    :: nat : int : 'S   ->   int : 'S
    :: nat : nat : 'S   ->   nat : 'S

    > ADD / x : y : S  =>  (x + y) : S

-  ``SUB``

::

    :: int : int : 'S   ->   int : 'S
    :: int : nat : 'S   ->   int : 'S
    :: nat : int : 'S   ->   int : 'S
    :: nat : nat : 'S   ->   int : 'S

    > SUB / x : y : S  =>  (x - y) : S

-  ``MUL``

::

    :: int : int : 'S   ->   int : 'S
    :: int : nat : 'S   ->   int : 'S
    :: nat : int : 'S   ->   int : 'S
    :: nat : nat : 'S   ->   nat : 'S

    > MUL / x : y : S  =>  (x * y) : S

-  ``EDIV`` Perform Euclidian division

::

    :: int : int : 'S   ->   option (pair int nat) : 'S
    :: int : nat : 'S   ->   option (pair int nat) : 'S
    :: nat : int : 'S   ->   option (pair int nat) : 'S
    :: nat : nat : 'S   ->   option (pair nat nat) : 'S

    > EDIV / x : 0 : S  =>  None : S
    > EDIV / x : y : S  =>  Some (Pair (x / y) (x % y)) : S
        iff y <> 0

Bitwise logical operators are also available on unsigned integers.

-  ``OR``

::

    :: nat : nat : 'S   ->   nat : 'S

    > OR / x : y : S  =>  (x | y) : S

-  ``AND`` (also available when the top operand is signed)

::

    :: nat : nat : 'S   ->   nat : 'S
    :: int : nat : 'S   ->   nat : 'S

    > AND / x : y : S  =>  (x & y) : S

-  ``XOR``

::

    :: nat : nat : 'S   ->   nat : 'S

    > XOR / x : y : S  =>  (x ^ y) : S

-  ``NOT`` The return type of ``NOT`` is an ``int`` and not a ``nat``.
   This is because the sign is also negated. The resulting integer is
   computed using two’s complement. For instance, the boolean negation
   of ``0`` is ``-1``. To get a natural back, a possibility is to use
   ``AND`` with an unsigned mask afterwards.

::

    :: nat : 'S   ->   int : 'S
    :: int : 'S   ->   int : 'S

    > NOT / x : S  =>  ~x : S

-  ``LSL``

::

    :: nat : nat : 'S   ->   nat : 'S

    > LSL / x : s : S  =>  (x << s) : S
        iff   s <= 256
    > LSL / x : s : S  =>  [FAIL]
        iff   s > 256

-  ``LSR``

::

    :: nat : nat : 'S   ->   nat : 'S

    > LSR / x : s : S  =>  (x >>> s) : S

-  ``COMPARE``: Integer/natural comparison

::

    :: int : int : 'S   ->   int : 'S
    :: nat : nat : 'S   ->   int : 'S

    > COMPARE / x : y : S  =>  -1 : S
        iff x < y
    > COMPARE / x : y : S  =>  0 : S
        iff x = y
    > COMPARE / x : y : S  =>  1 : S
        iff x > y

Operations on strings
~~~~~~~~~~~~~~~~~~~~~

Strings are mostly used for naming things without having to rely on
external ID databases. So what can be done is basically use string
constants as is, concatenate them and use them as keys.

-  ``CONCAT``: String concatenation.

::

    :: string : string : 'S   -> string : 'S

    > CONCAT / s : t : S  =>  (s ^ t) : S

-  ``COMPARE``: Lexicographic comparison.

::

    :: string : string : 'S   ->   int : 'S

    > COMPARE / s : t : S  =>  -1 : S
        iff s < t
    > COMPARE / s : t : S  =>  0 : S
        iff s = t
    > COMPARE / s : t : S  =>  1 : S
        iff s > t

Operations on pairs
~~~~~~~~~~~~~~~~~~~

-  ``PAIR``: Build a pair from the stack’s top two elements.

::

    :: 'a : 'b : 'S   ->   pair 'a 'b : 'S

    > PAIR / a : b : S  =>  (Pair a b) : S

-  ``CAR``: Access the left part of a pair.

::

    :: pair 'a _ : 'S   ->   'a : 'S

    > CAR / (Pair a _) : S  =>  a : S

-  ``CDR``: Access the right part of a pair.

::

    :: pair _ 'b : 'S   ->   'b : 'S

    > CDR / (Pair _ b) : S  =>  b : S

Operations on sets
~~~~~~~~~~~~~~~~~~

-  ``EMPTY_SET 'elt``: Build a new, empty set for elements of a given
   type.

   The ``'elt`` type must be comparable (the ``COMPARE``
   primitive must be defined over it).

::

    :: 'S   ->   set 'elt : 'S

    > EMPTY_SET _ / S  =>  {} : S

-  ``MEM``: Check for the presence of an element in a set.

::

    :: 'elt : set 'elt : 'S   ->  bool : 'S

    > MEM / x : {} : S  =>  false : S
    > MEM / x : { hd ; <tl> } : S  =>  r : S
        iff COMPARE / x : hd : []  =>  1 : []
        where MEM / x : v : { <tl> } : S  =>  r : S
    > MEM / x : { hd ; <tl> } : S  =>  true : S
        iff COMPARE / x : hd : []  =>  0 : []
    > MEM / x : { hd ; <tl> } : S  =>  false : S
        iff COMPARE / x : hd : []  =>  -1 : []

-  ``UPDATE``: Inserts or removes an element in a set, replacing a
   previous value.

::

    :: 'elt : bool : set 'elt : 'S   ->   set 'elt : 'S

    > UPDATE / x : false : {} : S  =>  {} : S
    > UPDATE / x : true : {} : S  =>  { x } : S
    > UPDATE / x : v : { hd ; <tl> } : S  =>  { hd ; <tl'> } : S
        iff COMPARE / x : hd : []  =>  1 : []
        where UPDATE / x : v : { <tl> } : S  =>  { <tl'> } : S
    > UPDATE / x : false : { hd ; <tl> } : S  =>  { <tl> } : S
        iff COMPARE / x : hd : []  =>  0 : []
    > UPDATE / x : true : { hd ; <tl> } : S  =>  { hd ; <tl> } : S
        iff COMPARE / x : hd : []  =>  0 : []
    > UPDATE / x : false : { hd ; <tl> } : S  =>  { hd ; <tl> } : S
        iff COMPARE / x : hd : []  =>  -1 : []
    > UPDATE / x : true : { hd ; <tl> } : S  =>  { x ; hd ; <tl> } : S
        iff COMPARE / x : hd : []  =>  -1 : []

-  ``REDUCE``: Apply a function on a set passing the result of each
   application to the next one and return the last.

::

    :: lambda (pair 'elt * 'b) 'b : set 'elt : 'b : 'S   ->   'b : 'S

    > REDUCE / f : {} : b : S  =>  b : S
    > REDUCE / f : { hd : <tl> } : b : S  =>  REDUCE / f : { <tl> } : c : S
        where f / Pair hd b : []  =>  c : []

-  ``ITER body``: Apply the body expression to each element of a set.
   The body sequence has access to the stack.

::

    :: (set 'elt) : 'A   ->  'A
       iff body :: [ 'elt : 'A -> 'A ]

    > ITER body / {} : S  =>  S
    > ITER body / { hd ; <tl> } : S  =>  body; ITER body / hd : { <tl> } : S

-  ``SIZE``: Get the cardinality of the set.

::

    :: set 'elt : 'S -> nat : 'S

    > SIZE / {} : S  =>  0 : S
    > SIZE / { _ ; <tl> } : S  =>  1 + s : S
        where SIZE / { <tl> } : S  =>  s : S

Operations on maps
~~~~~~~~~~~~~~~~~~

-  ``EMPTY_MAP 'key 'val``: Build a new, empty map from keys of a
   given type to values of another given type.

   The ``'key`` type must be comparable (the ``COMPARE`` primitive must
   be defined over it).

::

    :: 'S -> map 'key 'val : 'S

    > EMPTY_MAP _ _ / S  =>  {} : S


-  ``GET``: Access an element in a map, returns an optional value to be
   checked with ``IF_SOME``.

::

    :: 'key : map 'key 'val : 'S   ->   option 'val : 'S

    > GET / x : {} : S  =>  None : S
    > GET / x : { Elt k v ; <tl> } : S  =>  opt_y : S
        iff COMPARE / x : k : []  =>  1 : []
	where GET / x : { <tl> } : S  =>  opt_y : S
    > GET / x : { Elt k v ; <tl> } : S  =>  Some v : S
        iff COMPARE / x : k : []  =>  0 : []
    > GET / x : { Elt k v ; <tl> } : S  =>  None : S
        iff COMPARE / x : k : []  =>  -1 : []

-  ``MEM``: Check for the presence of a binding for a key in a map.

::

    :: 'key : map 'key 'val : 'S   ->  bool : 'S

    > MEM / x : {} : S  =>  false : S
    > MEM / x : { Elt k v ; <tl> } : S  =>  r : S
        iff COMPARE / x : k : []  =>  1 : []
        where MEM / x : { <tl> } : S  =>  r : S
    > MEM / x : { Elt k v ; <tl> } : S  =>  true : S
        iff COMPARE / x : k : []  =>  0 : []
    > MEM / x : { Elt k v ; <tl> } : S  =>  false : S
        iff COMPARE / x : k : []  =>  -1 : []

-  ``UPDATE``: Assign or remove an element in a map.

::

    :: 'key : option 'val : map 'key 'val : 'S   ->   map 'key 'val : 'S

    > UPDATE / x : None : {} : S  =>  {} : S
    > UPDATE / x : Some y : {} : S  =>  { Elt x y } : S
    > UPDATE / x : opt_y : { Elt k v ; <tl> } : S  =>  { Elt k v ; <tl'> } : S
        iff COMPARE / x : k : []  =>  1 : []
	where UPDATE / x : opt_y : { <tl> } : S  =>  { <tl'> } : S
    > UPDATE / x : None : { Elt k v ; <tl> } : S  =>  { <tl> } : S
        iff COMPARE / x : k : []  =>  0 : []
    > UPDATE / x : Some y : { Elt k v ; <tl> } : S  =>  { Elt k y ; <tl> } : S
        iff COMPARE / x : k : []  =>  0 : []
    > UPDATE / x : None : { Elt k v ; <tl> } : S  =>  { Elt k v ; <tl> } : S
        iff COMPARE / x : k : []  =>  -1 : []
    > UPDATE / x : Some y : { Elt k v ; <tl> } : S  =>  { Elt x y ; Elt k v ; <tl> } : S
        iff COMPARE / x : k : []  =>  -1 : []


-  ``MAP``: Apply a function on a map and return the map of results
   under the same bindings.

::

    :: lambda (pair 'key 'val) 'b : map 'key 'val : 'S   ->   map 'key 'b : 'S

    > MAP / f : {} : S  =>  {} : S
    > MAP / f : { Elt k v ; <tl> } : S  =>  { Elt k (f (Pair k v)) ; <tl'> } : S
        where MAP / f : { <tl> } : S  =>  { <tl'> } : S


-  ``MAP body``: Apply the body expression to each element of a map. The
   body sequence has access to the stack.

::

    :: (map 'key 'val) : 'A   ->  (map 'key 'b) : 'A
       iff   body :: [ (pair 'key 'val) : 'A -> 'b : 'A ]

    > MAP body / {} : S  =>  {} : S
    > MAP body / { Elt k v ; <tl> } : S  =>  { Elt k (body (Pair k v)) ; <tl'> } : S
        where MAP body / { <tl> } : S  =>  { <tl'> } : S


-  ``REDUCE``: Apply a function on a map passing the result of each
   application to the next one and return the last.

::

    :: lambda (pair (pair 'key 'val) 'b) 'b : map 'key 'val : 'b : 'S   ->   'b : 'S

    > REDUCE / f : {} : b : S  =>  b : S
    > REDUCE / f : { Elt k v ; <tl> } : b : S  =>  REDUCE / f : { <tl> } : c : S
        where f / Pair (Pair k v) b : []  =>  c

-  ``ITER body``: Apply the body expression to each element of a map.
   The body sequence has access to the stack.

::

    :: (map 'elt 'val) : 'A   ->  'A
       iff   body :: [ (pair 'elt 'val) : 'A -> 'A ]

    > ITER body / {} : S  =>  S
    > ITER body / { Elt k v ; <tl> } : S  =>  body ; ITER body / (Pair k v) : { <tl> } : S

-  ``SIZE``: Get the cardinality of the map.

::

    :: map 'key 'val : 'S -> nat : 'S

    > SIZE / {} : S  =>  0 : S
    > SIZE / { _ ; <tl> } : S  =>  1 + s : S
        where  SIZE / { <tl> } : S  =>  s : S


Operations on ``big_maps``
~~~~~~~~~~~~~~~~~~~~~~~~~~

The behavior of these operations is the same as if they were normal
maps, except that under the hood, the elements are loaded and
deserialized on demand.


-  ``GET``: Access an element in a ``big_map``, returns an optional value to be
   checked with ``IF_SOME``.

::

    :: 'key : big_map 'key 'val : 'S   ->   option 'val : 'S

-  ``MEM``: Check for the presence of an element in a ``big_map``.

::

    :: 'key : big_map 'key 'val : 'S   ->  bool : 'S

-  ``UPDATE``: Assign or remove an element in a ``big_map``.

::

    :: 'key : option 'val : big_map 'key 'val : 'S   ->   big_map 'key 'val : 'S


Operations on optional values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  ``SOME``: Pack a present optional value.

::

    :: 'a : 'S   ->   option 'a : 'S

    > SOME / v : S  =>  (Some v) : S

-  ``NONE 'a``: The absent optional value.

::

    :: 'S   ->   option 'a : 'S

    > NONE / v : S  =>  None : S

-  ``IF_NONE bt bf``: Inspect an optional value.

::

    :: option 'a : 'S   ->   'b : 'S
       iff   bt :: [ 'S -> 'b : 'S]
             bf :: [ 'a : 'S -> 'b : 'S]

    > IF_NONE bt bf / (None) : S  =>  bt / S
    > IF_NONE bt bf / (Some a) : S  =>  bf / a : S

Operations on unions
~~~~~~~~~~~~~~~~~~~~

-  ``LEFT 'b``: Pack a value in a union (left case).

::

    :: 'a : 'S   ->   or 'a 'b : 'S

    > LEFT / v : S  =>  (Left v) : S

-  ``RIGHT 'a``: Pack a value in a union (right case).

::

    :: 'b : 'S   ->   or 'a 'b : 'S

    > RIGHT / v : S  =>  (Right v) : S

-  ``IF_LEFT bt bf``: Inspect a value of a variant type.

::

    :: or 'a 'b : 'S   ->   'c : 'S
       iff   bt :: [ 'a : 'S -> 'c : 'S]
             bf :: [ 'b : 'S -> 'c : 'S]

    > IF_LEFT bt bf / (Left a) : S  =>  bt / a : S
    > IF_LEFT bt bf / (Right b) : S  =>  bf / b : S

-  ``IF_RIGHT bt bf``: Inspect a value of a variant type.

::

    :: or 'a 'b : 'S   ->   'c : 'S
       iff   bt :: [ 'b : 'S -> 'c : 'S]
             bf :: [ 'a : 'S -> 'c : 'S]

    > IF_RIGHT bt bf / (Right b) : S  =>  bt / b : S
    > IF_RIGHT bt bf / (Left a) : S  =>  bf / a : S

Operations on lists
~~~~~~~~~~~~~~~~~~~

-  ``CONS``: Prepend an element to a list.

::

    :: 'a : list 'a : 'S   ->   list 'a : 'S

    > CONS / a : { <l> } : S  =>  { a ; <l> } : S

-  ``NIL 'a``: The empty list.

::

    :: 'S   ->   list 'a : 'S

    > NIL / S  =>  {} : S

-  ``IF_CONS bt bf``: Inspect an optional value.

::

    :: list 'a : 'S   ->   'b : 'S
       iff   bt :: [ 'a : list 'a : 'S -> 'b : 'S]
             bf :: [ 'S -> 'b : 'S]

    > IF_CONS bt bf / { a ; <rest> } : S  =>  bt / a : { <rest> } : S
    > IF_CONS bt bf / {} : S  =>  bf / S

-  ``MAP``: Apply a function on a list from left to right and return the
   list of results in the same order.

::

    :: lambda 'a 'b : list 'a : 'S -> list 'b : 'S

    > MAP / f : { a ; <rest> } : S  =>  { f a ; <rest'> } : S
        where MAP / f : { <rest> } : S  =>  { <rest'> } : S
    > MAP / f : {} : S  =>  {} : S


-  ``MAP body``: Apply the body expression to each element of the list.
   The body sequence has access to the stack.

::

    :: (list 'elt) : 'A   ->  (list 'b) : 'A
       iff   body :: [ 'elt : 'A -> 'b : 'A ]

    > MAP body / { a ; <rest> } : S  =>  { body a ; <rest'> } : S
        where MAP body / { <rest> } : S  =>  { <rest'> } : S
    > MAP body / {} : S  =>  {} : S


-  ``REDUCE``: Apply a function on a list from left to right passing the
   result of each application to the next one and return the last.

::

    :: lambda (pair 'a 'b) 'b : list 'a : 'b : 'S -> 'b : 'S

    > REDUCE / f : { a : <rest> } : b : S  =>  REDUCE / f : { <rest> } : c : S
        where f / Pair a b : []  =>  c
    > REDUCE / f : {} : b : S  =>  b : S


-  ``SIZE``: Get the number of elements in the list.

::

    :: list 'elt : 'S -> nat : 'S

    > SIZE / { _ ; <rest> } : S  =>  1 + s : S
        where  SIZE / { <rest> } : S  =>  s : S
    > SIZE / {} : S  =>  0 : S


-  ``ITER body``: Apply the body expression to each element of a list.
   The body sequence has access to the stack.

::

    :: (list 'elt) : 'A   ->  'A
         iff body :: [ 'elt : 'A -> 'A ]
    > ITER body / { a ; <rest> } : S  =>  body ; ITER body / a : { <rest> } : S
    > ITER body / {} : S  =>  S


VI - Domain specific data types
-------------------------------

-  ``timestamp``: Dates in the real world.

-  ``tez``: A specific type for manipulating tokens.

-  ``contract 'param``: A contract, with the type of its code.

-  ``address``: An untyped contract address.

-  ``operation``: An internal operation emitted by a contract.

-  ``key``: A public cryptography key.

-  ``key_hash``: The hash of a public cryptography key.

-  ``signature``: A cryptographic signature.

VII - Domain specific operations
--------------------------------

Operations on timestamps
~~~~~~~~~~~~~~~~~~~~~~~~

Current Timestamps can be obtained by the ``NOW`` operation, or
retrieved from script parameters or globals.

-  ``ADD`` Increment / decrement a timestamp of the given number of
   seconds.

::

    :: timestamp : int : 'S -> timestamp : 'S
    :: int : timestamp : 'S -> timestamp : 'S

    > ADD / seconds : nat (t) : S  =>  (seconds + t) : S
    > ADD / nat (t) : seconds : S  =>  (t + seconds) : S

-  ``SUB`` Subtract a number of seconds from a timestamp.

::

    :: timestamp : int : 'S -> timestamp : 'S

    > SUB / seconds : nat (t) : S  =>  (seconds - t) : S

-  ``SUB`` Subtract two timestamps.

::

    :: timestamp : timestamp : 'S -> int : 'S

    > SUB / seconds(t1) : seconds(t2) : S  =>  (t1 - t2) : S

-  ``COMPARE``: Timestamp comparison.

::

    :: timestamp : timestamp : 'S   ->   int : 'S

    > COMPARE / seconds(t1) : seconds(t2) : S  =>  -1 : S
        iff t1 < t2
    > COMPARE / seconds(t1) : seconds(t2) : S  =>  0 : S
        iff t1 = t2
    > COMPARE / seconds(t1) : seconds(t2) : S  =>  1 : S
        iff t1 > t2


Operations on Tez
~~~~~~~~~~~~~~~~~

Tez are internally represented by a 64 bit signed integer. There are
restrictions to prevent creating a negative amount of tez. Operations
are limited to prevent overflow and mixing them with other numerical
types by mistake. They are also mandatory checked for under/overflows.

-  ``ADD``:

::

    :: tez : tez : 'S   ->   tez : 'S

    > ADD / x : y : S  =>  [FAIL]   on overflow
    > ADD / x : y : S  =>  (x + y) : S

-  ``SUB``:

::

    :: tez : tez : 'S   ->   tez : 'S

    > SUB / x : y : S  =>  [FAIL]
        iff   x < y
    > SUB / x : y : S  =>  (x - y) : S

-  ``MUL``

::

    :: tez : nat : 'S   ->   tez : 'S
    :: nat : tez : 'S   ->   tez : 'S

    > MUL / x : y : S  =>  [FAIL]   on overflow
    > MUL / x : y : S  =>  (x * y) : S

-  ``EDIV``

::

    :: tez : nat : 'S   ->   option (pair tez tez) : 'S
    :: tez : tez : 'S   ->   option (pair nat tez) : 'S

    > EDIV / x : 0 : S  =>  None
    > EDIV / x : y : S  =>  Some (Pair (x / y) (x % y)) : S
        iff y <> 0

-  ``COMPARE``

::

   :: tez : tez : ’S -> int : ’S

   > COMPARE / x : y : S  =>  -1 : S
       iff x < y
   > COMPARE / x : y : S  =>  0 : S
       iff x = y
   > COMPARE / x : y : S  =>  1 : S
       iff x > y

Operations on contracts
~~~~~~~~~~~~~~~~~~~~~~~

-  ``MANAGER``: Access the manager of a contract.

::

    :: address : 'S   ->   key_hash option : 'S
    :: contract 'p : 'S   ->   key_hash : 'S

-  ``CREATE_CONTRACT``: Forge a contract creation operation.

::

    :: key_hash : option key_hash : bool : bool : tez : lambda (pair 'p 'g) (pair (list operation) 'g) : 'g : 'S
       -> operation : address : 'S

As with non code-emitted originations the contract code takes as
argument the transferred amount plus an ad-hoc argument and returns an
ad-hoc value. The code also takes the global data and returns it to be
stored and retrieved on the next transaction. These data are initialized
by another parameter. The calling convention for the code is as follows:
``(Pair arg globals)) -> (Pair operations globals)``, as extrapolated from
the instruction type. The first parameters are the manager, optional
delegate, then spendable and delegatable flags and finally the initial
amount taken from the currently executed contract. The contract is
returned as a first class value (to be dropped, passed as parameter or stored).
The ``CONTRACT 'p`` instruction will fail until it is actually originated.

-  ``CREATE_CONTRACT { storage 'g ; parameter 'p ; code ... }``:
   Forge a new contract from a literal.

::

    :: key_hash : option key_hash : bool : bool : tez : 'g : 'S
       -> operation : address : 'S

Originate a contract based on a literal. This is currently the only way
to include transfers inside of an originated contract. The first
parameters are the manager, optional delegate, then spendable and
delegatable flags and finally the initial amount taken from the
currently executed contract.

-  ``CREATE_ACCOUNT``: Forge an account (a contract without code) creation operation.

::

    :: key_hash : option key_hash : bool : tez : 'S
       ->   operation : contract unit : 'S

Take as argument the manager, optional delegate, the delegatable flag
and finally the initial amount taken from the currently executed
contract.

-  ``TRANSFER_TOKENS``: Forge a transaction.

::

    :: 'p : tez : contract 'p : 'S   ->   operation : S

The parameter must be consistent with the one expected by the
contract, unit for an account.

-  ``BALANCE``: Push the current amount of tez of the current contract.

::

    :: 'S   ->   tez : 'S

-  ``ADDRESS``: Push the untyped version of a contract.

::

    :: contract _ : 'S   ->   address : 'S

-  ``CONTRACT 'p``: Push the untyped version of a contract.

::

    :: address : 'S   ->   contract 'p : 'S

    > CONTRACT / addr : S  =>  Some addr : S
        iff addr exists and is a contract of parameter type 'p
    > CONTRACT / addr : S  =>  Some addr : S
        iff 'p = unit and addr is an implicit contract
    > CONTRACT / addr : S  =>  None : S
        otherwise

-  ``SOURCE``: Push the source contract of the current
   transaction.

::

    :: 'S   ->   address : 'S

-  ``SELF``: Push the current contract.

::

    :: 'S   ->   contract 'p : 'S
       where   contract 'p is the type of the current contract

-  ``AMOUNT``: Push the amount of the current transaction.

::

    :: 'S   ->   tez : 'S

-  ``IMPLICIT_ACCOUNT``: Return a default contract with the given
   public/private key pair. Any funds deposited in this contract can
   immediately be spent by the holder of the private key. This contract
   cannot execute Michelson code and will always exist on the
   blockchain.

::

    :: key_hash : 'S   ->   contract unit : 'S

Special operations
~~~~~~~~~~~~~~~~~~

-  ``STEPS_TO_QUOTA``: Push the remaining steps before the contract
   execution must terminate.

::

    :: 'S   ->   nat : 'S

-  ``NOW``: Push the timestamp of the block whose validation triggered
   this execution (does not change during the execution of the
   contract).

::

    :: 'S   ->   timestamp : 'S

Cryptographic primitives
~~~~~~~~~~~~~~~~~~~~~~~~

-  ``HASH_KEY``: Compute the b58check of a public key.

::

    :: key : 'S   ->   key_hash : 'S

-  ``H``: Compute a cryptographic hash of the value contents using the
   Blake2B cryptographic hash function.

::

    :: 'a : 'S   ->   string : 'S

-  ``CHECK_SIGNATURE``: Check that a sequence of bytes has been signed
   with a given key.

::

    :: key : pair signature string : 'S   ->   bool : 'S

-  ``COMPARE``:

::

    :: key_hash : key_hash : 'S   ->   int : 'S

    > COMPARE / x : y : S  =>  -1 : S
        iff x < y
    > COMPARE / x : y : S  =>  0 : S
        iff x = y
    > COMPARE / x : y : S  =>  1 : S
        iff x > y

VIII - Macros
-------------

In addition to the operations above, several extensions have been added
to the language’s concrete syntax. If you are interacting with the node
via RPC, bypassing the client, which expands away these macros, you will
need to desugar them yourself.

These macros are designed to be unambiguous and reversible, meaning that
errors are reported in terms of desugared syntax. Below you’ll see
these macros defined in terms of other syntactic forms. That is how
these macros are seen by the node.

Compare
~~~~~~~

Syntactic sugar exists for merging ``COMPARE`` and comparison
combinators, and also for branching.

-  ``CMP{EQ|NEQ|LT|GT|LE|GE}``

::

    > CMP(\op) / S  =>  COMPARE ; (\op) / S

-  ``IF{EQ|NEQ|LT|GT|LE|GE} bt bf``

::

    > IF(\op) bt bf / S  =>  (\op) ; IF bt bf / S

-  ``IFCMP{EQ|NEQ|LT|GT|LE|GE} bt bf``

::

    > IFCMP(\op) / S  =>  COMPARE ; (\op) ; IF bt bf / S

Assertion Macros
~~~~~~~~~~~~~~~~

All assertion operations are syntactic sugar for conditionals with a
``FAIL`` instruction in the appropriate branch. When possible, use them
to increase clarity about illegal states.

-  ``ASSERT``:

::

    > ASSERT  =>  IF {} {FAIL}

-  ``ASSERT_{EQ|NEQ|LT|LE|GT|GE}``:

::

    > ASSERT_(\op)  =>  IF(\op) {} {FAIL}

-  ``ASSERT_CMP{EQ|NEQ|LT|LE|GT|GE}``:

::

    > ASSERT_CMP(\op)  =>  IFCMP(\op) {} {FAIL}

-  ``ASSERT_NONE``

::

    > ASSERT_NONE  =>  IF_NONE {} {FAIL}

-  ``ASSERT_SOME``

::

    > ASSERT_SOME  =>  IF_SOME {FAIL} {}

-  ``ASSERT_LEFT``:

::

    > ASSERT_LEFT  =>  IF_LEFT {} {FAIL}

-  ``ASSERT_RIGHT``:

::

    > ASSERT_RIGHT  =>  IF_LEFT {FAIL} {}

Syntactic Conveniences
~~~~~~~~~~~~~~~~~~~~~~

These are macros are simply more convenient syntax for various common
operations.

-  ``DII+P code``: A syntactic sugar for working deeper in the stack.

::

    > DII(\rest=I*)P code / S  =>  DIP (DI(\rest)P code) / S

-  ``DUU+P``: A syntactic sugar for duplicating the ``n``\ th element of
   the stack.

::

    > DUU(\rest=U*)P / S  =>  DIP (DU(\rest)P) ; SWAP / S

-  ``P(A*AI)+R``: A syntactic sugar for building nested pairs in bulk.

::

    > P(\fst=A*)AI(\rest=(A*AI)+)R / S  =>  P(\fst)AIR ; P(\rest)R / S
    > PA(\rest=A*)AIR / S  =>  DIP (P(\rest)AIR) / S

-  ``C[AD]+R``: A syntactic sugar for accessing fields in nested pairs.

::

    > CA(\rest=[AD]+)R / S  =>  CAR ; C(\rest)R / S
    > CD(\rest=[AD]+)R / S  =>  CDR ; C(\rest)R / S

-  ``IF_SOME bt bf``: Inspect an optional value.

::

    :: option 'a : 'S   ->   'b : 'S
       iff   bt :: [ 'a : 'S -> 'b : 'S]
             bf :: [ 'S -> 'b : 'S]

    > IF_SOME / (Some a) : S  =>  bt / a : S
    > IF_SOME / (None) : S  =>  bf / S

-  ``SET_CAR``: Set the first value of a pair.

::

    > SET_CAR  =>  CDR ; SWAP ; PAIR

-  ``SET_CDR``: Set the first value of a pair.

::

    > SET_CDR  =>  CAR ; PAIR

-  ``SET_C[AD]+R``: A syntactic sugar for setting fields in nested
   pairs.

::

    > SET_CA(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CAR ; SET_C(\rest)R } ; CDR ; SWAP ; PAIR } / S
    > SET_CD(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CDR ; SET_C(\rest)R } ; CAR ; PAIR } / S

-  ``MAP_CAR`` code: Transform the first value of a pair.

::

    > MAP_CAR code  =>  DUP ; CDR ; SWAP ; code ; CAR ; PAIR

-  ``MAP_CDR`` code: Transform the first value of a pair.

::

    > MAP_CDR code  =>  DUP ; CDR ; code ; SWAP ; CAR ; PAIR

-  ``MAP_C[AD]+R`` code: A syntactic sugar for transforming fields in
   nested pairs.

::

    > MAP_CA(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CAR ; MAP_C(\rest)R code } ; CDR ; SWAP ; PAIR } / S
    > MAP_CD(\rest=[AD]+)R / S   =>
        { DUP ; DIP { CDR ; MAP_C(\rest)R code } ; CAR ; PAIR } / S

IX - Concrete syntax
--------------------

The concrete language is very close to the formal notation of the
specification. Its structure is extremely simple: an expression in the
language can only be one of the four following constructs.

1. An integer.
2. A character string.
3. The application of a primitive to a sequence of expressions.
4. A sequence of expressions.

This simple four cases notation is called Micheline.

Constants
~~~~~~~~~

There are two kinds of constants:

1. Integers or naturals in decimal (no prefix), hexadecimal (0x prefix),
   octal (0o prefix) or binary (0b prefix).
2. Strings with usual escapes ``\n``, ``\t``, ``\b``, ``\r``, ``\\``,
   ``\"``. The encoding of a Michelson source file must be UTF-8, and
   non-ASCII characters can only appear in comments. No line break can
   appear in a string. Any non-printable characters must be escaped
   using two hexadecimal characters, as in ``\xHH`` or the
   predefine escape sequences above..

Primitive applications
~~~~~~~~~~~~~~~~~~~~~~

A primitive application is a name followed by arguments

::

    prim arg1 arg2

When a primitive application is the argument to another primitive
application, it must be wrapped with parentheses.

::

    prim (prim1 arg11 arg12) (prim2 arg21 arg22)

Sequences
~~~~~~~~~

Successive expression can be grouped as a single sequence expression
using curly braces as delimiters and semicolon as separators.

::

    { expr1 ; expr2 ; expr3 ; expr4 }

A sequence can be passed as argument to a primitive.

::

    prim arg1 arg2 { arg3_expr1 ; arg3_expr2 }

Primitive applications right inside a sequence cannot be wrapped.

::

    { (prim arg1 arg2) } # is not ok

Indentation
~~~~~~~~~~~

To remove ambiguities for human readers, the parser enforces some
indentation rules.

-  For sequences:

   -  All expressions in a sequence must be aligned on the same column.
   -  An exception is made when consecutive expressions fit on the same
      line, as long as the first of them is correctly aligned.
   -  All expressions in a sequence must be indented to the right of the
      opening curly brace by at least one column.
   -  The closing curly brace cannot be on the left of the opening one.

-  For primitive applications:

   -  All arguments in an application must be aligned on the same
      column.
   -  An exception is made when consecutive arguments fit on the same
      line, as long as the first of them is correctly aligned.
   -  All arguments in a sequence must be indented to the right of the
      primitive name by at least one column.

.. _annotations-1:

Annotations
~~~~~~~~~~~

Sequences and primitive applications can receive an annotation.

An annotation is a lowercase identifier that starts with an ``@`` sign.
It comes after the opening curly brace for sequence, and after the
primitive name for primitive applications.

::

    { @annot
      expr ;
      expr ;
      ... }

    (prim @annot arg arg ...)

Differences with the formal notation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The concrete syntax follows the same lexical conventions as the
specification: instructions are represented by uppercase identifiers,
type constructors by lowercase identifiers, and constant constructors
are Capitalized.

All domain specific constants are Micheline strings with specific
formats:

-  ``tez`` amounts are written using the same notation as JSON schemas
   and the command line client: thousands are optionally separated by
   commas, and so goes for mutez.

   -  in regexp form: ``([0-9]{1,3}(,[0-9]{3})+)|[0-9]+(\.[0.9]{2})?``
   -  ``"1234567"`` means 1234567 tez
   -  ``"1,234,567"`` means 1234567 tez
   -  ``"1234567.89"`` means 1234567890000 mutez
   -  ``"1,234,567.0"`` means 123456789 tez
   -  ``"10,123.456,789"`` means 10123456789 mutez
   -  ``"1234,567"`` is invalid
   -  ``"1,234,567.123456"`` is invalid

-  ``timestamp``\ s are written using ``RFC 339`` notation.
-  ``contract``\ s are the raw strings returned by JSON RPCs or the
   command line interface and cannot be forged by hand so their format
   is of no interest here.
-  ``key``\ s are ``Blake2B`` hashes of ``ed25519`` public keys encoded
   in ``base58`` format with the following custom alphabet:
   ``"eXMNE9qvHPQDdcFx5J86rT7VRm2atAypGhgLfbS3CKjnksB4"``.
-  ``signature``\ s are ``ed25519`` signatures as a series of
   hex-encoded bytes.

To prevent errors, control flow primitives that take instructions as
parameters require sequences in the concrete syntax.

::

    IF { instr1_true ; instr2_true ; ... }
       { instr1_false ; instr2_false ; ... }

Main program structure
~~~~~~~~~~~~~~~~~~~~~~

The toplevel of a smart contract file must be an un-delimited sequence
of four primitive applications (in no particular order) that provide its
``parameter``, ``return`` and ``storage`` types, as well as its
``code``.

See the next section for a concrete example.

Comments
~~~~~~~~

A hash sign (``#``) anywhere outside of a string literal will make the
rest of the line (and itself) completely ignored, as in the following
example.

::

    { PUSH nat 1 ; # pushes 1
      PUSH nat 2 ; # pushes 2
      ADD }        # computes 2 + 1

Comments that span on multiple lines or that stop before the end of the
line can also be written, using C-like delimiters (``/* ... */``).

X - JSON syntax
---------------

Micheline expressions are encoded in JSON like this:

-  An integer ``N`` is an object with a single field ``"int"`` whose
   value is the decimal representation as a string.

   ``{ "int": "N" }``

-  A string ``"contents"`` is an object with a single field ``"string"``
   whose value is the decimal representation as a string.

   ``{ "string": "contents" }``

-  A sequence is a JSON array.

   ``[ expr, ... ]``

-  A primitive application is an object with two fields ``"prim"`` for
   the primitive name and ``"args"`` for the arguments (that must
   contain an array). A third optional field ``"annot"`` may contains
   an annotation, including the ``@`` sign.

   { “prim”: “pair”, “args”: [ { “prim”: “nat”, args: [] }, { “prim”:
   “nat”, args: [] } ], “annot”: “@numbers” }\`

As in the concrete syntax, all domain specific constants are encoded as
strings.

XI - Examples
-------------

Contracts in the system are stored as a piece of code and a global data
storage. The type of the global data of the storage is fixed for each
contract at origination time. This is ensured statically by checking on
origination that the code preserves the type of the global data. For
this, the code of the contract is checked to be of  type
``lambda (pair ’arg ’global) -> (pair (list operation) ’global)`` where
``’global`` is the type of the original global store given on origination.
The contract also takes a parameter and returns a list of internal operations,
hence the complete calling convention above. The internal operations are
queued for execution when the contract returns.

Empty contract
~~~~~~~~~~~~~~

The simplest contract is the contract for which the ``parameter`` and
``storage`` are all of type ``unit``. This contract is as follows:

::

    code { CDR ;           # keep the storage
           NIl operation ; # return no internal operation
           PAIR };         # respect the calling convention
    storage unit;
    parameter unit;

Reservoir contract
~~~~~~~~~~~~~~~~~~

We want to create a contract that stores tez until a timestamp ``T`` or
a maximum amount ``N`` is reached. Whenever ``N`` is reached before
``T``, all tokens are reversed to an account ``B`` (and the contract is
automatically deleted). Any call to the contract’s code performed after
``T`` will otherwise transfer the tokens to another account ``A``.

We want to build this contract in a reusable manner, so we do not
hard-code the parameters. Instead, we assume that the global data of the
contract are ``(Pair (Pair T N) (Pair A B))``.

Hence, the global data of the contract has the following type

::

    'g =
      pair
        (pair timestamp tez)
        (pair (contract unit) (contract unit))

Following the contract calling convention, the code is a lambda of type

::

    lambda
      (pair unit 'g)
      (pair (list operation) 'g)

written as

::

    lambda
      (pair
         unit
         (pair
           (pair timestamp tez)
           (pair (contract unit) (contract unit))))
      (pair
         (list operation)
         (pair
            (pair timestamp tez)
            (pair (contract unit) (contract unit))))

The complete source ``reservoir.tz`` is:

::

    parameter unit ;
    storage
      (pair
         (pair (timestamp @T) (tez @N)) # T N
         (pair (contract @A unit) (contract @B  unit))) ; # A B
    code
      { CDR ; DUP ; CAAR ; # T
        NOW ; COMPARE ; LE ;
        IF { DUP ; CADR ; # N
             BALANCE ;
             COMPARE ; LE ;
             IF { NIL operation ; PAIR }
                { DUP ; CDDR ; # B
                  BALANCE ; UNIT ;
                  TRANSFER_TOKENS ;
                  NIL operation ; SWAP ; CONS ;
                  PAIR } }
           { DUP ; CDAR ; # A
             BALANCE ;
             UNIT ;
             TRANSFER_TOKENS ;
             NIL operation ; SWAP ; CONS ;
             PAIR } }

Reservoir contract (variant with broker and status)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We basically want the same contract as the previous one, but instead of
leaving it empty, we want to keep it alive, storing a flag ``S`` so that we
can tell afterwards if the tokens have been transferred to ``A`` or
``B``. We also want a broker ``X`` to get some fee ``P`` in any case.

We thus add variables ``P`` and ``S`` and ``X`` to the global data of
the contract, now
``(Pair (S, Pair (T, Pair (Pair P N) (Pair X (Pair A B)))))``. ``P`` is
the fee for broker ``A``, ``S`` is the state, as a string ``"open"``,
``"timeout"`` or ``"success"``.

At the beginning of the transaction:

::

     S is accessible via a CDAR
     T               via a CDDAR
     P               via a CDDDAAR
     N               via a CDDDADR
     X               via a CDDDDAR
     A               via a CDDDDDAR
     B               via a CDDDDDDR

For the contract to stay alive, we test that all least ``(Tez "1.00")``
is still available after each transaction. This value is given as an
example and must be updated according to the actual Tezos minimal value
for contract balance.

The complete source ``scrutable_reservoir.tz`` is:

::

    parameter unit ;
    storage
      (pair
         string # S
         (pair
            timestamp # T
            (pair
               (pair tez tez) # P N
               (pair
                  (contract unit) # X
                  (pair (contract unit) (contract unit)))))) ; # A B
    code
      { DUP ; CDAR ; # S
        PUSH string "open" ;
        COMPARE ; NEQ ;
        IF { FAIL } # on "success", "timeout" or a bad init value
           { DUP ; CDDAR ; # T
             NOW ;
             COMPARE ; LT ;
             IF { # Before timeout
                  # We compute ((1 + P) + N) tez for keeping the contract alive
                  PUSH tez "1.00" ;
                  DIP { DUP ; CDDDAAR } ; ADD ; # P
                  DIP { DUP ; CDDDADR } ; ADD ; # N
                  # We compare to the cumulated amount
                  BALANCE ;
                  COMPARE; LT ;
                  IF { # Not enough cash, we just accept the transaction
                       # and leave the global untouched
                       CDR ; NIL operation ; PAIR }
                     { # Enough cash, successful ending
                       # We update the global
                       CDDR ; PUSH string "success" ; PAIR ;
                       # We transfer the fee to the broker
                       DUP ; CDDAAR ; # P
                       DIP { DUP ; CDDDAR } ; # X
                       UNIT ; TRANSFER_TOKENS ;
                       # We transfer the rest to A
                       DIP { DUP ; CDDADR ; # N
                             DIP { DUP ; CDDDDAR } ; # A
                             UNIT ; TRANSFER_TOKENS } ;
                       NIL operation ; SWAP ; CONS ; SWAP ; CONS ;
                       PAIR } }
                { # After timeout, we refund
                  # We update the global
                  CDDR ; PUSH string "timeout" ; PAIR ;
                  # We try to transfer the fee to the broker
                  PUSH tez "1.00" ; BALANCE ; SUB ; # available
                  DIP { DUP ; CDDAAR } ; # P
                  COMPARE ; LT ; # available < P
                  IF { PUSH tez "1.00" ; BALANCE ; SUB ; # available
                       DIP { DUP ; CDDDAR } ; # X
                       UNIT ; TRANSFER_TOKENS }
                     { DUP ; CDDAAR ; # P
                       DIP { DUP ; CDDDAR } ; # X
                       UNIT ; TRANSFER_TOKENS } ;
                  # We transfer the rest to B
                  DIP { PUSH tez "1.00" ; BALANCE ; SUB ; # available
                        DIP { DUP ; CDDDDDR } ; # B
                        UNIT ; TRANSFER_TOKENS } ;
                  NIL operation ; SWAP ; CONS ; SWAP ; CONS ;
                  PAIR } } }

Forward contract
~~~~~~~~~~~~~~~~

We want to write a forward contract on dried peas. The contract takes as
global data the tons of peas ``Q``, the expected delivery date ``T``,
the contract agreement date ``Z``, a strike ``K``, a collateral ``C``
per ton of dried peas, and the accounts of the buyer ``B``, the seller
``S`` and the warehouse ``W``.

These parameters as grouped in the global storage as follows:

::

    Pair
      (Pair (Pair Q (Pair T Z)))
      (Pair
         (Pair K C)
         (Pair (Pair B S) W))

of type

::

    pair
      (pair nat (pair timestamp timestamp))
      (pair
         (pair tez tez)
         (pair (pair account account) account))

The 24 hours after timestamp ``Z`` are for the buyer and seller to store
their collateral ``(Q * C)``. For this, the contract takes a string as
parameter, matching ``"buyer"`` or ``"seller"`` indicating the party for
which the tokens are transferred. At the end of this day, each of them
can send a transaction to send its tokens back. For this, we need to
store who already paid and how much, as a ``(pair tez tez)`` where the
left component is the buyer and the right one the seller.

After the first day, nothing cam happen until ``T``.

During the 24 hours after ``T``, the buyer must pay ``(Q * K)`` to the
contract, minus the amount already sent.

After this day, if the buyer didn’t pay enough then any transaction will
send all the tokens to the seller.

Otherwise, the seller must deliver at least ``Q`` tons of dried peas to
the warehouse, in the next 24 hours. When the amount is equal to or
exceeds ``Q``, all the tokens are transferred to the seller.
For storing the quantity of peas already
delivered, we add a counter of type ``nat`` in the global storage. For
knowing this quantity, we accept messages from W with a partial amount
of delivered peas as argument.

After this day, any transaction will send all the tokens to the buyer
(not enough peas have been delivered in time).

Hence, the global storage is a pair, with the counters on the left, and
the constant parameters on the right, initially as follows.

::

    Pair
      (Pair 0 (Pair 0_00 0_00))
      (Pair
         (Pair (Pair Q (Pair T Z)))
         (Pair
            (Pair K C)
            (Pair (Pair B S) W)))

of type

::

    pair
      (pair nat (pair tez tez))
      (pair
         (pair nat (pair timestamp timestamp))
         (pair
            (pair tez tez)
            (pair (pair account account) account)))

The parameter of the transaction will be either a transfer from the
buyer or the seller or a delivery notification from the warehouse of
type ``(or string nat)``.

At the beginning of the transaction:

::

    Q is accessible via a CDDAAR
    T               via a CDDADAR
    Z               via a CDDADDR
    K               via a CDDDAAR
    C               via a CDDDADR
    B               via a CDDDDAAR
    S               via a CDDDDADR
    W               via a CDDDDDR
    the delivery counter via a CDAAR
    the amount versed by the seller via a CDADDR
    the argument via a CAR

The contract returns a unit value, and we assume that it is created with
the minimum amount, set to ``(Tez "1.00")``.

The complete source ``forward.tz`` is:

::

    parameter
      (or string nat) ;
    storage
      (pair
         (pair nat (pair tez tez)) # counter from_buyer from_seller
         (pair
            (pair nat (pair timestamp timestamp)) # Q T Z
            (pair
               (pair tez tez) # K C
               (pair
                  (pair (contract unit) (contract unit)) # B S
                  (contract unit))))) ; # W
    code
      { DUP ; CDDADDR ; # Z
        PUSH int 86400 ; SWAP ; ADD ; # one day in second
        NOW ; COMPARE ; LT ;
        IF { # Before Z + 24
             DUP ; CAR ; # we must receive (Left "buyer") or (Left "seller")
             IF_LEFT
               { DUP ; PUSH string "buyer" ; COMPARE ; EQ ;
                 IF { DROP ;
                      DUP ; CDADAR ; # amount already versed by the buyer
                      DIP { AMOUNT } ; ADD ; # transaction
                      #  then we rebuild the globals
                      DIP { DUP ; CDADDR } ; PAIR ; # seller amount
                      PUSH nat 0 ; PAIR ; # delivery counter at 0
                      DIP { CDDR } ; PAIR ; # parameters
                      # and return Unit
                      NIL operation ; PAIR }
                    { PUSH string "seller" ; COMPARE ; EQ ;
                      IF { DUP ; CDADDR ; # amount already versed by the seller
                           DIP { AMOUNT } ; ADD ; # transaction
                           #  then we rebuild the globals
                           DIP { DUP ; CDADAR } ; SWAP ; PAIR ; # buyer amount
                           PUSH nat 0 ; PAIR ; # delivery counter at 0
                           DIP { CDDR } ; PAIR ; # parameters
                           # and return Unit
                           NIL operation ; PAIR }
                         { FAIL } } } # (Left _)
               { FAIL } } # (Right _)
           { # After Z + 24
             # if balance is emptied, just fail
             BALANCE ; PUSH tez "0" ; IFCMPEQ { FAIL } {} ;
             # test if the required amount is reached
             DUP ; CDDAAR ; # Q
             DIP { DUP ; CDDDADR } ; MUL ; # C
             PUSH nat 2 ; MUL ;
             PUSH tez "1.00" ; ADD ;
             BALANCE ; COMPARE ; LT ; # balance < 2 * (Q * C) + 1
             IF { # refund the parties
                  CDR ; DUP ; CADAR ; # amount versed by the buyer
                  DIP { DUP ; CDDDAAR } ; # B
                  UNIT ; TRANSFER_TOKENS ;
                  NIL operation ; SWAP ; CONS ; SWAP ;
                  DUP ; CADDR ; # amount versed by the seller
                  DIP { DUP ; CDDDADR } ; # S
                  UNIT ; TRANSFER_TOKENS ; SWAP ;
                  DIP { CONS } ;
                  DUP ; CADAR ; DIP { DUP ; CADDR } ; ADD ;
                  BALANCE ; SUB ; # bonus to the warehouse
                  DIP { DUP ; CDDDDR } ; # W
                  UNIT ; TRANSFER_TOKENS ;
                  DIP { SWAP } ; CONS ;
                  # leave the storage as-is, as the balance is now 0
                  PAIR }
                { # otherwise continue
                  DUP ; CDDADAR ; # T
                  NOW ; COMPARE ; LT ;
                  IF { FAIL } # Between Z + 24 and T
                     { # after T
                       DUP ; CDDADAR ; # T
                       PUSH int 86400 ; ADD ; # one day in second
                       NOW ; COMPARE ; LT ;
                       IF { # Between T and T + 24
                            # we only accept transactions from the buyer
                            DUP ; CAR ; # we must receive (Left "buyer")
                            IF_LEFT
                              { PUSH string "buyer" ; COMPARE ; EQ ;
                                IF { DUP ; CDADAR ; # amount already versed by the buyer
                                     DIP { AMOUNT } ; ADD ; # transaction
                                     # The amount must not exceed Q * K
                                     DUP ;
                                     DIIP { DUP ; CDDAAR ; # Q
                                            DIP { DUP ; CDDDAAR } ; MUL ; } ; # K
                                     DIP { COMPARE ; GT ; # new amount > Q * K
                                           IF { FAIL } { } } ; # abort or continue
                                     #  then we rebuild the globals
                                     DIP { DUP ; CDADDR } ; PAIR ; # seller amount
                                     PUSH nat 0 ; PAIR ; # delivery counter at 0
                                     DIP { CDDR } ; PAIR ; # parameters
                                     # and return Unit
                                     NIL operation ; PAIR }
                                   { FAIL } } # (Left _)
                              { FAIL } } # (Right _)
                          { # After T + 24
                            # test if the required payment is reached
                            DUP ; CDDAAR ; # Q
                            DIP { DUP ; CDDDAAR } ; MUL ; # K
                            DIP { DUP ; CDADAR } ; # amount already versed by the buyer
                            COMPARE ; NEQ ;
                            IF { # not reached, pay the seller
                                 BALANCE ;
                                 DIP { DUP ; CDDDDADR } ; # S
                                 DIIP { CDR } ;
                                 UNIT ; TRANSFER_TOKENS ;
                                 NIL operation ; SWAP ; CONS ; PAIR }
                               { # otherwise continue
                                 DUP ; CDDADAR ; # T
                                 PUSH int 86400 ; ADD ;
                                 PUSH int 86400 ; ADD ; # two days in second
                                 NOW ; COMPARE ; LT ;
                                 IF { # Between T + 24 and T + 48
                                      # We accept only delivery notifications, from W
                                      DUP ; CDDDDDR ; MANAGER ; # W
                                      SOURCE ; MANAGER ;
                                      COMPARE ; NEQ ;
                                      IF { FAIL } {} ; # fail if not the warehouse
                                      DUP ; CAR ; # we must receive (Right amount)
                                      IF_LEFT
                                        { FAIL } # (Left _)
                                        { # We increment the counter
                                          DIP { DUP ; CDAAR } ; ADD ;
                                          # And rebuild the globals in advance
                                          DIP { DUP ; CDADR } ; PAIR ;
                                          DIP { CDDR } ; PAIR ;
                                          UNIT ; PAIR ;
                                          # We test if enough have been delivered
                                          DUP ; CDAAR ;
                                          DIP { DUP ; CDDAAR } ;
                                          COMPARE ; LT ; # counter < Q
                                          IF { CDR ; NIL operation } # wait for more
                                             { # Transfer all the money to the seller
                                               BALANCE ;
                                               DIP { DUP ; CDDDDADR } ; # S
                                               DIIP { CDR } ;
                                               UNIT ; TRANSFER_TOKENS ;
                                               NIL operation ; SWAP ; CONS } } ;
                                      PAIR }
                                    { # after T + 48, transfer everything to the buyer
                                      BALANCE ;
                                      DIP { DUP ; CDDDDAAR } ; # B
                                      DIIP { CDR } ;
                                      UNIT ; TRANSFER_TOKENS ;
                                      NIL operation ; SWAP ; CONS ;
                                      PAIR} } } } } } }

XII - Full grammar
------------------

::

    <data> ::=
      | <int constant>
      | <natural number constant>
      | <string constant>
      | <timestamp string constant>
      | <signature string constant>
      | <key string constant>
      | <key_hash string constant>
      | <tez string constant>
      | <contract string constant>
      | Unit
      | True
      | False
      | Pair <data> <data>
      | Left <data>
      | Right <data>
      | Some <data>
      | None
      | { <data> ; ... }
      | { Elt <data> <data> ; ... }
      | instruction
    <instruction> ::=
      | { <instruction> ... }
      | DROP
      | DUP
      | SWAP
      | PUSH <type> <data>
      | SOME
      | NONE <type>
      | UNIT
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
      | MAP { <instruction> ... }
      | REDUCE
      | ITER { <instruction> ... }
      | MEM
      | GET
      | UPDATE
      | IF { <instruction> ... } { <instruction> ... }
      | LOOP { <instruction> ... }
      | LOOP_LEFT { <instruction> ... }
      | LAMBDA <type> <type> { <instruction> ... }
      | EXEC
      | DIP { <instruction> ... }
      | FAIL
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
      | INT
      | MANAGER
      | SELF
      | TRANSFER_TOKENS
      | CREATE_ACCOUNT
      | CREATE_CONTRACT
      | IMPLICIT_ACCOUNT
      | NOW
      | AMOUNT
      | BALANCE
      | CHECK_SIGNATURE
      | H
      | HASH_KEY
      | STEPS_TO_QUOTA
      | SOURCE <type> <type>
    <type> ::=
      | <comparable type>
      | key
      | unit
      | signature
      | option <type>
      | list <type>
      | set <comparable type>
      | operation
      | contract <type>
      | pair <type> <type>
      | or <type> <type>
      | lambda <type> <type>
      | map <comparable type> <type>
      | big_map <comparable type> <type>
    <comparable type> ::=
      | int
      | nat
      | string
      | tez
      | bool
      | key_hash
      | timestamp

XIII - Reference implementation
-------------------------------

The language is implemented in OCaml as follows:

-  The lower internal representation is written as a GADT whose type
   parameters encode exactly the typing rules given in this
   specification. In other words, if a program written in this
   representation is accepted by OCaml’s typechecker, it is guaranteed
   type-safe. This of course also valid for programs not handwritten but
   generated by OCaml code, so we are sure that any manipulated code is
   type-safe.

   In the end, what remains to be checked is the encoding of the typing
   rules as OCaml types, which boils down to half a line of code for
   each instruction. Everything else is left to the venerable and well
   trusted OCaml.

-  The interpreter is basically the direct transcription of the
   rewriting rules presented above. It takes an instruction, a stack and
   transforms it. OCaml’s typechecker ensures that the transformation
   respects the pre and post stack types declared by the GADT case for
   each instruction.

   The only things that remain to we reviewed are value dependent
   choices, such as that we did not swap true and false when
   interpreting the If instruction.

-  The input, untyped internal representation is an OCaml ADT with the
   only 5 grammar constructions: ``String``, ``Int``, ``Seq`` and
   ``Prim``. It is the target language for the parser, since not all
   parsable programs are well typed, and thus could simply not be
   constructed using the GADT.

-  The typechecker is a simple function that recognizes the abstract
   grammar described in section X by pattern matching, producing the
   well-typed, corresponding GADT expressions. It is mostly a checker,
   not a full inferrer, and thus takes some annotations (basically the
   input and output of the program, of lambdas and of uninitialized maps
   and sets). It works by performing a symbolic evaluation of the
   program, transforming a symbolic stack. It only needs one pass over
   the whole program.

   Here again, OCaml does most of the checking, the structure of the
   function is very simple, what we have to check is that we transform a
   ``Prim ("If", ...)`` into an ``If``, a ``Prim ("Dup", ...)`` into a
   ``Dup``, etc.
