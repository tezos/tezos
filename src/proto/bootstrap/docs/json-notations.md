JSON Notations in the Tezos System
==================================

I - Constants
-------------

II - Programs, Data and Types
----------------------------

When a new scripted contract is opened in the system, its code must be
given, along with the initial value of its global storage, and the
types of both. For the contract origination RPC, a script is a JSON
object with two fields, as follows. The same format is used when
introspecting the node, returning the current state of the storage.

    { "code":
      { "code": [ /* instruction */, ... ],
        "argType": /* type */,
        "retType": /* type */,
        "storageType": /* type */ },
      "storage":
      { "storage": /* tagged data */,
        "storageType": /* type */ } }

All the sub fields contain expressions in a common meta JSON format
described in the next section. These expressions map to the syntactic
structure of the (whitespace sensitive) concrete syntax described in
the language specification document.

### Generic Representation of Expressions

The language has three basic syntactical constructs (notwithstanding
the small shortcuts and variations allowed by the concrete
syntax). An expression can be a constant, a sequence, or the
application of a primitive.

  * A Constant is encoded as one of these three cases.

      * `{ "float": "f" }`
        where `"f"` is the textual representation
        of a floating point number in the concrete source language.
      * `{ "int": "n" }`
        where `"n"` is the textual representation
        of a floating point number in the concrete source language.
      * `{ "string": f }`
        where `s` is a JSON string.

     Constants for timestamps, signatures, keys, tez amounts and
     contracts are just strings constants that respect the same
     specific string formats as in the concrete language.

  * A sequence is a JSON array:

        [ /* expression 1 */, /* expression 2 */, ... ]

  * A primitive application is of the form:

        { "name": [ /* argument 1 */, /* argument 2 */, ...] }

    If a primitive is not applied to any argument, a shortcut is:

        "name"

    The name of the primitive is expected to be in lowercase.

### Examples

Originating a contract with a script that does nothing can be done
with the following JSON script description.

    { "code":
      { "code": [ "cdr",
                  { "push": [ "void" ] }
                  "pair" ],
        "argType": "void",
        "retType": "void",
        "storageType": "void" },
      "storage":
      { "storage": "void",
        "storageType": "void" } }

### Full grammar

    /* tagged data */ ::=
      | { "int8": [ /* int constant */ ] }
      | { "int16": [ /* int constant */ ] }
      | { "int32": [ /* int constant */ ] }
      | { "int64": [ /* int constant */ ] }
      | { "uint8": [ /* int constant */ ] }
      | { "uint16": [ /* int constant */ ] }
      | { "uint32": [ /* int constant */ ] }
      | { "uint64": [ /* int constant */ ] }
      | "void"
      | "true"
      | "false"
      | /* string constant */
      | /* float constant */
      | { "timestamp": [ /* timestamp constant */ ] }
      | { "signature": [ /* signature constant */ ] }
      | { "tez": [ /* tez constant */ ] }
      | { "key": [ /* key constant */ ] }
      | { "left": [ /* tagged data */, /* type */ ] }
      | { "right": [ /* type */, /* tagged data */ ] }
      | { "or": [ /* type */, /* type */, /* untagged data */ ] }
      | { "ref": [ /* tagged data */ ] }
      | { "ref": [ /* type */, /* untagged data */ ] }
      | { "some": [ /* tagged data */ ] }
      | { "some": [ /* type */, /* untagged data */ ] }
      | { "none": [ /* type */ ] }
      | { "option": [ /* type */, /* untagged data */ ] }
      | { "pair": [ /* tagged data */, /* tagged data */ ] }
      | { "pair": [ /* type */,
                    /* type */,
                    /* untagged data */,
                    /* untagged data */ ] }
      | { "list": [ /* type */, /* untagged data */ ... ] }
      | { "set": [ /* comparable type */, /* untagged data */ ... ] }
      | { "map": [ /* comparable type */,
                   /* type */,
                   { "item": [ /* untagged data */, /* untagged data */ ] } ... ] }
      | { "contract": [ /* type */, /* type */, /* contract constant */ ] }
      | { "lambda": [ /* type */, /* type */, [ /* instruction */ ... ] ] }
    /* untagged data */ ::=
      | /* int constant */
      | /* string constant */
      | /* float constant */
      | /* timestamp constant */
      | /* signature constant */
      | /* key constant */
      | /* tez constant */
      | /* contract constant */
      | "void"
      | "true"
      | "false"
      | { "pair": [ /* untagged data */, /* untagged data */ ] }
      | { "left": [ /* untagged data */ ] }
      | { "right": [ /* untagged data */ ] }
      | { "ref": [ /* untagged data */ ] }
      | { "some": [ /* untagged data */ ] }
      | "none"
      | { "list": [ /* untagged data */ ... ] }
      | { "set": [ /* untagged data */ ... ] }
      | { "map": [ { "item": [ /* untagged data */, /* untagged data */ ] } ... ] }
    /* instruction */ ::=
      | [ /* instruction */ ... ]
      | "drop"
      | "dup"
      | "swap"
      | { "push": [ /* tagged data */ ] }
      | "some"
      | { "none": [ /* type */ ] }
      | { "if_none": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | "pair"
      | "car"
      | "cdr"
      | { "left": [ /* type */ ] }
      | { "right": [ /* type */ ] }
      | { "if_left": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "nil": [ /* type */ ] }
      | "cons"
      | { "if_cons": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "empty_set": [ /* type */ ] }
      | { "empty_map": [ /* comparable type */, /* type */ ] }
      | "iter"
      | "map"
      | "reduce"
      | "mem"
      | "get"
      | "update"
      | "ref"
      | "deref"
      | "set"
      | { "if": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "loop": [ [ /* instruction */ ... ] ] }
      | { "lambda": [ /* type */, /* type */, [ /* instruction */ ... ] ] }
      | "exec"
      | { "dip": [ [ /* instruction */ ... ] ] }
      | "fail"
      | "nop"
      | "concat"
      | "add"
      | "sub"
      | "mul"
      | "div"
      | "abs"
      | "neg"
      | "mod"
      | "lsl"
      | "lsr"
      | "or"
      | "and"
      | "xor"
      | "not"
      | "compare"
      | "eq"
      | "neq"
      | "lt"
      | "gt"
      | "le"
      | "ge"
      | "cast"
      | "checked_abs"
      | "checked_neg"
      | "checked_add"
      | "checked_sub"
      | "checked_mul"
      | "checked_cast"
      | "floor"
      | "ceil"
      | "inf"
      | "nan"
      | "isnan"
      | "nanan"
      | "manager"
      | "transfer_funds"
      | "create_account"
      | "create_contract"
      | "now"
      | "amount"
      | "balance"
      | "check_signature"
      | "h"
      | "steps_to_quota"
      | { "source": [ /* type */, /* type */ ] }
    /* type */ ::=
      | "int8"
      | "int16"
      | "int32"
      | "int64"
      | "uint8"
      | "uint16"
      | "uint32"
      | "uint64"
      | "void"
      | "string"
      | "float"
      | "tez"
      | "bool"
      | "key"
      | "timestamp"
      | "signature"
      | { "ref": [ /* type */ ] }
      | { "option": [ /* type */ ] }
      | { "list": [ /* type */ ] }
      | { "set": [ /* comparable type */ ] }
      | { "contract": [ /* type */, /* type */ ] }
      | { "pair": [ /* type */, /* type */ ] }
      | { "union": [ /* type */, /* type */ ] }
      | { "lambda": [ /* type */, /* type */ ] }
      | { "map": [ /* comparable type */, /* type */ ] }
    /* comparable type */ ::=
      | "int8"
      | "int16"
      | "int32"
      | "int64"
      | "uint8"
      | "uint16"
      | "uint32"
      | "uint64"
      | "string"
      | "float"
      | "tez"
      | "bool"
      | "key"
      | "timestamp"
