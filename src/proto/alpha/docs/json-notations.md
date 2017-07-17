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
      { "storage": /* data */,
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

    The name of the primitive must respect the same case policy as the
    concrete sytax.

### Examples

Originating a contract with a script that does nothing can be done
with the following JSON script description.

    { "code":
      { "code": [ "CDR",
                  { "PUSH": [ "nat", { "int": "3" } ] }
                  "PAIR" ],
        "argType": "unit",
        "retType": "nat",
        "storageType": "unit" },
      "storage":
      { "storage": "Unit",
        "storageType": "unit" } }

### Full grammar

    /* data */ ::=
      | /* int constant */
      | /* string constant */
      | /* float constant */
      | /* timestamp constant */
      | /* signature constant */
      | /* key constant */
      | /* tez constant */
      | /* contract constant */
      | "Unit"
      | "True"
      | "False"
      | { "Pair": [ /* data */, /* data */ ] }
      | { "Left": [ /* data */ ] }
      | { "Right": [ /* data */ ] }
      | { "Some": [ /* data */ ] }
      | "None"
      | { "List": [ /* data */ ... ] }
      | { "Set": [ /* data */ ... ] }
      | { "Map": [ { "item": [ /* data */, /* data */ ] } ... ] }
    /* instruction */ ::=
      | [ /* instruction */ ... ]
      | "DROP"
      | "DUP"
      | "SWAP"
      | { "PUSH": [ /* type */ /* data */ ] }
      | "SOME"
      | { "NONE": [ /* type */ ] }
      | { "IF_NONE": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | "PAIR"
      | "CAR"
      | "CDR"
      | { "LEFT": [ /* type */ ] }
      | { "RIGHT": [ /* type */ ] }
      | { "IF_LEFT": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "NIL": [ /* type */ ] }
      | "CONS"
      | { "IF_CONS": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "EMPTY_SET": [ /* type */ ] }
      | { "EMPTY_MAP": [ /* comparable type */, /* type */ ] }
      | "MAP"
      | "REDUCE"
      | "MEM"
      | "GET"
      | "UPDATE"
      | { "IF": [ [ /* instruction */ ... ], [ /* instruction */ ... ] ] }
      | { "LOOP": [ [ /* instruction */ ... ] ] }
      | { "LAMBDA": [ /* type */, /* type */, [ /* instruction */ ... ] ] }
      | "EXEC"
      | { "DIP": [ [ /* instruction */ ... ] ] }
      | "FAIL"
      | "CONCAT"
      | "ADD"
      | "SUB"
      | "MUL"
      | "DIV"
      | "ABS"
      | "NEG"
      | "MOD"
      | "LSL"
      | "LSR"
      | "OR"
      | "AND"
      | "XOR"
      | "NOT"
      | "COMPARE"
      | "EQ"
      | "NEQ"
      | "LT"
      | "GT"
      | "LE"
      | "GE"
      | "CAST"
      | "CHECKED_ABS"
      | "CHECKED_NEG"
      | "CHECKED_ADD"
      | "CHECKED_SUB"
      | "CHECKED_MUL"
      | "CHECKED_CAST"
      | "FLOOR"
      | "CEIL"
      | "INF"
      | "NAN"
      | "ISNAN"
      | "NANAN"
      | "MANAGER"
      | "TRANSFER_FUNDS"
      | "DEFAULT_ACCOUNT"
      | "CREATE_ACCOUNT"
      | "CREATE_CONTRACT"
      | "NOW"
      | "AMOUNT"
      | "BALANCE"
      | "CHECK_SIGNATURE"
      | "H"
      | "STEPS_TO_QUOTA"
      | { "SOURCE": [ /* type */, /* type */ ] }
    /* type _/ ::=
      | "int"
      | "nat"
      | "unit"
      | "string"
      | "float"
      | "tez"
      | "bool"
      | "key"
      | "timestamp"
      | "signature"
      | { "option": [ /* type */ ] }
      | { "list": [ /* type */ ] }
      | { "set": [ /* comparable type */ ] }
      | { "contract": [ /* type */, /* type */ ] }
      | { "pair": [ /* type */, /* type */ ] }
      | { "or": [ /* type */, /* type */ ] }
      | { "lambda": [ /* type */, /* type */ ] }
      | { "map": [ /* comparable type */, /* type */ ] }
    /* comparable type */ ::=
      | "int"
      | "nat"
      | "string"
      | "float"
      | "tez"
      | "bool"
      | "key"
      | "timestamp"
