(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove [channel], [exit], ...
   * Remove polymorphic comparisons
   * Remove floating-point arithmetic
   * Remove string conversion functions for float
   * Remove deprecated functions

*)


(** The initially opened module.

    This module provides the basic operations over the built-in types
    (numbers, booleans, byte sequences, strings, exceptions, references,
    lists, arrays, input-output channels, ...).

    This module is automatically opened at the beginning of each compilation.
    All components of this module can therefore be referred by their short
    name, without prefixing them by [Pervasives].
*)


(** {1 Exceptions} *)

external raise : exn -> 'a = "%raise"
(** Raise the given exception value *)

external raise_notrace : exn -> 'a = "%raise_notrace"
(** A faster version [raise] which does not record the backtrace.
    @since 4.02.0
*)

val invalid_arg : string -> 'a
(** Raise exception [Invalid_argument] with the given string. *)

val failwith : string -> 'a
(** Raise exception [Failure] with the given string. *)

exception Exit
(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs. *)


(** {1 Boolean operations} *)

external not : bool -> bool = "%boolnot"
(** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** The boolean 'and'. Evaluation is sequential, left-to-right:
    in [e1 && e2], [e1] is evaluated first, and if it returns [false],
    [e2] is not evaluated at all.
    Right-associative operator at precedence level 3/11. *)


external ( || ) : bool -> bool -> bool = "%sequor"
(** The boolean 'or'. Evaluation is sequential, left-to-right:
    in [e1 || e2], [e1] is evaluated first, and if it returns [true],
    [e2] is not evaluated at all.
    Right-associative operator at precedence level 2/11.
*)

(** {1 Debugging} *)

external __LOC__ : string = "%loc_LOC"
(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d".
    @since 4.02.0
*)

external __FILE__ : string = "%loc_FILE"
(** [__FILE__] returns the name of the file currently being
    parsed by the compiler.
    @since 4.02.0
*)

external __LINE__ : int = "%loc_LINE"
(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler.
    @since 4.02.0
*)

external __MODULE__ : string = "%loc_MODULE"
(** [__MODULE__] returns the module name of the file being
    parsed by the compiler.
    @since 4.02.0
*)

external __POS__ : string * int * int * int = "%loc_POS"
(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line.
    @since 4.02.0
*)

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d".
    @since 4.02.0
*)

external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
(** [__LINE__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler.
    @since 4.02.0
*)

external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
(** [__POS_OF__ expr] returns a pair [(loc,expr)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line.
    @since 4.02.0
*)

(** {1 Composition operators} *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
(** Reverse-application operator: [x |> f |> g] is exactly equivalent
    to [g (f (x))].
    Left-associative operator at precedence level 4/11.
    @since 4.01
*)

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Application operator: [g @@ f @@ x] is exactly equivalent to
    [g (f (x))].
    Right-associative operator at precedence level 5/11.
    @since 4.01
*)

(** {1 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
    All operations are taken modulo 2{^31} (or 2{^63}).
    They do not fail on overflow. *)

external ( ~- ) : int -> int = "%negint"
(** Unary negation. You can also write [- e] instead of [~- e].
    Unary operator at precedence level 9/11 for [- e]
    and 11/11 for [~- e]. *)

external ( ~+ ) : int -> int = "%identity"
(** Unary addition. You can also write [+ e] instead of [~+ e].
    Unary operator at precedence level 9/11 for [+ e]
    and 11/11 for [~+ e].
    @since 3.12.0
*)

external succ : int -> int = "%succint"
(** [succ x] is [x + 1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [x - 1]. *)

external ( + ) : int -> int -> int = "%addint"
(** Integer addition.
    Left-associative operator at precedence level 6/11. *)

external ( - ) : int -> int -> int = "%subint"
(** Integer subtraction.
    Left-associative operator at precedence level 6/11. *)

external ( * ) : int -> int -> int = "%mulint"
(** Integer multiplication.
    Left-associative operator at precedence level 7/11. *)

external ( / ) : int -> int -> int = "%divint"
(** Integer division.
    Raise [Division_by_zero] if the second argument is 0.
    Integer division rounds the real quotient of its arguments towards zero.
    More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
    less than or equal to the real quotient of [x] by [y].  Moreover,
    [(- x) / y = x / (- y) = - (x / y)].
    Left-associative operator at precedence level 7/11. *)

external ( mod ) : int -> int -> int = "%modint"
(** Integer remainder.  If [y] is not zero, the result
    of [x mod y] satisfies the following properties:
    [x = (x / y) * y + x mod y] and
    [abs(x mod y) <= abs(y) - 1].
    If [y = 0], [x mod y] raises [Division_by_zero].
    Note that [x mod y] is negative only if [x < 0].
    Raise [Division_by_zero] if [y] is zero.
    Left-associative operator at precedence level 7/11. *)

val abs : int -> int
(** Return the absolute value of the argument.  Note that this may be
    negative if the argument is [min_int]. *)

val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)


(** {2 Bitwise operations} *)

external ( land ) : int -> int -> int = "%andint"
(** Bitwise logical and.
    Left-associative operator at precedence level 7/11. *)

external ( lor ) : int -> int -> int = "%orint"
(** Bitwise logical or.
    Left-associative operator at precedence level 7/11. *)

external ( lxor ) : int -> int -> int = "%xorint"
(** Bitwise logical exclusive or.
    Left-associative operator at precedence level 7/11. *)

val lnot : int -> int
(** Bitwise logical negation. *)

external ( lsl ) : int -> int -> int = "%lslint"
(** [n lsl m] shifts [n] to the left by [m] bits.
    The result is unspecified if [m < 0] or [m >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform.
    Right-associative operator at precedence level 8/11. *)

external ( lsr ) : int -> int -> int = "%lsrint"
(** [n lsr m] shifts [n] to the right by [m] bits.
    This is a logical shift: zeroes are inserted regardless of
    the sign of [n].
    The result is unspecified if [m < 0] or [m >= bitsize].
    Right-associative operator at precedence level 8/11. *)

external ( asr ) : int -> int -> int = "%asrint"
(** [n asr m] shifts [n] to the right by [m] bits.
    This is an arithmetic shift: the sign bit of [n] is replicated.
    The result is unspecified if [m < 0] or [m >= bitsize].
    Right-associative operator at precedence level 8/11. *)


(** {1 String operations}

    More string operations are provided in module {!String}.
*)

val ( ^ ) : string -> string -> string
(** String concatenation.
    Right-associative operator at precedence level 5/11. *)


(** {1 Character operations}

    More character operations are provided in module {!Char}.
*)

external int_of_char : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val char_of_int : int -> char
(** Return the character with the given ASCII code.
    Raise [Invalid_argument "char_of_int"] if the argument is
    outside the range 0--255. *)


(** {1 Unit operations} *)

external ignore : 'a -> unit = "%ignore"
(** Discard the value of its argument and return [()].
    For instance, [ignore(f x)] discards the result of
    the side-effecting function [f].  It is equivalent to
    [f x; ()], except that the latter may generate a
    compiler warning; writing [ignore(f x)] instead
    avoids the warning. *)


(** {1 String conversion functions} *)

val string_of_bool : bool -> string
(** Return the string representation of a boolean. As the returned values
    may be shared, the user should not modify them directly.
*)

val bool_of_string_opt: string -> bool option
(** Convert the given string to a boolean.
    Return [None] if the string is not
    ["true"] or ["false"].
    @since 4.05
*)

val string_of_int : int -> string
(** Return the string representation of an integer, in decimal. *)

val int_of_string_opt: string -> int option
(** Convert the given string to an integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]), in hexadecimal (if it begins with [0x] or
    [0X]), in octal (if it begins with [0o] or [0O]), or in binary
    (if it begins with [0b] or [0B]).

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*max_int+1]].  If the input exceeds {!max_int}
    it is converted to the signed integer
    [min_int + input - max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.

    Return [None] if the given string is not a valid representation of
    an integer, or if the integer represented exceeds the range of
    integers representable in type [int].
    @since 4.05
*)

(** {1 Pair operations} *)

external fst : 'a * 'b -> 'a = "%field0"
(** Return the first component of a pair. *)

external snd : 'a * 'b -> 'b = "%field1"
(** Return the second component of a pair. *)


(** {1 List operations}

    More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation.  Not tail-recursive (length of the first argument).
    Right-associative operator at precedence level 5/11. *)


(** {1 References} *)

type 'a ref = { mutable contents : 'a }
(** The type of references (mutable indirection cells) containing
    a value of type ['a]. *)

external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
(** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents].
    Unary operator at precedence level 11/11.*)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
(** [r := a] stores the value of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v].
    Right-associative operator at precedence level 1/11. *)

external incr : int ref -> unit = "%incr"
(** Increment the integer contained in the given reference.
    Equivalent to [fun r -> r := succ !r]. *)

external decr : int ref -> unit = "%decr"
(** Decrement the integer contained in the given reference.
    Equivalent to [fun r -> r := pred !r]. *)

(** {1 Result type} *)

(** @since 4.03.0 *)
type ('a,'b) result = Ok of 'a | Error of 'b

(** {1 Operations on format strings} *)

(** Format strings are character strings with special lexical conventions
    that defines the functionality of formatted input/output functions. Format
    strings are used to read data with formatted input functions from module
    {!Scanf} and to print data with formatted output functions from modules
    {!Printf} and {!Format}.

    Format strings are made of three kinds of entities:
    - {e conversions specifications}, introduced by the special character ['%']
    followed by one or more characters specifying what kind of argument to
    read or print,
    - {e formatting indications}, introduced by the special character ['@']
    followed by one or more characters specifying how to read or print the
    argument,
    - {e plain characters} that are regular characters with usual lexical
    conventions. Plain characters specify string literals to be read in the
    input or printed in the output.

    There is an additional lexical rule to escape the special characters ['%']
    and ['@'] in format strings: if a special character follows a ['%']
    character, it is treated as a plain character. In other words, ["%%"] is
    considered as a plain ['%'] and ["%@"] as a plain ['@'].

    For more information about conversion specifications and formatting
    indications available, read the documentation of modules {!Scanf},
    {!Printf} and {!Format}.
*)

(** Format strings have a general and highly polymorphic type
    [('a, 'b, 'c, 'd, 'e, 'f) format6].
    The two simplified types, [format] and [format4] below are
    included for backward compatibility with earlier releases of
    OCaml.

    The meaning of format string type parameters is as follows:

    - ['a] is the type of the parameters of the format for formatted output
      functions ([printf]-style functions);
      ['a] is the type of the values read by the format for formatted input
      functions ([scanf]-style functions).

    - ['b] is the type of input source for formatted input functions and the
      type of output target for formatted output functions.
      For [printf]-style functions from module {!Printf}, ['b] is typically
      [out_channel];
      for [printf]-style functions from module {!Format}, ['b] is typically
      {!Format.formatter};
      for [scanf]-style functions from module {!Scanf}, ['b] is typically
      {!Scanf.Scanning.in_channel}.

      Type argument ['b] is also the type of the first argument given to
      user's defined printing functions for [%a] and [%t] conversions,
      and user's defined reading functions for [%r] conversion.

    - ['c] is the type of the result of the [%a] and [%t] printing
      functions, and also the type of the argument transmitted to the
      first argument of [kprintf]-style functions or to the
      [kscanf]-style functions.

    - ['d] is the type of parameters for the [scanf]-style functions.

    - ['e] is the type of the receiver function for the [scanf]-style functions.

    - ['f] is the final result type of a formatted input/output function
      invocation: for the [printf]-style functions, it is typically [unit];
      for the [scanf]-style functions, it is typically the result type of the
      receiver function.
*)

type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
(** Converts a format string into a string. *)

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
(** [format_of_string s] returns a format string read from the string
    literal [s].
    Note: [format_of_string] can not convert a string argument that is not a
    literal. If you need this functionality, use the more general
    {!Scanf.format_from_string} function.
*)

val ( ^^ ) :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
  ('a, 'b, 'c, 'd, 'g, 'h) format6
(** [f1 ^^ f2] catenates format strings [f1] and [f2]. The result is a
    format string that behaves as the concatenation of format strings [f1] and
    [f2]: in case of formatted output, it accepts arguments from [f1], then
    arguments from [f2]; in case of formatted input, it returns results from
    [f1], then results from [f2].
    Right-associative operator at precedence level 5/11. *)
