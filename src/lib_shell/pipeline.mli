(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Pipelines: carry a bundle of inputs through a series of transformations
    whilst preserving the order of the original input. *)

(** Steps are the building blocks of pipeline. A step is essentially a function
    from a given type to another. *)
type ('a, 'b) step
val sync: ('a -> 'b) -> ('a, 'b) step
val async_s: ('a -> 'b Lwt.t) -> ('a, 'b) step
val async_p: ('a -> 'b Lwt.t) -> ('a, 'b) step

(** Error management *)
val all_ok:
  ('a, ('a, 'b) result) step
val map_in_err:
  ('erra -> 'errb) ->
  (('a, 'errb) result, 'b) step ->
  (('a, 'erra) result, 'b) step
val map_out_err:
  ('erra -> 'errb) ->
  ('a, ('b, 'erra) result) step ->
  ('a, ('b, 'errb) result) step
val with_err:
  ('a, ('b, 'err) result) step ->
  (('a, 'err) result, ('b, 'err) result) step

val recover:
  ('err -> 'a) ->
  (('a, 'err) result, 'a) step

(** Carrying ID through a pipeline *)
val with_key:
  ('a, 'b) step ->
  (('key * 'a), ('key * 'b)) step
val init_key: ('a, ('a * 'a)) step


(** Pipelines are essentially lists of steps. *)
(* Recommended use: [cons f @@ cons g @@ nil] *)

type ('i, 'o) pipe
val nil: ('x, 'x) pipe
val cons: ('a, 'b) step -> ('b, 'c) pipe -> ('a, 'c) pipe


(** Core funcitonality:
    [run ?pool pipe input] runs all the elements of [input] through the steps of
    [pipeline]. All the while it maintains the following invariants:
    - There are never more than [pool] unresolved high-level promises at any one
    time. A high-level promise is one that corresponds to a call to one of the
    step function. (Note that each such promise can create additional promises
    which are not limited by the [pool] parameter of [run].) By default, impose
    no limits at all.
    - The elements maintain the same ordering all throughout the pipeline. In
    other words, if [x] is before [y] in [input], then for any step [s], the
    high-level promise of [s] for [x] will be created before the high-level
    promise of [s] for [y].
*)
val run: ?pool:int -> ('i, 'o) pipe -> 'i list -> ('o, exn) result list Lwt.t

(** Post-processing: useful to deal with pipeline built around error management
    or id marking combinators. *)
val partition_by_error:
  (('o, 'err) result, exn) result list ->
  ('o list * 'err list * exn list)
val index_by_key:
  (('key * 'o), exn) result list ->
  'index ->
  ('key -> 'o -> 'index -> 'index) ->
  'index * exn list
