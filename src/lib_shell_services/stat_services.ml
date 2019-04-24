(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Data_encoding
open Gc
let gc_stat_encoding =
  conv
    (fun
      { minor_words ; promoted_words ; major_words ;
        minor_collections ; major_collections ;
        heap_words ; heap_chunks ; live_words ; live_blocks ;
        free_words ; free_blocks ; largest_free ; fragments ;
        compactions ; top_heap_words ; stack_size ; } ->
      ((minor_words, promoted_words, major_words, minor_collections,
        major_collections),
       ((heap_words, heap_chunks, live_words, live_blocks, free_words),
        (free_blocks, largest_free, fragments, compactions,
         top_heap_words, stack_size))))
    (fun
      ((minor_words, promoted_words, major_words, minor_collections,
        major_collections),
       ((heap_words, heap_chunks, live_words, live_blocks, free_words),
        (free_blocks, largest_free, fragments, compactions,
         top_heap_words, stack_size)))  ->
      { minor_words ; promoted_words ; major_words ;
        minor_collections ; major_collections ;
        heap_words ; heap_chunks ; live_words ; live_blocks ;
        free_words ; free_blocks ; largest_free ; fragments ;
        compactions ; top_heap_words ; stack_size ; })
    (merge_objs
       (obj5
          (req "minor_words" float)
          (req "promoted_words" float)
          (req "major_words" float)
          (req "minor_collections" int31)
          (req "major_collections" int31))
       (merge_objs
          (obj5
             (req "heap_words" int31)
             (req "heap_chunks" int31)
             (req "live_words" int31)
             (req "live_blocks" int31)
             (req "free_words" int31))
          (obj6
             (req "free_blocks" int31)
             (req "largest_free" int31)
             (req "fragments" int31)
             (req "compactions" int31)
             (req "top_heap_words" int31)
             (req "stack_size" int31)))
    )

module S = struct

  let gc_stat =
    RPC_service.get_service
      ~description:"Gets stats from the OCaml Garbage Collector"
      ~query: RPC_query.empty
      ~output:gc_stat_encoding
      RPC_path.(root / "stats" / "gc")

end

let gc_stat ctxt =
  RPC_context.make_call S.gc_stat ctxt () () ()
