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

type linux_proc_statm = {
  page_size : int ;
  size : int64;
  resident : int64 ;
  shared : int64 ;
  text : int64 ;
  lib : int64 ;
  data : int64 ;
  dt : int64
}

type darwin_ps_stats = {
  page_size : int ;
  mem : float ;
  resident : int64 }


type unix = Linux | Darwin

type mem_stat = Statm of linux_proc_statm | Ps of darwin_ps_stats

module S : sig
  val gc:
    ([ `GET ], unit, unit, unit, unit, Gc.stat) RPC_service.service

  val memory:
    ([ `GET ], unit, unit, unit, unit, mem_stat) RPC_service.service

end

val gc:
  #RPC_context.simple -> Gc.stat Error_monad.tzresult Lwt.t

val memory:
  #RPC_context.simple -> mem_stat Error_monad.tzresult Lwt.t
