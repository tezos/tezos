(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include Tezos_stdlib
include Tezos_error_monad
include Tezos_rpc
include Tezos_clic
include Tezos_crypto
include Tezos_micheline

module Data_encoding = Tezos_data_encoding.Data_encoding

module List = struct
  include List
  include Tezos_stdlib.TzList
end
module String = struct
  include String
  include Tezos_stdlib.TzString
end

module Time = Time
module Fitness = Fitness
module Block_header = Block_header
module Operation = Operation
module Protocol = Protocol

module Test_chain_status = Test_chain_status
module Preapply_result = Preapply_result

module Block_locator = Block_locator
module Mempool = Mempool

module P2p_addr = P2p_addr
module P2p_identity = P2p_identity
module P2p_peer = P2p_peer
module P2p_point = P2p_point
module P2p_connection = P2p_connection
module P2p_stat = P2p_stat
module P2p_version = P2p_version

module Distributed_db_version = Distributed_db_version
module Network_version = Network_version

module Lwt_exit = Lwt_exit

include Utils.Infix
include Error_monad

module Internal_event = Internal_event
