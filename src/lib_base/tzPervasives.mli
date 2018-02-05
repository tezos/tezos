(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include (module type of (struct include Tezos_stdlib end))
include (module type of (struct include Tezos_data_encoding end))
include (module type of (struct include Tezos_stdlib_lwt end))
include (module type of (struct include Tezos_error_monad end))
include (module type of (struct include Tezos_rpc end))
include (module type of (struct include Tezos_crypto end))

module List : sig
  include (module type of (struct include List end))
  include (module type of (struct include Tezos_stdlib.TzList end))
end
module String : sig
  include (module type of (struct include String end))
  include (module type of (struct include Tezos_stdlib.TzString end))
end

module Time = Time
module Data_encoding_ezjsonm = Data_encoding_ezjsonm
module Fitness = Fitness
module Block_header = Block_header
module Operation = Operation
module Protocol = Protocol
module Test_network_status = Test_network_status
module Preapply_result = Preapply_result
module Block_locator = Block_locator
module Mempool = Mempool

module Net_id = Net_id
module Block_hash = Block_hash
module Operation_hash = Operation_hash
module Operation_list_hash = Operation_list_hash
module Operation_list_list_hash = Operation_list_list_hash
module Context_hash = Context_hash
module Protocol_hash = Protocol_hash

module P2p_addr = P2p_addr
module P2p_identity = P2p_identity
module P2p_peer = P2p_peer
module P2p_point = P2p_point
module P2p_connection = P2p_connection
module P2p_stat = P2p_stat
module P2p_version = P2p_version

module Protocol_environment = Protocol_environment

include (module type of (struct include Utils.Infix end))
include (module type of (struct include Error_monad end))
