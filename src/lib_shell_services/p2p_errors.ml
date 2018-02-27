(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(************************ p2p io scheduler ********************************)

type error += Connection_closed

(***************************** p2p socket *********************************)

type error += Decipher_error
type error += Invalid_message_size
type error += Encoding_error
type error += Rejected_socket_connection
type error += Decoding_error
type error += Myself of P2p_connection.Id.t
type error += Not_enough_proof_of_work of P2p_peer.Id.t
type error += Invalid_auth
type error += Invalid_chunks_size of { value: int ; min: int ; max: int }

(***************************** p2p pool ***********************************)

type error += Pending_connection
type error += Connected
type error += Connection_refused
type error += Rejected of P2p_peer.Id.t
type error += Too_many_connections
type error += Closed_network
