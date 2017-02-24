(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let () =
  Webclient_version.register_services
    Client_proto_main.protocol Webclient_proto_service_directory.root ;
  Webclient_version.register_static_files
    Client_proto_main.protocol Webclient_proto_static.root
