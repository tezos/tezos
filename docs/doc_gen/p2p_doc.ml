(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocols = [
  "Alpha", "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" ;
]

let main _node =
  (* Style : hack *)
  Format.printf "%a@." Rst.pp_raw_html Rst.style ;
  (* Script : hack *)
  Format.printf "%a@." Rst.pp_raw_html Rst.script ;
  (* Page title *)
  Format.printf "%a" Rst.pp_h1 "P2P message format" ;
  (* include/copy usage.rst from input  *)
  let rec loop () =
    let s = read_line () in
    Format.printf "%s@\n" s ;
    loop () in
  begin try loop () with End_of_file -> () end ;
  Format.printf "@\n" ;
  (* Data *)
  Format.printf "%a@\n@\n%a@\n@."
    Rst.pp_h1 "Block header (shell)"
    Data_encoding.Binary_schema.pp
    (Data_encoding.Binary.describe Block_header.encoding) ;
  Format.printf "%a@\n@\n%a@\n@."
    Rst.pp_h1 "Operation (shell)"
    Data_encoding.Binary_schema.pp
    (Data_encoding.Binary.describe Operation.encoding) ;
  List.iter
    (fun (_name, hash) ->
       let hash = Protocol_hash.of_b58check_exn hash in
       let (module Proto) = Registered_protocol.get_exn hash in
       Format.printf "%a@\n@\n%a@\n@."
         Rst.pp_h1 "Block_header (alpha-specific)"
         Data_encoding.Binary_schema.pp
         (Data_encoding.Binary.describe Proto.block_header_data_encoding) ;
       Format.printf "%a@\n@\n%a@\n@."
         Rst.pp_h1 "Operation (alpha-specific)"
         Data_encoding.Binary_schema.pp
         (Data_encoding.Binary.describe Proto.operation_data_encoding) ;
    )
    protocols ;
  return ()

let () =
  Lwt_main.run (Node_helpers.with_node main)
