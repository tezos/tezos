(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let hex_of_buffer buf =
  let `Hex s = MBytes.to_hex (MBytes_buffer.to_mbytes buf) in
  s

let assert_hex_eq buf =
  Assert.equal ~prn:(fun x -> x) (hex_of_buffer buf)

let () =
  let buf1 = MBytes_buffer.create ~initial_size:1 () in
  MBytes_buffer.write_char buf1 'a' ;
  assert_hex_eq buf1 "61" ;
  MBytes_buffer.write_char buf1 'Q' ;
  assert_hex_eq buf1 "6151" ;
  MBytes_buffer.write_int32 buf1 Int32.max_int ;
  assert_hex_eq buf1 "61517fffffff" ;
  let buf2 = MBytes_buffer.create ~initial_size:1 () in
  MBytes_buffer.write_sized buf2 (fun () -> MBytes_buffer.write_string_data buf2 "abc") ;
  assert_hex_eq buf2 "00000003616263"
