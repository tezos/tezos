(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

let no_exception f =
  try f ()
  with
  | Json_encoding.Cannot_destruct _
  | Json_encoding.Unexpected _
  | Json_encoding.No_case_matched _
  | Json_encoding.Bad_array_size _
  | Json_encoding.Missing_field _
  | Json_encoding.Unexpected_field _
  | Json_encoding.Bad_schema _ as exn ->
      Alcotest.failf
        "@[v 2>json failed:@ %a@]"
        (fun ppf -> Json_encoding.print_error ppf) exn
  | Binary.Read_error error ->
      Alcotest.failf
        "@[v 2>bytes reading failed:@ %a@]"
        Binary.pp_read_error error

let check_raises expected f =
  match f () with
  | exception exn when expected exn -> ()
  | exception exn ->
      Alcotest.failf "Unexpected exception: %s." (Printexc.to_string exn)
  | _ -> Alcotest.failf "Expecting exception, got success."

let chunked_read sz encoding bytes =
  let status =
    List.fold_left
      (fun status chunk ->
         match status with
         | Binary.Await f -> f chunk
         | Success _ when MBytes.length chunk <> 0 -> Error Extra_bytes
         | Success _ | Error _ -> status)
      (Binary.read_stream encoding)
      (MBytes.cut sz bytes) in
  match status with
  | Success { stream ; _ } when not (Binary_stream.is_empty stream) ->
      Binary.Error Extra_bytes
  | _ -> status

let streamed_read encoding bytes =
  List.fold_left
    (fun (status, count as acc) chunk ->
       match status with
       | Binary.Await f -> (f chunk, succ count)
       | Success _ | Error _ -> acc)
    (Binary.read_stream encoding, 0)
    (MBytes.cut 1 bytes)
