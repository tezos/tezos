(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let test_rt_opt name pp enc dec input =
  let encoded = enc input in
  match dec encoded with
  | Some output ->
      if output = input then
        ()
      else
        Format.kasprintf failwith
          "%s failed for %a: got %a" name pp input pp output
  | None ->
      Format.kasprintf failwith
        "%s failed for %a: unable to decode" name pp input
  | exception exc ->
      Format.kasprintf failwith
        "%s failed for %a: exception whilst decoding: %s"
        name pp input (Printexc.to_string exc)

let test_decode_opt_safe name pp dec encoded =
  match dec encoded with
  | Some _ | None -> ()
  | exception exc ->
      Format.kasprintf failwith
        "%s failed for %a: exception whilst decoding: %s"
        name pp encoded (Printexc.to_string exc)

let test_decode_opt_fail name pp dec encoded =
  match dec encoded with
  | Some _ ->
      Format.kasprintf failwith
        "%s failed for %a: successful decoding of invalid input"
        name pp encoded
  | None -> ()
  | exception exc ->
      Format.kasprintf failwith
        "%s failed for %a: exception whilst decoding: %s"
        name pp encoded (Printexc.to_string exc)
