(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

let to_root = function
  | `O ctns -> `O ctns
  | `A ctns -> `A ctns
  | `Null -> `O []
  | oth -> `A [ oth ]

let write_file file json =
  let json = to_root json in
  protect begin fun () ->
    Lwt_io.with_file ~mode:Output file begin fun chan ->
      let str = Data_encoding.Json.to_string ~minify:false json in
      Lwt_io.write chan str >>= fun _ ->
      return ()
    end
  end

let read_file file =
  protect begin fun () ->
    Lwt_io.with_file ~mode:Input file begin fun chan ->
      Lwt_io.read chan >>= fun str ->
      return (Ezjsonm.from_string str :> Data_encoding.json)
    end
  end
