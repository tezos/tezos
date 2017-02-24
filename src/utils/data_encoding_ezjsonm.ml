(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

let to_string j = Ezjsonm.to_string ~minify:false (to_root j)

let from_string s =
  try Ok (Ezjsonm.from_string s :> Data_encoding.json)
  with Ezjsonm.Parse_error (_, msg) -> Error msg

let from_stream (stream: string Lwt_stream.t) =
  let buffer = ref "" in
  Lwt_stream.filter_map
    (fun str ->
       buffer := !buffer ^ str ;
       try
         let json = Ezjsonm.from_string !buffer in
         buffer := "" ;
         Some (Ok json)
       with Ezjsonm.Parse_error _ ->
         None)
    stream

let write_file file json =
  let json = to_root json in
  protect begin fun () ->
    Lwt_io.with_file ~mode:Output file begin fun chan ->
      let str = to_string json in
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

let () =
  Error_monad.json_to_string := to_string
