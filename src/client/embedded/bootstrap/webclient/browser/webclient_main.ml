(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

module Services = Webclient_proto_services.Make (struct
    type root = unit
  end)

let call_service service params input =
  let write_json_body json =
    let jsobj =
      Json_repr.convert
        (module Json_repr.Ezjsonm)
        (module Json_repr_browser.Repr)
        json in
    Js._JSON##stringify jsobj in
  let read_json_body body =
    Json_repr.convert
      (module Json_repr_browser.Repr)
      (module Json_repr.Ezjsonm)
      (Js._JSON##parse body) in
  let path, json = RPC.forge_request service params input in
  let url = String.concat "/" path in
  let xhr = XmlHttpRequest.create () in
  let t, u = Lwt.wait () in
  xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      if xhr##.readyState = XmlHttpRequest.DONE then
        let response = read_json_body xhr##.responseText in
        Lwt.wakeup u response) ;
  xhr##_open (Js.string "POST") (Js.string url) Js._true ;
  xhr##send (Js.Opt.return (write_json_body json)) ;
  t >>= fun json ->
  match RPC.read_answer service json with
  | Ok res -> Lwt.return res
  | Error msg -> Lwt.fail_with msg

let () = Lwt.async @@ fun () ->
  call_service Services.contracts () () >>= fun names ->
  call_service Services.hash () () >>= fun hash ->
  let list = Tyxml_js.Html.(ul (List.map (fun n -> (li [ pcdata n ])) names)) in
  Tyxml_js.Register.id "receptacle"
    Tyxml_js.Html.
      [ h2 [ pcdata "Block: " ; pcdata hash ] ;
        h2 [ pcdata "Contract aliases:" ] ;
        list ] ;
  Lwt.return ()
