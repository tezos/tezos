(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  allowed_headers : string list ;
  allowed_origins : string list ;
}

let default = { allowed_headers = [] ; allowed_origins = [] }

let check_origin_matches origin allowed_origin =
  String.equal "*" allowed_origin ||
  String.equal allowed_origin origin ||
  begin
    let allowed_w_slash = allowed_origin ^ "/" in
    let len_a_w_s = String.length allowed_w_slash in
    let len_o = String.length origin in
    (len_o >= len_a_w_s) &&
    String.equal allowed_w_slash @@ String.sub origin 0 len_a_w_s
  end

let find_matching_origin allowed_origins origin =
  let matching_origins =
    List.filter (check_origin_matches origin) allowed_origins in
  let compare_by_length_neg a b =
    ~- (compare (String.length a) (String.length b)) in
  let matching_origins_sorted =
    List.sort compare_by_length_neg matching_origins in
  match matching_origins_sorted with
  | [] -> None
  | x :: _ -> Some x

let add_allow_origin headers cors origin_header =
  match origin_header with
  | None -> headers
  | Some origin ->
      match find_matching_origin cors.allowed_origins origin with
      | None -> headers
      | Some allowed_origin ->
          Cohttp.Header.add headers
            "Access-Control-Allow-Origin" allowed_origin

let add_headers headers cors origin_header =
  let cors_headers =
    Cohttp.Header.add_multi headers
      "Access-Control-Allow-Headers" cors.allowed_headers in
  add_allow_origin cors_headers cors origin_header
