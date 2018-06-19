(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let packet_length = 64
let channel = 0x0101
let apdu = 0x05
let ping = 0x02

module Status = struct
  type t =
    | Invalid_pin of int
    | Incorrect_length
    | Incompatible_file_structure
    | Security_status_unsatisfied
    | Conditions_of_use_not_satisfied
    | Incorrect_data
    | File_not_found
    | Incorrect_params
    | Ins_not_supported
    | Technical_problem of int
    | Ok

  let of_int = function
    | 0x6700 -> Incorrect_length
    | 0x6981 -> Incompatible_file_structure
    | 0x6982 -> Security_status_unsatisfied
    | 0x6985 -> Conditions_of_use_not_satisfied
    | 0x6a80 -> Incorrect_data
    | 0x9404 -> File_not_found
    | 0x6b00 -> Incorrect_params
    | 0x6d00 -> Ins_not_supported
    | 0x9000 -> Ok
    | v when v >= 0x63c0 && v <= 0x63cf -> Invalid_pin (v land 0x0f)
    | v when v >= 0x6f00 && v <= 0x6fff -> Technical_problem (v land 0xff)
    | v -> invalid_arg (Printf.sprintf "Status.of_int: got 0x%x" v)

  let to_string = function
    | Invalid_pin i -> Printf.sprintf "Invalid pin %d" i
    | Incorrect_length -> "Incorrect length"
    | Incompatible_file_structure -> "Incompatible file structure"
    | Security_status_unsatisfied -> "Security status unsatisfied"
    | Conditions_of_use_not_satisfied -> "Conditions of use not satisfied"
    | Incorrect_data -> "Incorrect data"
    | File_not_found -> "File not found"
    | Incorrect_params -> "Incorrect params"
    | Ins_not_supported -> "Instruction not supported"
    | Technical_problem i -> Printf.sprintf "Technical problem %d" i
    | Ok -> "Ok"

  let show t = to_string t

  let pp ppf t =
    Format.pp_print_string ppf (to_string t)
end

module Header = struct
  type t = {
    cmd : [`Ping | `Apdu] ;
    seq : int ;
  }

  let read cs =
    let open Cstruct in
    if BE.get_uint16 cs 0 <> channel then
      invalid_arg "Transport.read_header: invalid channel id" ;
    let cmd = match get_uint8 cs 2 with
      | 0x05 -> `Apdu
      | 0x02 -> `Ping
      | _ -> invalid_arg "Transport.read_header: invalid command tag"
    in
    let seq = BE.get_uint16 cs 3 in
    { cmd ; seq }, Cstruct.shift cs 5

  let check_exn ?cmd ?seq t =
    begin match cmd with
      | None -> ()
      | Some expected ->
        if expected <> t.cmd then failwith "Header.check: unexpected command"
    end ;
    begin match seq with
      | None -> ()
      | Some expected ->
        if expected <> t.seq then failwith "Header.check: unexpected seq num"
    end
end

let write_ping ?(buf=Cstruct.create packet_length) h =
  let open Cstruct in
  BE.set_uint16 buf 0 channel ;
  set_uint8 buf 2 ping ;
  BE.set_uint16 buf 3 0 ;
  memset (sub buf 5 59) 0 ;
  match Hidapi.write h (to_bigarray (sub buf 0 packet_length)) with
  | Error msg -> failwith msg
  | Ok nb_written when nb_written <> packet_length -> failwith "Transport.write_ping"
  | _ -> ()

let write_apdu
    ?pp
    ?(buf=Cstruct.create packet_length)
    h p =
  let apdu_len = Apdu.length p in
  let apdu_buf = Cstruct.create apdu_len in
  let _nb_written = Apdu.write apdu_buf p in
  begin match pp with
    | None -> ()
    | Some pp ->
      Format.fprintf pp "-> %a@." Cstruct.hexdump_pp apdu_buf
  end ;
  let apdu_p = ref 0 in (* pos in the apdu buf *)
  let i = ref 0 in (* packet id *)
  let open Cstruct in

  (* write first packet *)
  BE.set_uint16 buf 0 channel ;
  set_uint8 buf 2 apdu ;
  BE.set_uint16 buf 3 !i ;
  BE.set_uint16 buf 5 apdu_len ;
  let nb_to_write = (min apdu_len (packet_length - 7)) in
  blit apdu_buf 0 buf 7 nb_to_write ;
  begin match Hidapi.write h (to_bigarray (sub buf 0 packet_length)) with
    | Error msg -> failwith msg
    | Ok nb_written when nb_written <> packet_length ->
      failwith "Transport.write_apdu"
    | _ -> ()
  end ;
  apdu_p := !apdu_p + nb_to_write ;
  incr i ;

  (* write following packets *)
  while !apdu_p < apdu_len do
    memset buf 0 ;
    BE.set_uint16 buf 0 channel ;
    set_uint8 buf 2 apdu ;
    BE.set_uint16 buf 3 !i ;
    let nb_to_write = (min (apdu_len - !apdu_p) (packet_length - 5)) in
    blit apdu_buf !apdu_p buf 5 nb_to_write ;
    begin match Hidapi.write h (to_bigarray (sub buf 0 packet_length)) with
      | Error err -> failwith err
      | Ok nb_written when nb_written <> packet_length ->
        failwith "Transport.write_apdu"
      | _ -> ()
    end ;
    apdu_p := !apdu_p + nb_to_write ;
    incr i
  done

let read ?(buf=Cstruct.create packet_length) h =
  let expected_seq = ref 0 in
  let full_payload = ref (Cstruct.create 0) in
  let payload = ref (Cstruct.create 0) in
  (* let pos = ref 0 in *)
  let rec inner () =
    begin match Hidapi.read ~timeout:600000 h
                  (Cstruct.to_bigarray buf) packet_length with
      | Error err -> failwith err
      | Ok nb_read when nb_read <> packet_length ->
        failwith (Printf.sprintf "Transport.read: read %d bytes" nb_read)
      | _ -> ()
    end ;
    let hdr, buf = Header.read buf in
    Header.check_exn ~seq:!expected_seq hdr ;
    if hdr.seq = 0 then begin (* first frame *)
      let len = Cstruct.BE.get_uint16 buf 0 in
      let cs = Cstruct.shift buf 2 in
      payload := Cstruct.create len ;
      full_payload := !payload ;
      let nb_to_read = min len (packet_length - 7) in
      Cstruct.blit cs 0 !payload 0 nb_to_read ;
      payload := Cstruct.shift !payload nb_to_read ;
      (* pos := !pos + nb_to_read ; *)
      expected_seq := !expected_seq + 1 ;
    end else begin (* next frames *)
      (* let rem = Bytes.length !payload - !pos in *)
      let nb_to_read = min (Cstruct.len !payload) (packet_length - 5) in
      Cstruct.blit buf 0 !payload 0 nb_to_read ;
      payload := Cstruct.shift !payload nb_to_read ;
      (* pos := !pos + nb_to_read ; *)
      expected_seq := !expected_seq + 1
    end ;
    if Cstruct.len !payload = 0 then
      if hdr.cmd = `Ping then Status.Ok, Cstruct.create 0
      else
        (* let sw_pos = Bytes.length !payload - 2 in *)
        let payload_len = Cstruct.len !full_payload in
        Status.of_int Cstruct.(BE.get_uint16 !full_payload (payload_len - 2)),
        Cstruct.sub !full_payload 0 (payload_len - 2)
    else inner ()
  in
  inner ()

let ping ?buf h =
  write_ping ?buf h ;
  match read ?buf h with
  | Status.Ok, _ -> ()
  | s, _ -> failwith ((Status.to_string s))

let apdu ?pp ?(msg="") ?buf h apdu =
  write_apdu ?pp ?buf h apdu ;
  match read ?buf h with
  | Status.Ok, payload ->
    begin match pp with
      | None -> ()
      | Some pp ->
        Format.fprintf pp "<- %a %a@." Status.pp Status.Ok Cstruct.hexdump_pp payload
    end ;
    payload
  | s, payload ->
    begin match pp with
      | None -> ()
      | Some pp ->
        Format.fprintf pp "<- %a %a@." Status.pp s Cstruct.hexdump_pp payload
    end ;
    failwith ((Status.to_string s) ^ " " ^ msg)

let write_payload
    ?pp ?(msg="write_payload") ?buf ?(mark_last=false) ~cmd ?p1 ?p2 h cs =
  let rec inner cs =
    let cs_len = Cstruct.len cs in
    let lc = min Apdu.max_data_length cs_len in
    let last = lc = cs_len in
    let p1 = match last, mark_last, p1 with
      | true, true, None -> Some 0x80
      | true, true, Some p1 -> Some (0x80 lor p1)
      | _ -> p1 in
    let response = apdu ?pp ~msg ?buf h
        Apdu.(create ?p1 ?p2 ~lc ~data:(Cstruct.sub cs 0 lc) cmd) in
    if last then response
    else inner (Cstruct.shift cs lc) in
  if Cstruct.len cs = 0 then cs else inner cs

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
