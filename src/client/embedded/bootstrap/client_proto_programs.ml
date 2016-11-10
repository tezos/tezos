(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_proto_args

let report_parse_error _prefix exn _lexbuf =
  let open Lexing in
  let open Script_located_ir in
  let print_loc ppf ((sl, sc), (el, ec)) =
    if sl = el then
      if sc = ec then
      Format.fprintf ppf
        "at line %d character %d"
        sl sc
      else
      Format.fprintf ppf
        "at line %d characters %d to %d"
        sl sc ec
    else
      Format.fprintf ppf
        "from line %d character %d to line %d character %d"
        sl sc el ec in
  match exn with
  | Missing_program_field n ->
      Cli_entries.error "missing script %s" n
  | Illegal_character (loc, c) ->
      Cli_entries.error "%a, illegal character %C" print_loc loc c
  | Illegal_escape (loc, c) ->
      Cli_entries.error "%a, illegal escape sequence %S" print_loc loc c
  | Failure s ->
      Cli_entries.error "%s" s
  | exn ->
      Cli_entries.error "%s" @@ Printexc.to_string exn

let parse_program s =
  let lexbuf = Lexing.from_string s in
  try
    Lwt.return
      (Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf |>
       List.map Script_located_ir.strip_locations |> fun fields ->
       let rec get_field n = function
         | Script.Prim (_, pn, [ ctns ]) :: _ when n = pn -> ctns
         | _ :: rest -> get_field n rest
         | [] -> raise (Script_located_ir.Missing_program_field n) in
       Script.{ code = get_field "code" fields ;
                arg_type = get_field "parameter" fields ;
                ret_type = get_field "return" fields ;
                storage_type = get_field "storage" fields }
      )
  with
  | exn -> report_parse_error "program: " exn lexbuf

let rec print_ir locations ppf node =
  let open Script in
  let rec do_seq = function
    | [] -> assert false
    | [ last ] -> Format.fprintf ppf "%a }@]" (print_ir locations) last
    | fst :: rest -> Format.fprintf ppf "%a ;@ " (print_ir locations) fst ; do_seq rest in
  let rec do_args = function
    | [] -> assert false
    | [ last ] -> Format.fprintf ppf "%a@]" (print_ir locations) last
    | fst :: rest -> Format.fprintf ppf "%a@," (print_ir locations) fst ; do_args rest in
  let print_location ppf loc =
    if locations loc then begin
      Format.fprintf ppf " /* %d */" loc
  end in
  match node with
  | String (_, s) -> Format.fprintf ppf "%S" s
  | Int (_, s) -> Format.fprintf ppf "%s" s
  | Seq (_, [ one ]) -> print_ir locations ppf one
  | Seq (_, []) -> Format.fprintf ppf "{}" ;
  | Seq (_, seq) ->
      Format.fprintf ppf "{ @[<v>" ;
      do_seq seq
  | Prim (loc, name, []) ->
      Format.fprintf ppf "%s%a" name print_location loc
  | Prim (loc, name, seq) ->
      Format.fprintf ppf "@[<v 2>%s%a@," name print_location loc;
      do_args seq

let print_program locations ppf c =
  Format.fprintf ppf
    "@[<v 2>storage@,%a@]@."
    (print_ir (fun _ -> false)) (c : Script.code).Script.storage_type ;
  Format.fprintf ppf
    "@[<v 2>parameter@,%a@]@."
    (print_ir (fun _ -> false)) (c : Script.code).Script.arg_type ;
  Format.fprintf ppf
    "@[<v 2>return@,%a@]@."
    (print_ir (fun _ -> false)) (c : Script.code).Script.ret_type ;
  Format.fprintf ppf
    "@[<v 2>code@,%a@]"
    (print_ir locations) (c : Script.code).Script.code

let parse_data s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> Lwt.return (Script_located_ir.strip_locations node)
    | _ -> Cli_entries.error "single data expression expected"
  with
  | exn -> report_parse_error "data: " exn lexbuf

let parse_data_type s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> Lwt.return (Script_located_ir.strip_locations node)
    | _ -> Cli_entries.error "single type expression expected"
  with
  | exn -> report_parse_error "data_type: " exn lexbuf

module Program = Client_aliases.Alias (struct
    type t = Script.code
    let encoding = Script.code_encoding
    let of_source s = parse_program s
    let to_source p = Lwt.return (Format.asprintf "%a" (print_program (fun _ -> false)) p)
    let name = "program"
  end)

let commands () =
  let open Cli_entries in
  register_group "programs" "Commands for managing the record of known programs" ;
  [
    command
      ~group: "programs"
      ~desc: "lists all known programs"
      (fixed [ "list" ; "known" ; "programs" ])
      (fun () -> Program.load () >>= fun list ->
        List.iter (fun (n, _) -> message "%s" n) list ; Lwt.return ()) ;
    command
      ~group: "programs"
      ~desc: "remember a program under some name"
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun name hash () -> Program.add name hash) ;
    command
      ~group: "programs"
      ~desc: "forget a remembered program"
      (prefixes [ "forget" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (name, _) () -> Program.del name) ;
    command
      ~group: "programs"
      ~desc: "display a program"
      (prefixes [ "show" ; "known" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (_, program) () ->
         Program.to_source program >>= fun source ->
         Format.printf "%s\n" source ;
         Lwt.return ()) ;
    command
      ~group: "programs"
      ~desc: "ask the node to typecheck a program"
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun program () ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_code (block ()) program >>= function
         | Ok type_map ->
             message "Well typed" ;
             print_program
               (fun l -> List.mem_assoc l type_map)
               Format.std_formatter program ;
             Format.printf "@." ;
             List.iter
               (fun (loc, (before, after)) ->
                  Format.printf
                    "%3d@[<v 0> : [ @[<v 0>%a ]@]@,-> [ @[<v 0>%a ]@]@]@."
                    loc
                    (Format.pp_print_list (print_ir (fun _ -> false)))
                    before
                    (Format.pp_print_list (print_ir (fun _ -> false)))
                    after)
               type_map ;
             Lwt.return ()
         | Error errs ->
             pp_print_error Format.err_formatter errs ;
             error "ill-typed program") ;
    command
      ~group: "programs"
      ~desc: "ask the node to typecheck a tagged data expression"
      (prefixes [ "typecheck" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to typecheck" parse_data
       @@ prefixes [ "against" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"the expected type" parse_data
       @@ stop)
      (fun data exp_ty () ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_untagged_data
           (block ()) (data, exp_ty) >>= function
         | Ok () ->
             message "Well typed" ;
             Lwt.return ()
         | Error errs ->
             pp_print_error Format.err_formatter errs ;
             error "ill-typed data") ;
    command
      ~group: "programs"
      ~desc: "ask the node to compute the hash of an untagged data expression \
              using the same algorithm as script instruction H"
      (prefixes [ "hash" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash" parse_data
       @@ stop)
      (fun data () ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data (block ()) data >>= function
         | Ok hash ->
             message "%S" hash;
             Lwt.return ()
         | Error errs ->
             pp_print_error Format.err_formatter errs ;
             error "ill-formed data") ;
    command
      ~group: "programs"
      ~desc: "ask the node to compute the hash of an untagged data expression \
              using the same algorithm as script instruction H, sign it using \
              a given secret key, and display it using the format expected by \
              script instruction CHECK_SIGNATURE"
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash" parse_data
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.alias_param
       @@ stop)
      (fun data (_, key) () ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data (block ()) data >>= function
         | Ok hash ->
             let signature = Ed25519.sign key (MBytes.of_string hash) in
             message "Hash: %S@.Signature: %S"
               hash
               (signature |>
                Data_encoding.Binary.to_bytes Ed25519.signature_encoding |>
                Hex_encode.hex_of_bytes) ;
             Lwt.return ()
         | Error errs ->
             pp_print_error Format.err_formatter errs ;
             error "ill-formed data") ;
  ]
