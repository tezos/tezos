(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519
open Client_proto_args

let report_parse_error _prefix exn _lexbuf =
  let open Lexing in
  let open Script_located_ir in
  let print_loc ppf (s, e) =
    if s.line = e.line then
      if s.column = e.column then
        Format.fprintf ppf
          "at line %d character %d"
          s.line s.column
      else
        Format.fprintf ppf
          "at line %d characters %d to %d"
          s.line s.column e.column
    else
      Format.fprintf ppf
        "from line %d character %d to line %d character %d"
        s.line s.column e.line e.column in
  match exn with
  | Missing_program_field n ->
      failwith "missing script %s" n
  | Illegal_character (loc, c) ->
      failwith "%a, illegal character %C" print_loc loc c
  | Illegal_escape (loc, c) ->
      failwith "%a, illegal escape sequence %S" print_loc loc c
  | Failure s ->
      failwith "%s" s
  | exn ->
      failwith "%s" @@ Printexc.to_string exn

let print_location_mark ppf = function
  | None -> ()
  | Some l -> Format.fprintf ppf " /* %d */" l

let no_locations _ = None

let rec print_expr_unwrapped locations ppf = function
  | Script.Prim (loc, name, []) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "%s" name
        | Some _ as l -> Format.fprintf ppf "(%s%a)" name print_location_mark l
      end
  | Script.Prim (loc, name, args) ->
      Format.fprintf ppf "@[<hov 2>%s%a@ %a@]"
        name print_location_mark (locations loc)
        (Format.pp_print_list
           ~pp_sep: Format.pp_print_space
           (print_expr locations))
        args
  | Script.Seq (loc, []) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "{}"
        | Some _ as l -> Format.fprintf ppf "{%a }" print_location_mark l
      end
  | Script.Seq (loc, exprs) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "@[<hv 2>{ "
        | Some _ as l -> Format.fprintf ppf "@[<hv 2>{%a@ " print_location_mark l
      end ;
      Format.fprintf ppf "%a@] }"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf " ;@ ")
           (print_expr_unwrapped locations))
        exprs
  | Script.Int (loc, n) ->
      Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
  | Script.String (loc, s) ->
      Format.fprintf ppf "%S%a" s print_location_mark (locations loc)

and print_expr locations ppf = function
  | Script.Prim (_, _, _ :: _) as expr ->
      Format.fprintf ppf "(%a)" (print_expr_unwrapped locations) expr
  | expr -> print_expr_unwrapped locations ppf expr

let print_storage ppf ({ storage } : Script.storage) =
  print_expr no_locations ppf storage

let print_stack ppf = function
  | [] -> Format.fprintf ppf "[]"
  | more ->
      Format.fprintf ppf "@[<hov 2>[ %a ]@]"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf " :@ ")
           (print_expr_unwrapped no_locations))
        more

let print_typed_code locations ppf (expr, type_map) =
  let rec print_typed_code_unwrapped ppf expr =
    match expr with
    | Script.Prim (loc, name, []) ->
        Format.fprintf ppf "%s%a"
          name print_location_mark (locations loc)
    | Script.Prim (loc, name, args) ->
        Format.fprintf ppf "@[<hov 2>%s%a@ %a@]"
          name print_location_mark (locations loc)
          (Format.pp_print_list
             ~pp_sep: Format.pp_print_space
             print_typed_code)
          args
    | Script.Seq (loc, []) ->
        begin match List.assoc loc type_map with
          | exception Not_found -> Format.fprintf ppf "{}"
          | (first, _) ->
              match locations loc with
              | None ->
                  Format.fprintf ppf "{} /* %a */"
                    print_stack first
              | Some _ as l ->
                  Format.fprintf ppf "{%a %a }"
                    print_location_mark l print_stack first
        end
    | Script.Seq (loc, exprs) ->
        begin match locations loc with
          | None ->
              Format.fprintf ppf "@[<v 2>{ "
          | Some _ as l ->
              Format.fprintf ppf "@[<v 2>{%a@,"
                print_location_mark l
        end ;
        let rec loop = function
          | [] -> assert false
          | [ Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _) as expr ] ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a }@]"
                      print_typed_code_unwrapped expr
                | (before, after) ->
                    Format.fprintf ppf "/* %a */@,%a@,/* %a */ }@]"
                      print_stack before
                      print_typed_code_unwrapped expr
                      print_stack after
              end ;
          | Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _) as expr :: rest ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a ;@,"
                      print_typed_code_unwrapped expr ;
                    loop rest
                | (before, _) ->
                    Format.fprintf ppf "/* %a */@,%a ;@,"
                      print_stack before
                      print_typed_code_unwrapped expr ;
                    loop rest
              end ;
          | [ Seq (_, _) as expr ] ->
              Format.fprintf ppf "%a }@]"
                print_typed_code_unwrapped expr
          | Seq (_, _) as expr :: rest ->
              Format.fprintf ppf "%a@,"
                print_typed_code_unwrapped expr ;
              loop rest in
        loop exprs ;
    | Script.Int (loc, n) ->
        Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
    | Script.String (loc, s) ->
        Format.fprintf ppf "%S%a" s print_location_mark (locations loc)
  and print_typed_code ppf = function
    | Script.Prim (_, _, _ :: _) as expr ->
        Format.fprintf ppf "(%a)" print_typed_code_unwrapped expr
    | expr -> print_typed_code_unwrapped ppf expr in
  print_typed_code_unwrapped ppf expr

let print_program locations ppf ((c : Script.code), type_map) =
  Format.fprintf ppf
    "@[<v 0>@[<hov 2>storage@ %a ;@]@,\
     @[<hov 2>parameter@ %a ;@]@,\
     @[<hov 2>return@ %a ;@]@,\
     @[<hov 2>code@ %a@]@]"
    (print_expr no_locations) c.storage_type
    (print_expr no_locations) c.arg_type
    (print_expr no_locations) c.ret_type
    (print_typed_code locations) (c.code, type_map)

let collect_error_locations errs =
  let open Script_typed_ir in
  let open Script_ir_translator in
  let rec collect acc = function
    | (Ill_typed_data (_, _, _)
      | Ill_formed_type (_, _)
      | Ill_typed_contract (_, _, _, _, _)) :: _
    | [] -> acc
    | (Invalid_arity (loc, _, _, _)
      | Invalid_namespace (loc, _, _, _)
      | Invalid_primitive (loc, _, _)
      | Invalid_case (loc, _)
      | Invalid_kind (loc, _, _)
      | Fail_not_in_tail_position loc
      | Undefined_cast (loc, _, _)
      | Undefined_binop (loc, _, _, _)
      | Undefined_unop (loc, _, _)
      | Bad_return (loc, _, _)
      | Bad_stack (loc, _, _, _)
      | Unmatched_branches (loc, _, _)
      | Transfer_in_lambda loc
      | Invalid_constant (loc, _, _)
      | Invalid_contract (loc, _)
      | Comparable_type_expected (loc, _)) :: rest ->
        collect (loc :: acc) rest
    | _ :: rest -> collect acc rest in
  collect [] errs

let report_typechecking_errors ?show_types cctxt errs =
  let open Client_commands in
  let open Script_typed_ir in
  let open Script_ir_translator in
  let rec print_ty (type t) ppf (ty : t ty) =
    let expr = unparse_ty ty in
    print_expr no_locations ppf expr in
  let rec print_stack_ty (type t) ?(depth = max_int) ppf (s : t stack_ty) =
    let rec loop
      : type t. int -> Format.formatter -> t stack_ty -> unit
      = fun depth ppf -> function
        | Empty_t -> ()
        | _ when depth <= 0 ->
            Format.fprintf ppf "..."
        | Item_t (last, Empty_t) ->
            Format.fprintf ppf "%a"
              print_ty last
        | Item_t (last, rest) ->
            Format.fprintf ppf "%a :@ %a"
              print_ty last (loop (depth - 1)) rest in
    match s with
    | Empty_t ->
        Format.fprintf ppf "[]"
    | sty ->
        Format.fprintf ppf "@[<hov 2>[ %a ]@]" (loop depth) sty in
  let rec print_enumeration ppf = function
    | [ single ] ->
        Format.fprintf ppf "%a"
          Format.pp_print_text single
    | [ prev ; last ] ->
        Format.fprintf ppf "%a@ or@ %a"
          Format.pp_print_text prev Format.pp_print_text last
    | first :: rest ->
        Format.fprintf ppf "%a,@ %a"
          Format.pp_print_text first print_enumeration rest
    | [] -> assert false in
  let print_typechecking_error locations err =
    let print_loc ppf loc =
      match locations loc with
      | None ->
          Format.fprintf ppf "At (unmarked) location %d, " loc
      | Some loc ->
          Format.fprintf ppf "At mark /* %d */, " loc in
    match err with
    | Ill_typed_data (name, expr, ty) ->
        cctxt.warning
          "@[<hv 0>@[<hov 2>Ill typed %adata:@ %a@]@ \
           @[<hov 2>is not an expression of type@ %a@]@]"
          (fun ppf -> function
             | None -> ()
             | Some s -> Format.fprintf ppf "%s " s)
          name
          (print_expr locations) expr
          print_ty ty
    | Ill_formed_type (name, expr) ->
        cctxt.warning
          "@[<hov 2>Ill formed type %aexpression@ %a@]"
          (fun ppf -> function
             | None -> ()
             | Some s -> Format.fprintf ppf "%s " s)
          name
          (print_expr locations) expr
    | Ill_typed_contract (expr, arg_ty, ret_ty, storage_ty, type_map) ->
        (match show_types with
         | Some prog -> cctxt.message "%a\n" (print_program no_locations) (prog, type_map)
         | None -> Lwt.return ()) >>= fun () ->
        cctxt.warning
          "@[<v 2>Ill typed contract:@ %a@]"
          (print_program locations)
          ({ Script.storage_type = unparse_ty storage_ty ;
             arg_type = unparse_ty arg_ty ;
             ret_type = unparse_ty ret_ty ;
             code = expr }, [])
    | Invalid_arity (loc, name, exp, got) ->
        cctxt.warning
          "%aprimitive %s expects %d arguments but is given %d."
          print_loc loc name exp got
    | Invalid_namespace (loc, name, exp, got) ->
        let human_namespace = function
          | Instr_namespace -> ("an", "instruction")
          | Type_namespace -> ("a", "type name")
          | Constant_namespace -> ("a", "constant constructor") in
        cctxt.warning
          "@[%aunexpected %s %s, only@ %s@ %s@ can@ be@ used@ here."
          print_loc loc
          (snd (human_namespace got))
          name
          (fst (human_namespace exp)) (snd (human_namespace exp))
    | Invalid_primitive (loc, exp, got) ->
        cctxt.warning
          "@[%ainvalid primitive %s, only@ %a@ can@ be@ used@ here."
          print_loc loc
          got
          print_enumeration exp
    | Invalid_case (loc, name) ->
        cctxt.warning
          "%a%s is not a valid primitive name."
          print_loc loc
          name
    | Invalid_kind (loc, exp, got) ->
        let human_kind = function
          | Seq_kind -> ("a", "sequence")
          | Prim_kind -> ("a", "primitive")
          | Int_kind -> ("an", "int")
          | String_kind -> ("a", "string") in
        cctxt.warning
          "@[%aunexpected %s, only@ %a@ can@ be@ used@ here."
          print_loc loc
          (snd (human_kind got))
          print_enumeration
          (List.map (fun k -> let (a, n) = human_kind k in a ^ " " ^ n) exp)
    | Fail_not_in_tail_position loc ->
        cctxt.warning
          "%aThe FAIL instruction must appear in a tail position."
          print_loc loc
    | Undefined_cast (loc, tya, tyb) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%atype cast is undefined from@ %a@]@ \
           @[<hov 2>to@ %a.@]@]"
          print_loc loc
          print_ty tya
          print_ty tyb
    | Undefined_binop (loc, name, tya, tyb) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%aoperator %s is undefined between@ %a@]@ \
           @[<hov 2>and@ %a.@]@]"
          print_loc loc
          name
          print_ty tya
          print_ty tyb
    | Undefined_unop (loc, name, ty) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%aoperator %s is undefined on@ %a@]@]"
          print_loc loc
          name
          print_ty ty
    | Bad_return (loc, got, exp) ->
        cctxt.warning
          "@[<v 2>%awrong stack type at end of body:@,\
           - @[<hov>expected return stack type:@ %a,@]@,\
           - @[<hov>actual stack type:@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_stack_ty ppf) (Item_t (exp, Empty_t))
          (fun ppf -> print_stack_ty ppf) got
    | Bad_stack (loc, name, depth, sty) ->
        cctxt.warning
          "@[<hov 2>%awrong stack type for instruction %s:@ %a.@]"
          print_loc loc name (print_stack_ty ~depth) sty
    | Unmatched_branches (loc, sta, stb) ->
        cctxt.warning
          "@[<v 2>%atwo branches don't end with the same stack type:@,\
           - @[<hov>first stack type:@ %a,@]@,\
           - @[<hov>other stack type:@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_stack_ty ppf) sta
          (fun ppf -> print_stack_ty ppf) stb
    | Transfer_in_lambda loc ->
        cctxt.warning
          "%aThe TRANSFER_TOKENS instruction cannot appear in a lambda."
          print_loc loc
    | Bad_stack_length ->
        cctxt.warning
          "Bad stack length."
    | Bad_stack_item lvl ->
        cctxt.warning
          "Bad stack item %d ."
          lvl
    | Invalid_constant (loc, got, exp) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%avalue@ %a@]@ \
           @[<hov 2>is invalid for type@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_expr no_locations ppf) got
          print_ty exp
    | Invalid_contract (loc, contract) ->
        cctxt.warning
          "%ainvalid contract %a."
          print_loc loc Contract.pp contract
    | Comparable_type_expected (loc, ty) ->
        cctxt.warning "%acomparable type expected."
          print_loc loc >>= fun () ->
        cctxt.warning "@[<hov 0>@[<hov 2>Type@ %a@]@ is not comparable.@]"
          print_ty ty
    | Bad_sign ty ->
        begin match ty with
          | Int_t kind ->
              let signed = match kind with
                | Script_int.Int8 -> true
                | Script_int.Int16 -> true
                | Script_int.Int32 -> true
                | Script_int.Int64 -> true
                | Script_int.Uint8 -> false
                | Script_int.Uint16 -> false
                | Script_int.Uint32 -> false
                | Script_int.Uint64 -> false in
              if signed then
                cctxt.warning "Unsigned integer type expected."
              else
                cctxt.warning "Signed integer type expected."
          | _ -> assert false
        end
    | Inconsistent_types (tya, tyb) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>Type@ %a@]@ \
           @[<hov 2>is not compatible with type@ %a.@]@]"
          print_ty tya print_ty tyb
    | err ->
        cctxt.warning "%a"
          Local_environment.Environment.Error_monad.pp_print_error [ err ] in
  let rec print_typechecking_error_trace locations errs =
    let locations = match errs with
      | (Ill_typed_data (_, _, _)
        | Ill_formed_type (_, _)
        | Ill_typed_contract (_, _, _, _, _)) :: rest ->
          let collected =
            collect_error_locations rest in
          let assoc, _ =
            List.fold_left
              (fun (acc, i) l ->
                 if List.mem_assoc l acc then
                   (acc, i)
                 else
                   ((l, i) :: acc, i + 1))
              ([], 1) collected in
          (fun l -> try Some (List.assoc l assoc) with Not_found -> None)
      | _ -> locations in
    match errs with
    | [] -> Lwt.return ()
    | err :: errs ->
        print_typechecking_error locations err >>= fun () ->
        print_typechecking_error_trace locations errs in
  Lwt_list.iter_s
    (function
      | Ecoproto_error errs ->
          print_typechecking_error_trace no_locations errs
      | err -> cctxt.warning "%a" pp_print_error [ err ])
    errs

type 'a parsed =
  { ast : 'a ;
    source : string ;
    loc_table : (string * (int * Script_located_ir.location) list) list }

let parse_program source =
  let lexbuf = Lexing.from_string source in
  try
    return
      (Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf |> fun fields ->
       let rec get_field n = function
         | Script_located_ir.Prim (_, pn, [ ctns ]) :: _ when n = pn -> ctns
         | _ :: rest -> get_field n rest
         | [] -> raise (Script_located_ir.Missing_program_field n) in
       let code, code_loc_table =
         Script_located_ir.strip_locations (get_field "code" fields) in
       let arg_type, parameter_loc_table =
         Script_located_ir.strip_locations (get_field "parameter" fields) in
       let ret_type, return_loc_table =
         Script_located_ir.strip_locations (get_field "return" fields) in
       let storage_type, storage_loc_table =
         Script_located_ir.strip_locations (get_field "storage" fields) in
       let ast = Script.{ code ; arg_type ; ret_type ; storage_type } in
       let loc_table =
         [ "code", code_loc_table ;
           "parameter", parameter_loc_table ;
           "return", return_loc_table ;
           "storage", storage_loc_table ] in
       { ast ; source ; loc_table })
  with
  | exn -> report_parse_error "program: " exn lexbuf

let parse_data source =
  let lexbuf = Lexing.from_string source in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] ->
        let ast, loc_table = Script_located_ir.strip_locations node in
        let loc_table = [ "data", loc_table ] in
        return { ast ; source ; loc_table }
    | _ -> failwith "single data expression expected"
  with
  | exn -> report_parse_error "data: " exn lexbuf

let parse_data_type source =
  let lexbuf = Lexing.from_string source in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] ->
        let ast, loc_table = Script_located_ir.strip_locations node in
        let loc_table = [ "data", loc_table ] in
        return { ast ; source ; loc_table }
    | _ -> failwith "single type expression expected"
  with
  | exn -> report_parse_error "data_type: " exn lexbuf

let unexpand_macros type_map (program : Script.code) =
  let open Script in
  let rec caddr type_map acc = function
    | [] -> Some (List.rev acc)
    | Prim (loc, "CAR" , []) :: rest when List.mem_assoc loc type_map ->
        caddr type_map ((loc, "A") :: acc) rest
    | Prim (loc, "CDR" , []) :: rest when List.mem_assoc loc type_map ->
        caddr type_map ((loc, "D") :: acc) rest
    | _ -> None in
  let rec unexpand type_map node =
    match node with
    | Seq (loc, l) ->
        begin match caddr type_map [] l with
          | None ->
              let type_map, l =
                List.fold_left
                  (fun (type_map, acc) e ->
                     let type_map, e = unexpand type_map e in
                     type_map, e :: acc)
                  (type_map, [])
                  l in
              type_map, Seq (loc, List.rev l)
          | Some l ->
              let locs, steps = List.split l in
              let name = "C" ^ String.concat "" steps ^ "R" in
              let first, last = List.hd locs, List.hd (List.rev locs) in
              let (before, _) = List.assoc first type_map in
              let (_, after) = List.assoc last type_map in
              let type_map =
                List.filter
                  (fun (loc, _) -> not (List.mem loc locs))
                  type_map in
              let type_map = (loc, (before, after)) :: type_map in
              type_map, Prim (loc, name, [])
        end
    | oth -> type_map, oth in
  let type_map, code = unexpand type_map program.code in
  type_map, { program with code }

module Program = Client_aliases.Alias (struct
    type t = Script.code parsed
    let encoding =
      let open Data_encoding in
      let loc_table_encoding =
        assoc (list (tup2 uint16 Script_located_ir.location_encoding)) in
      conv
        (fun { ast ; source ; loc_table } -> (ast, source, loc_table))
        (fun (ast, source, loc_table) -> { ast ; source ; loc_table })
        (obj3
           (req "ast" Script.code_encoding)
           (req "source" string)
           (req "loc_table" loc_table_encoding))
    let of_source _cctxt s = parse_program s
    let to_source _ { source } = return source
    let name = "program"
  end)

let group =
  { Cli_entries.name = "programs" ;
    title = "Commands for managing the record of known programs" }

let commands () =
  let open Cli_entries in
  let show_types = ref false in
  let show_types_arg =
    "-details",
    Arg.Set show_types,
    "Show the types of each instruction" in
  let emacs_mode = ref false in
  let emacs_mode_arg =
    "-emacs",
    Arg.Set emacs_mode,
    "Output in michelson-mode.el compatible format" in
  let trace_stack = ref false in
  let trace_stack_arg =
    "-trace-stack",
    Arg.Set trace_stack,
    "Show the stack after each step" in
  let amount, amount_arg =
    Client_proto_args.tez_arg
      ~name:"-amount"
      ~desc:"The amount of the transfer in \xEA\x9C\xA9."
      ~default: "0.00" in
  [

    command ~group ~desc: "lists all known programs"
      (fixed [ "list" ; "known" ; "programs" ])
      (fun cctxt ->
         Program.load cctxt >>=? fun list ->
         Lwt_list.iter_s (fun (n, _) -> cctxt.message "%s" n) list >>= fun () ->
         return ()) ;

    command ~group ~desc: "remember a program under some name"
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun name hash cctxt -> Program.add cctxt name hash) ;

    command ~group ~desc: "forget a remembered program"
      (prefixes [ "forget" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (name, _) cctxt -> Program.del cctxt name) ;

    command ~group ~desc: "display a program"
      (prefixes [ "show" ; "known" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (_, program) cctxt ->
         Program.to_source cctxt program >>=? fun source ->
         cctxt.message "%s\n" source >>= fun () ->
         return ()) ;

    command ~group ~desc: "ask the node to run a program"
      ~args: [ trace_stack_arg ; amount_arg ]
      (prefixes [ "run" ; "program" ]
       @@ Program.source_param
       @@ prefixes [ "on" ; "storage" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the storage data"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "and" ; "input" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the input data"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun program storage input cctxt ->
         let open Data_encoding in
         if !trace_stack then
           Client_proto_rpcs.Helpers.trace_code cctxt.rpc_config
             cctxt.config.block program.ast (storage.ast, input.ast, !amount) >>= function
           | Ok (storage, output, trace) ->
               cctxt.message
                 "@[<v 0>@[<v 2>storage@,%a@]@,\
                  @[<v 2>output@,%a@]@,@[<v 2>trace@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output
                 (Format.pp_print_list
                    (fun ppf (loc, gas, stack) ->
                       Format.fprintf ppf
                         "- @[<v 0>location: %d (remaining gas: %d)@,\
                          [ @[<v 0>%a ]@]@]"
                         loc gas
                         (Format.pp_print_list (print_expr no_locations))
                         stack))
                 trace >>= fun () ->
               return ()
           | Error errs ->
               report_typechecking_errors cctxt errs >>= fun () ->
               cctxt.error "error running program" >>= fun () ->
               return ()
         else
           Client_proto_rpcs.Helpers.run_code cctxt.rpc_config
             cctxt.config.block program.ast (storage.ast, input.ast, !amount) >>= function
           | Ok (storage, output) ->
               cctxt.message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>output@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output >>= fun () ->
               return ()
           | Error errs ->
               report_typechecking_errors cctxt errs >>= fun () ->
               cctxt.error "error running program" >>= fun () ->
               return ()) ;

    command ~group ~desc: "ask the node to typecheck a program"
      ~args: [ show_types_arg ; emacs_mode_arg ]
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun program cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_code
           cctxt.rpc_config cctxt.config.block program.ast >>= fun res ->
         if !emacs_mode then
           let emacs_type_map type_map =
             (Utils.filter_map
                (fun (n, loc) ->
                   try
                     let bef, aft = List.assoc n type_map in
                     Some (loc, bef, aft)
                   with
                     Not_found -> None)
                (List.assoc "code" program.loc_table),
              []) in
           begin match res with
             | Ok type_map ->
                 Lwt.return (emacs_type_map type_map)
             | Error errs ->
                 let msg = Buffer.create 5000 in
                 let cctxt = Client_commands.make_context
                     (fun _ t -> Buffer.add_string msg t ; Buffer.add_char msg '\n' ; Lwt.return ()) in
                 match errs with
                 | Ecoproto_error (Script_ir_translator.Ill_formed_type
                                     (Some ("return" | "parameter" | "storage" as field), _) :: errs) :: _ ->
                     report_typechecking_errors cctxt [ Ecoproto_error errs ] >>= fun () ->
                     Lwt.return ([], [ List.assoc 0 (List.assoc field program.loc_table), Buffer.contents msg ])
                 | Ecoproto_error (Script_ir_translator.Ill_typed_contract (_, _, _, _, type_map) :: errs) :: _ ->
                     (report_typechecking_errors cctxt [ Ecoproto_error errs ]  >>= fun () ->
                      let (types, _) = emacs_type_map type_map in
                      let loc = match collect_error_locations errs with
                        | hd :: _ -> hd
                        | [] -> 0 in
                      Lwt.return (types, [ List.assoc loc (List.assoc "code" program.loc_table), Buffer.contents msg ]))
                 | _ -> Lwt.return ([], [])
           end >>= fun (types, errors) ->
           cctxt.message
             "(@[<v 0>(types . (@[<v 0>%a@]))@,\
              (errors . (@[<v 0>%a@])))@]"
             (Format.pp_print_list
                (fun ppf (({ Script_located_ir.point = s },
                           { Script_located_ir.point = e }),
                          bef, aft) ->
                  Format.fprintf ppf "(%d %d \"%s\")" (s + 1) (e + 1)
                    (String.concat "\\n"
                       (String.split_on_char '\n'
                          (Format.asprintf "@[<v 0>%a@, \\u2B87@,%a@]"
                             print_stack bef print_stack aft)))))
             types
             (Format.pp_print_list
                (fun ppf (({ Script_located_ir.point = s },
                           { Script_located_ir.point = e }),
                          err) ->
                  Format.fprintf ppf "(%d %d %S)" (s + 1) (e + 1) err))
             errors >>= fun () ->
           return ()
         else
           match res with
           | Ok type_map ->
               let type_map, program = unexpand_macros type_map program.ast in
               cctxt.message "Well typed" >>= fun () ->
               if !show_types then
                 cctxt.message "%a" (print_program no_locations) (program, type_map) >>= fun () ->
                 return ()
               else return ()
           | Error errs ->
               report_typechecking_errors
                 ?show_types:(if !show_types then Some program.ast else None) cctxt errs >>= fun () ->
               failwith "ill-typed program") ;

    command ~group ~desc: "ask the node to typecheck a data expression"
      (prefixes [ "typecheck" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to typecheck"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "against" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"the expected type"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun data exp_ty cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_data cctxt.Client_commands.rpc_config
           cctxt.config.block (data.ast, exp_ty.ast) >>= function
         | Ok () ->
             cctxt.message "Well typed" >>= fun () ->
             return ()
         | Error errs ->
             report_typechecking_errors cctxt errs >>= fun () ->
             failwith "ill-typed data") ;

    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H"
      (prefixes [ "hash" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun data cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt.Client_commands.rpc_config
           cctxt.config.block (data.ast) >>= function
         | Ok hash ->
             cctxt.message "%S" hash >>= fun () ->
             return ()
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs  >>= fun () ->
             failwith "ill-formed data") ;

    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H, sign it using \
              a given secret key, and display it using the format expected by \
              script instruction CHECK_SIGNATURE"
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.alias_param
       @@ stop)
      (fun data (_, key) cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt.rpc_config
           cctxt.config.block (data.ast) >>= function
         | Ok hash ->
             let signature = Ed25519.sign key (MBytes.of_string hash) in
             cctxt.message "Hash: %S@.Signature: %S"
               hash
               (signature |>
                Data_encoding.Binary.to_bytes Ed25519.Signature.encoding |>
                Hex_encode.hex_of_bytes) >>= fun () ->
             return ()
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs >>= fun () ->
             failwith "ill-formed data") ;

  ]
