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

let report_parse_error cctxt _prefix exn _lexbuf =
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
      cctxt.Client_commands.error "missing script %s" n
  | Illegal_character (loc, c) ->
      cctxt.Client_commands.error "%a, illegal character %C" print_loc loc c
  | Illegal_escape (loc, c) ->
      cctxt.Client_commands.error "%a, illegal escape sequence %S" print_loc loc c
  | Failure s ->
      cctxt.Client_commands.error "%s" s
  | exn ->
      cctxt.Client_commands.error "%s" @@ Printexc.to_string exn

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

let print_typed_code locations ppf (expr, type_map) =
  let print_stack ppf = function
    | [] -> Format.fprintf ppf "[]"
    | more ->
        Format.fprintf ppf "@[<hov 2>[ %a ]@]"
          (Format.pp_print_list
             ~pp_sep: (fun ppf () -> Format.fprintf ppf " :@ ")
             (print_expr_unwrapped no_locations))
          more in
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

let report_typechecking_errors cctxt errs =
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
  let rec collect_locations acc = function
    | (Ill_typed_data (_, _, _)
      | Ill_formed_type (_, _)
      | Ill_typed_contract (_, _, _, _)) :: _
    | [] ->
        let assoc, _ =
          List.fold_left
            (fun (acc, i) l ->
               if List.mem_assoc l acc then
                 (acc, i)
               else
                 ((l, i) :: acc, i + 1))
            ([], 1) acc in
        (fun l -> try Some (List.assoc l assoc) with Not_found -> None)
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
        collect_locations (loc :: acc) rest
    | _ :: rest -> collect_locations acc rest in
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
    | Ill_typed_contract (expr, arg_ty, ret_ty, storage_ty) ->
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
        | Ill_typed_contract (_, _, _, _)) :: rest ->
          collect_locations [] rest
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

let parse_program cctxt s =
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
  | exn -> report_parse_error cctxt "program: " exn lexbuf

let parse_data cctxt s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> Lwt.return (Script_located_ir.strip_locations node)
    | _ -> cctxt.Client_commands.error "single data expression expected"
  with
  | exn -> report_parse_error cctxt "data: " exn lexbuf

let parse_data_type cctxt s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> Lwt.return (Script_located_ir.strip_locations node)
    | _ -> cctxt.Client_commands.error "single type expression expected"
  with
  | exn -> report_parse_error cctxt "data_type: " exn lexbuf

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
              let type_map = (loc, (before, after)):: type_map in
              type_map, Prim (loc, name, [])
        end
    | oth -> type_map, oth in
  let type_map, code = unexpand type_map program.code in
  type_map, { program with code }

module Program = Client_aliases.Alias (struct
    type t = Script.code
    let encoding = Script.code_encoding
    let of_source cctxt s = parse_program cctxt s
    let to_source _ p = Lwt.return (Format.asprintf "%a" (print_program no_locations) (p, []))
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
  let trace_stack = ref false in
  let trace_stack_arg =
    "-trace-stack",
    Arg.Set trace_stack,
    "Show the stack after each step" in
  [
    command ~group ~desc: "lists all known programs"
      (fixed [ "list" ; "known" ; "programs" ])
      (fun cctxt -> Program.load cctxt >>= fun list ->
        Lwt_list.iter_s (fun (n, _) -> cctxt.message "%s" n) list) ;
    command ~group ~desc: "remember a program under some name"
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun name hash cctxt ->
         Program.add cctxt name hash) ;
    command ~group ~desc: "forget a remembered program"
      (prefixes [ "forget" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (name, _) cctxt ->
         Program.del cctxt name) ;
    command ~group ~desc: "display a program"
      (prefixes [ "show" ; "known" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun (_, program) cctxt ->
         Program.to_source cctxt program >>= fun source ->
         cctxt.message "%s\n" source) ;
    command ~group ~desc: "ask the node to run a program"
      ~args: [ trace_stack_arg ]
      (prefixes [ "run" ; "program" ]
       @@ Program.source_param
       @@ prefixes [ "on" ; "storage" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the storage data" parse_data
       @@ prefixes [ "and" ; "input" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the input data" parse_data
       @@ stop)
      (fun program storage input cctxt ->
         let open Data_encoding in
         if !trace_stack then
           Client_proto_rpcs.Helpers.trace_code cctxt
             cctxt.config.block program (storage, input) >>= function
           | Ok (storage, output, trace) ->
               cctxt.message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>output@,%a@]@,@[<v 2>trace@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output
                 (Format.pp_print_list
                    (fun ppf (loc, gas, stack) ->
                       Format.fprintf ppf
                         "- @[<v 0>location: %d (remaining gas: %d)@,[ @[<v 0>%a ]@]@]"
                         loc gas
                         (Format.pp_print_list (print_expr no_locations))
                         stack))
                 trace
           | Error errs ->
               cctxt.warning "%a" pp_print_error errs >>= fun () ->
               cctxt.error "error running program"
         else
           Client_proto_rpcs.Helpers.run_code cctxt
             cctxt.config.block program (storage, input) >>= function
           | Ok (storage, output) ->
               cctxt.message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>output@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output
           | Error errs ->
               cctxt.warning "%a" pp_print_error errs >>= fun () ->
               cctxt.error "error running program") ;
    command ~group ~desc: "ask the node to typecheck a program"
      ~args: [ show_types_arg ]
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun program cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_code cctxt cctxt.config.block program >>= function
         | Ok type_map ->
             let type_map, program = unexpand_macros type_map program in
             cctxt.message "Well typed" >>= fun () ->
             if !show_types then
               cctxt.message "%a" (print_program no_locations) (program, type_map)
             else Lwt.return ()
         | Error errs ->
             report_typechecking_errors cctxt errs >>= fun () ->
             cctxt.error "ill-typed program") ;
    command ~group ~desc: "ask the node to typecheck a data expression"
      (prefixes [ "typecheck" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to typecheck" parse_data
       @@ prefixes [ "against" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"the expected type" parse_data
       @@ stop)
      (fun data exp_ty cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_data cctxt
           cctxt.config.block (data, exp_ty) >>= function
         | Ok () ->
             cctxt.message "Well typed"
         | Error errs ->
             report_typechecking_errors cctxt errs >>= fun () ->
             cctxt.error "ill-typed data") ;
    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H"
      (prefixes [ "hash" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash" parse_data
       @@ stop)
      (fun data cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt
           cctxt.config.block data >>= function
         | Ok hash ->
             cctxt.message "%S" hash
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs  >>= fun () ->
             cctxt.error "ill-formed data") ;
    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H, sign it using \
              a given secret key, and display it using the format expected by \
              script instruction CHECK_SIGNATURE"
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash" parse_data
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.alias_param
       @@ stop)
      (fun data (_, key) cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt
           cctxt.config.block data >>= function
         | Ok hash ->
             let signature = Ed25519.sign key (MBytes.of_string hash) in
             cctxt.message "Hash: %S@.Signature: %S"
               hash
               (signature |>
                Data_encoding.Binary.to_bytes Ed25519.Signature.encoding |>
                Hex_encode.hex_of_bytes)
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs >>= fun () ->
             cctxt.error "ill-formed data") ;
  ]
