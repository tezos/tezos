(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type ('p, 'ctx) parameter =
  { converter: ('ctx -> string -> 'p tzresult Lwt.t) ;
    autocomplete: ('ctx -> string list tzresult Lwt.t) option }

let parameter ?autocomplete converter =
  { converter ; autocomplete }

let compose_parameters { converter = c1; autocomplete = a1' } { converter = c2; autocomplete = a2' } =
  { converter = (fun ctx s ->
        c1 ctx s >>= function
        | Ok r -> return r
        | Error _ -> c2 ctx s);
    autocomplete = match a1' with
      | None -> a2'
      | Some a1 -> match a2' with
        | None -> a1'
        | Some a2 -> Some (fun ctx ->
            a1 ctx >>=? fun r1 ->
            a2 ctx >>=? fun r2 ->
            return (List.concat [r1; r2]))
  }

let map_parameter ~f { converter; autocomplete } =
  { converter = (fun ctx s -> converter ctx s >>|? f);
    autocomplete
  }

type label =
  { long : string ;
    short : char option }

type ('a, 'ctx) arg =
  | Arg : { doc : string ;
            label : label ;
            placeholder : string ;
            kind : ('p, 'ctx) parameter } ->
    ('p option, 'ctx) arg
  | DefArg : { doc : string ;
               label : label ;
               placeholder : string ;
               kind : ('p, 'ctx) parameter ;
               default : string } -> ('p, 'ctx) arg
  | Switch : { label : label ;
               doc : string } ->
    (bool, 'ctx) arg
  | Constant : 'a  -> ('a, 'ctx) arg

type ('a, 'arg) args =
  | NoArgs : (unit, 'args) args
  | AddArg : ('a, 'args) arg * ('b, 'args) args ->
    ('a * 'b, 'args) args

(* A simple structure for command interpreters.
   This is more generic than the exported one, see end of file. *)
type ('a, 'ctx) params =
  | Prefix : string * ('a, 'ctx) params ->
    ('a, 'ctx) params
  | Param : string * string *
            ('p, 'ctx) parameter *
            ('a, 'ctx) params ->
    ('p -> 'a, 'ctx) params
  | Stop :
      ('ctx -> unit tzresult Lwt.t, 'ctx) params
  | Seq : string * string *
          ('p, 'ctx) parameter ->
    ('p list -> 'ctx -> unit tzresult Lwt.t, 'ctx) params

type (_, _) options =
    Argument : { spec : ('a, 'arg) args ;
                 converter : 'a -> 'b } -> ('b, 'arg) options

(* A command group *)
type group =
  { name : string ;
    title : string }

(* A command wraps a callback with its type and info *)
type 'arg command =
  | Command
    : { params : ('a, 'iarg) params ;
        options : ('b, 'iarg) options ;
        handler : 'b -> 'a ;
        desc : string ;
        group : group option ;
        conv : 'arg -> 'iarg }
    -> 'arg command

type error += Bad_argument of int * string
type error += Unterminated_command : string list * 'ctx command list -> error
type error += Command_not_found : string list * 'ctx command list -> error
type error += Unknown_option : string * 'ctx command option -> error
type error += Option_expected_argument : string * 'ctx command option -> error
type error += Bad_option_argument : string * 'ctx command option -> error
type error += Multiple_occurences : string * 'ctx command option -> error
type error += Extra_arguments : string list * 'ctx command -> error

let trim s = (* config-file wokaround *)
  TzString.split '\n' s |>
  List.map String.trim |>
  String.concat "\n"

let print_desc ppf doc =
  let short, long = try
      let len = String.index doc '\n' in
      String.sub doc 0 len,
      Some (String.sub doc (len + 1) (String.length doc - len - 1))
    with _ -> doc, None in
  match long with
  | None ->
      Format.fprintf ppf "%s" short
  | Some doc ->
      Format.fprintf ppf "%s@{<full>@\n  @[<hov 0>%a@]@}" short Format.pp_print_text doc

let print_label ppf = function
  | { long ; short = None } -> Format.fprintf ppf "--%s" long
  | { long ; short = Some short } -> Format.fprintf ppf "-%c --%s" short long

let print_options_detailed (type ctx) =
  let help_option : type a.Format.formatter -> (a, ctx) arg -> unit =
    fun ppf -> function
      | Arg { label ; placeholder ; doc ; _ } ->
          Format.fprintf ppf "@{<opt>%a <%s>@}: %a"
            print_label label placeholder
            print_desc doc ;
      | DefArg { label ; placeholder ; doc ; default ; _ } ->
          Format.fprintf ppf "@{<opt>%a <%s>@}: %a"
            print_label label placeholder
            print_desc (doc ^ "\nDefaults to `" ^ default ^ "`.")
      | Switch { label ; doc } ->
          Format.fprintf ppf "@{<opt>%a@}: %a"
            print_label label
            print_desc doc
      | Constant _ -> () in
  let rec help : type b. Format.formatter -> (b, ctx) args -> unit =
    fun ppf -> function
      | NoArgs -> ()
      | AddArg (arg, NoArgs) ->
          Format.fprintf ppf "%a"
            help_option arg
      | AddArg (arg, rest) ->
          Format.fprintf ppf "%a@,%a"
            help_option arg help rest
  in help

let has_args : type a ctx. (a, ctx) args -> bool = function
  | NoArgs -> false
  | AddArg (_,_) -> true

let print_options_brief (type ctx) =
  let help_option :
    type a. Format.formatter -> (a, ctx) arg -> unit =
    fun ppf -> function
      | DefArg { label ; placeholder ; _ } ->
          Format.fprintf ppf "[@{<opt>%a <%s>@}]"
            print_label label placeholder
      | Arg { label ; placeholder ; _ } ->
          Format.fprintf ppf "[@{<opt>%a <%s>@}]"
            print_label  label placeholder
      | Switch { label ; _ } ->
          Format.fprintf ppf "[@{<opt>%a@}]"
            print_label label
      | Constant _ -> ()
  in let rec help : type b. Format.formatter -> (b, ctx) args -> unit =
       fun ppf -> function
         | NoArgs -> ()
         | AddArg (arg, NoArgs) ->
             Format.fprintf ppf "%a" help_option arg
         | AddArg (arg, rest) ->
             Format.fprintf ppf "%a@ %a"
               help_option arg help rest
  in help

let print_highlight highlight_strings formatter str =
  let rec print_string = function
    | [] -> Format.fprintf formatter "%s" str
    | regex :: tl ->
        begin match Re.Str.full_split regex str with
          | []
          | [ Re.Str.Text _ ] -> print_string tl
          | list ->
              List.iter
                (function
                  | Re.Str.Text text -> Format.fprintf formatter "%s" text
                  | Re.Str.Delim delimiter ->
                      Format.fprintf formatter "@{<hilight>%s@}" delimiter)
                list
        end
  in print_string (List.map Re.Str.regexp_string highlight_strings)

let print_commandline ppf (highlights, options, args) =
  let rec print
    : type a ctx. Format.formatter -> (a, ctx) params -> unit =
    fun ppf -> function
      | Stop -> Format.fprintf ppf "%a" print_options_brief options
      | Seq (n, _, _) when not (has_args options) ->
          Format.fprintf ppf "[@{<arg>%s@}...]" n
      | Seq (n, _, _) ->
          Format.fprintf ppf "[@{<arg>%s@}...] %a" n print_options_brief options
      | Prefix (n, Stop) when not (has_args options) ->
          Format.fprintf ppf "@{<kwd>%a@}" (print_highlight highlights) n
      | Prefix (n, next) ->
          Format.fprintf ppf "@{<kwd>%a@} %a"
            (print_highlight highlights) n print next
      | Param (n, _, _, Stop) when not (has_args options) ->
          Format.fprintf ppf "@{<arg>%s@}" n
      | Param (n, _, _, next) ->
          Format.fprintf ppf "@{<arg>%s@} %a" n print next in
  Format.fprintf ppf "@{<commandline>%a@}" print args

let rec print_params_detailed
  : type a b ctx. (b, ctx) args -> Format.formatter -> (a, ctx) params -> unit
  = fun spec ppf -> function
    | Stop -> print_options_detailed ppf spec
    | Seq (n, desc, _) ->
        Format.fprintf ppf "@{<arg>%s@}: %a"
          n print_desc (trim desc) ;
        begin match spec with
          | NoArgs -> ()
          | _ -> Format.fprintf ppf "@,%a" print_options_detailed spec
        end
    | Prefix (_, next) ->
        print_params_detailed spec ppf next
    | Param (n, desc, _, Stop) ->
        Format.fprintf ppf "@{<arg>%s@}: %a"
          n print_desc (trim desc);
        begin match spec with
          | NoArgs -> ()
          | _ -> Format.fprintf ppf "@,%a" print_options_detailed spec
        end
    | Param (n, desc, _, next) ->
        Format.fprintf ppf "@{<arg>%s@}: %a@,%a"
          n print_desc (trim desc) (print_params_detailed spec) next

let contains_params_args :
  type arg ctx. (arg, ctx) params -> (_, ctx) args -> bool
  = fun params args ->
    let rec help : (arg, ctx) params -> bool = function
      | Stop -> has_args args
      | Seq (_, _, _) -> true
      | Prefix (_, next) -> help next
      | Param (_, _, _, _) -> true
    in help params

let print_command :
  type ctx.
  ?prefix:(Format.formatter -> unit -> unit) ->
  ?highlights:string list -> Format.formatter -> ctx command -> unit
  = fun
    ?(prefix = (fun _ () -> ()))
    ?(highlights=[]) ppf
    (Command { params ; desc ; options = Argument { spec ; _ } ; _ }) ->
    if contains_params_args params spec
    then
      Format.fprintf ppf "@{<command>%a%a@{<short>@,@{<commanddoc>%a@,%a@}@}@}"
        prefix ()
        print_commandline (highlights, spec, params)
        print_desc desc
        (print_params_detailed spec) params
    else
      Format.fprintf ppf "@{<command>%a%a@{<short>@,@{<commanddoc>%a@}@}@}"
        prefix ()
        print_commandline (highlights, spec, params)
        print_desc desc

type ex_command = Ex : _ command -> ex_command

let group_commands commands =
  let (grouped, ungrouped) =
    List.fold_left
      (fun (grouped, ungrouped) (Ex (Command { group ; _ }) as command) ->
         match group with
         | None -> (grouped, command :: ungrouped)
         | Some group ->
             match
               List.find_opt (fun ({ name ; _ }, _) -> group.name = name) grouped with
             | None -> ((group, ref [ command ]) :: grouped, ungrouped)
             | Some ({ title ; _ }, r) ->
                 if title <> group.title then
                   invalid_arg "Clic.usage: duplicate group name" ;
                 r := command :: !r ;
                 (grouped, ungrouped))
      ([], [])
      commands in
  List.map (fun (g, c) -> (g, List.rev !c))
    (match ungrouped with
     | [] -> grouped
     | l -> (grouped @
             [ { name = "misc" ;
                 title = "Miscellaneous commands" },
               ref l ]))

let print_group print_command ppf ({ title ; _ }, commands) =
  Format.fprintf ppf "@{<title>%s@}@,@{<list>%a@}"
    title
    (Format.pp_print_list print_command) commands

type formatter_state =
  Format.formatter_out_functions * Format.formatter_tag_functions * bool

type format = Plain | Ansi | Html
type verbosity = Terse | Short | Details | Full

let setup_formatter ppf format verbosity =
  let skip = ref false in
  let orig_out_functions, _, _ as orig_state =
    Format.pp_get_formatter_out_functions ppf (),
    Format.pp_get_formatter_tag_functions ppf (),
    Format.pp_get_print_tags ppf () in
  begin
    Format.pp_print_flush ppf () ;
    Format.pp_set_formatter_out_functions ppf
      { out_string =
          (fun s b a ->
             if s = "\000\000\000" then skip := true
             else if s = "\255\255\255" then skip := false
             else if not !skip then orig_out_functions.out_string s b a) ;
        out_spaces = (fun n -> if not !skip then orig_out_functions.out_spaces n) ;
        out_newline = (fun () -> if not !skip then orig_out_functions.out_newline ()) ;
        out_flush = (fun () -> if not !skip then orig_out_functions.out_flush ()) ;
        out_indent = orig_out_functions.out_indent } ;
    let levels = ref [] in
    let setup_level (level, op) =
      if op level verbosity then
        Format.fprintf ppf "@<0>%s" "\255\255\255"
      else Format.fprintf ppf "@<0>%s" "\000\000\000" in
    let push_level level =
      levels := level :: !levels ;
      setup_level level in
    let pop_level () =
      match !levels with
      | _ :: level :: rest -> levels := level :: rest ; setup_level level
      | [ _ ] | [] -> Pervasives.failwith "Clic: unclosed verbosity tag" in
    push_level (Terse, (<=)) ;
    let push_level_tag tag =
      let push op = function
        | "full" -> push_level (Full, op)
        | "details" -> push_level (Details, op)
        | "short" -> push_level (Short, op)
        | "terse" -> push_level (Terse, op)
        | tag -> Pervasives.failwith ("Clic: invalid semantic tag <" ^ tag ^ ">") in
      if String.length tag > 0 && String.get tag 0 = '=' then
        push (=) (String.sub tag 1 (String.length tag - 1))
      else if String.length tag > 0 && String.get tag 0 = '-' then
        push (>) (String.sub tag 1 (String.length tag - 1))
      else push (<=) tag in
    let pop_level_tag = function
      | "full" | "details" | "short" | "terse"
      | "-full" | "-details" | "-short" | "-terse"
      | "=full" | "=details" | "=short" | "=terse" -> pop_level ()
      | tag -> Pervasives.failwith ("Clic: invalid semantic tag <" ^ tag ^ ">") in
    match format with
    | Ansi ->
        let color_num = function
          | `Auto -> None
          | `Black -> Some 0
          | `Red -> Some 1
          | `Green -> Some 2
          | `Yellow -> Some 3
          | `Blue -> Some 4
          | `Magenta -> Some 5
          | `Cyan -> Some 6
          | `White -> Some 7 in
        let ansi_format ppf (fg, bg, b, u) =
          Format.fprintf ppf "@<0>%s" "\027[0m" ;
          match
            (match color_num fg with Some n -> [ string_of_int (30 + n) ] | None -> []) @
            (match color_num bg with Some n -> [ string_of_int (40 + n) ] | None -> []) @
            (if b then [ "1" ] else []) @
            (if u then [ "4" ] else [])
          with
          | [] -> ()
          | l -> Format.fprintf ppf "@<0>%s" ("\027[" ^ String.concat ";" l ^ "m") in
        let ansi_stack = ref [ (`Auto, `Auto, false, false) ] in
        let push_ansi_format (fg, bg, b, u) =
          let format = match !ansi_stack with
            | (pfg, pbg, pb, pu) :: _ ->
                (Option.unopt ~default: pfg fg,
                 Option.unopt ~default: pbg bg,
                 pb || b,
                 pu || u)
            | [] -> assert false in
          ansi_stack := format :: !ansi_stack ;
          Format.fprintf ppf "@<0>%a" ansi_format format in
        let pop_ansi_format () =
          Format.fprintf ppf "@<0>%s" "\027[0m" ;
          match !ansi_stack with
          | _ :: format :: rest ->
              ansi_stack := format :: rest ;
              Format.fprintf ppf "@<0>%a" ansi_format format
          | [ _ ] | [] -> Pervasives.failwith "Clic: unclosed ansi format" in
        Format.pp_set_formatter_tag_functions ppf
          { mark_open_tag = (fun _ -> "") ;
            mark_close_tag = (fun _ -> "") ;
            print_open_tag = begin function
              | "title" -> push_ansi_format (None, None, true, true)
              | "commandline" -> Format.fprintf ppf "@[<hov 4>"
              | "commanddoc" -> Format.fprintf ppf "  @[<v 0>"
              | "opt" -> push_ansi_format (Some `Green, None, false, false)
              | "arg" -> push_ansi_format (Some `Yellow, None, false, false) ; Format.fprintf ppf "<"
              | "kwd" -> push_ansi_format (None, None, false, true)
              | "error" -> push_ansi_format (Some `Red, None, true, true)
              | "warning" -> push_ansi_format (Some `Yellow, None, true, true)
              | "hilight" -> push_ansi_format (Some `White, Some `Yellow, true, true)
              | "list" -> Format.fprintf ppf "  @[<v 0>"
              | "command" -> Format.fprintf ppf "@[<v 0>"
              | "document" -> Format.fprintf ppf "@[<v 0>"
              | other -> push_level_tag other
            end ;
            print_close_tag = begin function
              | "title" -> Format.fprintf ppf ":" ; pop_ansi_format ()
              | "commandline" -> Format.fprintf ppf "@]"
              | "commanddoc" -> Format.fprintf ppf "@]"
              | "opt" -> pop_ansi_format ()
              | "arg" -> Format.fprintf ppf ">" ; pop_ansi_format ()
              | "kwd" -> pop_ansi_format ()
              | "error" -> pop_ansi_format ()
              | "warning" -> pop_ansi_format ()
              | "hilight" -> pop_ansi_format ()
              | "command" | "list" -> Format.fprintf ppf "@]"
              | "document" -> Format.fprintf ppf "@]"
              | other -> pop_level_tag other
            end } ;
        Format.pp_set_print_tags ppf true
    | Plain ->
        Format.pp_set_formatter_tag_functions ppf
          { mark_open_tag = (fun _ -> "") ;
            mark_close_tag = (fun _ -> "") ;
            print_open_tag = begin function
              | "title" -> ()
              | "commandline" -> Format.fprintf ppf "@[<hov 4>"
              | "commanddoc" -> Format.fprintf ppf "  @[<v 0>"
              | "opt" -> ()
              | "arg" -> Format.fprintf ppf "<"
              | "kwd" -> ()
              | "hilight" -> ()
              | "error" -> ()
              | "warning" -> ()
              | "list" -> Format.fprintf ppf "  @[<v 0>"
              | "command" -> Format.fprintf ppf "@[<v 0>"
              | "document" -> Format.fprintf ppf "@[<v 0>"
              | other -> push_level_tag other
            end ;
            print_close_tag = begin function
              | "title" -> Format.fprintf ppf ":"
              | "commandline" -> Format.fprintf ppf "@]"
              | "commanddoc" -> Format.fprintf ppf "@]"
              | "opt" -> ()
              | "arg" -> Format.fprintf ppf ">"
              | "kwd" -> ()
              | "error" -> ()
              | "warning" -> ()
              | "hilight" -> ()
              | "command" | "list" -> Format.fprintf ppf "@]"
              | "document" -> Format.fprintf ppf "@]"
              | other -> pop_level_tag other
            end } ;
        Format.pp_set_print_tags ppf true
    | Html ->
        Format.pp_set_formatter_tag_functions ppf
          { mark_open_tag = (fun _ -> "") ;
            mark_close_tag = (fun _ -> "") ;
            print_open_tag = begin function
              | "title" -> Format.fprintf ppf "\003h3\004"
              | "commandline" -> Format.fprintf ppf "\003div class='cmdline'\004@[<h>"
              | "commanddoc" -> Format.fprintf ppf "\003div class='cmddoc'\004"
              | "opt" -> Format.fprintf ppf "\003span class='opt'\004"
              | "arg" -> Format.fprintf ppf "\003span class='arg'\004"
              | "kwd" -> Format.fprintf ppf "\003span class='kwd'\004"
              | "hilight" -> ()
              | "error" -> ()
              | "warning" -> ()
              | "list" -> Format.fprintf ppf "\003ul\004@\n"
              | "command" -> Format.fprintf ppf "\003li\004@\n"
              | "document" ->
                  Format.fprintf ppf
                    "@[<v 0>\003style\004\
                     .cmdline { font-family: monospace }\
                     .cmddoc { white-space: pre-wrap ; font-family: monospace; line-height: 170%%; margin: 0 0 20px 0 }\
                     .cmdline { background: #343131; padding: 2px 8px;	border-radius:10px; color: white; margin: 5px; }\
                     .cmdline+.cmddoc { margin: -5px 5px 0 20px; padding: 5px }\
                     .opt,.arg { background: #343131; font-weight: bold;  padding: 2px 4px; border-radius:5px; }\
                     .kwd { font-weight: bold; } .opt { color:#CF0; background: #460; } .arg { color: #CEF; background: #369; }\
                     \003/style\004@\n" ;
              | other -> push_level_tag other
            end ;
            print_close_tag = begin function
              | "title" -> Format.fprintf ppf "\003/h3\004@\n"
              | "commandline" -> Format.fprintf ppf "@]\003/div\004@\n"
              | "commanddoc" -> Format.fprintf ppf "\003/div\004@\n"
              | "opt" | "arg" | "kwd" -> Format.fprintf ppf "\003/span\004"
              | "error" | "warning" | "hilight" -> ()
              | "list" -> Format.fprintf ppf "\003/ul\004@\n"
              | "command" -> Format.fprintf ppf "\003/li\004@\n"
              | "document" -> Format.fprintf ppf "@]"
              | other -> pop_level_tag other
            end } ;
        let orig_out_functions =
          Format.pp_get_formatter_out_functions ppf () in
        Format.pp_set_formatter_out_functions ppf
          { orig_out_functions with
            out_string = (fun s i j ->
                let buf = Buffer.create (j - i) in
                for n = i to j - 1 do match String.get s n with
                  | '\003' -> Buffer.add_char buf '<'
                  | '\004' -> Buffer.add_char buf '>'
                  | '>' -> Buffer.add_string buf "&gt;"
                  | '<' -> Buffer.add_string buf "&lt;"
                  | c -> Buffer.add_char buf c
                done ;
                let s' = Buffer.contents buf in
                orig_out_functions.out_string s' 0 (String.length s'))} ;
        Format.pp_set_print_tags ppf true
  end ;
  orig_state

let restore_formatter ppf (out_functions, tag_functions, tags) =
  Format.pp_print_flush ppf () ;
  Format.pp_set_formatter_out_functions ppf out_functions ;
  Format.pp_set_formatter_tag_functions ppf tag_functions ;
  Format.pp_set_print_tags ppf tags

let usage_internal ppf ~executable_name ~global_options ?(highlights=[]) commands =
  let by_group = group_commands commands in
  let (Argument { spec ; _ }) = global_options in
  let print_groups =
    Format.pp_print_list
      ~pp_sep: (fun ppf () -> Format.fprintf ppf "@,@,")
      (print_group (fun ppf (Ex command) -> print_command ?prefix:None ~highlights ppf command)) in
  Format.fprintf ppf
    "@{<document>@{<title>Usage@}@,\
     @{<list>\
     @{<command>@{<commandline>\
     %s [@{<opt>global options@}] @{<kwd>command@} [@{<opt>command options@}]@}@}@,\
     @{<command>@{<commandline>\
     %s @{<opt>--help@} (for global options)@}@}@,\
     @{<command>@{<commandline>\
     %s [@{<opt>global options@}] @{<kwd>command@} @{<opt>--help@} (for command options)@}@}\
     @}@,@,\
     @{<title>To browse the documentation@}@,\
     @{<list>\
     @{<command>@{<commandline>\
     %s [@{<opt>global options@}] @{<kwd>man@} (for a list of commands)@}@}@,\
     @{<command>@{<commandline>\
     %s [@{<opt>global options@}] @{<kwd>man@} @{<opt>-v 3@} (for the full manual)@}@}\
     @}@,@,\
     @{<title>Global options (must come before the command)@}@,\
     @{<commanddoc>%a@}%a\
     %a@}@."
    executable_name executable_name executable_name executable_name executable_name
    print_options_detailed spec
    (fun ppf () -> if by_group <> [] then Format.fprintf ppf "@,@,") ()
    print_groups by_group

let constant c = Constant c

let arg ~doc ?short ~long ~placeholder kind =
  Arg { doc ;
        label = { long ; short } ;
        placeholder ;
        kind }

let default_arg ~doc ?short ~long ~placeholder ~default kind =
  DefArg { doc ;
           placeholder ;
           label = { long ; short } ;
           kind ;
           default }

let switch ~doc ?short ~long () =
  Switch { doc ; label = { long ; short } }

let parse_arg :
  type a ctx. ?command:_ command -> (a, ctx) arg -> string list TzString.Map.t -> ctx -> a tzresult Lwt.t =
  fun ?command spec args_dict ctx ->
    match spec with
    | Arg { label = { long ; short = _ } ; kind = { converter ; _  } ; _ } ->
        begin match TzString.Map.find_opt long args_dict with
          | None
          | Some [] -> return_none
          | Some [ s ] ->
              (trace
                 (Bad_option_argument ("--" ^ long, command))
                 (converter ctx s)) >>|? fun x ->
              Some x
          | Some (_ :: _) ->
              fail (Multiple_occurences ("--" ^ long, command))
        end
    | DefArg { label = { long ; short = _ } ; kind = { converter ; _ } ; default ; _ } ->
        converter ctx default >>= fun default ->
        begin match default with
          | Ok x -> return x
          | Error _ ->
              invalid_arg
                (Format.sprintf
                   "Value provided as default for '%s' could not be parsed by converter function."
                   long) end >>=? fun default ->
        begin match TzString.Map.find_opt long args_dict with
          | None
          | Some [] -> return default
          | Some [ s ] ->
              (trace
                 (Bad_option_argument (long, command))
                 (converter ctx s))
          | Some (_ :: _) ->
              fail (Multiple_occurences (long, command))
        end
    | Switch { label = { long ; short = _ } ; _ } ->
        begin match TzString.Map.find_opt long args_dict with
          | None
          | Some [] -> return_false
          | Some [ _ ] -> return_true
          | Some (_ :: _) -> fail (Multiple_occurences (long, command))
        end
    | Constant c -> return c

(* Argument parsing *)
let rec parse_args :
  type a ctx. ?command:_ command -> (a, ctx) args -> string list TzString.Map.t -> ctx -> a tzresult Lwt.t =
  fun ?command spec args_dict ctx ->
    match spec with
    | NoArgs -> return_unit
    | AddArg (arg, rest) ->
        parse_arg ?command arg args_dict ctx >>=? fun arg ->
        parse_args ?command rest args_dict ctx >>|? fun rest ->
        (arg, rest)

let empty_args_dict = TzString.Map.empty

let rec make_arities_dict :
  type a b. (a, b) args -> (int * string) TzString.Map.t -> (int * string) TzString.Map.t =
  fun args acc -> match args with
    | NoArgs -> acc
    | AddArg (arg, rest) ->
        let recur { long ; short } num =
          (match short with
           | None -> acc
           | Some c -> TzString.Map.add ("-" ^ String.make 1 c) (num, long) acc) |>
          TzString.Map.add ("-" ^ long) (num, long) |>
          TzString.Map.add ("--" ^ long) (num, long) |>
          make_arities_dict rest in
        match arg with
        | Arg { label ; _ } -> recur label 1
        | DefArg { label ; _ } -> recur label 1
        | Switch { label ; _ } -> recur label 0
        | Constant _c -> make_arities_dict rest acc

type error += Help : 'a command option -> error

let check_help_flag ?command = function
  | ("-h" | "--help") :: _ -> fail (Help command)
  | _ -> return_unit

let add_occurrence long value acc =
  match TzString.Map.find_opt long acc with
  | Some v -> TzString.Map.add long v acc
  | None -> TzString.Map.add long [ value ] acc

let make_args_dict_consume ?command spec args =
  let rec make_args_dict completing arities acc args =
    check_help_flag ?command args >>=? fun () ->
    match args with
    | [] -> return (acc, [])
    | arg :: tl ->
        if String.length arg > 0 && String.get arg 0 = '-' then
          if TzString.Map.mem arg arities then
            let arity, long = TzString.Map.find arg arities in
            check_help_flag ?command tl >>=? fun () ->
            match arity, tl with
            | 0, tl' ->
                make_args_dict completing arities (add_occurrence long "" acc) tl'
            | 1, value :: tl' ->
                make_args_dict completing arities (add_occurrence long value acc) tl'
            | 1, [] when completing ->
                return (acc, [])
            | 1, [] ->
                fail (Option_expected_argument (arg, None))
            | _, _ ->
                raise (Failure "cli_entries: Arguments with arity not equal to 1 or 0 not supported")
          else
            fail (Unknown_option (arg, None))
        else return (acc, args)
  in make_args_dict false (make_arities_dict spec TzString.Map.empty) TzString.Map.empty args

let make_args_dict_filter ?command spec args =
  let rec make_args_dict arities (dict, other_args) args =
    check_help_flag ?command args >>=? fun () ->
    match args with
    | [] -> return (dict, other_args)
    | arg :: tl ->
        if TzString.Map.mem arg arities
        then let arity, long = TzString.Map.find arg arities in
          check_help_flag ?command tl >>=? fun () ->
          match arity, tl with
          | 0, tl -> make_args_dict arities (add_occurrence long "" dict, other_args) tl
          | 1, value :: tl' -> make_args_dict arities (add_occurrence long value dict, other_args) tl'
          | 1, [] -> fail (Option_expected_argument (arg, command))
          | _, _ ->
              raise (Failure "cli_entries: Arguments with arity not equal to 1 or 0 not supported")
        else make_args_dict arities (dict, arg :: other_args) tl
  in make_args_dict
    (make_arities_dict spec TzString.Map.empty)
    (TzString.Map.empty, [])
    args >>|? fun (dict, remaining) ->
  (dict, List.rev remaining)

let (>>) arg1 arg2 = AddArg (arg1, arg2)
let args1 spec =
  Argument { spec = spec >> NoArgs;
             converter = fun (arg, ()) -> arg }
let args2 spec1 spec2 =
  Argument { spec = spec1 >> (spec2 >> NoArgs) ;
             converter = fun (arg1, (arg2, ())) -> arg1, arg2 }
let args3 spec1 spec2 spec3 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> NoArgs)) ;
             converter = fun (arg1, (arg2, (arg3, ()))) -> arg1, arg2, arg3 }
let args4 spec1 spec2 spec3 spec4 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> NoArgs))) ;
             converter = fun (arg1, (arg2, (arg3, (arg4, ())))) -> arg1, arg2, arg3, arg4 }
let args5 spec1 spec2 spec3 spec4 spec5 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> NoArgs)))) ;
             converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, ()))))) -> arg1, arg2, arg3, arg4, arg5 }
let args6 spec1 spec2 spec3 spec4 spec5 spec6 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> NoArgs))))) ;
             converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, ())))))) ->
               arg1, arg2, arg3, arg4, arg5, spec6 }
let args7 spec1 spec2 spec3 spec4 spec5 spec6 spec7 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> NoArgs)))))) ;
             converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, ()))))))) ->
               arg1, arg2, arg3, arg4, arg5, spec6, spec7 }
let args8 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 =
  Argument { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> NoArgs))))))) ;
             converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, ())))))))) ->
               arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8 }
let args9 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> NoArgs)))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, ()))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9 }
let args10 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> NoArgs))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, ())))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10 }
let args11 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> NoArgs)))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, ()))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11 }

let args12 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 spec12 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (spec12 >> NoArgs))))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, (spec12, ())))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11, spec12 }

let args13 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 spec12 spec13 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (spec12 >> (spec13 >> NoArgs)))))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, (spec12, (spec13, ()))))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11, spec12, spec13 }

let args14 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 spec12 spec13 spec14 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (spec12 >> (spec13 >> (spec14 >> NoArgs))))))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, (spec12, (spec13, (spec14, ())))))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11, spec12, spec13, spec14 }

let args15 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 spec12 spec13 spec14 spec15 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (spec12 >> (spec13 >> (spec14 >> (spec15 >> NoArgs)))))))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, (spec12, (spec13, (spec14, (spec15, ()))))))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11, spec12, spec13, spec14, spec15 }

let args16 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10 spec11 spec12 spec13 spec14 spec15 spec16 =
  Argument
    { spec = spec1 >> (spec2 >> (spec3 >> (spec4 >> (spec5 >> (spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (spec12 >> (spec13 >> (spec14 >> (spec15 >> (spec16 >> NoArgs))))))))))))))) ;
      converter = fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (spec8, (spec9, (spec10, (spec11, (spec12, (spec13, (spec14, (spec15, (spec16, ())))))))))))))))) ->
        arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10, spec11, spec12, spec13, spec14, spec15, spec16 }

let args17 spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 spec9 spec10
    spec11 spec12 spec13 spec14 spec15 spec16 spec17 =
  Argument
    { spec = spec1 >>
        (spec2 >> (spec3 >> (spec4 >> (spec5 >> (
             spec6 >> (spec7 >> (spec8 >> (spec9 >> (spec10 >> (spec11 >> (
                 spec12 >> (spec13 >> (spec14 >> (spec15 >> (
                     spec16 >> (spec17 >> NoArgs)))))))))))))))) ;
      converter =
        fun (arg1, (arg2, (arg3, (arg4, (arg5, (spec6, (spec7, (
            spec8, (spec9, (spec10, (spec11, (spec12, (spec13, (
                spec14, (spec15, (spec16, (spec17, ()))))))))))))))))) ->
          arg1, arg2, arg3, arg4, arg5, spec6, spec7, spec8, spec9, spec10,
          spec11, spec12, spec13, spec14, spec15, spec16, spec17 }

(* Some combinators for writing commands concisely. *)
let param ~name ~desc kind next = Param (name, desc, kind, next)
let seq_of_param param =
  match param Stop with
  | Param (n, desc, parameter, Stop) -> Seq (n, desc, parameter)
  | _ -> invalid_arg "Clic.seq_of_param"

let prefix keyword next = Prefix (keyword, next)
let rec fixed =
  function [] -> Stop | n :: r -> Prefix (n, fixed r)
let rec prefixes p next =
  match p with [] -> next | n :: r -> Prefix (n, prefixes r next)
let stop = Stop
let no_options = Argument { spec=NoArgs ; converter=fun () -> () }
let command ?group ~desc options params handler =
  Command { params ; options ; handler ; desc ; group ; conv = (fun x -> x) }

(* Param combinators *)
let string ~name ~desc next =
  param ~name ~desc { converter=(fun _ s -> return s) ; autocomplete=None } next

let string_contains ~needle ~haystack =
  try
    Some (Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0)
  with Not_found ->
    None

let rec search_params_prefix : type a arg. string -> (a, arg) params -> bool =
  fun prefix -> function
    | Prefix (keyword, next) ->
        begin
          match string_contains ~needle:prefix ~haystack:keyword with
          | None -> search_params_prefix prefix next
          | Some _ -> true
        end
    | Param (_, _, _, next) -> search_params_prefix prefix next
    | Stop -> false
    | Seq _ -> false

let search_command keyword (Command { params ; _ }) =
  search_params_prefix keyword params


(* Command execution *)
let exec
    (type ctx)
    (Command { options = (Argument { converter ; spec = options_spec }) ;
               params = spec ; handler ; conv ; _ } as command)
    (ctx : ctx) params args_dict =
  let rec exec
    : type ctx a. int -> ctx -> (a, ctx) params -> a -> string list -> unit tzresult Lwt.t
    = fun i ctx spec cb params ->
      match spec, params with
      | Stop, _ -> cb ctx
      | Seq (_, _, { converter ; _ }), seq ->
          let rec do_seq i acc = function
            | [] -> return (List.rev acc)
            | p :: rest ->
                Lwt.catch
                  (fun () -> converter ctx p)
                  (function
                    | Failure msg -> Error_monad.failwith "%s" msg
                    | exn -> fail (Exn exn))
                |> trace (Bad_argument (i, p)) >>=? fun v ->
                do_seq (succ i) (v :: acc) rest in
          do_seq i [] seq >>=? fun parsed ->
          cb parsed ctx
      | Prefix (n, next), p :: rest when n = p ->
          exec (succ i) ctx next cb rest
      | Param (_, _, { converter ; _ }, next), p :: rest ->
          Lwt.catch
            (fun () -> converter ctx p)
            (function
              | Failure msg -> Error_monad.failwith "%s" msg
              | exn -> fail (Exn exn))
          |> trace (Bad_argument (i, p)) >>=? fun v ->
          exec (succ i) ctx next (cb v) rest
      | _ -> raise (Failure ("cli_entries internal error: exec no case matched"))
  in
  let ctx = conv ctx in
  parse_args ~command options_spec args_dict ctx >>=? fun parsed_options ->
  exec 1 ctx spec (handler (converter parsed_options)) params

[@@@ocaml.warning "-30"]

(* Command dispatch tree *)
type 'arg level =
  { stop : ('arg) command option ;
    prefix : (string * 'arg tree) list }
and 'arg param_level =
  { stop : 'arg command option ;
    autocomplete : ('arg -> string list tzresult Lwt.t) option ;
    tree : 'arg tree }
and 'ctx tree =
  | TPrefix : 'ctx level -> 'ctx tree
  | TParam : 'ctx param_level -> 'ctx tree
  | TStop : 'ctx command -> 'ctx tree
  | TSeq : 'ctx command * ('ctx -> string list tzresult Lwt.t) option -> 'ctx tree
  | TEmpty : 'ctx tree

let has_options : type ctx. ctx command -> bool =
  fun (Command { options = Argument { spec ; _ } ; _ }) ->
    let args_help : type a ctx. (a, ctx) args -> bool = function
      | NoArgs -> false
      | AddArg (_, _) -> true
    in args_help spec

let insert_in_dispatch_tree :
  type ctx. ctx tree -> ctx command -> ctx tree =
  fun root (Command { params ; conv ; _ } as command) ->
    let access_autocomplete :
      type p ctx. (p, ctx) parameter -> (ctx -> string list tzresult Lwt.t) option =
      fun { autocomplete ; _ } -> autocomplete in
    let rec insert_tree
      : type a ictx.
        (ctx -> ictx) ->
        ctx tree -> (a, ictx) params -> ctx tree
      = fun conv t c ->
        let insert_tree t c = insert_tree conv t c in
        match t, c with
        | TEmpty, Stop -> TStop command
        | TEmpty, Seq (_, _, { autocomplete ; _ }) ->
            TSeq (command,
                  Option.map autocomplete ~f:(fun a c -> a (conv c)))
        | TEmpty, Param (_, _, param, next) ->
            let autocomplete = access_autocomplete param in
            let autocomplete = Option.map autocomplete ~f:(fun a c -> a (conv c)) in
            TParam { tree = insert_tree TEmpty next ; stop = None ; autocomplete}
        | TEmpty, Prefix (n, next) ->
            TPrefix { stop = None ; prefix = [ (n, insert_tree TEmpty next) ] }
        | TStop cmd, Param (_, _, param, next) ->
            let autocomplete = access_autocomplete param in
            let autocomplete = Option.map autocomplete ~f:(fun a c -> a (conv c)) in
            if not (has_options cmd)
            then TParam { tree = insert_tree TEmpty next ;
                          stop = Some cmd ;
                          autocomplete }
            else raise (Failure "Command cannot have both prefix and options")
        | TStop cmd, Prefix (n, next) ->
            TPrefix { stop = Some cmd ;
                      prefix = [ (n, insert_tree TEmpty next) ] }
        | TParam t, Param (_, _, _, next) ->
            TParam { t with tree = insert_tree t.tree next }
        | TPrefix ({ prefix ; _ } as l), Prefix (n, next) ->
            let rec insert_prefix = function
              | [] -> [ (n, insert_tree TEmpty next) ]
              | (n', t) :: rest when n = n' -> (n, insert_tree t next) :: rest
              | item :: rest -> item :: insert_prefix rest in
            TPrefix { l with prefix = insert_prefix prefix }
        | TPrefix ({ stop = None ; _ } as l), Stop ->
            TPrefix { l with stop = Some command }
        | TParam ({ stop = None ; _ } as l), Stop ->
            TParam { l with stop = Some command }
        | _, _ ->
            Pervasives.failwith
              "Clic.Command_tree.insert: conflicting commands" in
    insert_tree conv root params


let make_dispatch_tree commands =
  List.fold_left insert_in_dispatch_tree TEmpty commands

let rec gather_commands ?(acc=[]) tree =
  match tree with
  | TEmpty -> acc
  | TSeq (c, _)
  | TStop c -> c :: acc
  | TPrefix { stop ; prefix } ->
      gather_assoc ~acc:(match stop with
          | None -> acc
          | Some c -> c :: acc)
        prefix
  | TParam { tree ; stop ; _ } ->
      gather_commands tree
        ~acc:(match stop with
            | None -> acc
            | Some c -> c :: acc)

and gather_assoc ?(acc=[]) trees =
  List.fold_left (fun acc (_, tree) -> gather_commands tree ~acc) acc trees

let find_command tree initial_arguments =
  let rec traverse tree arguments acc =
    match tree, arguments with
    | (TStop _ | TSeq _
      | TPrefix { stop = Some _ ; _ }
      | TParam { stop = Some _ ; _}), ("-h" | "--help") :: _ ->
        begin match gather_commands tree with
          | [] -> assert false
          | [ command ] -> fail (Help (Some command))
          | more -> fail (Unterminated_command (initial_arguments, more))
        end
    | TStop c, [] -> return (c, empty_args_dict, initial_arguments)
    | TStop (Command { options = Argument { spec ; _ } ; _ } as command), remaining ->
        make_args_dict_filter ~command spec remaining >>=? fun (args_dict, unparsed) ->
        begin match unparsed with
          | [] -> return (command, args_dict, initial_arguments)
          | hd :: _ ->
              if String.length hd > 0 && String.get hd 0 = '-' then
                fail (Unknown_option (hd, Some command))
              else
                fail (Extra_arguments (unparsed, command))
        end
    | TSeq (Command { options = Argument { spec ; _ } ; _ } as command, _), remaining ->
        if List.exists (function "-h" | "--help" -> true | _ -> false) remaining then
          fail (Help (Some command))
        else
          make_args_dict_filter ~command spec remaining >>|? fun (dict, remaining) ->
          (command, dict, List.rev_append acc remaining)
    | TPrefix { stop = Some cmd ; _ }, [] ->
        return (cmd, empty_args_dict, initial_arguments)
    | TPrefix { stop = None ; prefix }, ([] | ("-h" | "--help") :: _) ->
        fail (Unterminated_command (initial_arguments, gather_assoc prefix))
    | TPrefix { prefix ; _ }, hd_arg :: tl ->
        begin
          try
            return (List.assoc hd_arg prefix)
          with Not_found -> fail (Command_not_found (List.rev acc, gather_assoc prefix))
        end >>=? fun tree' ->
        traverse tree' tl (hd_arg :: acc)
    | TParam { stop = None ; _ }, ([] | ("-h" | "--help") :: _) ->
        fail (Unterminated_command (initial_arguments, gather_commands tree))
    | TParam { stop = Some c ; _ }, [] ->
        return (c, empty_args_dict, initial_arguments)
    | TParam { tree ; _ }, parameter :: arguments' ->
        traverse tree arguments' (parameter :: acc)
    | TEmpty, _ ->
        fail (Command_not_found (List.rev acc, []))
  in traverse tree initial_arguments []


let get_arg_label (type a) (arg : (a, _) arg) =
  match arg with
  | Arg { label ; _ } -> label
  | DefArg { label ; _ } -> label
  | Switch { label ; _ } -> label
  | Constant _ -> assert false

let get_arg
  : type a ctx. (a, ctx) arg -> string list
  = fun arg ->
    let { long ; short } = get_arg_label arg in
    ("--" ^ long) :: match short with None -> [] | Some c -> [ "-" ^ String.make 1 c ]

let rec list_args : type arg ctx. (arg, ctx) args -> string list = function
  | NoArgs -> []
  | AddArg (Constant _, args) -> list_args args
  | AddArg (arg, args) -> get_arg arg @ list_args args

let complete_func autocomplete cctxt =
  match autocomplete with
  | None -> return_nil
  | Some autocomplete -> autocomplete cctxt

let list_command_args (Command { options = Argument { spec ; _ } ; _ }) =
  list_args spec

let complete_arg : type a ctx. ctx -> (a, ctx) arg -> string list tzresult Lwt.t =
  fun ctx -> function
    | Arg { kind = { autocomplete ; _ } ; _ } -> complete_func autocomplete ctx
    | DefArg { kind = { autocomplete ; _ } ; _ } -> complete_func autocomplete ctx
    | Switch _ -> return_nil
    | Constant _ -> return_nil

let rec remaining_spec :
  type a ctx. TzString.Set.t -> (a, ctx) args -> string list =
  fun seen -> function
    | NoArgs -> []
    | AddArg (Constant _, rest) ->
        remaining_spec seen rest
    | AddArg (arg, rest) ->
        let { long ; _ } = get_arg_label arg in
        if TzString.Set.mem long seen
        then remaining_spec seen rest
        else get_arg arg @ remaining_spec seen rest

let complete_options (type ctx) continuation args args_spec ind (ctx : ctx) =
  let arities = make_arities_dict args_spec TzString.Map.empty in
  let rec complete_spec : type a. string -> (a, ctx) args -> string list tzresult Lwt.t =
    fun name -> function
      | NoArgs -> return_nil
      | AddArg (Constant _, rest) ->
          complete_spec name rest
      | AddArg (arg, rest) ->
          if (get_arg_label arg).long = name
          then complete_arg ctx arg
          else complete_spec name rest in
  let rec help args ind seen =
    match args with
    | _ when ind = 0 ->
        continuation args 0 >>|? fun cont_args ->
        cont_args @ remaining_spec seen args_spec
    | [] ->
        Pervasives.failwith
          "cli_entries internal autocomplete error"
    | arg :: tl ->
        if TzString.Map.mem arg arities
        then
          let arity, long = TzString.Map.find arg arities in
          let seen = TzString.Set.add long seen in
          match arity, tl with
          | 0, args when ind = 0 ->
              continuation args 0 >>|? fun cont_args ->
              remaining_spec seen args_spec @ cont_args
          | 0, args -> help args (ind - 1) seen
          | 1, _ when ind = 1 -> complete_spec arg args_spec
          | 1, _ :: tl -> help tl (ind - 2) seen
          | _ -> Pervasives.failwith "cli_entries internal error, invalid arity"
        else continuation args ind
  in help args ind TzString.Set.empty

let complete_next_tree cctxt = function
  | TPrefix { stop; prefix } ->
      return
        ((match stop with
            | None -> []
            | Some command -> list_command_args command)
         @ (List.map fst prefix))
  | TSeq (command, autocomplete) ->
      complete_func autocomplete cctxt >>|? fun completions ->
      completions @ (list_command_args command)
  | TParam { autocomplete ; _ } ->
      complete_func autocomplete cctxt
  | TStop command -> return (list_command_args command)
  | TEmpty -> return_nil

let complete_tree cctxt tree index args =
  let rec help tree args ind =
    if ind = 0
    then complete_next_tree cctxt tree
    else
      match tree, args with
      | TSeq _, _ -> complete_next_tree cctxt tree
      | TPrefix { prefix ; _ }, hd :: tl ->
          begin
            try help (List.assoc hd prefix) tl (ind - 1)
            with Not_found -> return_nil
          end
      | TParam { tree ; _ }, _ :: tl ->
          help tree tl (ind - 1)
      | TStop Command { options = Argument { spec ; _ } ; conv ;_ }, args ->
          complete_options (fun _ _ -> return_nil) args spec ind (conv cctxt)
      | (TParam _ | TPrefix _), []
      | TEmpty, _ -> return_nil
  in help tree args index

let autocompletion ~script ~cur_arg ~prev_arg ~args ~global_options commands cctxt =
  let tree = make_dispatch_tree commands in
  let rec ind n = function
    | [] -> None
    | hd :: tl ->
        if hd = prev_arg
        then Some (Option.unopt ~default:(n + 1) (ind (n + 1) tl))
        else (ind (n + 1) tl) in
  begin
    if prev_arg = script
    then complete_next_tree cctxt tree >>|? fun command_completions ->
      begin
        let (Argument { spec ; _ }) = global_options in
        list_args spec @ command_completions
      end
    else
      match ind 0 args with
      | None ->
          return_nil
      | Some index ->
          begin
            let Argument { spec ; _ } = global_options in
            complete_options
              (fun args ind -> complete_tree cctxt tree ind args)
              args spec index cctxt
          end
  end >>|? fun completions ->
  List.filter
    (fun completion -> Re.Str.(string_match (regexp_string cur_arg) completion 0))
    completions

let parse_global_options global_options ctx args =
  let Argument { spec ; converter } = global_options in
  make_args_dict_consume spec args >>=? fun (dict, remaining) ->
  parse_args spec dict ctx >>=? fun nested ->
  return (converter nested, remaining)

let dispatch commands ctx args =
  let tree = make_dispatch_tree commands in
  match args with
  | [] | [ "-h" | "--help" ] ->
      fail (Help None)
  | _ ->
      find_command tree args >>=? fun (command, args_dict, filtered_args) ->
      exec command ctx filtered_args args_dict

type error += No_manual_entry of string list

let manual_group =
  { name = "man" ;
    title = "Access the documentation" }

let add_manual ~executable_name ~global_options format ppf commands =
  let rec with_manual = lazy
    (commands @
     [ command
         ~group:manual_group
         ~desc:"Print documentation of commands.\n\
                Add search keywords to narrow list.\n\
                Will display only the commands by default, \
                unless [-verbosity <2|3>] is passed or the list \
                of matching commands if less than 3."
         (args2
            (arg
               ~doc:"level of details\n\
                     0. Only shows command mnemonics, without documentation.\n\
                     1. Shows command mnemonics with short descriptions.\n\
                     2. Show commands and arguments with short descriptions\n\
                     3. Show everything"
               ~long:"verbosity"
               ~short:'v'
               ~placeholder:"0|1|2|3"
               (parameter
                  ~autocomplete: (fun _ -> return [ "0" ; "1" ; "2" ; "3" ])
                  (fun _ arg -> match arg with
                     | "0" -> return Terse
                     | "1" -> return Short
                     | "2" -> return Details
                     | "3" -> return Full
                     | _ -> failwith "Level of details out of range")))
            (default_arg
               ~doc:"the manual's output format"
               ~placeholder: "plain|colors|html"
               ~long: "format"
               ~default:
                 (match format with
                  | Ansi -> "colors"
                  | Plain -> "plain"
                  | Html -> "html")
               (parameter
                  ~autocomplete: (fun _ -> return [ "colors" ; "plain" ; "html" ])
                  (fun _ arg -> match arg with
                     | "colors" -> return Ansi
                     | "plain" -> return Plain
                     | "html" -> return Html
                     | _ -> failwith "Unknown manual format"))))
         (prefix "man"
            (seq_of_param (string ~name:"keyword"
                             ~desc:"keyword to search for\n\
                                    If several are given they must all appear in the command.")))
         (fun (verbosity, format) keywords _ ->
            let commands =
              List.fold_left
                (fun commands keyword -> List.filter (search_command keyword) commands)
                (Lazy.force with_manual)
                keywords in
            let verbosity = match verbosity with
              | Some verbosity -> verbosity
              | None when List.length commands <= 3 -> Full
              | None -> Short in
            match commands with
            | [] -> fail (No_manual_entry keywords)
            | _ ->
                let state = setup_formatter ppf format verbosity in
                let commands = List.map (fun c -> Ex c) commands in
                usage_internal ppf ~executable_name ~global_options ~highlights:keywords commands ;
                restore_formatter ppf state ;
                return_unit) ]) in
  Lazy.force with_manual

let pp_cli_errors ppf ~executable_name ~global_options ~default errs =
  let pp_one = function
    | Bad_argument (i, v) ->
        Format.fprintf ppf
          "Erroneous command line argument %d (%s)." i v ;
        Some []
    | Option_expected_argument (arg, command) ->
        Format.fprintf ppf
          "Command line option @{<opt>%s@} expects an argument." arg ;
        Some (Option.unopt_map ~f:(fun command -> [ Ex command ]) ~default:[] command)
    | Bad_option_argument (arg, command) ->
        Format.fprintf ppf
          "Wrong value for command line option @{<opt>%s@}." arg ;
        Some (Option.unopt_map ~f:(fun command -> [ Ex command ]) ~default:[] command)
    | Multiple_occurences (arg, command) ->
        Format.fprintf ppf
          "Command line option @{<opt>%s@} appears multiple times." arg ;
        Some (Option.unopt_map ~f:(fun command -> [ Ex command ]) ~default:[] command)
    | No_manual_entry [ keyword ] ->
        Format.fprintf ppf
          "No manual entry that match @{<hilight>%s@}."
          keyword ;
        Some []
    | No_manual_entry (keyword :: keywords) ->
        Format.fprintf ppf
          "No manual entry that match %a and @{<hilight>%s@}."
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
             (fun ppf keyword -> Format.fprintf ppf "@{<hilight>%s@}" keyword))
          keywords
          keyword ;
        Some []
    | Unknown_option (option, command) ->
        Format.fprintf ppf
          "Unexpected command line option @{<opt>%s@}."
          option ;
        Some (Option.unopt_map ~f:(fun command -> [ Ex command ]) ~default:[] command)
    | Extra_arguments (extra, command) ->
        Format.fprintf ppf
          "Extra command line arguments:@, @[<h>%a@]."
          (Format.pp_print_list (fun ppf -> Format.fprintf ppf "%s")) extra ;
        Some [ Ex command ]
    | Unterminated_command (_, commands) ->
        Format.fprintf ppf
          "@[<v 2>Unterminated command, here are possible completions.@,%a@]"
          (Format.pp_print_list
             (fun ppf (Command { params ; options = Argument { spec ; _ } ; _ }) ->
                print_commandline ppf ([], spec, params))) commands ;
        Some (List.map (fun c -> Ex c) commands)
    | Command_not_found ([], _all_commands) ->
        Format.fprintf ppf
          "@[<v 0>Unrecognized command.@,\
           Try using the @{<kwd>man@} command to get more information.@]" ;
        Some []
    | Command_not_found (_, commands) ->
        Format.fprintf ppf
          "@[<v 0>Unrecognized command.@,\
           Did you mean one of the following?@,  @[<v 0>%a@]@]"
          (Format.pp_print_list
             (fun ppf (Command { params ; options = Argument { spec ; _ } ; _ }) ->
                print_commandline ppf ([], spec, params))) commands ;
        Some (List.map (fun c -> Ex c) commands)
    | err -> default ppf err ; None in
  let rec pp acc errs =
    let return command =
      match command, acc with
      | None, _ -> acc
      | Some command, Some commands -> Some (command @ commands)
      | Some command, None -> Some command in
    match errs with
    | [] -> None
    | [ last ] -> return (pp_one last)
    | err :: errs ->
        let acc = return (pp_one err) in
        Format.fprintf ppf "@," ;
        pp acc errs in
  Format.fprintf ppf "@[<v 2>@{<error>@{<title>Error@}@}@," ;
  match pp None errs with
  | None ->
      Format.fprintf ppf "@]@\n"
  | Some commands ->
      Format.fprintf ppf "@]@\n@\n@[<v 0>%a@]"
        (fun ppf commands -> usage_internal ppf ~executable_name ~global_options commands)
        commands

let usage ppf ~executable_name ~global_options commands =
  usage_internal ppf
    ~executable_name ~global_options
    (List.map (fun c -> Ex c) commands)

let map_command f (Command c) =
  (Command { c with conv = (fun x -> c.conv (f x)) })
