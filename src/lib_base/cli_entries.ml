(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Command Line Parsing *)

open Error_monad

(* User catchable exceptions *)
type error += Bad_argument of int * string
type error += Option_expected_argument of string
type error += Unknown_option of string

type ('p, 'ctx) parameter =
  { converter: ('ctx -> string -> 'p tzresult Lwt.t) ;
    autocomplete: ('ctx -> string list tzresult Lwt.t) option }

let parameter ?autocomplete converter =
  { converter ; autocomplete }

type ('a, 'ctx) arg =
  | Arg : { doc : string ;
            parameter : string ;
            placeholder : string ;
            kind : ('p, 'ctx) parameter } ->
    ('p option, 'ctx) arg
  | DefArg : { doc : string ;
               parameter : string ;
               placeholder : string ;
               kind : ('p, 'ctx) parameter ;
               default : string } -> ('p, 'ctx) arg
  | Switch : { doc : string ;
               parameter : string } ->
    (bool, 'ctx) arg

let arg ~doc ~parameter ~placeholder kind =
  Arg { doc ;
        parameter ;
        placeholder ;
        kind }

let default_arg ~doc ~parameter ~placeholder ~default kind =
  DefArg { doc ;
           placeholder ;
           parameter ;
           kind ;
           default }

let switch ~doc ~parameter =
  Switch { doc ; parameter }

type ('a, 'arg) args =
  | NoArgs : (unit, 'args) args
  | AddArg : ('a, 'args) arg * ('b, 'args) args ->
    ('a * 'b, 'args) args

let parse_arg :
  type a ctx. (a, ctx) arg -> string option TzString.Map.t -> ctx -> a tzresult Lwt.t =
  fun spec args_dict ctx ->
    match spec with
    | Arg { parameter ; kind = { converter ; _  } ; _ } ->
        begin
          try
            begin
              match TzString.Map.find parameter args_dict with
              | None -> return None
              | Some s ->
                  (converter ctx s) >>|? fun x ->
                  Some x
            end
          with Not_found ->
            return None
        end
    | DefArg { parameter ; kind = { converter ; _ } ; default ; _ } ->
        converter ctx default >>= fun default ->
        begin match default with
          | Ok x -> return x
          | Error _ ->
              invalid_arg
                (Format.sprintf
                   "Value provided as default for '%s' could not be parsed by converter function."
                   parameter) end >>=? fun default ->
        begin try
            match TzString.Map.find parameter args_dict with
            | None -> return default
            | Some s -> converter ctx s
          with Not_found -> return default
        end
    | Switch { parameter ; _ } ->
        return (TzString.Map.mem parameter args_dict)

(* Argument parsing *)
let rec parse_args :
  type a ctx. (a, ctx) args -> string option TzString.Map.t -> ctx -> a tzresult Lwt.t =
  fun spec args_dict ctx ->
    match spec with
    | NoArgs -> return ()
    | AddArg (arg, rest) ->
        parse_arg arg args_dict ctx >>=? fun arg ->
        parse_args rest args_dict ctx >>|? fun rest ->
        (arg, rest)

let empty_args_dict = TzString.Map.empty

let rec make_arities_dict :
  type a b. int TzString.Map.t -> (a, b) args -> int TzString.Map.t =
  fun acc -> function
    | NoArgs -> acc
    | AddArg (arg, rest) ->
        let recur parameter num =
          make_arities_dict (TzString.Map.add parameter num acc) rest in
        begin
          match arg with
          | Arg { parameter ; _ } -> recur parameter 1
          | DefArg { parameter ; _ } -> recur parameter 1
          | Switch { parameter ; _ } -> recur parameter 0
        end

let check_help_flag error = function
  | ("-help" | "--help") :: _ -> fail error
  | _ -> return ()

(* ignore_autocomplete is a hack to have the initial arguments get parsed
   even if autocomplete command is running *)
let make_args_dict_consume help_flag ignore_autocomplete spec args =
  let rec make_args_dict completing arities acc args =
    check_help_flag help_flag args >>=? fun () ->
    match args with
    | [] -> return (acc, [])
    | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: remaining_args
      when ignore_autocomplete ->
        make_args_dict true arities acc remaining_args >>=? fun (dict, _) ->
        return (dict, "bash_autocomplete" :: prev_arg :: cur_arg :: script :: remaining_args)
    | arg :: tl ->
        if TzString.Map.mem arg arities
        then let arity = TzString.Map.find arg arities in
          check_help_flag help_flag tl >>=? fun () ->
          match arity, tl with
          | 0, tl' -> make_args_dict completing arities (TzString.Map.add arg None acc) tl'
          | 1, value :: tl' ->
              make_args_dict completing arities (TzString.Map.add arg (Some value) acc) tl'
          | 1, [] when completing ->
              return (acc, [])
          | 1, [] ->
              fail (Option_expected_argument arg)
          | _, _ ->
              raise (Failure "cli_entries: Arguments with arity not equal to 1 or 0 not supported")
        else return (acc, args)
  in make_args_dict false (make_arities_dict TzString.Map.empty spec) TzString.Map.empty args

let make_args_dict_filter help_flag spec args =
  let rec make_args_dict arities (dict, other_args) args =
    check_help_flag help_flag args >>=? fun () ->
    match args with
    | [] -> return (dict, other_args)
    | arg :: tl ->
        if TzString.Map.mem arg arities
        then let arity = TzString.Map.find arg arities in
          check_help_flag help_flag tl >>=? fun () ->
          match arity, tl with
          | 0, tl -> make_args_dict arities (TzString.Map.add arg None dict, other_args) tl
          | 1, value :: tl' -> make_args_dict arities (TzString.Map.add arg (Some value) dict, other_args) tl'
          | 1, [] -> fail (Option_expected_argument arg)
          | _, _ ->
              raise (Failure "cli_entries: Arguments with arity not equal to 1 or 0 not supported")
        else make_args_dict arities (dict, arg :: other_args) tl
  in make_args_dict
    (make_arities_dict TzString.Map.empty spec)
    (TzString.Map.empty, [])
    args >>|? fun (dict, remaining) ->
  (dict, List.rev remaining)

let make_args_dict help_handler spec args =
  make_args_dict_consume help_handler false spec args >>=? fun (args, remaining) ->
  match remaining with
  | [] -> return args
  | hd :: _ -> fail (Unknown_option hd)

type (_, _) options =
    Argument : { spec : ('a, 'arg) args ;
                 converter : 'a -> 'b } -> ('b, 'arg) options
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

(* A simple structure for command interpreters.
   This is more generic than the exported one, see end of file. *)
type ('a, 'ctx, 'ret) params =
  | Prefix : string * ('a, 'ctx, 'ret) params ->
    ('a, 'ctx, 'ret) params
  | Param : string * string *
            ('p, 'ctx) parameter *
            ('a, 'ctx, 'ret) params ->
    ('p -> 'a, 'ctx, 'ret) params
  | Stop :
      ('ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params
  | Seq : string * string *
          ('p, 'ctx) parameter ->
    ('p list -> 'ctx -> 'ret tzresult Lwt.t, 'ctx, 'ret) params

(* A command group *)
type group =
  { name : string ;
    title : string }

(* A command wraps a callback with its type and info *)
type ('arg, 'ret) command =
  | Command
    : { params : ('a, 'arg, 'ret) params ;
        options : ('b, 'arg) options ;
        handler : 'b -> 'a ;
        desc : string ;
        group : group option }
    -> ('arg, 'ret) command

type format = [ `Plain | `Ansi | `Html ]

type error += Extra_arguments : string list * (_, _) command -> error
type error += Not_enough_args : string list * ('a, 'b) command list -> error
type error += Command_not_found : string list * ('a, 'b) command list -> error
type error += Help_flag : ('a, 'b) command list -> error (* when -help appears in input *)
type error += Help_cmd : string list * ('a, 'b) command list * format * [ `Terse | `Short | `Args | `Full ] -> error    (* ./tezos-client help *)
type error += Bare_help : error   (* ./tezos-client or ./tezos-client -help *)
type error += Autocomplete_command : string list -> error

let parse_initial_options :
  type a ctx. (a, ctx) options -> ctx -> string list -> (a * string list) tzresult Lwt.t =
  fun (Argument { spec ; converter }) ctx args ->
    make_args_dict_consume Bare_help true spec args >>=? fun (dict, remaining) ->
    parse_args spec dict ctx >>|? fun nested ->
    (converter nested, remaining)

(* Some combinators for writing commands concisely. *)
let param ~name ~desc kind next = Param (name, desc, kind, next)
let seq_of_param param =
  match param Stop with
  | Param (n, desc, parameter, Stop) -> Seq (n, desc, parameter)
  | _ -> invalid_arg "Cli_entries.seq_of_param"

let prefix keyword next = Prefix (keyword, next)
let rec fixed =
  function [] -> Stop | n :: r -> Prefix (n, fixed r)
let rec prefixes p next =
  match p with [] -> next | n :: r -> Prefix (n, prefixes r next)
let stop = Stop
let no_options = Argument { spec=NoArgs ; converter=fun () -> () }
let command ?group ~desc options params handler =
  Command { params ; options ; handler ; desc ; group }

(* Param combinators *)
let string ~name ~desc next =
  param ~name ~desc { converter=(fun _ s -> return s) ; autocomplete=None } next

(* Help commands *)
let help_group =
  { name = "man" ;
    title = "Access the documentation" }

let string_contains ~needle ~haystack =
  try
    Some (Str.search_forward (Str.regexp_string needle) haystack 0)
  with Not_found ->
    None

let rec search_params_prefix : type a arg ret. string -> (a, arg, ret) params -> bool =
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

let rec help_commands commands =
  [ command
      ~group:help_group
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
            ~parameter:"-verbosity"
            ~placeholder:"0|1|2|3"
            (parameter
               ~autocomplete: (fun _ -> return [ "0" ; "1" ; "2" ; "3" ])
               (fun _ arg -> match arg with
                  | "0" -> return `Terse
                  | "1" -> return `Short
                  | "2" -> return `Args
                  | "3" -> return `Full
                  | _ -> failwith "Level of details out of range")))
         (default_arg
            ~doc:"the manual's output format"
            ~placeholder: "plain|colors|html"
            ~parameter: "-format"
            ~default: (if Unix.isatty Unix.stdout then "colors" else "plain")
            (parameter
               ~autocomplete: (fun _ -> return [ "colors" ; "plain" ; "html" ])
               (fun _ arg -> match arg with
                  | "colors" -> return `Ansi
                  | "plain" -> return `Plain
                  | "html" -> return `Html
                  | _ -> failwith "Unknown manual format"))))
      (prefix "man"
         (seq_of_param (string ~name:"keyword"
                          ~desc:"keyword to search for\n\
                                 If several are given they must all appear in the command.")))
      (fun (verbosity, format) keywords _ ->
         let commands =
           List.fold_left
             (fun commands keyword -> List.filter (search_command keyword) commands)
             (help_commands [] @ commands)
             keywords in
         let verbosity = match verbosity with
           | Some verbosity -> verbosity
           | None when List.length commands <= 3 -> `Full
           | None -> `Short in
         fail (Help_cmd (keywords, commands, format, verbosity))) ]

(* Command execution *)
let exec
    (type ctx) (type ret)
    (Command { options = (Argument { converter ; spec = options_spec }) ;
               params = spec ;
               handler ;
               _ })
    (ctx : ctx) params args_dict =
  let rec exec
    : type a. int -> (a, ctx, ret) params -> a -> string list -> ret tzresult Lwt.t
    = fun i spec cb params ->
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
          exec (succ i) next cb rest
      | Param (_, _, { converter ; _ }, next), p :: rest ->
          Lwt.catch
            (fun () -> converter ctx p)
            (function
              | Failure msg -> Error_monad.failwith "%s" msg
              | exn -> fail (Exn exn))
          |> trace (Bad_argument (i, p)) >>=? fun v ->
          exec (succ i) next (cb v) rest
      | _ -> raise (Failure ("cli_entries internal error: exec no case matched"))
  in
  parse_args options_spec args_dict ctx >>=? fun parsed_options ->
  exec 1 spec (handler (converter parsed_options)) params

(* Command dispatch tree *)
type ('arg, 'ret) level =
  { stop : ('arg, 'ret) command option ;
    prefix : (string * ('arg, 'ret) tree) list }
and ('arg, 'ret) param_level =
  { stop : ('arg, 'ret) command option ;
    autocomplete : ('arg -> string list tzresult Lwt.t) option ;
    tree : ('arg, 'ret) tree }
and ('ctx, 'ret) tree =
  | TPrefix : ('ctx, 'ret) level -> ('ctx, 'ret) tree
  | TParam : ('ctx, 'ret) param_level -> ('ctx, 'ret) tree
  | TStop : ('ctx, 'ret) command -> ('ctx, 'ret) tree
  | TSeq : ('ctx, 'ret) command * ('ctx -> string list tzresult Lwt.t) option -> ('ctx, 'ret) tree
  | TEmpty : ('ctx, 'ret) tree

let has_options : type ret ctx. (ctx, ret) command -> bool =
  fun (Command { options = Argument { spec ; _ } ; _ }) ->
    let args_help : type a. (a, ctx) args -> bool = function
      | NoArgs -> false
      | AddArg (_, _) -> true
    in args_help spec

let insert_in_dispatch_tree
    (type ctx) (type ret)
    root (Command { params ; _ } as command) =
  let access_autocomplete :
    type p. (p, ctx) parameter -> (ctx -> string list tzresult Lwt.t) option =
    fun { autocomplete ; _ } -> autocomplete in
  let rec insert_tree
    : type a. (ctx, ret) tree -> (a, ctx, ret) params -> (ctx, ret) tree
    = fun t c -> match t, c with
      | TEmpty, Stop -> TStop command
      | TEmpty, Seq (_, _, { autocomplete ; _ }) -> TSeq (command, autocomplete)
      | TEmpty, Param (_, _, param, next) ->
          TParam { tree = insert_tree TEmpty next ; stop = None ; autocomplete=access_autocomplete param}
      | TEmpty, Prefix (n, next) ->
          TPrefix { stop = None ; prefix = [ (n, insert_tree TEmpty next) ] }
      | TStop cmd, Param (_, _, param, next) ->
          if not (has_options cmd)
          then TParam { tree = insert_tree TEmpty next ;
                        stop = Some cmd ;
                        autocomplete=access_autocomplete param }
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
            "Cli_entries.Command_tree.insert: conflicting commands" in
  insert_tree root params

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
  let rec help tree arguments acc =
    match tree, arguments with
    | (TStop _ | TSeq _
      | TPrefix { stop = Some _ ; _ }
      | TParam { stop = Some _ ; _}), ("-help" | "--help") :: _ ->
        fail (Help_flag ( gather_commands tree))
    | TStop c, [] -> return (c, empty_args_dict, initial_arguments)
    | TStop (Command { options = Argument { spec ; _ } ; _ } as c), args ->
        if not (has_options c)
        then fail (Extra_arguments (List.rev acc, c))
        else make_args_dict (Help_flag [c]) spec args >>=? fun args_dict ->
          return (c, args_dict, initial_arguments)
    | TSeq (Command { options = Argument { spec ; _ } ; _ } as c, _), remaining ->
        if List.exists (function "-help" | "--help" -> true | _ -> false) remaining then
          fail (Help_flag ( gather_commands tree))
        else
          make_args_dict_filter (Help_flag [c]) spec remaining >>|? fun (dict, remaining) ->
          (c, dict, List.rev_append acc remaining)
    | TPrefix { stop = Some cmd ; _ }, [] ->
        return (cmd, empty_args_dict, initial_arguments)
    | TPrefix { stop = None ; prefix }, ([] | ("-help" | "--help") :: _) ->
        fail (Not_enough_args (initial_arguments, gather_assoc prefix))
    | TPrefix { prefix ; _ }, hd_arg :: tl ->
        begin
          try
            return (List.assoc hd_arg prefix)
          with Not_found -> fail (Command_not_found (List.rev acc, gather_assoc prefix))
        end >>=? fun tree' ->
        help tree' tl (hd_arg :: acc)
    | TParam { stop = None ; _ }, ([] | ("-help" | "--help") :: _) ->
        fail (Not_enough_args (initial_arguments, gather_commands tree))
    | TParam { stop = Some c ; _ }, [] ->
        return (c, empty_args_dict, initial_arguments)
    | TParam { tree ; _ }, parameter :: arguments' ->
        help tree arguments' (parameter :: acc)
    | TEmpty, _ ->
        fail (Command_not_found (List.rev acc, []))
  in help tree initial_arguments []

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

let print_options_detailed (type ctx) =
  let help_option : type a.Format.formatter -> (a, ctx) arg -> unit =
    fun ppf -> function
      | Arg { parameter ; placeholder ; doc ; _ } ->
          Format.fprintf ppf "@{<opt>%s <%s>@}: %a"
            parameter placeholder print_desc doc ;
      | DefArg { parameter ; placeholder ; doc ; default ; _ } ->
          Format.fprintf ppf "@{<opt>%s <%s>@}: %a"
            parameter placeholder print_desc (doc ^ "\nDefaults to `" ^ default ^ "`.")
      | Switch { parameter ; doc } ->
          Format.fprintf ppf "@{<opt>%s@}: %a"
            parameter print_desc doc in
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
      | DefArg { parameter ; placeholder ; _ } ->
          Format.fprintf ppf "[@{<opt>%s <%s>@}]" parameter placeholder
      | Arg { parameter ; placeholder ; _ } ->
          Format.fprintf ppf "[@{<opt>%s <%s>@}]" parameter placeholder
      | Switch { parameter ; _ } ->
          Format.fprintf ppf "[@{<opt>%s@}]" parameter
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
        begin match Str.full_split regex str with
          | []
          | [ Str.Text _ ] -> print_string tl
          | list ->
              List.iter
                (function
                  | Str.Text text -> Format.fprintf formatter "%s" text
                  | Str.Delim delimiter ->
                      Format.fprintf formatter "@{<hilight>%s@}" delimiter)
                list
        end
  in print_string (List.map Str.regexp_string highlight_strings)

let print_commandline ppf (highlights, options, args) =
  let rec print
    : type a ctx ret. Format.formatter -> (a, ctx, ret) params -> unit =
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
  : type a b ctx ret. (b, ctx) args -> Format.formatter -> (a, ctx, ret) params -> unit
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
  type a b arg ctx. (a, arg, ctx) params -> (b, _) args -> bool
  = fun params args ->
    let rec help : (a, arg, ctx) params -> bool = function
      | Stop -> has_args args
      | Seq (_, _, _) -> true
      | Prefix (_, next) -> help next
      | Param (_, _, _, _) -> true
    in help params

let print_command :
  type ctx ret.
  ?prefix:(Format.formatter -> unit -> unit) ->
  ?highlights:string list -> Format.formatter -> (ctx, ret) command -> unit
  = fun
    ?(prefix = (fun _ () -> ()))
    ?(highlights=[]) ppf
    (Command { params ; desc ; options = Argument { spec ; _ } ; _ }) ->
    if contains_params_args params spec
    then
      Format.fprintf ppf "@{<command>%a%a@{<short>@,@{<commanddoc>%a@{<args>@,%a@}@}@}@}"
        prefix ()
        print_commandline (highlights, spec, params)
        print_desc desc
        (print_params_detailed spec) params
    else
      Format.fprintf ppf "@{<command>%a%a@{<short>@,@{<commanddoc>%a@}@}@}"
        prefix ()
        print_commandline (highlights, spec, params)
        print_desc desc

let group_commands commands =
  let (grouped, ungrouped) =
    List.fold_left
      (fun (grouped, ungrouped) (Command { group ; _ } as command) ->
         match group with
         | None ->
             (grouped, command :: ungrouped)
         | Some group ->
             try
               let ({ title ; _ }, r) =
                 List.find (fun ({ name ; _ }, _) -> group.name = name) grouped in
               if title <> group.title then
                 invalid_arg "Cli_entries.usage: duplicate group name" ;
               r := command :: !r ;
               (grouped, ungrouped)
             with Not_found ->
               ((group, ref [ command ]) :: grouped, ungrouped))
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
  Format.fprintf ppf "@{<title>%s@}@,@{<section>%a@}"
    title
    (Format.pp_print_list print_command) commands

let setup_ppf ppf format verbosity =
  let skip = ref false in
  let orig_out_functions =
    Format.pp_get_formatter_out_functions ppf () in
  Format.pp_set_formatter_out_functions ppf
    { out_string =
        (fun s b a ->
           if s = "\000\000\000" then skip := true
           else if s = "\255\255\255" then skip := false
           else if not !skip then orig_out_functions.out_string s b a) ;
      out_spaces = (fun n -> if not !skip then orig_out_functions.out_spaces n) ;
      out_newline = (fun () -> if not !skip then orig_out_functions.out_newline ()) ;
      out_flush = (fun () -> if not !skip then orig_out_functions.out_flush ()) } ;
  let levels = ref [] in
  let setup_level level =
    match verbosity, level with
    | (`Full, (`Terse | `Short | `Args | `Full))
    | (`Args, (`Terse | `Short | `Args))
    | (`Short, (`Terse | `Short))
    | `Terse, `Terse -> Format.fprintf ppf "@<0>%s" "\255\255\255"
    | _ -> Format.fprintf ppf "@<0>%s" "\000\000\000" in
  let push_level level =
    levels := level :: !levels ;
    setup_level level in
  let pop_level _ =
    match !levels with
    | _ :: level :: rest -> levels := level :: rest ; setup_level level
    | [ _ ] | [] -> Pervasives.failwith "Cli_entries: unclosed verbosity tag" in
  push_level `Terse ;
  match format with
  | `Ansi ->
      let color_num = function
        | `Black -> None
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
      Format.pp_set_formatter_tag_functions ppf
        { mark_open_tag = (fun _ -> "") ;
          mark_close_tag = (fun _ -> "") ;
          print_open_tag = begin function
            | "title" -> Format.fprintf ppf "@<0>%a" ansi_format (`White, `Black, true, true)
            | "commandline" -> Format.fprintf ppf "@[<hov 4>"
            | "commanddoc" -> Format.fprintf ppf "  @[<v 0>"
            | "opt" -> Format.fprintf ppf "@<0>%a" ansi_format (`Green, `Black, false, false)
            | "arg" -> Format.fprintf ppf "@<0>%a<" ansi_format (`Yellow, `Black, false, false)
            | "kwd" -> Format.fprintf ppf "@<0>%a" ansi_format (`White, `Black, false, true)
            | "hilight" -> Format.fprintf ppf "@<0>%a" ansi_format (`Black, `Yellow, false, true)
            | "section" -> Format.fprintf ppf "  @[<v 0>"
            | "command" -> Format.fprintf ppf "@[<v 0>"
            | "full" -> push_level `Full
            | "args" -> push_level `Args
            | "short" -> push_level `Short
            | "terse" -> push_level `Terse
            | "document" -> Format.fprintf ppf "@[<v 0>"
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
          end ;
          print_close_tag = begin function
            | "title" -> Format.fprintf ppf ":@<0>%s" "\027[0m"
            | "commandline" -> Format.fprintf ppf "@]"
            | "commanddoc" -> Format.fprintf ppf "@]"
            | "opt" -> Format.fprintf ppf "@<0>%s" "\027[0m"
            | "arg" -> Format.fprintf ppf ">@<0>%s" "\027[0m"
            | "kwd" -> Format.fprintf ppf "@<0>%s" "\027[0m"
            | "hilight" -> Format.fprintf ppf "@<0>%s" "\027[0m"
            | "command" | "section" -> Format.fprintf ppf "@]"
            | "full" -> pop_level `Full
            | "args" -> pop_level `Args
            | "short" -> pop_level `Short
            | "terse" -> pop_level `Terse
            | "document" -> Format.fprintf ppf "@]"
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
          end } ;
      Format.pp_set_print_tags ppf true
  | `Plain ->
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
            | "section" -> Format.fprintf ppf "  @[<v 0>"
            | "command" -> Format.fprintf ppf "@[<v 0>"
            | "full" -> push_level `Full
            | "args" -> push_level `Args
            | "short" -> push_level `Short
            | "terse" -> push_level `Terse
            | "document" -> Format.fprintf ppf "@[<v 0>"
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
          end ;
          print_close_tag = begin function
            | "title" -> Format.fprintf ppf ":"
            | "commandline" -> Format.fprintf ppf "@]"
            | "commanddoc" -> Format.fprintf ppf "@]"
            | "opt" -> ()
            | "arg" -> Format.fprintf ppf ">"
            | "kwd" -> ()
            | "hilight" -> ()
            | "command" | "section" -> Format.fprintf ppf "@]"
            | "full" -> pop_level `Full
            | "args" -> pop_level `Args
            | "short" -> pop_level `Short
            | "terse" -> pop_level `Terse
            | "document" -> Format.fprintf ppf "@]"
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
          end } ;
      Format.pp_set_print_tags ppf true
  | `Html ->
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
            | "section" -> Format.fprintf ppf "\003ul\004@\n"
            | "command" -> Format.fprintf ppf "\003li\004@\n"
            | "full" -> push_level `Full
            | "args" -> push_level `Args
            | "short" -> push_level `Short
            | "terse" -> push_level `Terse
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
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
          end ;
          print_close_tag = begin function
            | "title" -> Format.fprintf ppf "\003/h3\004@\n"
            | "commandline" -> Format.fprintf ppf "@]\003/div\004@\n"
            | "commanddoc" -> Format.fprintf ppf "\003/div\004@\n"
            | "opt" -> Format.fprintf ppf "\003/span\004"
            | "arg" -> Format.fprintf ppf "\003/span\004"
            | "kwd" -> Format.fprintf ppf "\003/span\004"
            | "hilight" -> ()
            | "section" -> Format.fprintf ppf "\003/ul\004@\n"
            | "command" -> Format.fprintf ppf "\003/li\004@\n"
            | "full" -> pop_level `Full
            | "args" -> pop_level `Args
            | "short" -> pop_level `Short
            | "terse" -> pop_level `Terse
            | "document" -> Format.fprintf ppf "@]"
            | _ -> Pervasives.failwith "Cli_entries: invalid semantic tag"
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

let usage ppf ?global_options ?(highlights=[]) commands format verbosity =
  setup_ppf ppf format verbosity ;
  let usage ppf (by_group, options) =
    let exe = Filename.basename Sys.executable_name in
    let print_groups =
      Format.pp_print_list
        ~pp_sep: (fun ppf () -> Format.fprintf ppf "@,@,")
        (print_group (print_command ?prefix:None ~highlights)) in
    match options with
    | None ->
        Format.fprintf ppf
          "@[<v>%a@]"
          print_groups by_group
    | Some (Argument { spec ; _ })->
        Format.fprintf ppf
          "@[<v>@{<title>Usage@}@,\
           @{<section>\
           @{<command>@{<commandline>\
           %s [@{<opt>global options@}] command @{<opt>[command options]@}@}@}@,\
           @{<command>@{<commandline>\
           %s @{<opt>-help@} (for global options)@}@}@,\
           @{<command>@{<commandline>\
           %s [@{<opt>global options@}] command @{<opt>-help@} (for command options)@}@}\
           @}@,@,\
           @{<title>To browse the documentation@}@,\
           @{<section>\
           @{<command>@{<commandline>\
           %s [@{<opt>global options@}] man (for a list of commands)@}@}@,\
           @{<command>@{<commandline>\
           %s [@{<opt>global options@}] man @{<opt>-verbosity 3@} (for the full manual)@}@}\
           @}@,@,\
           @{<title>Global options (must come before the command)@}@,\
           @{<commanddoc>%a@}%a\
           %a@]"
          exe exe exe exe exe
          print_options_detailed spec
          (fun ppf () -> if by_group <> [] then Format.fprintf ppf "@,@,") ()
          print_groups by_group in
  Format.fprintf ppf "@{<document>%a" usage (group_commands commands, global_options) ;
  if List.mem verbosity [ `Terse ; `Short ] then
    Format.fprintf ppf "@\n@\nUse option [@{<opt>-verbosity 3@}] for option descriptions." ;
  Format.fprintf ppf "@}"

let command_usage ppf commands format verbosity =
  setup_ppf ppf format verbosity ;
  let exe = Filename.basename Sys.executable_name in
  let prefix ppf () =
    Format.fprintf ppf "@{<kwd>%s@} @{<opt>[global options]@} " exe in
  Format.fprintf ppf
    "@[<v 2>@{<title>Command usage@}@,\
     %a@,@{<kwd>%s@} @{<opt>-help@} (for global options)@]"
    (Format.pp_print_list (print_command ~prefix ~highlights:[]))
    commands
    exe

let get_arg : type a ctx. (a, ctx) arg -> string = function
  | Arg { parameter ; _ } -> parameter
  | DefArg { parameter ; _ } -> parameter
  | Switch { parameter ; _ } -> parameter

let rec list_args : type arg ctx. (arg, ctx) args -> string list = function
  | NoArgs -> []
  | AddArg (arg, args) -> (get_arg arg) :: (list_args args)

let complete_func autocomplete cctxt =
  match autocomplete with
  | None -> return []
  | Some autocomplete -> autocomplete cctxt

let list_command_args (Command { options = Argument { spec ; _ } ; _ }) =
  list_args spec

module StringSet = Set.Make(String)

let get_arg_parameter (type a) (arg : (a, _) arg) =
  match arg with
  | Arg { parameter ; _ } -> parameter
  | DefArg { parameter ; _ } -> parameter
  | Switch { parameter ; _ } -> parameter

let complete_arg : type a ctx. ctx -> (a, ctx) arg -> string list tzresult Lwt.t =
  fun ctx -> function
    | Arg { kind = { autocomplete ; _ } ; _ } -> complete_func autocomplete ctx
    | DefArg { kind = { autocomplete ; _ } ; _ } -> complete_func autocomplete ctx
    | Switch _ -> return []

let rec remaining_spec :
  type a ctx. StringSet.t -> (a, ctx) args -> string list =
  fun seen -> function
    | NoArgs -> []
    | AddArg (arg, rest) ->
        let parameter = get_arg_parameter arg in
        if StringSet.mem parameter seen
        then (remaining_spec seen rest)
        else parameter :: (remaining_spec seen rest)

let complete_options (type ctx) continuation args args_spec ind (ctx : ctx) =
  let arities = make_arities_dict TzString.Map.empty args_spec in
  let rec complete_spec : type a. string -> (a, ctx) args -> string list tzresult Lwt.t =
    fun name -> function
      | NoArgs -> return []
      | AddArg (arg, rest) ->
          if (get_arg_parameter arg) = name
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
          let seen = StringSet.add arg seen in
          begin
            match TzString.Map.find arg arities, tl with
            | 0, args when ind = 0 ->
                continuation args 0 >>|? fun cont_args ->
                remaining_spec seen args_spec @ cont_args
            | 0, args -> help args (ind - 1) seen
            | 1, _ when ind = 1 -> complete_spec arg args_spec
            | 1, _ :: tl -> help tl (ind - 2) seen
            | _ -> Pervasives.failwith "cli_entries internal error, invalid arity"
          end
        else continuation args ind
  in help args ind StringSet.empty

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
  | TEmpty -> return []

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
            with Not_found -> return []
          end
      | TParam { tree ; _ }, _ :: tl ->
          help tree tl (ind - 1)
      | TStop Command { options = Argument { spec ; _ } ; _ }, args ->
          complete_options (fun _ _ -> return []) args spec ind cctxt
      | (TParam _ | TPrefix _), []
      | TEmpty, _ -> return []
  in help tree args index


let autocomplete ~script ~cur_arg ~prev_arg ~args ~tree ~global_options cctxt =
  (* Interp: (ind 0) is the index of the cursor *)
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
        match global_options with
        | None -> command_completions
        | Some (Argument { spec ; _ }) ->
            remaining_spec StringSet.empty spec
            @ command_completions
      end
    else
      match ind 0 args with
      | None -> return []
      | Some index ->
          begin
            match global_options with
            | None -> complete_tree cctxt tree index args
            | Some (Argument { spec ; _ }) ->
                complete_options (fun args ind -> complete_tree cctxt tree ind args)
                  args spec index cctxt
          end
  end >>|? fun completions ->
  List.filter
    (fun completion -> Str.string_match (Str.regexp_string cur_arg) completion 0)
    completions

(* Try a list of commands on a list of arguments *)
let dispatch ?global_options commands ctx args =
  let commands = help_commands commands @ commands in
  let tree = make_dispatch_tree commands in
  match args with
  | [] | [ "-help" | "--help" ] -> fail Bare_help
  | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: remaining_args ->
      autocomplete ~script ~cur_arg ~prev_arg ~args:remaining_args ~global_options ~tree ctx
      >>= fun completions ->
      fail (Autocomplete_command
              (match completions with
               | Ok completions -> completions
               | Error _ -> []))
  | _ ->
      find_command tree args >>=? fun (command, args_dict, filtered_args) ->
      exec command ctx filtered_args args_dict

let handle_cli_errors ~stdout ~stderr ~global_options = function
  | Ok _ ->
      return 0
  | Error [ e ] ->              (* Should only be one error here *)
      begin match e with
        | Extra_arguments (_, cmd) ->
            setup_ppf stderr (if Unix.isatty Unix.stderr then `Ansi else `Plain) `Terse ;
            Format.fprintf stderr
              "Extra arguments provided for command:@;<1 2>@[%a@]@."
              (print_command ?prefix:None ~highlights:[]) cmd;
            return 1
        | Not_enough_args (_, cmds) ->
            setup_ppf stderr (if Unix.isatty Unix.stderr then `Ansi else `Plain) `Terse ;
            Format.fprintf stderr
              "@[<v 2>Unterminated command, here are possible completions:@,%a@]@."
              (Format.pp_print_list
                 (fun ppf (Command { params ; options = Argument { spec ; _ } ; _ }) ->
                    print_commandline ppf ([], spec, params))) cmds;
            return 1
        | Command_not_found ([], _) ->
            Format.fprintf stderr
              "Unrecognized command. Try using the 'man' command to get more information.@." ;
            return 1
        | Command_not_found (_, cmds) ->
            Format.fprintf stderr
              "@[<v 2>Unrecognized command, did you mean one of the following:@,%a@]@."
              (Format.pp_print_list
                 (fun ppf (Command { params ; options = Argument { spec ; _ } ; _ }) ->
                    print_commandline ppf ([], spec, params)))  cmds;
            return 1
        | Bad_argument (pos, arg) ->
            Format.fprintf stderr
              "The argument '%s' given in position %d was invalid.@."
              arg
              pos ;
            return 1
        | Option_expected_argument option ->
            Format.fprintf stderr
              "The option '%s' expected an argument, but you did not provide one.@."
              option ;
            return 1
        | Unknown_option option ->
            Format.fprintf stderr
              "While parsing options, encountered unexpected argument '%s'.@."
              option ;
            return 1
        | Help_cmd ([ highlight ], [], _, _) ->
            Format.fprintf stderr "No command found that match %s.@." highlight ;
            return 0
        | Help_cmd (highlight :: highlights, [], _, _) ->
            Format.fprintf stderr "No command found that match %a%s and %s.@."
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                 (fun ppf s -> Format.fprintf ppf "%s" s)) highlights
              (match highlights with [ _ ] | [] -> "" | _::_ -> ",")
              highlight ;
            return 0
        | Help_cmd (highlights, commands, format, verbosity) ->
            let global_options =
              if highlights = [] then Some global_options else None in
            usage stdout ?global_options ~highlights commands format verbosity ;
            Format.fprintf stdout "@." ;
            return 0
        | Bare_help ->
            let format = if Unix.isatty Unix.stdout then `Ansi else `Plain in
            usage stdout ~global_options ?highlights:None [] format `Terse ;
            Format.fprintf stdout "@." ;
            return 0
        | Autocomplete_command (completions) ->
            Format.fprintf stdout "%a@."
              (Format.pp_print_list
                 ~pp_sep:Format.pp_print_newline
                 Format.pp_print_string)
              completions ;
            return 0
        | Help_flag commands ->
            let format = if Unix.isatty Unix.stdout then `Ansi else `Plain in
            command_usage stdout commands format `Verbose ;
            Format.fprintf stdout "@." ;
            return 0
        | e -> fail e
      end
  | (Error _) as errors -> Lwt.return errors

let () =
  register_error_kind
    `Branch
    ~id: "cli.bad_argument"
    ~title: "Bad argument"
    ~description: "Error in a command line argument"
    ~pp:
      (fun ppf (i, v) ->
         Format.fprintf ppf "Error in command line argument %d (%s)" i v)
    Data_encoding.(obj2 (req "index" uint8) (req "value" string))
    (function Bad_argument (i, v) -> Some (i, v) | _ -> None)
    (fun (i, v) -> Bad_argument (i, v)) ;
  register_error_kind
    `Branch
    ~id: "cli.option_expected_argument"
    ~title: "Option without argument"
    ~description: "Option expected argument, but did not receive one"
    ~pp:
      (fun ppf arg ->
         Format.fprintf ppf "The option '%s' expected an argument, but did not receive one" arg)
    Data_encoding.(obj1 (req "arg" string))
    (function Option_expected_argument arg -> Some arg | _ -> None)
    (fun arg -> Option_expected_argument arg) ;
  register_error_kind
    `Branch
    ~id: "cli.unknown_option"
    ~title: "Unknown option"
    ~description: "While parsing options, encountered unknown option"
    ~pp:
      (fun ppf arg ->
         Format.fprintf ppf
           (if (String.length arg) > 0 && (String.get arg 0) = '-'
            then "Encountered an unknown option '%s' while parsing the command"
            else "Expected a flag, but instead encountered '%s'") arg)
    Data_encoding.(obj1 (req "arg" string))
    (function Unknown_option arg -> Some arg | _ -> None)
    (fun arg -> Unknown_option arg)
