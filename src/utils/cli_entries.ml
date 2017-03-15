(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Command Line Parsing *)

open Lwt

(* User catchable exceptions *)
exception Command_not_found
exception Bad_argument of int * string * string
exception Command_failed of string

(* A simple structure for command interpreters.
   This is more generic than the exported one, see end of file. *)
type ('a, 'arg, 'ret) params =
  | Prefix : string * ('a, 'arg, 'ret) params ->
    ('a, 'arg, 'ret) params
  | Param : string * string *
            ('arg -> string -> 'p Lwt.t) *
            ('a, 'arg, 'ret) params ->
    ('p -> 'a, 'arg, 'ret) params
  | Stop :
      ('arg -> 'ret Lwt.t, 'arg, 'ret) params
  | More :
      (string list -> 'arg -> 'ret Lwt.t, 'arg, 'ret) params
  | Seq : string * string *
          ('arg -> string -> 'p Lwt.t) ->
    ('p list -> 'arg -> 'ret Lwt.t, 'arg, 'ret) params

(* A command group *)
type group =
  { name : string ;
    title : string }

(* A command wraps a callback with its type and info *)
type ('arg, 'ret) command =
  | Command
    : { params: ('a, 'arg, 'ret) params ;
        handler : 'a ;
        desc : string ;
        group : group option ;
        args : (Arg.key * Arg.spec * Arg.doc) list }
    -> ('arg, 'ret) command

(* Some combinators for writing commands concisely. *)
let param ~name ~desc kind next = Param (name, desc, kind, next)
let seq ~name ~desc kind = Seq (name, desc, kind)
let seq_of_param param =
  match param Stop with
  | Param (n, desc, f, Stop) -> Seq (n, desc, f)
  | _ -> invalid_arg "Cli_entries.seq_of_param"

let prefix keyword next = Prefix (keyword, next)
let rec fixed =
  function [] -> Stop | n :: r -> Prefix (n, fixed r)
let rec prefixes p next =
  match p with [] -> next | n :: r -> Prefix (n, prefixes r next)
let stop = Stop
let more = More
let void = Stop
let any = More
let command ?group ?(args = []) ~desc params handler =
  Command { params ; handler ; desc ; group ; args }

(* Param combinators *)
let string ~name ~desc next =
  param name desc (fun _ s -> return s) next

(* Command execution *)
let exec
    (type arg) (type ret)
    (Command { params ; handler }) (last : arg) args =
  let rec exec
    : type a. int -> (a, arg, ret) params -> a -> string list -> ret Lwt.t
    = fun i params cb args ->
    match params, args with
    | Stop, [] -> cb last
    | Stop, _ -> Lwt.fail Command_not_found
    | Seq (_, _, f), seq ->
        let rec do_seq i acc = function
          | [] -> Lwt.return (List.rev acc)
          | p :: rest ->
              catch
                (fun () -> f last p)
                (function
                  | Failure msg -> Lwt.fail (Bad_argument (i, p, msg))
                  | exn -> Lwt.fail exn) >>= fun v ->
              do_seq (succ i) (v :: acc) rest in
        do_seq i [] seq >>= fun parsed ->
        cb parsed last
    | More, rest -> cb rest last
    | Prefix (n, next), p :: rest when n = p ->
        exec (succ i) next cb rest
    | Param (_, _, f, next), p :: rest ->
        catch
          (fun () -> f last p)
          (function
            | Failure msg -> Lwt.fail (Bad_argument (i, p, msg))
            | exn -> Lwt.fail exn) >>= fun v ->
        exec (succ i) next (cb v) rest
    | _ -> Lwt.fail Command_not_found
  in exec 1 params handler args

(* Command dispatch tree *)
type ('arg, 'ret) level =
  { stop : ('arg, 'ret) command option ;
    prefix : (string * ('arg, 'ret) tree) list }
and ('arg, 'ret) param_level =
  { stop : ('arg, 'ret) command option ;
    tree : ('arg, 'ret) tree }
and ('arg, 'ret) tree =
  | TPrefix of ('arg, 'ret) level
  | TParam of ('arg, 'ret) param_level
  | TStop of ('arg, 'ret) command
  | TMore of ('arg, 'ret) command
  | TEmpty

let insert_in_dispatch_tree
    (type arg) (type ret)
    root (Command { params } as command) =
  let rec insert_tree
    : type a. (arg, ret) tree -> (a, arg, ret) params -> (arg, ret) tree
    = fun t c -> match t, c with
      | TEmpty, Stop -> TStop command
      | TEmpty, More -> TMore command
      | TEmpty, Seq _ -> TMore command
      | TEmpty, Param (_, _, _, next) ->
          TParam { tree = insert_tree TEmpty next ; stop = None }
      | TEmpty, Prefix (n, next) ->
          TPrefix { stop = None ; prefix = [ (n, insert_tree TEmpty next) ] }
      | TStop command, Param (_, _, _, next) ->
          TParam { tree = insert_tree TEmpty next ; stop = Some command }
      | TStop command, Prefix (n, next) ->
          TPrefix { stop = Some command ;
                    prefix = [ (n, insert_tree TEmpty next) ] }
      | TParam t, Param (_, _, _, next) ->
          TParam { t with tree = insert_tree t.tree next }
      | TPrefix ({ prefix } as l), Prefix (n, next) ->
          let rec insert_prefix = function
            | [] -> [ (n, insert_tree TEmpty next) ]
            | (n', t) :: rest when n = n' -> (n, insert_tree t next) :: rest
            | item :: rest -> item :: insert_prefix rest in
          TPrefix { l with prefix = insert_prefix prefix }
      | TPrefix ({ stop = None } as l), Stop ->
          TPrefix { l with stop = Some command }
      | TParam ({ stop = None } as l), Stop ->
          TParam { l with stop = Some command }
      | _, _ ->
          Pervasives.failwith
            "Cli_entries.Command_tree.insert: conflicting commands" in
  insert_tree root params

let make_dispatch_tree commands =
  List.fold_left insert_in_dispatch_tree TEmpty commands

let tree_dispatch tree last args =
  let rec loop = function
    | TStop c, [] -> exec c last args
    | TPrefix { stop = Some c }, [] -> exec c last args
    | TMore c, _ -> exec c last args
    | TPrefix { prefix }, n :: rest ->
        begin try
            let t = List.assoc n prefix in
            loop (t, rest)
          with Not_found -> Lwt.fail Command_not_found end
    | TParam { tree }, _ :: rest ->
        loop (tree, rest)
    | _, _ -> Lwt.fail Command_not_found
  in
  loop (tree, args)

let inline_tree_dispatch tree () =
  let state = ref (tree, []) in
  fun arg -> match !state, arg with
    | (( TStop c |
         TMore c |
         TPrefix { stop = Some c } |
         TParam { stop = Some c}), acc),
      `End ->
        state := (TEmpty, []) ;
        `Res (fun last -> exec c last (List.rev acc))
    | (TMore c, acc), `Arg n ->
        state := (TMore c, n :: acc) ;
        `Nop
    | (TPrefix { prefix }, acc), `Arg n ->
        begin try
            let t = List.assoc n prefix in
            state := (t, n :: acc) ;
            begin match t with
              | TStop (Command { args })
              | TMore (Command { args }) -> `Args args
              | _ -> `Nop end
          with Not_found -> `Fail Command_not_found end
    | (TParam { tree }, acc), `Arg n ->
        state := (tree, n :: acc) ;
        begin match tree with
          | TStop (Command { args })
          | TMore (Command { args }) -> `Args args
          | _ -> `Nop end
    | _, _ -> `Fail Command_not_found

(* Try a list of commands on a list of arguments *)
let dispatch commands =
  let tree = make_dispatch_tree commands in
  tree_dispatch tree

(* Argument-by-argument dispatcher to be used during argument parsing *)
let inline_dispatch commands =
  let tree = make_dispatch_tree commands in
  inline_tree_dispatch tree

(* Command line help for a set of commands *)
let usage
    (type arg) (type ret)
    ~commands options =
  let trim s = (* config-file wokaround *)
    Utils.split '\n' s |>
    List.map String.trim |>
    String.concat "\n" in
  let rec help
    : type a. Format.formatter -> (a, arg, ret) params -> unit
    = fun ppf -> function
    | Stop -> ()
    | More -> Format.fprintf ppf "..."
    | Seq (n, "", _) -> Format.fprintf ppf "[ (%s) ...]" n
    | Seq (_, desc, _) -> Format.fprintf ppf "[ (%s) ... ]" desc
    | Prefix (n, Stop) -> Format.fprintf ppf "%s" n
    | Param (n, "", _, Stop) -> Format.fprintf ppf "(%s)" n
    | Param (_, desc, _, Stop) -> Format.fprintf ppf "(%s)" desc
    | Prefix (n, next) -> Format.fprintf ppf "%s %a" n help next
    | Param (n, "", _, next) -> Format.fprintf ppf "(%s) %a" n help next
    | Param (_, desc, _, next) -> Format.fprintf ppf "(%s) %a" desc help next in
  let rec help_sum
    : type a. Format.formatter -> (a, arg, ret) params -> unit
    = fun ppf -> function
    | Stop -> ()
    | More -> Format.fprintf ppf "..."
    | Seq (n, _, _) -> Format.fprintf ppf "[ (%s) ... ]" n
    | Prefix (n, Stop) -> Format.fprintf ppf "%s" n
    | Param (n, _, _, Stop) -> Format.fprintf ppf "(%s)" n
    | Prefix (n, next) -> Format.fprintf ppf "%s %a" n help_sum next
    | Param (n, _, _, next) -> Format.fprintf ppf "(%s) %a" n help_sum next in
  let rec help_args
    : type a. Format.formatter -> (a, arg, ret) params -> unit
    = fun ppf -> function
      | Stop -> ()
      | More -> Format.fprintf ppf "..."
      | Seq (n, desc, _) ->
          Format.fprintf ppf "(%s): @[<hov>%a@]"
            n Format.pp_print_text (trim desc)
      | Prefix (_, next) -> help_args ppf next
      | Param (n, desc, _, Stop) ->
          Format.fprintf ppf "(%s): @[<hov>%a@]"
            n Format.pp_print_text (trim desc)
      | Param (n, desc, _, next) ->
          Format.fprintf ppf "(%s): @[<hov>%a@]@,%a"
            n Format.pp_print_text (trim desc) help_args next in
  let option_help ppf (n, opt, desc) =
    Format.fprintf ppf "%s%s" n
      Arg.(let rec example opt = match opt with
          | Unit _ -> ""
          | Bool _ -> " <true | false>"
          | Set _ -> ""
          | Clear _ -> ""
          | String _ -> " <text>"
          | Set_string _ -> " <text>"
          | Int _ -> " <integer>"
          | Set_int _ -> " <integer>"
          | Float _ -> " <number>"
          | Set_float _ -> " <number>"
          | Tuple opts -> List.map example opts |> String.concat ""
          | Symbol (syms, _) -> " <" ^ String.concat " | " syms ^ ">"
          | Rest _ -> "" in example opt) ;
    if desc <> "" then
      Format.fprintf ppf "@,  @[<hov>%a@]" Format.pp_print_text (trim desc) in
  let command_help ppf (Command { params ; desc ; args }) =
    let small = Format.asprintf "@[<h>%a@]" help params in
    let desc = trim desc in
    if String.length small < 50 then begin
      Format.fprintf ppf "@[<v 2>%s@,@[<hov>%a@]"
        small Format.pp_print_text desc
    end else begin
      Format.fprintf ppf "@[<v 2>%a@,@[<hov 0>%a@]@,%a"
        help_sum params
        Format.pp_print_text desc
        help_args params ;
    end ;
    if args = [] then
      Format.fprintf ppf "@]"
    else
      Format.fprintf ppf "@,%a@]"
        (Format.pp_print_list option_help)
        args in
  let rec group_help ppf ({ title }, commands) =
    Format.fprintf ppf "@[<v 2>%s:@,%a@]"
      title
      (Format.pp_print_list command_help) commands in
  let usage ppf (by_group, options) =
    Format.fprintf ppf
      "@[<v>@[<v 2>Usage:@,%s [ options ] command [ command options ]@]@,\
       @[<v 2>Options:@,%a@]@,\
       %a@]"
      Sys.argv.(0)
      (Format.pp_print_list option_help) options
      (Format.pp_print_list group_help) by_group in
  let by_group =
    let ungrouped = ref [] in
    let grouped =
      List.fold_left
        (fun acc (Command { group } as command) ->
           match group with
           | None ->
               ungrouped := command :: !ungrouped ;
               acc
           | Some group ->
               try
                 let ({ title }, r) =
                   List.find (fun ({ name }, _) -> group.name = name) acc in
                 if title <> group.title then
                   invalid_arg "Cli_entries.usage: duplicate group name" ;
                 r := command :: !r ;
                 acc
               with Not_found ->
                 (group, ref [ command ]) :: acc)
        [] commands in
    let misc = match !ungrouped with
      | [] -> []
      | l ->
          [ { name = "untitled" ; title = "Miscellaneous commands" },
            List.rev l ]
    in
    List.map (fun (g, c) -> (g, List.rev !c)) grouped @ misc in
  Format.asprintf "%a" usage (by_group, options)
