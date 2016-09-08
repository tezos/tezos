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

(* A simple structure for command interpreters. *)
type 'a params =
  | Prefix : string * 'a params -> 'a params
  | Param : string * string * (string -> 'p Lwt.t) * 'a params -> ('p -> 'a) params
  | Stop : (unit -> unit Lwt.t) params
  | More : (string list -> unit Lwt.t) params
  | Seq : string * string * (string -> 'p Lwt.t) -> ('p list -> unit Lwt.t) params

(* A command wraps a callback with its type and info *)
and command =
  | Command
    : 'a params * 'a *
      desc option * tag list * group option *
      (Arg.key * Arg.spec * Arg.doc) list
    -> command

and desc = string
and group = string
and tag = string

(* Associates group names with group titles *)
let groups : (group * string) list ref = ref []
let register_group group title =
  try ignore @@ List.assoc group !groups with
  | Not_found -> groups := (group, title) :: !groups
let group_title group =
  try List.assoc group !groups with
  | Not_found -> group

(* Associates tag names with tag descriptions *)
let tags : (tag * string) list ref = ref []
let register_tag tag title =
  try ignore @@ List.assoc tag !tags with
  | Not_found -> tags := (tag, title) :: !tags
let tag_description tag =
  try List.assoc tag !tags with
  | Not_found -> "undocumented tag"

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
let command ?desc ?(tags = []) ?group ?(args = []) params cb =
  Command (params, cb, desc,tags, group, args)

(* Param combinators *)
let string n desc next = param n desc (fun s -> return s) next

(* Error combinators for use in commands *)
let kasprintf cont fmt =
  let buffer = Buffer.create 100 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.kfprintf (fun ppf ->
      Format.fprintf ppf "%!";
      cont (Buffer.contents buffer))
    ppf fmt
let error fmt = kasprintf (fun msg -> Lwt.fail (Command_failed msg)) fmt
let message fmt = kasprintf (Format.eprintf "%s\n%!") fmt
let answer fmt = kasprintf (Format.printf "%s\n%!") fmt
let param_error fmt = kasprintf (fun msg -> Lwt.fail (Failure msg)) fmt

(* Command execution *)
let exec (Command (params, cb, _, _, _, _)) args =
  let rec exec
    : type a. int -> a params -> a -> string list -> unit Lwt.t = fun i params cb args ->
    match params, args with
    | Stop, [] -> cb ()
    | Stop, _ -> Lwt.fail Command_not_found
    | Seq (_, _, f), seq ->
        let rec do_seq i acc = function
          | [] -> Lwt.return (List.rev acc)
          | p :: rest ->
              catch
                (fun () -> f p)
                (function
                  | Failure msg -> Lwt.fail (Bad_argument (i, p, msg))
                  | exn -> Lwt.fail exn) >>= fun v ->
              do_seq (succ i) (v :: acc) rest in
        do_seq i [] seq >>= fun parsed ->
        cb parsed
    | More, rest -> cb rest
    | Prefix (n, next), p :: rest when n = p ->
        exec (succ i) next cb rest
    | Param (_, _, f, next), p :: rest ->
        catch
          (fun () -> f p)
          (function
            | Failure msg -> Lwt.fail (Bad_argument (i, p, msg))
            | exn -> Lwt.fail exn) >>= fun v ->
        exec (succ i) next (cb v) rest
    | _ -> Lwt.fail Command_not_found
  in exec 1 params cb args

module Command_tree = struct
  type level =
    { stop : command option ;
      prefix : (string * tree) list }
  and param_level =
    { stop : command option ;
      tree : tree }
  and tree =
    | TPrefix of level
    | TParam of param_level
    | TStop of command
    | TMore of command
    | TEmpty
  let insert root (Command (params, _, _, _, _, _) as command) =
    let rec insert_tree
      : type a. tree -> a params -> tree
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
  let make commands =
    List.fold_left insert TEmpty commands
  let dispatcher tree args =
    let rec loop = function
      | TStop c, [] -> exec c args
      | TPrefix { stop = Some c }, [] -> exec c args
      | TMore c, _ -> exec c args
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
  let inline_dispatcher tree () =
    let state = ref (tree, []) in
    fun arg -> match !state, arg with
      | (( TStop c |
           TMore c |
           TPrefix { stop = Some c } |
           TParam { stop = Some c}), acc),
        `End ->
          state := (TEmpty, []) ;
          `Res (exec c (List.rev acc))
      | (TMore c, acc), `Arg n ->
          state := (TMore c, n :: acc) ;
          `Nop
      | (TPrefix { prefix }, acc), `Arg n ->
          begin try
              let t = List.assoc n prefix in
              state := (t, n :: acc) ;
              begin match t with
                | TStop (Command (_, _, _, _, _, args))
                | TMore (Command (_, _, _, _, _, args)) -> `Args args
                | _ -> `Nop end
            with Not_found -> `Fail Command_not_found end
      | (TParam { tree }, acc), `Arg n ->
          state := (tree, n :: acc) ;
          begin match tree with
            | TStop (Command (_, _, _, _, _, args))
            | TMore (Command (_, _, _, _, _, args)) -> `Args args
            | _ -> `Nop end
      | _, _ -> `Fail Command_not_found
end

(* Try a list of commands on a list of arguments *)
let dispatcher commands =
  let tree = Command_tree.make commands in
  fun args -> Command_tree.dispatcher tree args

(* Argument-by-argument dispatcher to be used during argument parsing *)
let inline_dispatcher commands =
  let tree = Command_tree.make commands in
  Command_tree.inline_dispatcher tree

(* Command line help for a set of commands *)
let usage commands options =
  let trim s = (* config-file wokaround *)
    Utils.split '\n' s |>
    List.map String.trim |>
    String.concat "\n" in
  let rec help : type a. Format.formatter -> a params -> unit = fun ppf -> function
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
  let rec help_sum : type a. Format.formatter -> a params -> unit = fun ppf -> function
    | Stop -> ()
    | More -> Format.fprintf ppf "..."
    | Seq (n, _, _) -> Format.fprintf ppf "[ (%s) ... ]" n
    | Prefix (n, Stop) -> Format.fprintf ppf "%s" n
    | Param (n, _, _, Stop) -> Format.fprintf ppf "(%s)" n
    | Prefix (n, next) -> Format.fprintf ppf "%s %a" n help_sum next
    | Param (n, _, _, next) -> Format.fprintf ppf "(%s) %a" n help_sum next in
  let rec help_args : type a. Format.formatter -> a params -> unit = fun ppf -> function
    | Stop -> ()
    | More -> Format.fprintf ppf "..."
    | Seq (n, desc, _) -> Format.fprintf ppf "(%s): @[<hov>%a@]" n Format.pp_print_text (trim desc)
    | Prefix (_, next) -> help_args ppf next
    | Param (n, desc, _, Stop) -> Format.fprintf ppf "(%s): @[<hov>%a@]" n Format.pp_print_text (trim desc)
    | Param (n, desc, _, next) -> Format.fprintf ppf "(%s): @[<hov>%a@]@,%a" n Format.pp_print_text (trim desc) help_args next in
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
  let command_help ppf (Command (p, _, desc, _, _, options)) =
    let small = Format.asprintf "@[<h>%a@]" help p in
    if String.length small < 50 then begin
      Format.fprintf ppf "@[<v 2>%s@,@[<hov>%a@]"
        small
        Format.pp_print_text (match desc with None -> "undocumented command" | Some desc -> trim desc)
    end else begin
      Format.fprintf ppf "@[<v 2>%a@,@[<hov 0>%a@]@,%a"
        help_sum p
        Format.pp_print_text (match desc with None -> "undocumented command" | Some desc -> trim desc)
        help_args p ;
    end ;
    if options = [] then
      Format.fprintf ppf "@]"
    else
      Format.fprintf ppf "@,%a@]" (Format.pp_print_list option_help) options in
  let rec group_help ppf (n, commands) =
    Format.fprintf ppf "@[<v 2>%s:@,%a@]"
      (match n with None -> "Miscellaneous commands" | Some n -> group_title n)
      (Format.pp_print_list command_help) !commands in
  let usage ppf (by_group, options) =
    Format.fprintf ppf
      "@[<v>@[<v 2>Usage:@,%s [ options ] command [ command options ]@]@,@[<v 2>Options:@,%a@]@,%a@]"
      Sys.argv.(0)
      (Format.pp_print_list option_help) options
      (Format.pp_print_list group_help) by_group in
  let by_group =
    List.fold_left
      (fun acc (Command (_, _, _, _, g, _) as c) ->
         try
           let r = List.assoc g acc in
           r := c :: !r ;
           acc
         with Not_found ->
           (g, ref [ c ]) :: acc)
      [] commands |> List.sort compare in
  Format.asprintf "%a" usage (by_group, options)
