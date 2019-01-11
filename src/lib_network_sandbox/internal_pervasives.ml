(** Local “Pervasives” module for flextesa.

See also ["docs/tutorials/flextesa.rst"]. *)

module List = Base.List
module String = Base.String
module Option = Base.Option
module Int = Base.Int
module Float = Base.Float
module Exn = Base.Exn

let ( // ) = Filename.concat
let ksprintf, sprintf = Printf.(ksprintf, sprintf)

(** Wrapper around the [EasyFormat] library to use for console display. *)
module EF = struct
  type t = Easy_format.t

  open Easy_format

  let default_list = list
  let default_atom = atom
  let default_label = label
  let atom ?(param = default_atom) s = Atom (s, param)
  let label ?(param = label) a b = Label ((a, param), b)

  let list ?(delimiters = ("", "")) ?(sep = "") ?(param = default_list) l =
    List ((fst delimiters, sep, snd delimiters, param), l)

  let ocaml_list = list ~delimiters:("[", "]") ~sep:";"

  let ocaml_tuple =
    list ~delimiters:("(", ")") ~sep:","
      ~param:
        { default_list with
          space_after_opening= false; space_before_closing= false }

  let shout = atom ~param:{atom_style= Some "shout"}
  let prompt = atom ~param:{atom_style= Some "prompt"}
  let highlight = atom ~param:{atom_style= Some "prompt"}
  let custom pr = Custom pr
  let pr f = custom (fun ppf -> f (Format.fprintf ppf))
  let desc_list s l = label s (list ~sep:"," l)
  let desc s v = label s v
  let af ?param fmt = Format.kasprintf (atom ?param) fmt

  let wrap s =
    String.split ~on:' ' s |> List.map ~f:String.strip
    |> List.filter ~f:(( <> ) "")
    |> List.map ~f:atom |> list

  let wf fmt = Format.kasprintf wrap fmt
  let haf fmt = Format.kasprintf highlight fmt
  let opt f = function None -> atom "-" | Some o -> f o
  let ocaml_string_list l = ocaml_list (ListLabels.map l ~f:(af "%S"))
  let exn e = wf "%a" Exn.pp e

  let markdown_verbatim ?(guard_length = 80) s =
    let guard = String.make guard_length '`' in
    af "\n%s\n%s\n%s@." guard s guard

  let ef_json msg json =
    desc (haf "%s" msg) (markdown_verbatim Ezjsonm.(to_string (wrap json)))
end

(** Debug-display module (non-cooperative output to [stderr]). *)
module Dbg = struct
  let e ef =
    EF.(
      list ~delimiters:("<DBG|", "|DBG>") ~sep:""
        ~param:
          { default_list with
            separator_style= Some "debug"
          ; align_closing= true
          ; space_after_opening= true
          ; space_before_closing= true }
        [ef]
      |> Easy_format.Pretty.to_stderr) ;
    Printf.eprintf "\n%!"

  let i (e : EF.t) = ignore e
  let f f = e (EF.pr f)
  let any v = Dum.to_eformat v
  let pp_any fmt v = Dum.to_formatter fmt v
end

(** An “typed error type” based on polymorphic variants *)
module Error = struct
  type +'a t =
    {error_value: 'a; attachments: (string * string) list}
    constraint 'a = [> ]

  let make ?(attach = []) error_value = {error_value; attachments= attach}

  let pp ~error fmt {error_value; attachments} =
    EF.(
      label (shout "Error: ")
        (list
           [ custom (fun fmt -> error fmt error_value)
           ; ocaml_list
               (List.map attachments ~f:(fun (k, v) ->
                    ocaml_tuple [atom k; atom v] )) ])
      |> Easy_format.Pretty.to_formatter fmt)
end

(** A wrapper around [('ok, 'a Error.t) result Lwt.t]. *)
module Asynchronous_result = struct
  type ('ok, 'a) t = ('ok, 'a Error.t) result Lwt.t

  let return o : (_, _) t = Lwt.return (Ok o)

  let yield () =
    (* https://github.com/ocsigen/lwt/issues/631 *)
    if false then Lwt_unix.auto_yield 0.005 () else Lwt_main.yield ()

  let fail ?attach error_value : (_, _) t =
    Lwt.return (Error (Error.make ?attach error_value))

  let bind (o : (_, _) t) f : (_, _) t =
    let open Lwt.Infix in
    o
    >>= function
    | Ok o -> yield () >>= fun () -> f o | Error _ as e -> Lwt.return e

  let bind_on_error (o : (_, _) t) ~f : (_, _) t =
    Lwt.bind o (function Ok o -> return o | Error e -> f e)

  let transform_error o ~f =
    Lwt.bind o (function
      | Ok o -> return o
      | Error {Error.error_value; attachments} -> f error_value attachments )

  let bind_on_result :
         ('ok, 'error) t
      -> f:(('ok, 'error Error.t) result -> ('ok2, 'error2) t)
      -> ('ok2, 'error2) t =
   fun o ~f -> Lwt.bind o f

  (** The module opened everywhere. *)
  module Std = struct let ( >>= ) = bind let return = return let fail = fail
  end

  open Std

  let run r on_error =
    match Lwt_main.run (r ()) with Ok o -> o | Error e -> on_error e

  let die n = fail (`Die n)

  module List_sequential = struct
    let iter l ~f =
      List.fold l ~init:(return ()) ~f:(fun pm x ->
          pm >>= fun () -> (f x : (_, _) t) )

    let iteri l ~f =
      List.fold l ~init:(return 0) ~f:(fun pm x ->
          pm >>= fun n -> (f n x : (_, _) t) >>= fun () -> return (n + 1) )
      >>= fun _ -> return ()
  end

  let map_option o ~f =
    match o with
    | None -> return None
    | Some s -> f s >>= fun o -> return (Some o)

  module Loop = struct
    let n_times times f =
      let rec loop n =
        match n with
        | n when n <= 0 -> return ()
        | n -> f (1 + times - n) >>= fun () -> loop (n - 1)
      in
      loop times
  end

  let run_application r =
    match Lwt_main.run (r ()) with
    | Ok () -> exit 0
    | Error {Error.error_value= `Die ret; _} -> exit ret
end

include Asynchronous_result.Std
module List_sequential = Asynchronous_result.List_sequential
module Loop = Asynchronous_result.Loop

module Lwt_exception = struct
  let fail ?attach (e : exn) = fail ?attach (`Lwt_exn e)

  let catch ?attach f x =
    Lwt.catch
      (fun () -> Lwt.bind (f x) @@ fun r -> return r)
      (fun exn -> fail ?attach exn)

  let pp fmt (`Lwt_exn e) =
    Format.fprintf fmt "LWT-Exception:@ %s" (Printexc.to_string e)
end

module System_error = struct
  let fail ?attach fmt = ksprintf (fun e -> fail ?attach (`Sys_error e)) fmt
  let pp fmt (`Sys_error e) = Format.fprintf fmt "System-error:@ %s" e
end

(** A wrapper around a structural type describing the result of
    external processes. *)
module Process_result = struct
  type t =
    < err: string list ; out: string list ; status: Unix.process_status >

  let status_to_string s =
    Lwt_unix.(
      match s with
      | WEXITED n -> sprintf "exited with %d" n
      | WSIGNALED n -> sprintf "was signaled: %d" n
      | WSTOPPED n -> sprintf "was stopped: %d" n)

  module Error = struct
    type output = t
    type t = [`Wrong_status of output * string]

    let wrong_status (res : output) msgf =
      ksprintf (fun msg -> fail (`Wrong_status (res, msg))) msgf

    let pp fmt = function
      | (`Wrong_status (res, msg) : [< t]) ->
          Format.fprintf fmt "Process-error, wrong status: '%s': %s"
            (status_to_string res#status)
            msg

    let fail_if_non_zero (res : output) msg =
      if res#status <> Unix.WEXITED 0 then
        wrong_status res "Non-zero exit status: %s" msg
      else return ()
  end
end

(** The state within this library is packed into an open object
    (structural) type, this module just defines the [application_name]
    method. *)
module Base_state = struct
  type base = < application_name: string >
  type 'a t = 'a constraint 'a = < base ; .. >
end

(** Some {!Lwt_unix} functions. *)
module System = struct let sleep f = Lwt_exception.catch Lwt_unix.sleep f
end

(** WIP [jq]-like manipulation in pure OCaml. *)
module Jqo = struct
  let of_string s = Ezjsonm.from_string s
  let to_string j = Ezjsonm.(to_string (wrap j))

  let field ~k = function
    | `O l -> List.Assoc.find_exn l ~equal:String.equal k
    | other -> ksprintf failwith "Jqo.field (%S) in %s" k (to_string other)

  let list_find ~f = function
    | `O l ->
        List.find_map_exn ~f:(fun (_, j) -> if f j then Some j else None) l
    | `A l -> List.find_exn ~f l
    | other -> ksprintf failwith "Jqo.list_find in %s" (to_string other)

  let list_exists ~f o =
    match list_find o ~f with _ -> true | exception _ -> false

  let remove_field o ~name =
    match o with
    | `O l -> `O (List.filter l ~f:(fun (k, _) -> k <> name))
    | other ->
        ksprintf failwith "Jqo.remove_field %S: No an object: %s" name
          (to_string other)
end
