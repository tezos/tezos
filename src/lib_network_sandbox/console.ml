open Internal_pervasives

type t =
  { color: bool
  ; buffer: Buffer.t
  ; channel: Lwt_io.output_channel
  ; with_timestamp: bool
  ; formatter: Format.formatter }

let make with_timestamp color =
  let channel = Lwt_io.stderr in
  let b = Buffer.create 42 in
  let formatter =
    Format.make_formatter (Buffer.add_substring b) (fun () -> ())
  in
  let bold = "\027[01m" in
  let red = "\027[31m" in
  let reset = "\027[m" in
  if color then (
    let color_of_tag = function
      | "prompt" -> Some bold
      | "shout" -> Some red
      | _ -> None
    in
    Format.(
      pp_set_formatter_tag_functions formatter
        { mark_open_tag= (fun _ -> "")
        ; mark_close_tag= (fun _ -> "")
        ; print_open_tag=
            (fun tag ->
              match color_of_tag tag with
              | Some c -> fprintf formatter "%s" c
              | None -> () )
        ; print_close_tag=
            (fun tag ->
              if color_of_tag tag <> None then fprintf formatter "%s" reset )
        } ;
      pp_set_tags formatter true) ) ;
  {color; buffer= b; channel; formatter; with_timestamp}

let pp fmt {color; _} = Format.fprintf fmt "@[<2>{Console:@ color: %b}@]" color

let cli_term () =
  let guess =
    let dumb =
      try match Sys.getenv "TERM" with "dumb" | "" -> true | _ -> false
      with Not_found -> true
    in
    let isatty = try Unix.(isatty stderr) with Unix.Unix_error _ -> false in
    if (not dumb) && isatty then true else false
  in
  Cmdliner.(
    Term.(
      pure make
      $ Arg.(
          value
            (flag
               (info ["with-timestamp"]
                  ~doc:"Display messages with time-stamps.")))
      $ Arg.(
          pure (function `Y -> true | `N -> false | `G -> guess)
          $
          let answers = [("none", `N); ("yes", `Y); ("auto", `G)] in
          let doc =
            sprintf "Control terminal colors (%s)."
              (String.concat ~sep:", " (List.map answers ~f:fst))
          in
          value & opt (enum answers) `G & info ["color"] ~doc)))

let do_output t =
  Lwt_exception.catch
    Lwt.(
      fun () ->
        Lwt_io.write t.channel (Buffer.contents t.buffer)
        >>= fun () -> Buffer.clear t.buffer ; return_unit)
    ()

let sayf (o : _ Base_state.t) (fmt : Format.formatter -> unit -> unit) :
    (_, _) Asynchronous_result.t =
  let date =
    if o#console.with_timestamp then
      let date = Tezos_stdlib_unix.Systime_os.now ()
                 |> Tezos_base.Time.System.to_notation in
      sprintf "[%s]" date
    else ""
  in
  let ppf = o#console.formatter in
  Format.(
    pp_open_hvbox ppf 2 ;
    pp_open_tag ppf "prompt" ;
    fprintf ppf "%s%s:" o#application_name date ;
    pp_close_tag ppf () ;
    pp_print_break ppf 2 0 ;
    fmt ppf () ;
    pp_print_newline ppf () ;
    pp_close_box ppf () ;
    pp_print_flush ppf ()) ;
  do_output o#console

let say (o : _ Base_state.t) ef : (_, _) Asynchronous_result.t =
  let date =
    if o#console.with_timestamp then
      let date = Tezos_stdlib_unix.Systime_os.now ()
                 |> Tezos_base.Time.System.to_notation in
      sprintf "[%s]" date
    else ""
  in
  let msg = EF.(label (ksprintf prompt "%s%s:" o#application_name date) ef) in
  let fmt = o#console.formatter in
  Format.(
    fprintf fmt "%a" Easy_format.Pretty.to_formatter msg ;
    pp_print_newline fmt () ;
    pp_print_flush fmt ()) ;
  do_output o#console

module Prompt = struct
  type item =
    { commands: string list
    ; doc: EF.t
    ; action:
           Base.Sexp.t list
        -> ( [`Help | `Quit | `Loop]
           , [`Lwt_exn of exn | `Command_line of string] )
           Asynchronous_result.t }

  let item doc commands action = {commands; doc; action}

  let quit ?(doc = EF.(af "Quit this prompt and continue")) commands =
    item doc commands (fun _ -> return `Quit)

  let help ?(doc = EF.(af "Display command help")) commands =
    item doc commands (fun _ -> return `Help)

  let unit_and_loop doc commands f =
    item doc commands (fun x -> f x >>= fun () -> return `Loop)

  let default_commands () = [quit ["q"; "quit"]; help ["h"; "help"]]

  let command ?(with_defaults = true) state ~commands =
    let commands =
      if with_defaults then default_commands () @ commands else commands
    in
    let rec loop () =
      say state EF.(af "Please enter command:")
      >>= fun () ->
      Lwt_exception.catch Lwt_io.read_line Lwt_io.stdin
      >>= fun line ->
      let open Base.Sexp in
      match Parsexp.Single.parse_string (sprintf "( %s )" line) with
      | Ok (List (Atom c :: more)) -> (
        match
          List.find commands ~f:(fun m ->
              List.mem m.commands c ~equal:String.equal )
        with
        | Some {action; _} -> (
            Asynchronous_result.bind_on_error (action more) ~f:(fun err ->
                say state
                  EF.(
                    desc (shout "Error in action:")
                      (custom (fun fmt ->
                           Error.pp fmt err ~error:(fun fmt -> function
                             | `Lwt_exn _ as e -> Lwt_exception.pp fmt e
                             | `Command_line s ->
                                 Format.fprintf fmt "Wrong command line: %s" s
                           ) )))
                >>= fun () -> return `Loop )
            >>= function
            | `Loop -> loop ()
            | `Help ->
                say state
                  EF.(
                    let cmdlist =
                      list ~sep:"|" ~delimiters:("[", "]")
                        ~param:
                          { default_list with
                            space_after_separator= false
                          ; space_before_closing= false
                          ; space_after_opening= false }
                    in
                    label (haf "Commands:")
                      (list
                         (List.map commands ~f:(fun {commands; doc; _} ->
                              label
                                ~param:
                                  {default_label with space_after_label= false}
                                (cmdlist (List.map ~f:(af "%S") commands))
                                (list [haf "->"; doc]) ))))
                >>= fun () -> loop ()
            | `Quit -> return () )
        | None ->
            say state
              EF.(
                desc
                  (ksprintf shout "Error, unknown command: %S" c)
                  (custom (fun fmt -> Base.Sexp.pp_hum_indent 4 fmt (List more))))
            >>= fun () -> loop () )
      | Ok other ->
          say state
            EF.(
              desc
                (shout "Error, cannot understand: ")
                (custom (fun fmt -> Base.Sexp.pp_hum_indent 4 fmt other)))
          >>= fun () -> loop ()
      | Error err ->
          say state
            EF.(
              desc (shout "Error: ")
                (custom (fun fmt ->
                     Parsexp.Parse_error.report fmt ~filename:"<command-line>"
                       err )))
          >>= fun () -> loop ()
    in
    loop ()
end

let display_errors_of_command state ?(should_output = false) cmd =
  let outputs () = List.exists cmd#out ~f:(fun s -> String.strip s <> "") in
  let success =
    let unix_success = cmd#status = Lwt_unix.WEXITED 0 in
    if should_output then unix_success && outputs () else unix_success
  in
  ( if success then return ()
  else
    say state
      EF.(
        let output l =
          match String.concat ~sep:"\n" l |> String.strip with
          | "" -> af "NONE"
          | more -> markdown_verbatim more
        in
        desc (shout "Error:")
          (list
             [ haf "Command %s" (Process_result.status_to_string cmd#status)
             ; desc (haf "Stdout:") (output cmd#out)
             ; desc (haf "Stderr:") (output cmd#err) ])) )
  >>= fun () -> return success
