open Error_monad

type output_format =
  | Rows of { separator : string ; escape : [ `No | `OCaml ] }

let rows separator escape = Rows { separator ; escape }

let tsv = rows "\t" `No

let csv = rows "," `OCaml

let clic_arg () =
  let open Clic in
  arg ~doc:"Make the output script-friendly" ~long:"for-script"
    ~placeholder:"FORMAT"
    (parameter (fun _ spec ->
         match String.lowercase_ascii spec with
         | "tsv" -> return tsv
         | "csv" -> return csv
         | other ->
             failwith
               "Cannot recognize format %S, please try 'TSV' or 'CSV'" other))


let fprintf_lwt chan fmt =
  Format.kasprintf
    (fun s ->
       protect (fun () -> Lwt_io.write chan s >>= fun () -> return_unit))
    fmt

let output ?(channel = Lwt_io.stdout) how_option ~for_human ~for_script =
  match how_option with
  | None -> for_human ()
  | Some (Rows { separator ; escape }) ->
      let open Format in
      iter_s
        (fun row ->
           fprintf_lwt channel "%a@."
             (pp_print_list
                ~pp_sep:(fun fmt () -> pp_print_string fmt separator)
                (fun fmt cell ->
                   match escape with
                   | `OCaml -> fprintf fmt "%S" cell
                   | `No -> pp_print_string fmt cell))
             row)
        (for_script ())
      >>=? fun () ->
      protect (fun () -> Lwt_io.flush channel >>= fun () -> return_unit)

let output_for_human how_option for_human =
  output how_option ~for_human ~for_script:(fun () -> [])

let output_row ?channel how_option ~for_human ~for_script =
  output ?channel how_option ~for_human
    ~for_script:(fun () -> [for_script ()])

