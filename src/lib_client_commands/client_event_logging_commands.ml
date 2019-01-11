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

let group =
  Clic.{ name = "event-logging-framework" ;
         title = "Commands to inspect the event-logging framework" }

let date_parameter option_name build =
  let open Clic in
  parameter (fun _ s ->
      let problem fmt = Printf.ksprintf invalid_arg fmt in
      try
        if String.length s <> 8 then problem "date should be `YYYYMMDD`" ;
        String.iteri
          (fun idx -> function
             | '0' .. '9' -> ()
             | other ->
                 problem "character %d is not a digit: '%c'." idx other)
          s ;
        let month = int_of_string (String.sub s 4 2) - 1 in
        if month < 0 then problem "The month cannot be '00'" ;
        if month > 11 then problem "The month cannot be more than '12'" ;
        let day = int_of_string (String.sub s 6 2) in
        if day > 31 then  problem "The month cannot be more than '31'" ;
        let t =
          let tm =
            Unix.{ 
              tm_sec = 0 ;
              tm_min = 0 ;
              tm_hour = 0 ;
              tm_mday = day ;
              tm_mon = month;
              tm_year = int_of_string (String.sub s 0 4) - 1900;
              tm_wday = 0;
              tm_yday  = 0;
              tm_isdst = false } in
          Unix.mktime tm |> fst in
        return (build t)
      with
      | Invalid_argument e -> failwith "In `%s %S`, %s" option_name s e
      | e -> failwith "Exn: %a" pp_exn e)

let flat_pp pp o =
  Format.(
    asprintf "%a" (fun fmt () ->
        pp_set_margin fmt 2_000_000 ;
        pp fmt o) ())

let commands () =
  let open Clic in
  let command ~desc = command ~group ~desc in
  [
    command
      ~desc:"Query the events from an event sink."
      (args7
         (arg
            ~doc:"Filter on event names"
            ~long:"names"
            ~placeholder:"LIST"
            (parameter (fun _ s ->
                 try return (String.split_on_char ',' s)
                 with _ -> failwith "List of names cannot be parsed")))
         (arg
            ~doc:"Filter on event sections (use '_' for no-section)"
            ~long:"sections"
            ~placeholder:"LIST"
            (parameter (fun _ s ->
                 try return (
                     String.split_on_char ',' s
                     |> List.map (function "_" -> None | other -> Some other))
                 with _ -> failwith "List of sections cannot be parsed")))
         (arg
            ~doc:"Filter out events before DATE"
            ~long:"since"
            ~placeholder:"DATE"
            (date_parameter "--since" (fun s -> (`Date (`Ge, s)))))
         (arg
            ~doc:"Filter out events after DATE"
            ~long:"until"
            ~placeholder:"DATE"
            (date_parameter "--until" (fun s -> (`Date (`Le, s)))))
         (switch
            ~doc:"Display events as JSON instead of pretty-printing them"
            ~long:"as-json"
            ())
         (switch
            ~doc:"Try to display unknown events"
            ~long:"dump-unknown"
            ())
         (Scriptable.clic_arg ())
      )
      (prefixes [ "query" ; "events" ; "from" ]
       @@ (param
             ~name:"Sink-Name"
             ~desc:"The URI of the SINK to query"
             (parameter (fun _ s ->
                  try return (Uri.of_string s)
                  with _ -> failwith "Uri cannot be parsed")))

       @@ stop)
      (fun
        (only_names, only_sections,
         since, until, as_json, dump_unknown, scriptable)
        uri
        (cctxt : #Client_context.full) ->
        let open Tezos_stdlib_unix in
        begin match Uri.scheme uri with
          | None | Some "unix-files" ->
              let script_row kind date evname data () =
                [kind; date; evname; data] in
              Scriptable.output_for_human scriptable (fun () ->
                  cctxt#message "### Events" >>= fun () ->
                  return_unit)
              >>=? fun () ->
              let on_unknown =
                if not dump_unknown then None else Some (fun path ->
                    Scriptable.output_row scriptable
                      ~for_human:(fun () ->
                          cctxt#message "Unknown: %s" path
                          >>= fun () ->
                          Lwt_stream.iter_s
                            (fun line -> cctxt#message "    |%s" line)
                            (Lwt_io.lines_of_file path)
                          >>= fun () ->
                          return_unit)
                      ~for_script:(script_row "unknown-event" "-" "-" path))
              in
              let time_query =
                match since, until with
                | None, None -> None
                | Some a, None | None, Some a -> Some a
                | Some a, Some b -> Some (`And (a, b)) in
              File_event_sink.Query.fold ?only_names ?on_unknown ?only_sections
                ?time_query uri ~init:()
                ~f:(fun () ~time_stamp ev ->
                    let o =
                      Internal_event.Generic.explode_event ev in
                    let time_string time_value =
                      let open Unix in
                      let tm = gmtime time_value in
                      Printf.sprintf "%04d%02d%02d-%02d%02d%02d-%04d"
                        (1900 + tm.tm_year)
                        (tm.tm_mon + 1) tm.tm_mday
                        tm.tm_hour tm.tm_min tm.tm_sec
                        ((time_value -. floor time_value)
                         *. 10_000. |> int_of_float)
                    in
                    let pp fmt o =
                      if as_json
                      then Data_encoding.Json.pp fmt o#json
                      else o#pp fmt () in
                    Scriptable.output_row scriptable
                      ~for_human:(fun () ->
                          cctxt#message "@[<2>* [%s %s]@ %a@]"
                            (time_string time_stamp) o#name pp o
                          >>= fun () ->
                          return_unit)
                      ~for_script:(fun () ->
                          let text = flat_pp pp o in
                          script_row "event" (time_string time_stamp) o#name text ()))
              >>=? begin function
                | ([], ()) -> return_unit
                | (errors_and_warnings, ()) ->
                    let open Format in
                    Scriptable.output scriptable
                      ~for_human:(fun () ->
                          cctxt#message
                            "### Some things were not perfect:@.@[<2>%a@]"
                            (pp_print_list
                               ~pp_sep:(fun fmt () -> fprintf fmt "@.")
                               (fun fmt item ->
                                  fprintf fmt "* %a"
                                    File_event_sink.Query.Report.pp item))
                            errors_and_warnings
                          >>= fun () ->
                          return_unit)
                      ~for_script:(fun () ->
                          let make_row e =
                            let text = flat_pp File_event_sink.Query.Report.pp e in
                            let tag =
                              match e with
                              | `Error _  ->  "error"
                              | `Warning _  ->  "warning" in
                            script_row tag "-" "-" text ()
                          in
                          List.map make_row errors_and_warnings)
              end
          | Some other ->
              cctxt#message "URI scheme %S not handled as of now." other
              >>= fun () ->
              return_unit
        end
      ) ;
    command
      ~desc:"Display configuration/state information about the \
             internal-event logging framework."
      no_options
      (prefixes [ "show" ; "event-logging" ] @@ stop)
      (fun () (cctxt : #Client_context.full) ->
         let pp_event_definitions fmt schs =
           let open Format in
           pp_open_box fmt 0 ;
           pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt "@;")
             (fun fmt obj_schema ->
                pp_open_box fmt 2 ;
                fprintf fmt "* `%s`:@ " obj_schema#name ;
                pp_print_text fmt obj_schema#doc ;
                pp_close_box fmt ())
             fmt
             schs;
           pp_close_box fmt ()
         in
         cctxt#message "Event logging framework:@.Sinks state:@ %a@.Events registered:@ %a"
           Internal_event.All_sinks.pp_state ()
           pp_event_definitions Internal_event.(
               All_definitions.get () |> List.map Generic.json_schema)
         >>= fun () ->
         return_unit
      ) ;
    command
      ~desc:"Output the JSON schema of an internal-event."
      no_options
      (prefixes [ "output" ; "schema" ; "of" ]
       @@ (param
             ~name:"Event-Name"
             ~desc:"Name of the event"
             (parameter (fun _ s -> return s)))
       @@ (prefix "to")
       @@ (param
             ~name:"File-path"
             ~desc:"Path to a JSON file"
             (parameter (fun _ s -> return s)))
       @@ stop)
      (fun () event path (cctxt : #Client_context.full) ->
         let open Internal_event in
         match All_definitions.find ((=) event) with
         | None ->
             failwith "Event %S not found" event
         | Some ev ->
             let o = Generic.json_schema ev in
             Lwt_io.with_file ~mode:Lwt_io.output path
               (fun chan ->
                  let v = Format.asprintf "%a" Json_schema.pp o#schema in
                  Lwt_io.write chan v)
             >>= fun () ->
             cctxt#message "Wrote schema of %s to %s" event path
             >>= fun () ->
             return_unit
      ) ;
  ]