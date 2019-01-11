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

module Configuration = struct
  type t = { activate : Uri.t list }

  let default =
    { activate = [
          Uri.make ~scheme:Internal_event.Lwt_log_sink.uri_scheme ()
        ] }

  let encoding =
    let open Data_encoding in
    conv
      (fun { activate } -> List.map Uri.to_string activate)
      (fun activate -> { activate = List.map Uri.of_string activate })
      (obj1
         (dft "activate"
            ~description: "List of URIs to activate/configure sinks."
            (list string) []))

  let of_file path =
    Lwt_utils_unix.Json.read_file path >>=? fun json ->
    protect (fun () -> return (Data_encoding.Json.destruct encoding json))

  let apply { activate } =
    List.fold_left
      (fun prev uri ->
         prev >>=? fun () ->
         Internal_event.All_sinks.activate uri)
      return_unit
      activate
end

let env_var_name = "TEZOS_EVENTS_CONFIG"

let init ?lwt_log_sink ?(configuration = Configuration.default) () =
  Lwt_log_sink_unix.initialize ?cfg:lwt_log_sink ()
  >>= fun () ->
  begin
    begin match Sys.(getenv_opt env_var_name) with
      | None ->
          return_unit
      | Some s ->
          let uris =
            String.split ' ' s
            |> List.map (String.split '\n') |> List.concat
            |> List.map (String.split '\t') |> List.concat
            |> List.filter ((<>) "")
            |> List.map Uri.of_string in
          List.fold_left
            (fun prev uri ->
               prev >>=? fun () ->
               match Uri.scheme uri with
               | None ->
                   Configuration.of_file (Uri.path uri) >>=? fun cfg ->
                   Configuration.apply cfg
               | Some _ ->
                   Internal_event.All_sinks.activate uri)
            return_unit
            uris >>=? fun () ->
          Internal_event.Debug_event.(
            emit (make "Loaded URIs from environment"
                    ~attach:(`O [ "variable", `String env_var_name ;
                                  "value", `String s ])))
    end >>=? fun () ->
    Configuration.apply configuration
  end
  >>= function
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf Lwt.fail_with
        "ERROR@ Initializing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_error el

let close () =
  Internal_event.All_sinks.close ()
  >>= function
  | Ok () -> Lwt.return_unit
  | Error el ->
      Format.kasprintf Lwt.fail_with
        "ERROR@ closing Internal_event_unix:@ %a\n%!"
        Error_monad.pp_print_error el
