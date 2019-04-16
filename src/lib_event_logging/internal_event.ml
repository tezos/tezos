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
module List = struct
  include List
  include Tezos_stdlib.TzList
end
module String = struct
  include String
  include Tezos_stdlib.TzString
end


let valid_char c =
  match c with
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z'
  | '@' | '-' | '_' | '+' | '=' | '~' -> true
  | _ -> false

let check_name_exn : string -> (string -> char -> exn) -> unit =
  fun name make_exn ->
    String.iter
      (fun c -> if valid_char c then () else raise (make_exn name c))
      name ;
    ()


type level = Lwt_log_core.level =
    Debug | Info | Notice | Warning | Error | Fatal
module Level = struct
  type t = level
  let default = Info
  let to_lwt_log t = t
  let to_string = Lwt_log_core.string_of_level
  let of_string = Lwt_log_core.level_of_string
  let encoding =
    let open Data_encoding in
    string_enum
      (List.map
         (fun l -> to_string l, l)
         [ Debug ; Info ; Notice ; Warning ; Error ; Fatal ])
end

module Section: sig
  type t = private string list
  val empty : t
  val make : string list -> t
  val make_sanitized : string list -> t
  val to_lwt_log : t -> Lwt_log_core.section
  val encoding : t Data_encoding.t
  val to_string_list : t -> string list
end = struct
  type t = string list
  let empty = []

  let make sl =
    List.iter
      (fun s ->
         check_name_exn s (fun name char ->
             Printf.ksprintf (fun s -> Invalid_argument s)
               "Internal_event.Section: invalid name %S (contains %c)" name char))
      sl;
    sl

  let make_sanitized sl =
    List.map
      (String.map (fun c -> if valid_char c then c else '_')) sl |> make

  let to_lwt_log s = Lwt_log_core.Section.make (String.concat "." s)

  let to_string_list t = t

  let encoding =
    let open Data_encoding in
    list string
end

module type EVENT_DEFINITION = sig
  type t

  val name : string

  val doc : string

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val level : t -> level
end

module Event_defaults = struct
  let level _ = Level.default
end

module type EVENT = sig
  include EVENT_DEFINITION

  val emit : ?section : Section.t -> (unit -> t) -> unit tzresult Lwt.t
end

type 'a event_definition = (module EVENT_DEFINITION with type t = 'a)

module type SINK = sig

  type t

  val uri_scheme : string

  val configure : Uri.t -> t tzresult Lwt.t

  val handle :
    t -> 'a event_definition ->
    ?section : Section.t -> (unit -> 'a) -> unit tzresult Lwt.t

  val close : t -> unit tzresult Lwt.t
end

type 'a sink_definition = (module SINK with type t = 'a)

module All_sinks = struct


  type registered =
    | Registered :
        { scheme : string ; definition : 'a sink_definition } -> registered

  type active =
    | Active : { scheme : string ; configuration : Uri.t ;
                 sink : 'a ; definition : 'a sink_definition } -> active

  let registered : registered list ref = ref []

  let active : active list ref = ref []

  let find_registered_exn scheme_to_find =
    List.find
      (function
        | Registered { scheme ;  _ } ->
            String.equal scheme scheme_to_find)
      !registered

  let register (type a) m =
    let module S = (val m : SINK with type t = a) in
    match find_registered_exn S.uri_scheme with
    | exception _ ->
        registered :=
          Registered { scheme = S.uri_scheme ; definition = m } :: !registered
    | _ ->
        (* This should be considered a programming error: *)
        Printf.ksprintf Pervasives.invalid_arg
          "Internal_event: registering duplicate URI scheme: %S" S.uri_scheme

  type activation_error_reason =
    | Missing_uri_scheme of string
    | Uri_scheme_not_registered of string
  type error += Activation_error of activation_error_reason

  let () =
    let description =
      "Activation of an Internal Event SINK with an URI failed" in
    let title = "Internal Event Sink: Wrong Activation URI" in
    register_error_kind `Permanent ~id:"internal-event-activation-error" ~title
      ~description
      ~pp:(fun ppf -> function
          | Missing_uri_scheme uri ->
              Format.fprintf ppf "%s: Missing URI scheme %S" title uri
          | Uri_scheme_not_registered uri ->
              Format.fprintf ppf "%s: URI scheme not registered %S" title uri)
      Data_encoding.(
        union [
          case ~title:"missing-uri-scheme"
            (Tag 0)
            (obj1 (req "missing-uri-scheme" (obj1 (req "uri" string))))
            (function Missing_uri_scheme uri -> Some uri | _ -> None)
            (fun uri -> Missing_uri_scheme uri) ;
          case ~title:"non-registered-uri-scheme"
            (Tag 2)
            (obj1 (req "non-registered-uri-scheme" (obj1 (req "uri" string))))
            (function Uri_scheme_not_registered uri -> Some uri | _ -> None)
            (fun uri -> Uri_scheme_not_registered uri) ;
        ])
      (function
        | Activation_error reason -> Some reason | _ -> None)
      (fun reason -> Activation_error reason)

  let activate uri =
    begin match Uri.scheme uri with
      | None -> fail (Activation_error (Missing_uri_scheme (Uri.to_string uri)))
      | Some scheme_to_activate ->
          let activate (type a) scheme definition =
            let module S = (val definition : SINK with type t = a) in
            S.configure uri >>=? fun sink ->
            return (Active { scheme ; configuration = uri ; definition ; sink })
          in
          begin match find_registered_exn scheme_to_activate with
            | Registered { scheme ;  definition } ->
                activate scheme definition
            | exception _ ->
                fail (Activation_error
                        (Uri_scheme_not_registered (Uri.to_string uri)))
          end
          >>=? fun act ->
          active := act :: !active ;
          return_unit
    end

  let close () =
    let close_one (type a) sink definition =
      let module S = (val definition : SINK with type t = a) in
      S.close sink in
    iter_s
      (fun (Active { sink ; definition ; _ }) -> close_one sink definition)
      !active


  let handle def section v =
    let handle (type a) sink definition =
      let module S = (val definition : SINK with type t = a) in
      S.handle ?section sink def v in
    List.fold_left
      (fun prev -> function Active { sink ; definition ; _ } ->
          prev >>=? fun () ->
          handle sink definition)
      return_unit
      !active

  let pp_state fmt () =
    let open Format in
    let pp_list_of_sinks name list pp =
      pp_open_box fmt 2 ;
      pp_print_if_newline fmt () ;
      pp_print_string fmt "* " ;
      fprintf fmt "%s: [" name;
      pp_print_cut fmt () ;
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt "," ; pp_print_space fmt ())
        pp
        fmt
        list ;
      pp_close_box fmt () ;
      pp_print_cut fmt () ;
      pp_print_string fmt "]" ;
    in
    pp_open_box fmt 0 ;
    pp_list_of_sinks "Registered sinks" !registered
      (fun fmt (Registered { scheme ; _ }) ->
         fprintf fmt "\"%s://..\"" scheme) ;
    pp_print_break fmt 2 0 ;
    pp_list_of_sinks "Active sinks" !active
      (fun fmt (Active { configuration ; _ }) ->
         fprintf fmt "\"%a\"" Uri.pp_hum configuration) ;
    pp_print_cut fmt () ;
    pp_close_box fmt () ;
    ()
end

module Generic = struct

  type definition =
    | Definition: (string * 'a event_definition) -> definition

  type event =
    | Event: (string * 'a event_definition * 'a) -> event

  type with_name = < doc : string; name : string >

  let json_schema (Definition (_, d))
    : < schema : Json_schema.schema ; with_name > =
    let aux (type a)  (ev : a event_definition) =
      let module E = (val ev : EVENT_DEFINITION with type t = a) in
      object
        method name = E.name
        method doc = E.doc
        method schema = Data_encoding.Json.schema E.encoding
      end in
    aux d

  let explode_event (Event (_, def, ev)) =
    let aux (type a) def ev =
      let module M = (val def : EVENT_DEFINITION with type t = a) in
      object
        method name = M.name
        method doc = M.doc
        method pp fmt () = M.pp fmt ev
        method json = Data_encoding.Json.construct M.encoding ev
      end in
    aux def ev
end

module All_definitions = struct

  open Generic

  let all : definition list ref = ref []

  let registration_exn fmt =
    Format.kasprintf
      (fun s ->
         (* This should be considered a programming error: *)
         Invalid_argument ("Internal_event registration error: " ^ s))
      fmt

  let add (type a) ev =
    let module E = (val ev : EVENT_DEFINITION with type t = a) in
    match List.find (function Definition (n, _) -> E.name = n) !all with
    | _ ->
        raise (registration_exn "duplicate Event name: %S" E.name)
    | exception _ ->
        check_name_exn E.name
          (registration_exn "invalid event name: %S contains '%c'") ;
        all := Definition (E.name, ev) :: !all

  let get () = !all

  let find match_name =
    match List.find (function Definition (n, _) -> match_name n) !all with
    | s -> Some s
    | exception _ -> None

end

module Make (E : EVENT_DEFINITION) : EVENT with type t = E.t = struct
  include E

  let emit ?section x =
    (* In order to evaluate the event at most once, we wrap it in a
       `Lazy.t`: *)
    let x = lazy (x ()) in
    All_sinks.handle (module E) section (fun () -> Lazy.force x)

  let () = All_definitions.add (module E)
end

module Legacy_logging = struct

  let sections = ref []

  module type LOG = sig
    val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
    val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
    val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
    val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
    val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a
    val fatal_error: ('a, Format.formatter, unit, unit) format4 -> 'a

    val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
    val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
    val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
    val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
    val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
    val lwt_fatal_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  end

  open Tezos_stdlib
  type ('a, 'b) msgf =
    (('a, Format.formatter, unit, 'b) format4 -> ?tags:Tag.set -> 'a) ->
    ?tags:Tag.set -> 'b
  type ('a, 'b) log = ('a, 'b) msgf -> 'b
  module type SEMLOG = sig
    module Tag = Tag
    val debug: ('a, unit) log
    val log_info: ('a, unit) log
    val log_notice: ('a, unit) log
    val warn: ('a, unit) log
    val log_error: ('a, unit) log
    val fatal_error: ('a, unit) log
    val lwt_debug: ('a, unit Lwt.t) log
    val lwt_log_info: ('a, unit Lwt.t) log
    val lwt_log_notice: ('a, unit Lwt.t) log
    val lwt_warn: ('a, unit Lwt.t) log
    val lwt_log_error: ('a, unit Lwt.t) log
    val lwt_fatal_error: ('a, unit Lwt.t) log
    val event : string Tag.def
    val exn : exn Tag.def
  end

  module Make_event (P : sig val name : string end) = struct
    let name_split = String.split_on_char '.' P.name
    let section = Section.make name_split

    module Definition = struct
      let name = "legacy_logging_event-" ^ String.concat "-" name_split

      type t = {
        message : string ;
        section : Section.t ;
        level : level ;
        tags : Tag.set ;
      }

      let make ?(tags = Tag.empty) level message =
        { message ; section ; level ; tags }


      let v0_encoding =
        let open Data_encoding in
        conv
          (fun { message ; section ; level ; tags } ->
             (message, section, level, tags))
          (fun (message, section, level, tags) ->
             { message ; section ; level ; tags })
          (obj4
             (req "message" string)
             (req "section" Section.encoding)
             (req "level" Level.encoding)
             (dft "tags"
                (conv
                   (fun tags -> Format.asprintf "%a" Tag.pp_set tags)
                   (fun _ -> Tag.empty)
                   string)
                Tag.empty)
          )

      let encoding =
        Data_encoding.With_version.(encoding ~name (first_version v0_encoding))

      let pp ppf { message ; _ } =
        let open Format in
        fprintf ppf "%s" message

      let doc = "Generic event legacy / string-based information logging."

      let level { level ; _ } = level
    end


    let () = sections := P.name :: !sections

    module Event = Make (Definition)

    let emit_async level fmt ?tags =
      Format.kasprintf
        (fun message ->
           Lwt.ignore_result
             (Event.emit ~section (fun () -> Definition.make ?tags level message)))
        fmt
    let emit_lwt level fmt ?tags =
      Format.kasprintf
        (fun message ->
           Event.emit ~section (fun () -> Definition.make ?tags level message)
           >>= function
           | Ok () -> Lwt.return_unit
           | Error el -> Format.kasprintf Lwt.fail_with "%a" pp_print_error el)
        fmt
  end

  module Make (P : sig val name : string end) = struct
    include Make_event(P)
    let emit_async = emit_async ?tags:None
    let debug f = emit_async Debug f
    let log_info f = emit_async Info f
    let log_notice f = emit_async Notice f
    let warn f = emit_async Warning f
    let log_error f = emit_async Error f
    let fatal_error f = emit_async Fatal f
    let emit_lwt = emit_lwt ?tags:None
    let lwt_debug f = emit_lwt Debug f
    let lwt_log_info f = emit_lwt Info f
    let lwt_log_notice f = emit_lwt Notice f
    let lwt_warn f = emit_lwt Warning f
    let lwt_log_error f = emit_lwt Error f
    let lwt_fatal_error f = emit_lwt Fatal f
  end
  module Make_semantic (P : sig val name : string end) = struct
    include Make_event(P)
    let debug (f: ('a, unit) msgf) = f (emit_async Debug) ?tags:None
    let log_info f        = f (emit_async Info) ?tags:None
    let log_notice f      = f (emit_async Notice) ?tags:None
    let warn f            = f (emit_async Warning) ?tags:None
    let log_error f       = f (emit_async Error) ?tags:None
    let fatal_error f     = f (emit_async Fatal) ?tags:None
    let lwt_debug f       = f (emit_lwt Debug) ?tags:None
    let lwt_log_info f    = f (emit_lwt Info) ?tags:None
    let lwt_log_notice f  = f (emit_lwt Notice) ?tags:None
    let lwt_warn f        = f (emit_lwt Warning) ?tags:None
    let lwt_log_error f   = f (emit_lwt Error) ?tags:None
    let lwt_fatal_error f = f (emit_lwt Fatal) ?tags:None
    module Tag = Tag
    let event =
      Tag.def ~doc:"String identifier for the class of event being logged"
        "event" Format.pp_print_text
    let exn =
      Tag.def ~doc:"Exception which was detected"
        "exception" (fun f e -> Format.pp_print_text f (Printexc.to_string e))
  end
end

module Error_event = struct
  type t = {
    message : string option ;
    severity : [ `Fatal | `Recoverable ] ;
    trace : Error_monad.error list ;
  }

  let make ?message ?(severity = `Recoverable) trace () =
    { message ; trace; severity }

  module Definition = struct
    let name = "error-event"

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding =
        conv
          (fun { message ; trace ; severity } -> (message, severity, trace))
          (fun (message, severity, trace) -> { message ; severity ; trace })
          (obj3
             (opt "message" string)
             (req "severity"
                (string_enum ["fatal", `Fatal; "recoverable", `Recoverable]))
             (req "trace" (list Error_monad.error_encoding)))
      in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp f x =
      Format.fprintf f "%s:@ %s" name
        (match x.message with Some x -> x | None -> "")

    let doc = "Generic event for any kind of error."

    let level { severity ; _ } =
      match severity with
      | `Fatal -> Fatal
      | `Recoverable -> Error
  end

  include (Make (Definition) : EVENT with type t := t)

  let log_error_and_recover ?section ?message ?severity f =
    f ()
    >>= function
    | Ok () -> Lwt.return_unit
    | Error el ->
        emit ?section (fun () -> make ?message ?severity el ())
        >>= function
        | Ok () -> Lwt.return_unit
        | Error el ->
            Format.kasprintf (Lwt_log.error)
              "Error while emitting error logging event !! %a"
              pp_print_error el
end

module Debug_event = struct
  type t = { message : string ; attachment : Data_encoding.Json.t }

  let make ?(attach = `Null) message () = { message ; attachment = attach }

  let v0_encoding =
    let open Data_encoding in
    conv
      (fun { message ; attachment } -> (message, attachment))
      (fun (message, attachment) -> { message ; attachment })
      (obj2 (req "message" string) (req "attachment" json))

  module Definition = struct
    let name = "debug-event"

    type nonrec t = t

    let encoding =
      Data_encoding.With_version.(encoding ~name (first_version v0_encoding))

    let pp ppf { message ; attachment } =
      let open Format in
      fprintf ppf "%s:@ %s@ %a" name message Data_encoding.Json.pp attachment

    let doc = "Generic event for semi-structured debug information."

    include Event_defaults
  end

  include (Make (Definition) : EVENT with type t := t)
end

module Lwt_worker_event = struct
  type t = { name : string ; event : [ `Started | `Ended | `Failed of string ] }

  let v0_encoding =
    let open Data_encoding in
    conv
      (fun { name ; event } -> (name, event))
      (fun (name, event) -> { name ; event })
      (obj2
         (req "name" string)
         (req "event"
            (union [
                case ~title:"started" (Tag 0)
                  (obj1 (req "kind" (constant "started")))
                  (function `Started -> Some () | _ -> None)
                  (fun () -> `Started) ;
                case ~title:"ended" (Tag 1)
                  (obj1 (req "kind" (constant "ended")))
                  (function `Ended -> Some () | _ -> None)
                  (fun () -> `Ended) ;
                case ~title:"failed" (Tag 2)
                  (obj2
                     (req "kind" (constant "failed"))
                     (req "exception" string))
                  (function `Failed s -> Some ((), s) | _ -> None)
                  (fun ((), s) -> `Failed s) ;
              ])
         ))

  module Definition = struct
    let name = "lwt-worker-event"

    type nonrec t = t

    let encoding =
      Data_encoding.With_version.(encoding ~name (first_version v0_encoding))

    let pp ppf { name ; event } =
      let open Format in
      fprintf ppf "Worker %s:@ %a" name
        (fun fmt -> function
           | `Failed msg -> fprintf ppf "Failed with %s" msg
           | `Ended -> fprintf fmt "Ended"
           | `Started -> fprintf fmt "Started")
        event

    let doc = "Generic event for callers of the function Lwt_utils.worker."

    let level { event ; _ } =
      match event with
      | `Failed _ -> Error
      | `Started | `Ended -> Info
  end

  include (Make (Definition) : EVENT with type t := t)

  let on_event name event =
    let section = Section.make_sanitized [ "lwt-worker"; name ] in
    Error_event.log_error_and_recover
      ~message:(Printf.sprintf "Trying to emit worker event for %S" name)
      ~severity:`Fatal
      (fun () -> emit ~section (fun () -> { name ; event }))
end



module Lwt_log_sink = struct

  (* let default_template = "$(date) - $(section): $(message)" *)

  let default_section = Lwt_log_core.Section.main

  module Sink : SINK = struct
    type t = unit

    let uri_scheme = "lwt-log"

    let configure _ = return_unit

    let handle (type a) () m ?section (v : unit -> a) =
      let module M = (val m : EVENT_DEFINITION with type t = a) in
      protect
        (fun () ->
           let ev = v () in
           let section =
             Option.unopt_map ~f:Section.to_lwt_log
               section ~default:default_section in
           let level = M.level ev in
           Format.kasprintf
             (Lwt_log_core.log ~section ~level)
             "%a" M.pp ev
           >>= fun () -> return_unit)

    let close _ =
      Lwt_log.close !Lwt_log.default
      >>= fun () ->
      return_unit
  end
  include Sink

  let () = All_sinks.register (module Sink)

end
