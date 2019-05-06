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

(* Tezos Protocol Implementation - Error Monad *)

(*-- Error classification ----------------------------------------------------*)

type error_category = [ `Branch | `Temporary | `Permanent ]

(* hack: forward reference from [Data_encoding_ezjsonm] *)
let json_to_string = ref (fun _ -> "")

let json_pp id encoding ppf x =
  Format.pp_print_string ppf @@
  !json_to_string @@
  let encoding =
    Data_encoding.(merge_objs (obj1 (req "id" string)) encoding) in
  Data_encoding.Json.construct encoding (id, x)

let set_error_encoding_cache_dirty = ref (fun () -> ())

module Make(Prefix : sig val id : string end) = struct

  type error = ..

  module type Wrapped_error_monad = sig
    type unwrapped = ..
    include Error_monad_sig.S with type error := unwrapped
    val unwrap : error -> unwrapped option
    val wrap : unwrapped -> error
  end

  type full_error_category =
    | Main of error_category
    | Wrapped of (module Wrapped_error_monad)

  (* the toplevel store for error kinds *)
  type error_kind =
      Error_kind :
        { id: string ;
          title: string ;
          description: string ;
          from_error: error -> 'err option ;
          category: full_error_category ;
          encoding_case: error Data_encoding.case ;
          pp: Format.formatter -> 'err -> unit ; } ->
      error_kind

  type error_info =
    { category : error_category ;
      id: string ;
      title : string ;
      description : string ;
      schema : Data_encoding.json_schema }

  let error_kinds
    : error_kind list ref
    = ref []

  let get_registered_errors () : error_info list =
    List.flatten
      (List.map
         (function
           | Error_kind { id = "" ; _ } -> []
           | Error_kind { id ; title ; description ; category = Main category ; encoding_case ; _ } ->
               [ { id ; title ; description ; category ;
                   schema = Data_encoding.Json.schema (Data_encoding.union [ encoding_case ]) } ]
           | Error_kind { category = Wrapped (module WEM) ; _ } ->
               List.map
                 (fun { WEM.id ; title ; description ; category ; schema } ->
                    { id ; title ; description ; category ; schema })
                 (WEM.get_registered_errors ()))
         !error_kinds)

  let error_encoding_cache = ref None
  let () =
    let cont = !set_error_encoding_cache_dirty in
    set_error_encoding_cache_dirty := fun () ->
      cont () ;
      error_encoding_cache := None

  let string_of_category = function
    | `Permanent -> "permanent"
    | `Temporary -> "temporary"
    | `Branch -> "branch"

  let pp_info
      ppf
      { category; id; title; description; schema } =
    Format.fprintf
      ppf
      "@[<v 2>category : %s\nid : %s\ntitle : %s\ndescription : %s\nschema : %a@]"
      (string_of_category category)
      id title description
      (Json_repr.pp (module Json_repr.Ezjsonm))
      (Json_schema.to_json schema)

  (* Catch all error when 'serializing' an error. *)
  type error += Unclassified of string

  let () =
    let id = "" in
    let category = Main `Temporary in
    let to_error msg = Unclassified msg in
    let from_error = function
      | Unclassified msg -> Some msg
      | error ->
          let msg = Obj.(extension_name @@ extension_constructor error) in
          Some ("Unclassified error: " ^ msg ^ ". Was the error registered?") in
    let title = "Generic error" in
    let description =  "An unclassified error" in
    let encoding_case =
      let open Data_encoding in
      case Json_only
        ~title:"Generic error"
        (def "generic_error" ~title ~description @@
         conv (fun x -> ((), x)) (fun ((), x) -> x) @@
         (obj2
            (req "kind" (constant "generic"))
            (req "error" string)))
        from_error to_error in
    let pp ppf s = Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s in
    error_kinds :=
      Error_kind { id ; title ; description ;
                   from_error ; category ; encoding_case ; pp } :: !error_kinds

  (* Catch all error when 'deserializing' an error. *)
  type error += Unregistred_error of Data_encoding.json

  let () =
    let id = "" in
    let category = Main `Temporary in
    let to_error msg = Unregistred_error msg in
    let from_error = function
      | Unregistred_error json -> Some json
      | _ -> None in
    let encoding_case =
      let open Data_encoding in
      case Json_only
        ~title:"Unregistred error"
        json from_error to_error in
    let pp ppf json =
      Format.fprintf ppf "@[<v 2>Unregistred error:@ %a@]"
        Data_encoding.Json.pp json in
    error_kinds :=
      Error_kind { id ; title = "" ; description = "" ;
                   from_error ; category ; encoding_case ; pp } :: !error_kinds

  let raw_register_error_kind
      category ~id:name ~title ~description ?pp
      encoding from_error to_error =
    let name = Prefix.id ^ name in
    if List.exists
        (fun (Error_kind { id ; _ }) -> name = id)
        !error_kinds then
      invalid_arg
        (Printf.sprintf
           "register_error_kind: duplicate error name: %s" name) ;
    let encoding_case =
      let open Data_encoding in
      match category with
      | Wrapped (module WEM) ->
          let unwrap err =
            match WEM.unwrap err with
            | Some (WEM.Unclassified _) -> None
            | Some (WEM.Unregistred_error _) ->
                Format.eprintf "What %s@." name ;
                None
            | res -> res in
          let wrap err =
            match err with
            | WEM.Unclassified _ ->
                failwith "ignore wrapped error when serializing"
            | WEM.Unregistred_error _ ->
                failwith "ignore wrapped error when deserializing"
            | res -> WEM.wrap res in
          case Json_only
            ~title:name
            WEM.error_encoding unwrap wrap
      | Main category ->
          let with_id_and_kind_encoding =
            merge_objs
              (obj2
                 (req "kind" (constant (string_of_category category)))
                 (req "id" (constant name)))
              encoding in
          case Json_only
            ~title
            ~description
            (conv
               (fun x -> (((), ()), x))
               (fun (((),()), x) -> x)
               with_id_and_kind_encoding)
            from_error to_error in
    !set_error_encoding_cache_dirty () ;
    error_kinds :=
      Error_kind
        { id = name ;
          category ;
          title ;
          description ;
          from_error ;
          encoding_case ;
          pp = Option.unopt ~default:(json_pp name encoding) pp }
      :: !error_kinds

  let register_wrapped_error_kind
      (module WEM : Wrapped_error_monad) ~id ~title ~description =
    raw_register_error_kind
      (Wrapped (module WEM))
      ~id ~title ~description
      ~pp:WEM.pp WEM.error_encoding WEM.unwrap WEM.wrap

  let register_error_kind
      category ~id ~title ~description ?pp
      encoding from_error to_error =
    if not (Data_encoding.is_obj encoding)
    then invalid_arg
        (Printf.sprintf
           "Specified encoding for \"%s%s\" is not an object, but error encodings must be objects."
           Prefix.id id) ;
    raw_register_error_kind
      (Main category)
      ~id ~title ~description ?pp
      encoding from_error to_error

  let error_encoding () =
    match !error_encoding_cache with
    | None ->
        let cases =
          List.map
            (fun (Error_kind { encoding_case ; _ }) -> encoding_case)
            !error_kinds in
        let json_encoding = Data_encoding.union cases in
        let encoding =
          Data_encoding.dynamic_size @@
          Data_encoding.splitted
            ~json:json_encoding
            ~binary:
              (Data_encoding.conv
                 (Data_encoding.Json.construct json_encoding)
                 (Data_encoding.Json.destruct json_encoding)
                 Data_encoding.json) in
        error_encoding_cache := Some encoding ;
        encoding
    | Some encoding -> encoding

  let error_encoding = Data_encoding.delayed error_encoding

  let json_of_error error =
    Data_encoding.Json.construct error_encoding error
  let error_of_json json =
    Data_encoding.Json.destruct error_encoding json

  let classify_error error =
    let rec find e = function
      | [] -> `Temporary
      (* assert false (\* See "Generic error" *\) *)
      | Error_kind { from_error ; category ; _ } :: rest ->
          match from_error e with
          | Some _ -> begin
              match category with
              | Main error_category -> error_category
              | Wrapped (module WEM) ->
                  match WEM.unwrap e with
                  | Some e -> WEM.classify_errors [ e ]
                  | None -> find e rest
            end
          | None -> find e rest in
    find error !error_kinds

  let classify_errors errors =
    List.fold_left
      (fun r e -> match r, classify_error e with
         | `Permanent, _ | _, `Permanent -> `Permanent
         | `Branch, _ | _, `Branch -> `Branch
         | `Temporary, `Temporary -> `Temporary)
      `Temporary errors

  let pp ppf error =
    let rec find = function
      | [] -> assert false (* See "Generic error" *)
      | Error_kind { from_error ; pp ; _ } :: errors ->
          match from_error error with
          | None -> find errors
          | Some x -> pp ppf x in
    find !error_kinds

  (*-- Monad definition --------------------------------------------------------*)

  let (>>=) = Lwt.(>>=)

  type 'a tzresult = ('a, error list) result

  let result_encoding t_encoding =
    let open Data_encoding in
    let errors_encoding =
      obj1 (req "error" (list error_encoding)) in
    let t_encoding =
      obj1 (req "result" t_encoding) in
    union
      ~tag_size:`Uint8
      [ case (Tag 0) t_encoding
          ~title:"Ok"
          (function Ok x -> Some x | _ -> None)
          (function res -> Ok res) ;
        case (Tag 1) errors_encoding
          ~title:"Error"
          (function Error x -> Some x | _ -> None)
          (fun errs -> Error errs) ]

  let return v = Lwt.return_ok v

  let return_unit = Lwt.return_ok ()

  let return_none = Lwt.return_ok None

  let return_some x = Lwt.return_ok (Some x)

  let return_nil = Lwt.return_ok []

  let return_true = Lwt.return_ok true

  let return_false = Lwt.return_ok false

  let error s = Error [ s ]

  let ok v = Ok v

  let fail s = Lwt.return_error [ s ]

  let (>>?) v f =
    match v with
    | Error _ as err -> err
    | Ok v -> f v

  let (>>=?) v f =
    v >>= function
    | Error _ as err -> Lwt.return err
    | Ok v -> f v

  let (>>|?) v f = v >>=? fun v -> Lwt.return_ok (f v)
  let (>|=) = Lwt.(>|=)

  let (>|?) v f = v >>? fun v -> Ok (f v)

  let rec map_s f l =
    match l with
    | [] -> return_nil
    | h :: t ->
        f h >>=? fun rh ->
        map_s f t >>=? fun rt ->
        return (rh :: rt)

  let mapi_s f l =
    let rec mapi_s f i l =
      match l with
      | [] -> return_nil
      | h :: t ->
          f i h >>=? fun rh ->
          mapi_s f (i+1) t >>=? fun rt ->
          return (rh :: rt)
    in
    mapi_s f 0 l

  let rec map_p f l =
    match l with
    | [] ->
        return_nil
    | x :: l ->
        let tx = f x and tl = map_p f l in
        tx >>= fun x ->
        tl >>= fun l ->
        match x, l with
        | Ok x, Ok l -> Lwt.return_ok (x :: l)
        | Error exn1, Error exn2 -> Lwt.return_error (exn1 @ exn2)
        | Ok _, Error exn
        | Error exn, Ok _ -> Lwt.return_error exn

  let mapi_p f l =
    let rec mapi_p f i l =
      match l with
      | [] ->
          return_nil
      | x :: l ->
          let tx = f i x and tl = mapi_p f (i+1) l in
          tx >>= fun x ->
          tl >>= fun l ->
          match x, l with
          | Ok x, Ok l -> Lwt.return_ok (x :: l)
          | Error exn1, Error exn2 -> Lwt.return_error (exn1 @ exn2)
          | Ok _, Error exn
          | Error exn, Ok _ -> Lwt.return_error exn in
    mapi_p f 0 l

  let rec map2_s f l1 l2 =
    match l1, l2 with
    | [], [] -> return_nil
    | _ :: _, [] | [], _ :: _ -> invalid_arg "Error_monad.map2_s"
    | h1 :: t1, h2 :: t2 ->
        f h1 h2 >>=? fun rh ->
        map2_s f t1 t2 >>=? fun rt ->
        return (rh :: rt)

  let mapi2_s f l1 l2 =
    let rec mapi2_s i f l1 l2 =
      match l1, l2 with
      | [], [] -> return_nil
      | _ :: _, [] | [], _ :: _ -> invalid_arg "Error_monad.mapi2_s"
      | h1 :: t1, h2 :: t2 ->
          f i h1 h2 >>=? fun rh ->
          mapi2_s (i+1) f t1 t2 >>=? fun rt ->
          return (rh :: rt) in
    mapi2_s 0 f l1 l2

  let rec map2 f l1 l2 =
    match l1, l2 with
    | [], [] -> Ok []
    | _ :: _, [] | [], _ :: _ -> invalid_arg "Error_monad.map2"
    | h1 :: t1, h2 :: t2 ->
        f h1 h2 >>? fun rh ->
        map2 f t1 t2 >>? fun rt ->
        Ok (rh :: rt)

  let rec filter_map_s f l =
    match l with
    | [] -> return_nil
    | h :: t ->
        f h >>=? function
        | None -> filter_map_s f t
        | Some rh ->
            filter_map_s f t >>=? fun rt ->
            return (rh :: rt)

  let rec filter_map_p f l =
    match l with
    | [] -> return_nil
    | h :: t ->
        let th = f h
        and tt = filter_map_p f t in
        th >>=? function
        | None -> tt
        | Some rh ->
            tt >>=? fun rt ->
            return (rh :: rt)

  let rec filter_s f l =
    match l with
    | [] -> return_nil
    | h :: t ->
        f h >>=? function
        | false -> filter_s f t
        | true ->
            filter_s f t >>=? fun t ->
            return (h :: t)

  let rec filter_p f l =
    match l with
    | [] -> return_nil
    | h :: t ->
        let jh = f h
        and t = filter_p f t in
        jh >>=? function
        | false -> t
        | true ->
            t >>=? fun t ->
            return (h :: t)

  let rec iter_s f l =
    match l with
    | [] -> return_unit
    | h :: t ->
        f h >>=? fun () ->
        iter_s f t

  let rec iter_p f l =
    match l with
    | [] -> return_unit
    | x :: l ->
        let tx = f x and tl = iter_p f l in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match tx_res, tl_res with
        | Ok (), Ok () -> Lwt.return_ok ()
        | Error exn1, Error exn2 -> Lwt.return_error (exn1 @ exn2)
        | Ok (), Error exn
        | Error exn, Ok () -> Lwt.return_error exn

  let rec iter2_p f l1 l2 =
    match l1, l2 with
    | [], [] -> return_unit
    | [], _ | _, [] -> invalid_arg "Error_monad.iter2_p"
    | x1 :: l1 , x2 :: l2 ->
        let tx = f x1 x2 and tl = iter2_p f l1 l2 in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match tx_res, tl_res with
        | Ok (), Ok () -> Lwt.return_ok ()
        | Error exn1, Error exn2 -> Lwt.return_error (exn1 @ exn2)
        | Ok (), Error exn
        | Error exn, Ok () -> Lwt.return_error exn

  let iteri2_p f l1 l2 =
    let rec iteri2_p i f l1 l2 =
      match l1, l2 with
      | [], [] -> return_unit
      | [], _ | _, [] -> invalid_arg "Error_monad.iteri2_p"
      | x1 :: l1 , x2 :: l2 ->
          let tx = f i x1 x2 and tl = iteri2_p (i+1) f l1 l2 in
          tx >>= fun tx_res ->
          tl >>= fun tl_res ->
          match tx_res, tl_res with
          | Ok (), Ok () -> Lwt.return_ok ()
          | Error exn1, Error exn2 -> Lwt.return_error (exn1 @ exn2)
          | Ok (), Error exn
          | Error exn, Ok () -> Lwt.return_error exn
    in
    iteri2_p 0 f l1 l2

  let rec fold_left_s f init l =
    match l with
    | [] -> return init
    | h :: t ->
        f init h >>=? fun acc ->
        fold_left_s f acc t

  let rec fold_right_s f l init =
    match l with
    | [] -> return init
    | h :: t ->
        fold_right_s f t init >>=? fun acc ->
        f h acc

  let rec join = function
    | [] -> return_unit
    | t :: ts ->
        t >>= function
        | Error _ as err ->
            join ts >>=? fun () ->
            Lwt.return err
        | Ok () ->
            join ts

  let record_trace err result =
    match result with
    | Ok _ as res -> res
    | Error errs -> Error (err :: errs)

  let trace err f =
    f >>= function
    | Error errs -> Lwt.return_error (err :: errs)
    | ok -> Lwt.return ok

  let record_trace_eval mk_err result =
    match result with
    | Ok _ as res -> res
    | Error errs ->
        mk_err () >>? fun err ->
        Error (err :: errs)

  let trace_eval mk_err f =
    f >>= function
    | Error errs ->
        mk_err () >>=? fun err ->
        Lwt.return_error (err :: errs)
    | ok -> Lwt.return ok

  let fail_unless cond exn =
    if cond then return_unit else fail exn

  let fail_when cond exn =
    if cond then fail exn else return_unit

  let unless cond f =
    if cond then return_unit else f ()

  let _when cond f =
    if cond then f () else return_unit

  let pp_print_error ppf errors =
    match errors with
    | [] ->
        Format.fprintf ppf "Unknown error@."
    | [error] ->
        Format.fprintf ppf "@[<v 2>Error:@ %a@]@." pp error
    | errors ->
        Format.fprintf ppf "@[<v 2>Error, dumping error stack:@,%a@]@."
          (Format.pp_print_list pp)
          (List.rev errors)

  type error += Assert_error of string * string

  let () =
    let id = "" in
    let category = Main `Permanent in
    let to_error (loc, msg) = Assert_error (loc, msg) in
    let from_error = function
      | Assert_error (loc, msg) -> Some (loc, msg)
      | _ -> None in
    let title = "Assertion error" in
    let description =  "An fatal assertion" in
    let encoding_case =
      let open Data_encoding in
      case Json_only ~title ~description
        (conv (fun (x, y) -> ((), x, y)) (fun ((), x, y) -> (x, y))
           ((obj3
               (req "kind" (constant "assertion"))
               (req "location" string)
               (req "error" string))))
        from_error to_error in
    let pp ppf (loc, msg) =
      Format.fprintf ppf
        "Assert failure (%s)%s"
        loc
        (if msg = "" then "." else ": " ^ msg) in
    error_kinds :=
      Error_kind { id ; title ; description ;
                   from_error ; category ; encoding_case ; pp } :: !error_kinds

  let _assert b loc fmt =
    if b then
      Format.ikfprintf (fun _ -> return_unit) Format.str_formatter fmt
    else
      Format.kasprintf (fun msg -> fail (Assert_error (loc, msg))) fmt


  type 'a tzlazy_state =
    | Remembered of 'a
    | Not_yet_known of (unit -> 'a tzresult Lwt.t)
  type 'a tzlazy = { mutable tzcontents: 'a tzlazy_state }
  let tzlazy c = { tzcontents = Not_yet_known c }
  let tzforce v = match v.tzcontents with
    | Remembered v -> return v
    | Not_yet_known c ->
        c () >>=? fun w ->
        v.tzcontents <- Remembered w;
        return w


end

include Make(struct let id = "" end)

let generic_error fmt =
  Format.kasprintf (fun s -> error (Unclassified s)) fmt

let failwith fmt =
  Format.kasprintf (fun s -> fail (Unclassified s)) fmt

type error += Exn of exn
let error s = Error [ s ]
let error_exn s = Error [ Exn s ]
let trace_exn exn f = trace (Exn exn) f
let generic_trace fmt =
  Format.kasprintf (fun str -> trace_exn (Failure str)) fmt
let record_trace_exn exn f = record_trace (Exn exn) f

let failure fmt =
  Format.kasprintf (fun str -> Exn (Failure str)) fmt

let pp_exn ppf exn = pp ppf (Exn exn)

let () =
  register_error_kind
    `Temporary
    ~id:"failure"
    ~title:"Generic error"
    ~description:"Unclassified error"
    ~pp:(fun ppf s -> Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Exn (Failure msg) -> Some msg
      | Exn exn -> Some (Printexc.to_string exn)
      | _ -> None)
    (fun msg -> Exn (Failure msg))

type error += Canceled

let protect ?on_error ?canceler t =
  let cancelation =
    match canceler with
    | None -> Lwt_utils.never_ending ()
    | Some canceler ->
        (Lwt_canceler.cancelation canceler >>= fun () ->
         fail Canceled ) in
  let res =
    Lwt.pick [ cancelation ;
               Lwt.catch t (fun exn -> fail (Exn exn)) ] in
  res >>= function
  | Ok _ -> res
  | Error err ->
      let canceled =
        Option.unopt_map canceler ~default:false ~f:Lwt_canceler.canceled in
      let err = if canceled then [Canceled] else err in
      match on_error with
      | None -> Lwt.return_error err
      | Some on_error ->
          Lwt.catch (fun () -> on_error err) (fun exn -> fail (Exn exn))

type error += Timeout

let () =
  register_error_kind
    `Temporary
    ~id:"utils.Timeout"
    ~title:"Timeout"
    ~description:"Timeout"
    Data_encoding.unit
    (function Timeout -> Some () | _ -> None)
    (fun () -> Timeout)

let with_timeout ?(canceler = Lwt_canceler.create ()) timeout f =
  let target = f canceler in
  Lwt.choose [ timeout ; (target >|= fun _ -> ()) ] >>= fun () ->
  if Lwt.state target <> Lwt.Sleep then begin
    Lwt.cancel timeout ;
    target
  end else begin
    Lwt_canceler.cancel canceler >>= fun () ->
    fail Timeout
  end

let errs_tag = Tag.def ~doc:"Errors" "errs" pp_print_error
