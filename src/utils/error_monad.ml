(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Protocol Implementation - Error Monad *)

(*-- Error classification ----------------------------------------------------*)

type error_category = [ `Branch | `Temporary | `Permanent ]

type 'err full_error_category =
  [ error_category | `Wrapped of 'err -> error_category ]

(* HACK: forward reference from [Data_encoding_ezjsonm] *)
let json_to_string = ref (fun _ -> "")

let json_pp id encoding ppf x =
  Format.pp_print_string ppf @@
  !json_to_string @@
  let encoding =
    Data_encoding.(merge_objs (obj1 (req "id" string)) encoding) in
  Data_encoding.Json.construct encoding (id, x)

module Make() = struct

  type error = ..

  (* the toplevel store for error kinds *)
  type error_kind =
      Error_kind :
        { id: string ;
          from_error: error -> 'err option ;
          category: 'err full_error_category ;
          encoding_case: error Data_encoding.case ;
          pp: Format.formatter -> 'err -> unit ; } ->
      error_kind

  type registred_errors = error_kind list

  let error_kinds
    : error_kind list ref
    = ref []

  let error_encoding_cache = ref None

  let string_of_category = function
    | `Permanent -> "permanent"
    | `Temporary -> "temporary"
    | `Branch -> "branch"
    | `Wrapped _ -> "wrapped"
  let raw_register_error_kind
      category ~id:name ~title ~description ?pp
      encoding from_error to_error =
    if List.exists
        (fun (Error_kind { id }) -> name = id)
        !error_kinds then
      invalid_arg
        (Printf.sprintf
           "register_error_kind: duplicate error name: %s" name) ;
    let encoding_case =
      let open Data_encoding in
      case
        (describe ~title ~description @@
         conv (fun x -> (((), ()), x)) (fun (((),()), x) -> x) @@
         merge_objs
           (obj2
              (req "kind" (constant (string_of_category category)))
              (req "id" (constant name)))
           encoding)
        from_error to_error in
    error_encoding_cache := None ;
    error_kinds :=
      Error_kind { id = name ;
                   category ;
                   from_error ;
                   encoding_case ;
                   pp = Utils.unopt ~default:(json_pp name encoding) pp } :: !error_kinds

  let register_wrapped_error_kind
      category ~id ~title ~description ?pp
      encoding from_error to_error =
    raw_register_error_kind
      (`Wrapped category)
      ~id ~title ~description ?pp
      encoding from_error to_error

  let register_error_kind
      category ~id ~title ~description ?pp
      encoding from_error to_error =
    raw_register_error_kind
      (category :> _ full_error_category)
      ~id ~title ~description ?pp
      encoding from_error to_error

  let error_encoding () =
    match !error_encoding_cache with
    | None ->
        let cases =
          List.map
            (fun (Error_kind { encoding_case }) -> encoding_case )
            !error_kinds in
        let json_encoding = Data_encoding.union cases in
        let encoding =
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

  let json_of_error error =
    Data_encoding.Json.(construct (error_encoding ())) error
  let error_of_json json =
    Data_encoding.Json.(destruct (error_encoding ())) json

  let classify_error error =
    let rec find e = function
      | [] -> `Temporary
          (* assert false (\* See "Generic error" *\) *)
      | Error_kind { from_error ; category } :: rest ->
          match from_error e with
          | Some x -> begin
              match category with
              | `Wrapped f -> f x
              | #error_category as x -> x
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
      | Error_kind { from_error ; pp } :: errors ->
          match from_error error with
          | None -> find errors
          | Some x -> pp ppf x in
    find !error_kinds

  let registred_errors () = !error_kinds


  (*-- Monad definition --------------------------------------------------------*)

  let (>>=) = Lwt.(>>=)

  type 'a tzresult = ('a, error list) result

  let result_encoding t_encoding =
    let open Data_encoding in
    let errors_encoding =
      describe ~title: "An erroneous result" @@
      obj1 (req "error" (list (error_encoding ()))) in
    let t_encoding =
      describe ~title: "A successful result" @@
      obj1 (req "result" t_encoding) in
    union
      ~tag_size:`Uint8
      [ case ~tag:0 t_encoding
          (function Ok x -> Some x | _ -> None)
          (function res -> Ok res) ;
        case ~tag:1 errors_encoding
          (function Error x -> Some x | _ -> None)
          (fun errs -> Error errs) ]

  let return v = Lwt.return (Ok v)

  let error s = Error [ s ]

  let ok v = Ok v

  let fail s = Lwt.return (Error [ s ])

  let (>>?) v f =
    match v with
    | Error _ as err -> err
    | Ok v -> f v

  let (>>=?) v f =
    v >>= function
    | Error _ as err -> Lwt.return err
    | Ok v -> f v

  let (>>|?) v f = v >>=? fun v -> Lwt.return (Ok (f v))
  let (>|=) = Lwt.(>|=)

  let (>|?) v f = v >>? fun v -> Ok (f v)

  let rec map_s f l =
    match l with
    | [] -> return []
    | h :: t ->
        f h >>=? fun rh ->
        map_s f t >>=? fun rt ->
        return (rh :: rt)

  let rec map_p f l =
    match l with
    | [] ->
        return []
    | x :: l ->
        let tx = f x and tl = map_p f l in
        tx >>= fun x ->
        tl >>= fun l ->
        match x, l with
        | Ok x, Ok l -> Lwt.return (Ok (x :: l))
        | Error exn1, Error exn2 -> Lwt.return (Error (exn1 @ exn2))
        | Ok _, Error exn
        | Error exn, Ok _ -> Lwt.return (Error exn)

  let rec map2_s f l1 l2 =
    match l1, l2 with
    | [], [] -> return []
    | _ :: _, [] | [], _ :: _ -> invalid_arg "Error_monad.map2_s"
    | h1 :: t1, h2 :: t2 ->
        f h1 h2 >>=? fun rh ->
        map2_s f t1 t2 >>=? fun rt ->
        return (rh :: rt)

  let rec map2 f l1 l2 =
    match l1, l2 with
    | [], [] -> Ok []
    | _ :: _, [] | [], _ :: _ -> invalid_arg "Error_monad.map2"
    | h1 :: t1, h2 :: t2 ->
        f h1 h2 >>? fun rh ->
        map2 f t1 t2 >>? fun rt ->
        Ok (rh :: rt)

  let rec map_filter_s f l =
    match l with
    | [] -> return []
    | h :: t ->
        f h >>=? function
        | None -> map_filter_s f t
        | Some rh ->
            map_filter_s f t >>=? fun rt ->
            return (rh :: rt)

  let rec iter_s f l =
    match l with
    | [] -> return ()
    | h :: t ->
        f h >>=? fun () ->
        iter_s f t

  let rec iter_p f l =
    match l with
    | [] -> return ()
    | x :: l ->
        let tx = f x and tl = iter_p f l in
        tx >>= fun tx_res ->
        tl >>= fun tl_res ->
        match tx_res, tl_res with
        | Ok (), Ok () -> Lwt.return (Ok ())
        | Error exn1, Error exn2 -> Lwt.return (Error (exn1 @ exn2))
        | Ok (), Error exn
        | Error exn, Ok () -> Lwt.return (Error exn)

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

  let record_trace err result =
    match result with
    | Ok _ as res -> res
    | Error errs -> Error (err :: errs)

  let trace err f =
    f >>= function
    | Error errs -> Lwt.return (Error (err :: errs))
    | ok -> Lwt.return ok

  let fail_unless cond exn =
    if cond then return () else fail exn

  let unless cond f =
    if cond then return () else f ()

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

type error += Unclassified of string

let () =
  let id = "" in
  let category = `Temporary in
  let to_error msg = Unclassified msg in
  let from_error = function
    | Unclassified msg -> Some msg
    | error ->
        let msg = Obj.(extension_name @@ extension_constructor error) in
        Some ("Unclassified error: " ^ msg ^ ".") in
  let title = "Generic error" in
  let description =  "An unclassified error" in
  let encoding_case =
    let open Data_encoding in
    case
      (describe ~title ~description @@
       conv (fun x -> ((), x)) (fun ((), x) -> x) @@
       (obj2
          (req "kind" (constant "generic"))
          (req "error" string)))
      from_error to_error in
  let pp = Format.pp_print_string in
  error_kinds :=
    Error_kind { id; from_error ; category; encoding_case ; pp } :: !error_kinds

let protect ~on_error t =
  t  >>= function
  | Ok res -> return res
  | Error err -> on_error err

end

include Make()

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


let protect ?on_error t =
  Lwt.catch t (fun exn -> fail (Exn exn)) >>= function
  | Ok res -> return res
  | Error err ->
      match on_error with
      | Some f -> f err
      | None -> Lwt.return (Error err)

let pp_exn ppf exn = pp ppf (Exn exn)

let () =
  register_error_kind
    `Temporary
    ~id:"failure"
    ~title:"Generic error"
    ~description:"Unclassified error"
    ~pp:Format.pp_print_string
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Exn (Failure msg) -> Some msg
      | Exn (Unix.Unix_error (err, fn, _)) ->
          Some ("Unix error in " ^ fn ^ ": " ^ Unix.error_message err)
      | Exn exn -> Some (Printexc.to_string exn)
      | _ -> None)
    (fun msg -> Exn (Failure msg))
