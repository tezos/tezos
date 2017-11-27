(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

let string_of_meth = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `DELETE -> "DELETE"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"

module MethMap = Map.Make(struct type t = meth let compare = compare end)
module StringMap = Map.Make(String)

module Internal = struct

  module Ty = struct

    type 'a witness = ..
    exception Not_equal
    type (_, _) eq = Eq : ('a, 'a) eq
    module type Ty = sig
      type t val witness : t witness
      val eq: 'a witness -> ('a, t) eq
    end
    type 'a id = (module Ty with type t = 'a)
    let new_id (type a) () =
      let module Ty = struct
        type t = a
        type 'a witness += Ty : t witness
        let witness = Ty
        let eq (type b) : b witness -> (b, t) eq =
          function Ty -> Eq | _ -> raise Not_equal
      end in
      (module Ty : Ty with type t = a)
    let eq : type a b. a id -> b id -> (a, b) eq =
      fun (module TyA) (module TyB) ->  TyB.eq TyA.witness

  end

  type descr = {
    name: string ;
    descr: string option ;
  }

  type 'a arg = {
    id: 'a Ty.id;
    destruct: string -> ('a, string) result ;
    construct: 'a -> string ;
    descr: descr ;
  }

  let from_arg x = x
  let to_arg x = x

  type (_,_) rpath =
    | Root : ('rkey, 'rkey) rpath
    | Static : ('rkey, 'key) rpath * string -> ('rkey, 'key) rpath
    | Dynamic : ('rkey, 'key) rpath * 'a arg -> ('rkey, 'key * 'a) rpath
    | DynamicTail : ('rkey, 'key) rpath * 'a arg -> ('rkey, 'key * 'a list) rpath

  type (_,_) path =
    | Path: ('prefix, 'params) rpath -> ('prefix, 'params) path
    | MappedPath:
        ('prefix, 'key) rpath * ('key -> 'params) * ('params -> 'key) ->
      ('prefix, 'params) path

  let from_path x = x
  let to_path x = x

  type 'a query =
    (* inspired from Irmin.Ty.record. *)
    | Fields: ('a, 'b) query_fields * 'b -> 'a query

  and ('a, 'b) query_fields =
    | F0: ('a, 'a) query_fields
    | F1: ('a, 'b) query_field * ('a, 'c) query_fields ->
      ('a, 'b -> 'c) query_fields

  and ('a, 'b) query_field = {
    fname : string ;
    ftype : 'b arg ;
    fdefault : 'b ;
    fget : 'a -> 'b ;
    fdescription : string option ;
  }

  let from_query x = x
  let to_query x = x

end

open Internal

module Arg = struct

  type descr = Internal.descr = {
    name: string ;
    descr: string option ;
  }
  type 'a t = 'a Internal.arg
  type 'a arg = 'a t

  let make ?descr ~name ~destruct ~construct () =
    let id = Ty.new_id () in
    let descr = { name ; descr } in
    { descr ; id ; construct ; destruct }

  let like arg ?descr name =
    { arg with id = Ty.new_id () ; descr = { name ; descr } }

  let descr (ty: 'a arg) = ty.descr

  let bool : bool arg =
    let bool_of_string s =
      match String.lowercase_ascii s with
      | "false" | "no" -> Ok false
      | _ -> Ok true in
    let string_of_bool = function
      | true -> "yes"
      | false -> "no" in
    make ~name:"bool" ~destruct:bool_of_string ~construct:string_of_bool ()
  let int =
    let int_of_string s =
      try Ok (int_of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse integer value: %S." s) in
    make ~name:"int" ~destruct:int_of_string ~construct:string_of_int ()
  let float =
    let float_of_string s =
      try Ok (float_of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse float value: %S." s) in
    make ~name:"float" ~destruct:float_of_string ~construct:string_of_float ()
  let int32 =
    let int32_of_string s =
      try Ok (Int32.of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse int32 value: %S." s) in
    make ~name:"int32" ~destruct:int32_of_string ~construct:Int32.to_string ()
  let int64 =
    let int64_of_string s =
      try Ok (Int64.of_string s)
      with Failure _ ->
        Error (Printf.sprintf "Cannot parse int64 value: %S." s) in
    make ~name:"int64" ~destruct:int64_of_string ~construct:Int64.to_string ()
  let string =
    make ~name:"string" ~destruct:(fun x -> Ok x) ~construct:(fun x -> x) ()

end

module Path = struct

  type ('a, 'b) t = ('a, 'b) Internal.path
  type ('a, 'b) path = ('a, 'b) Internal.path
  type ('a, 'b) rpath = ('a, 'b) Internal.rpath

  type 'prefix context = ('prefix, 'prefix) path

  let root = Path Root
  let open_root = Path Root

  let add_suffix (type p pr) (path : (p, pr) path) name =
    match path with
    | Path (DynamicTail _) -> invalid_arg "Resto.Path.add_suffix"
    | MappedPath (DynamicTail _, _, _) -> invalid_arg "Resto.Path.add_suffix"
    | Path path -> Path (Static (path, name))
    | MappedPath (path, map, rmap) ->
        MappedPath (Static (path, name), map, rmap)

  let add_arg (type p pr) (path : (p, pr) path)  arg =
    match path with
    | Path (DynamicTail _) -> invalid_arg "Resto.Path.add_arg"
    | MappedPath (DynamicTail _, _, _) -> invalid_arg "Resto.Path.add_arg"
    | Path path -> Path (Dynamic (path, arg))
    | MappedPath (path, map, rmap) ->
        MappedPath (Dynamic (path, arg),
                    (fun (x, y) -> (map x, y)),
                    (fun (x, y) -> (rmap x, y)))

  let add_final_args (type p pr) (path : (p, pr) path)  arg =
    match path with
    | Path (DynamicTail _) -> invalid_arg "Resto.Path.add_final_arg"
    | MappedPath (DynamicTail _, _, _) -> invalid_arg "Resto.Path.add_final_arg"
    | Path path -> Path (DynamicTail (path, arg))
    | MappedPath (path, map, rmap) ->
        MappedPath (DynamicTail (path, arg),
                    (fun (x, y) -> (map x, y)),
                    (fun (x, y) -> (rmap x, y)))

  let map map rmap = function
    | Path p -> MappedPath (p, map, rmap)
    | MappedPath (p, map', rmap') ->
        MappedPath (p, (fun x -> map (map' x)), (fun x -> rmap' (rmap x)))

  let prefix
    : type p pr a. (pr, a) path -> (a, p) path -> (pr, p) path
    = fun p1 p2 ->
      let rec prefix
        : type pr a k.
          (pr, a) path -> (a, k) rpath -> (pr, k) path
        = fun p1 p2 ->
          match p2 with
          | Root -> p1
          | Static (path, name) -> add_suffix (prefix p1 path) name
          | Dynamic (path, arg) -> add_arg (prefix p1 path) arg
          | DynamicTail (path, arg) -> add_final_args (prefix p1 path) arg
      in
      match p1 with
      | Path (DynamicTail _) -> invalid_arg "Resto.Path.prefix"
      | MappedPath (DynamicTail _, _, _) -> invalid_arg "Resto.Path.prefix"
      | _ ->
          match p2 with
          | Path p2 -> prefix p1 p2
          | MappedPath (p2, m, rm) -> map m rm (prefix p1 p2)

  let (/) = add_suffix
  let (/:) = add_arg
  let (/:*) = add_final_args

end

module Query = struct

  type 'a t = 'a Internal.query
  type 'a query = 'a Internal.query
  type ('a, 'b) field = ('a, 'b) Internal.query_field

  type ('a, 'b, 'c) open_query =
    ('a, 'c) query_fields -> 'b * ('a, 'b) query_fields

  let field ?descr fname ftype fdefault fget =
    { fname; ftype; fdefault ; fget ; fdescription = descr }

  let query : 'b -> ('a, 'b, 'b) open_query =
    fun c fs -> c, fs

  let app : type a b c d.
    (a, b, c -> d) open_query -> (a, c) query_field -> (a, b, d) open_query
    = fun r f fs ->
      let c, fs = r (F1 (f, fs)) in
      c, fs

  let seal : type a b. (a, b, a) open_query -> a t =
    fun r ->
      let c, fs = r F0 in
      Fields (fs, c)

  let (|+) = app

  let empty = Fields (F0 , ())

  type 'a efield = Field: ('a, 'b) query_field -> 'a efield
  let fold_fields (type fs) ~f ~init fs =
    let rec loop : type f. _ -> (fs, f) query_fields -> _ = fun acc -> function
      | F0 -> acc
      | F1 (field, fs) -> loop (f acc (Field field)) fs in
    loop init fs

  type 'a parsed_field =
    | Parsed: ('a, 'b) query_field * 'b option -> 'a parsed_field

  let rec rebuild
    : type fs f. _ -> (fs, f) query_fields -> f -> fs
    = fun map fs f ->
      match fs with
      | F0 -> f
      | F1 (field, fs) ->
          let Parsed (field', v) = StringMap.find field.fname map in
          let Ty.Eq = Ty.eq field.ftype.id field'.ftype.id in
          let v = match v with None -> field.fdefault | Some v -> v in
          rebuild map fs (f v)

  exception Invalid of string
  type untyped = (string * string) list
  let parse (Fields (fs, f)) =
    let fields =
      fold_fields
        ~f:(fun map (Field f) -> StringMap.add f.fname (Parsed (f, None)) map)
        ~init:StringMap.empty
        fs in
    fun query ->
      let fail fmt = Format.kasprintf (fun s -> raise (Invalid s)) fmt in
      let fields =
        List.fold_left
          begin fun fields (name, value) ->
            match StringMap.find name fields with
            | exception Not_found -> fields
            | (Parsed (f, Some _)) ->
                (* TODO add an option to parse multiple as list. *)
                fail "Duplicate argument '%s' in query string." name
            | (Parsed (f, None)) ->
                match f.ftype.destruct value with
                | Error error ->
                    fail "Failed to parse argument '%s' (%S): %s"
                      name value error
                | Ok v -> StringMap.add name (Parsed (f, Some v)) fields
          end
          fields query in
      rebuild fields fs f

end

module Description = struct

  type request = {
    recurse: bool ;
  }

  let request_query =
    let open Query in
    query (fun recurse -> { recurse })
    |+ field "recurse" Arg.bool false (fun t -> t.recurse)
    |> seal

  type 'schema service = {
    description: string option ;
    path: path_item list ;
    meth: meth ;
    query: query_item list ;
    input: 'schema option ;
    output: 'schema ;
    error: 'schema ;
  }

  and path_item =
    | PStatic of string
    | PDynamic of Arg.descr
    | PDynamicTail of Arg.descr

  and query_item = {
    name: string ;
    description: string option ;
  }

  type 'schema directory =
    | Empty
    | Static of 'schema static_directory
    | Dynamic of string option

  and 'schema static_directory = {
    services: 'schema service MethMap.t ;
    subdirs: 'schema static_subdirectories option ;
  }

  and 'schema static_subdirectories =
    | Suffixes of 'schema directory Map.Make(String).t
    | Arg of Arg.descr * 'schema directory

  let rec pp_print_directory ppf =
    let open Format in
    function
    | Empty ->
        fprintf ppf "<empty>"
    | Static dir ->
        fprintf ppf "@[%a@]" pp_print_static_directory dir
    | Dynamic None ->
        fprintf ppf "<dyntree>"
    | Dynamic (Some descr) ->
        fprintf ppf "<dyntree> : %s" descr

  and pp_print_static_directory ppf =
    let open Format in
    function
    | { services ; subdirs = None } when MethMap.is_empty services ->
        fprintf ppf "{}"
    | { services ; subdirs = None } ->
        fprintf ppf "@[<v>%a@]"
          pp_print_dispatch_services services
    | { services ; subdirs = Some subdirs } when MethMap.is_empty services ->
        fprintf ppf "%a"
          pp_print_static_subdirectories subdirs
    | { services ; subdirs = Some subdirs } ->
        fprintf ppf "@[<v>%a@ %a@]"
          pp_print_dispatch_services services
          pp_print_static_subdirectories subdirs

  and pp_print_static_subdirectories ppf =
    let open Format in
    function
    | Suffixes map ->
        let print_binding ppf (name, tree) =
          fprintf ppf "@[<hov 2>%s:@ %a@]"
            name pp_print_directory tree in
        fprintf ppf "@[<v>%a@]"
          (pp_print_list ~pp_sep:pp_print_cut print_binding)
          (StringMap.bindings map)
    | Arg (arg, tree) ->
        fprintf ppf "@[<hov 2>[:%s:]@ @[%a@]@]"
          (arg.name) pp_print_directory tree

  and pp_print_dispatch_services ppf services =
    MethMap.iter
      begin fun meth s ->
        match s with
        | { description = None ; meth ; _ } ->
            Format.fprintf ppf "<%s>" (string_of_meth meth)
        | { description = Some descr ; meth ; _ } ->
            Format.fprintf ppf "<%s> : %s" (string_of_meth meth) descr
      end
      services

end

module type ENCODING = sig
  type 'a t
  type schema
  val unit : unit t
  val schema : 'a t -> schema
  val description_request_encoding : Description.request t
  val description_answer_encoding : schema Description.directory t
end

module MakeService(Encoding : ENCODING) = struct

  module Internal = struct
    include Internal
    type ('query, 'input, 'output, 'error) types = {
      query : 'query query ;
      input : 'input input ;
      output : 'output Encoding.t ;
      error : 'error Encoding.t ;
    }
    and _ input =
      | No_input : unit input
      | Input : 'input Encoding.t -> 'input input
    type (+'meth, 'prefix, 'params, 'query,
          'input, 'output, 'error) iservice = {
      description : string option ;
      meth : 'meth ;
      path : ('prefix, 'params) path ;
      types : ('query, 'input, 'output, 'error) types ;
    } constraint 'meth = [< meth ]
    let from_service x = x
    let to_service x = x

    type (_, _) eq =
      | Eq : (('query, 'input, 'output, 'error) types,
              ('query, 'input, 'output, 'error) types) eq
    exception Not_equal
    let eq :
      type query1 input1 output1 error1  query2 input2 output2 error2.
      (query1, input1, output1, error1) types ->
      (query2, input2, output2, error2) types ->
      ((query1, input1, output1, error1) types,
       (query2, input2, output2, error2) types) eq
      = fun x y ->
        if Obj.magic x == Obj.magic y then
          Obj.magic Eq (* FIXME *)
        else
          raise Not_equal

  end
  include Internal
  open Path

  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) Internal.iservice
  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t

  let get_service ?description ~query ~output ~error path =
    let input = No_input in
    { meth = `GET ; description ; path ;
      types = { query ; input ; output ; error } }

  let post_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    { meth = `POST ; description ; path ;
      types = { query ; input ; output ; error } }

  let delete_service ?description ~query ~output ~error path =
    let input = No_input in
    { meth = `DELETE ; description ; path ;
      types = { query ; input ; output ; error } }

  let put_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    { meth = `PUT ; description ; path ;
      types = { query ; input ; output ; error } }

  let patch_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    { meth = `PATCH ; description ; path ;
      types = { query ; input ; output ; error } }

  let prefix path s = { s with path = Path.prefix path s.path }

  let map f g (s : (_,_,_,_,_,_,_) service) =
    { s with path = Path.map f g s.path }


  let query
    : type pr p i q o e.
      (_, pr, p, q, i, o, e) service -> q Query.t
    = fun { types } -> types.query

  let input_encoding
    : type pr p i q o e.
      (_, pr , p, q, i, o, e) service -> i input
    = fun { types } -> types.input

  let output_encoding
    : type pr p i q o e.
      (_, pr, p, q, i, o, e) service -> o Encoding.t
    = fun { types } -> types.output

  let error_encoding
    : type pr p i q o e.
      (_, pr, p, q, i, o, e) service -> e Encoding.t
    = fun { types } -> types.error

  type ('prefix, 'params) description_service =
    ([ `GET ], 'prefix, 'params * string list, Description.request,
     unit, Encoding.schema Description.directory, unit) service

  let description_service ?description path =
    let description =
      match description with
      | Some descr -> descr
      | None -> "<TODO>"
    in
    get_service
      ~description
      ~query:Description.request_query
      ~output:Encoding.description_answer_encoding
      ~error:Encoding.unit
      Path.(path /:* Arg.string)

  type 'input request = {
    meth: meth ;
    path: string list ;
    query: (string * string) list ;
    input: 'input input ;
  }

  let forge_request_args
    : type p. (unit, p) path -> p -> string list
    = fun path args ->
      let rec forge_request_args
        : type k. (unit, k) rpath -> k -> string list -> string list
        = fun path args acc ->
          match path, args with
          | Root, _ ->
              acc
          | Static (path, name), args ->
              forge_request_args path args (name :: acc)
          | Dynamic (path, arg), (args, x) ->
              forge_request_args path args (arg.construct x :: acc)
          | DynamicTail (path, arg), (args, xs) ->
              forge_request_args path args
                (List.fold_right (fun x acc -> arg.construct x :: acc) xs acc) in
      match path with
      | Path path -> forge_request_args path args []
      | MappedPath (path, _, rmap) -> forge_request_args path (rmap args) []

  let forge_request_query
    : type q. q query -> q -> (string * string) list
    = fun (Fields (fields, _)) q ->
      let rec loop : type t. (q, t) query_fields -> _ = function
        | F0 -> []
        | F1 ({ fname ; ftype ; fget ; _ }, fields) ->
            (fname, ftype.construct (fget q)) :: loop fields in
      loop fields

  let forge_request
    : type p i q o e.
      (_, unit, p, q, i, o, e) service -> p -> q -> i request
    = fun s args query ->
      { meth = s.meth ;
        path = forge_request_args s.path args ;
        query = forge_request_query s.types.query query ;
        input = s.types.input ;
      }

  let forge_request =
    (forge_request
     : (meth, _, _, _, _, _, _) service -> _
     :> ([< meth], _, _, _, _, _, _) service -> _ )

end
