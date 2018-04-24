(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

module Answer = struct

  (** Return type for service handler *)
  type ('o, 'e) t =
    [ `Ok of 'o (* 200 *)
    | `OkStream of 'o stream (* 200 *)
    | `Created of string option (* 201 *)
    | `No_content (* 204 *)
    | `Unauthorized of 'e option (* 401 *)
    | `Forbidden of 'e option (* 403 *)
    | `Not_found of 'e option (* 404 *)
    | `Conflict of 'e option (* 409 *)
    | `Error of 'e option (* 500 *)
    ]

  and 'a stream = {
    next: unit -> 'a option Lwt.t ;
    shutdown: unit -> unit ;
  }

  let return x = Lwt.return (`Ok x)
  let return_stream x = Lwt.return (`OkStream x)

end

module Make (Encoding : ENCODING) = struct

  module Service = Resto.MakeService(Encoding)

  module Curry = struct

    type (_,_,_,_,_,_) conv =
      | Z : (unit, 'g, 'g, unit, 'f, 'f) conv
      | S : ('t, 'g, 'b * 's, 'rt, 'f, 'r) conv ->
        ('t * 'b, 'g, 's, 'a * 'rt, 'a -> 'f, 'r) conv
    let reverse
      : type a c d e f. (a, c, unit, d, e, f) conv -> a -> c
      = fun c v ->
        let rec reverse
          : type a c d e f g. (a, c, d, e, f, g) conv -> a -> d -> c
          = fun c v acc ->
            match c, v with
            | Z, _ -> acc
            | S c, (v, x) -> reverse c v (x, acc) in
        reverse c v ()
    let rec curry
      : type a b c d e f. (a, b, c, d, e, f) conv -> e -> d -> f
      = fun c f ->
        match c with
        | Z -> fun () -> f
        | S c -> (fun (v, x) -> curry c (f v) x)
    let curry c f =
      let f = curry c f in
      fun x -> f (reverse c x)

  end

  type step =
    | Static of string
    | Dynamic of Arg.descr
    | DynamicTail of Arg.descr

  type conflict =
    | CService of meth
    | CDir
    | CBuilder
    | CTail
    | CTypes of Arg.descr * Arg.descr
    | CType of Arg.descr * string list

  exception Conflict of step list * conflict

  open Resto.Internal

  type lookup_error =
    [ `Not_found (* 404 *)
    | `Method_not_allowed of meth list (* 405 *)
    | `Cannot_parse_path of string list * Arg.descr * string (* 400 *)
    ]

  type ('query, 'input, 'output, 'error) types
    = ('query, 'input, 'output, 'error) Service.Internal.types
    = {
      query : 'query Resto.Query.t ;
      input : 'input Service.input ;
      output : 'output Encoding.t ;
      error : 'error Encoding.t ;
    }

  type 'key t =
    | Empty : 'key t
    | Static : 'key static_directory -> 'key t
    | Dynamic : string option * ('key -> 'key directory Lwt.t) -> 'key t
    | DynamicTail : 'a arg * ('key * 'a list) t -> 'key t

  and 'key directory = 'key t
  and 'key static_directory = {
    services : 'key registered_service_builder MethMap.t ;
    subdirs : 'key static_subdirectories option
  }

  and _ static_subdirectories =
    | Suffixes: 'key directory StringMap.t -> 'key static_subdirectories
    | Arg: 'a Resto.Internal.arg * ('key * 'a) directory -> 'key static_subdirectories

  and registered_service =
    | Service :
        { types : ('q, 'i, 'o, 'e) types ;
          handler : ('q -> 'i -> ('o, 'e) Answer.t Lwt.t) ;
        } -> registered_service

  and 'key registered_service_builder = {
    meth : Resto.meth ;
    description : Encoding.schema Description.service ;
    builder : 'key -> registered_service Lwt.t ;
  }

  let empty = Empty

  let rec map_directory
    : type a b.
      (a -> b Lwt.t) -> b directory -> a directory
    = fun f t ->
      match t with
      | Empty -> Empty
      | Dynamic (descr, builder) ->
          let builder a = f a >>= builder >|= map_directory f in
          Dynamic (descr, builder)
      | DynamicTail (arg, dir) ->
          DynamicTail (arg, map_directory (fun (x, l) -> f x >|= fun x -> (x, l)) dir)
      | Static dir ->
          Static (map_static_directory f dir)

  and map_static_directory
    : type a b.
      (a -> b Lwt.t) -> b static_directory -> a static_directory
    = fun f t ->
      { services = MethMap.map (map_registered_service f) t.services ;
        subdirs = map_option (map_static_subdirectories f) t.subdirs ;
      }

  and map_static_subdirectories
    : type a b.
      (a -> b Lwt.t) -> b static_subdirectories -> a static_subdirectories
    = fun f t ->
      match t with
      | Suffixes map ->
          Suffixes (StringMap.map (map_directory f) map)
      | Arg (arg, dir) ->
          let dir = map_directory (fun (a, x) -> f a >|= fun a -> (a, x)) dir in
          Arg (arg, dir)

  and map_registered_service
    : type a b.
      (a -> b Lwt.t) -> b registered_service_builder -> a registered_service_builder
    = fun f rs ->
      { rs with builder = (fun p -> f p >>= fun p -> rs.builder p) }

  let map = map_directory

  let prefix
    : type p pr. (pr, p) Path.path -> p directory -> pr directory
    = fun path dir ->
      let rec prefix
        : type k pr. (pr, k) Resto.Internal.path -> k directory -> pr directory
        = fun path dir ->
          match path with
          | Root -> dir
          | Static (path, name) ->
              let subdirs = Suffixes (StringMap.singleton name dir) in
              prefix path (Static { subdirs = Some subdirs ;
                                    services = MethMap.empty })
          | Dynamic (path, arg) ->
              let subdirs = Arg (arg, dir) in
              prefix path (Static { subdirs = Some subdirs ;
                                    services = MethMap.empty })
          | DynamicTail _ ->
              invalid_arg "RestoDirectory.prefix" in
      prefix (Resto.Internal.to_path path) dir

  let conflict steps kind = raise (Conflict (steps, kind))

  let rec merge
    : type p.
      step list -> p directory -> p directory -> p directory
    = fun path t1 t2 ->
      match t1, t2 with
      | Empty, t -> t
      | t, Empty -> t
      | Static n1, Static n2 ->
          Static (merge_static_directory path n1 n2)
      | Dynamic _, _
      | _, Dynamic _ -> conflict path CBuilder
      | DynamicTail _, _
      | _, DynamicTail _ -> conflict path CTail

  and merge_static_directory
    : type p.
      step list -> p static_directory -> p static_directory -> p static_directory
    = fun path t1 t2 ->
      let subdirs =
        match t1.subdirs, t2.subdirs with
        | None, None -> None
        | None, Some dir | Some dir, None -> Some dir
        | Some d1, Some d2 ->
            match d1, d2 with
            | Suffixes m1, Suffixes m2 ->
                let merge =
                  StringMap.fold
                    (fun n t m ->
                       let st =
                         try StringMap.find n m with Not_found -> empty in
                       StringMap.add n (merge (Static n :: path) st t) m) in
                Some (Suffixes (merge m1 m2))
            | Arg (arg1, subt1), Arg (arg2, subt2) ->
                begin
                  try let Eq = Ty.eq arg1.id arg2.id in
                    let subt = merge (Dynamic arg1.descr :: path) subt1 subt2 in
                    Some (Arg (arg1, subt))
                  with Ty.Not_equal ->
                    conflict path (CTypes (arg1.descr, arg2.descr))
                end
            | Arg (arg, _), Suffixes m ->
                conflict path
                  (CType (arg.descr, List.map fst (StringMap.bindings m)))
            | Suffixes m, Arg (arg, _) ->
                conflict path
                  (CType (arg.descr, List.map fst (StringMap.bindings m))) in
      let services =
        MethMap.fold
          begin fun meth s map ->
            if MethMap.mem meth map then
              conflict path (CService meth)
            else
              MethMap.add meth s map
          end
          t1.services t2.services in
      { subdirs ; services }

  let merge x y = merge [] x y

  let rec describe_directory
    : type a. recurse:bool -> ?arg:a ->
    a directory -> Encoding.schema Description.directory Lwt.t
    = fun ~recurse ?arg dir ->
      match dir with
      | Empty -> Lwt.return Description.Empty
      | Dynamic (descr, builder) -> begin
          match arg with
          | None ->
              Lwt.return (Dynamic descr : Encoding.schema Description.directory)
          | Some arg ->
              builder arg >>= fun dir -> describe_directory ~recurse dir
        end
      | DynamicTail ( _, dir) -> describe_directory ~recurse dir
      | Static dir ->
          describe_static_directory recurse dir >>= fun dir ->
          Lwt.return (Static dir : Encoding.schema Description.directory)

  and describe_static_directory
    : type a.
      bool -> a static_directory ->
      Encoding.schema Description.static_directory Lwt.t
    = fun recurse dir ->
      let services = MethMap.map describe_service dir.services in
      begin
        if recurse then
          match dir.subdirs with
          | None -> Lwt.return_none
          | Some subdirs ->
              describe_static_subdirectories subdirs >>= fun dirs ->
              Lwt.return (Some dirs)
        else
          Lwt.return_none
      end >>= fun subdirs ->
      Lwt.return ({ services ; subdirs } : Encoding.schema Description.static_directory)

  and describe_static_subdirectories
    : type a.
      a static_subdirectories ->
      Encoding.schema Description.static_subdirectories Lwt.t
    = fun dir ->
      match dir with
      | Suffixes map ->
          StringMap.fold (fun key dir map ->
              map >>= fun map ->
              describe_directory ~recurse:true dir >>= fun dir ->
              Lwt.return (StringMap.add key dir map))
            map (Lwt.return StringMap.empty) >>= fun map ->
          Lwt.return (Suffixes map : Encoding.schema Description.static_subdirectories)
      | Arg (arg, dir) ->
          describe_directory ~recurse:true dir >>= fun dir ->
          Lwt.return (Arg (arg.descr, dir)
                      : Encoding.schema Description.static_subdirectories)

  and describe_service
    : type a.
      a registered_service_builder -> Encoding.schema Description.service
    = fun { description ; _ } -> description

  and describe_query
    : type a.
      a Resto.Internal.query -> Description.query_item list
    = fun (Fields (fields, _)) ->
      let rec loop : type a b. (a, b) query_fields -> _ = function
        | F0 -> []
        | F1 (f, fs) ->
            { Description.name = field_name f ;
              description = field_description f ;
              kind = field_kind f } :: loop fs in
      loop fields


  (****************************************************************************
   * Lookup
   ****************************************************************************)

  type resolved_directory =
      Dir: 'a static_directory * 'a -> resolved_directory

  let rec resolve
    : type a.
      string list -> a directory -> a -> string list ->
      (resolved_directory, _) result Lwt.t
    = fun prefix dir args path ->
      match path, dir with
      | _, Empty -> Lwt.return_error `Not_found
      | path, Dynamic (_, builder) ->
          builder args >>= fun dir -> resolve prefix dir args path
      | path, DynamicTail (arg, dir) -> begin
          match
            List.fold_right
              (fun e acc ->
                 match acc with
                 | Error _ as err -> err
                 | Ok (prefix, path) ->
                     match arg.destruct e with
                     | Ok s -> Ok (e :: prefix, s :: path)
                     | Error msg ->
                         Error (`Cannot_parse_path (List.rev (e :: prefix), arg.descr, msg)))

              path (Ok (prefix, []))
          with
          | Ok (prefix, path) -> resolve prefix dir (args, path) []
          | Error _ as err -> Lwt.return err
        end
      | [], Static sdir -> Lwt.return_ok (Dir (sdir, args))
      | _name :: _path, Static { subdirs = None ; _ } ->
          Lwt.return_error `Not_found
      | name :: path,
        Static { subdirs = Some (Suffixes static) ; _ } -> begin
          match StringMap.find name static with
          | exception Not_found -> Lwt.return_error `Not_found
          | dir -> resolve (name :: prefix) dir args path
        end
      | name :: path, Static { subdirs = Some (Arg (arg, dir)) ; _ } ->
          match arg.destruct name with
          | Ok x -> resolve (name :: prefix) dir (args, x) path
          | Error msg ->
              Lwt.return_error @@
              `Cannot_parse_path (List.rev (name :: prefix), arg.descr, msg)

  let lookup
    : type a.
      a directory -> a -> meth -> string list ->
      (registered_service, lookup_error) result Lwt.t
    = fun dir args meth path ->
      resolve [] dir args path >>= function
      | Error _ as err -> Lwt.return err
      | Ok (Dir (dir, args)) -> begin
          match MethMap.find meth dir.services with
          | exception Not_found -> begin
              match MethMap.bindings dir.services with
              | [] -> Lwt.return_error `Not_found
              | l -> Lwt.return_error (`Method_not_allowed (List.map fst l))
            end
          | rs -> rs.builder args >>= Lwt.return_ok
        end

  let lookup =
    (lookup
     : _ -> _ -> _ -> _ -> (_, lookup_error) result Lwt.t
     :> _ -> _ -> _ -> _ -> (_, [> lookup_error ]) result Lwt.t )

  let allowed_methods
    : type a.
      a directory -> a -> string list ->
      (Resto.meth list, lookup_error) result Lwt.t
    = fun dir args path ->
      resolve [] dir args path >>= function
      | Error err -> Lwt.return_error err
      | Ok (Dir (dir, _)) -> begin
          match MethMap.bindings dir.services with
          | [] -> Lwt.return_error `Not_found
          | l -> Lwt.return_ok (List.map fst l)
        end

  let allowed_methods =
    (allowed_methods
     : _ -> _ -> _ -> (_, lookup_error) result Lwt.t
     :> _ -> _ -> _ -> (_, [> lookup_error]) result Lwt.t)


  let rec build_dynamic_dir : type p. p directory -> p -> p directory Lwt.t =
    fun dir args ->
      match dir with
      | Dynamic (_, builder) ->
          builder args >>= fun dir -> build_dynamic_dir dir args
      | _ -> Lwt.return dir

  let rec transparent_resolve
    : type pr p.
      pr directory -> (pr, p) path -> p -> p directory option Lwt.t
    = fun dir path rargs ->
      match path with
      | Root -> Lwt.return_some dir
      | Static (path, name) -> begin
          transparent_resolve dir path rargs >>= function
          | None -> Lwt.return_none
          | Some dir ->
              build_dynamic_dir dir rargs >>= function
              | Dynamic (_,_) -> assert false (* should not happen. *)
              | Static { subdirs = Some (Suffixes s) ; _ } ->
                  Lwt.return_some (StringMap.find name s)
              | Empty -> Lwt.return_none
              | Static _ -> Lwt.return_none
              | DynamicTail _ -> Lwt.return_none
        end
      | Dynamic (ipath, iarg) -> begin
          transparent_resolve dir ipath (fst rargs) >>= function
          | None -> Lwt.return_none
          | Some dir ->
              build_dynamic_dir dir (fst rargs) >>= function
              | Dynamic (_, _) -> assert false (* should not happen. *)
              | Static { subdirs = Some (Arg (arg, dir)) ; _ } -> begin
                  match Ty.eq iarg.id arg.id with
                  | exception Ty.Not_equal ->
                      Lwt.return_none
                  | Eq ->
                      Lwt.return_some (dir : (_ * _) directory :> p directory)
                end
              | Empty -> Lwt.return_none
              | Static _ -> Lwt.return_none
              | DynamicTail _ -> Lwt.return_none
        end
      | DynamicTail (path, arg) -> begin
          transparent_resolve dir path (fst rargs) >>= function
          | None -> Lwt.return_none
          | Some dir ->
              build_dynamic_dir dir (fst rargs) >>= function
              | Dynamic (_,_) -> assert false (* should not happen. *)
              | DynamicTail (iarg, dir) -> begin
                  match Ty.eq iarg.id arg.id with
                  | exception Ty.Not_equal ->
                      Lwt.return_none
                  | Eq ->
                      Lwt.return_some (dir : (_ * _) directory :> p directory)
                end
              | Empty -> Lwt.return_none
              | Static _ -> Lwt.return_none
        end

  let transparent_lookup :
    type prefix params query input output error.
    prefix directory ->
    (_, prefix, params, query, input, output, error) Service.t ->
    params -> query -> input -> (output, error) Answer.t Lwt.t =
    fun dir service params query body ->
      let service = Service.Internal.to_service service in
      transparent_resolve dir service.path params >>= function
      | None -> Lwt.return (`Not_found None)
      | Some (Static { services ; _ }) -> begin
          try
            (MethMap.find service.meth services).builder
              params >>= fun (Service { handler ; types }) ->
            match Service.Internal.eq types service.types with
            | exception Service.Internal.Not_equal ->
                Lwt.return (`Not_found None)
            | Service.Internal.Eq ->
                (handler query body
                 : (_, _) Answer.t Lwt.t :> (output, error) Answer.t Lwt.t)
          with Not_found -> Lwt.return (`Not_found None)
        end
      | Some _ -> Lwt.return (`Not_found None)

  let transparent_lookup =
    ( transparent_lookup
      : _ -> (Resto.meth, _, _, _, _, _, _) Service.t ->
      _ -> _ -> _ -> (_, _) Answer.t Lwt.t
      :> _ -> ([< Resto.meth ], _, _, _, _, _, _) Service.t ->
      _ -> _ -> _ -> [> (_, _) Answer.t ] Lwt.t)

  let rec describe_rpath
    : type a b. Description.path_item list ->
      (a, b) path -> Description.path_item list
    = fun acc path ->
      match path with
      | Root -> acc
      | Static (rpath, name) ->
          describe_rpath (PStatic name :: acc) rpath
      | Dynamic (rpath, arg) ->
          describe_rpath (PDynamic arg.descr :: acc) rpath
      | DynamicTail (rpath, arg) ->
          describe_rpath (PDynamicTail arg.descr :: acc) rpath

  (****************************************************************************
   * Registration
   ****************************************************************************)

  let rec step_of_path
    : type p rk. (rk, p) path -> step list -> step list
    = fun path acc ->
      match path with
      | Root -> acc
      | Static (path, name) -> step_of_path path (Static name :: acc)
      | Dynamic (path, arg) -> step_of_path path (Dynamic arg.descr :: acc)
      | DynamicTail (path, arg) -> step_of_path path (DynamicTail arg.descr :: acc)
  let step_of_path p = step_of_path p []

  let conflict path kind = raise (Conflict (step_of_path path, kind))

  let rec insert
    : type k rk.
      (rk, k) path -> rk directory -> k directory * (k directory -> rk directory)
    = fun path dir ->
      match path with
      | Root -> dir, (fun x -> x)
      | Static (subpath, name) -> begin
          let subdir, rebuild = insert subpath dir in
          let dirmap, services =
            match subdir with
            | Empty ->
                StringMap.empty, MethMap.empty
            | Static { subdirs = None ; services } ->
                StringMap.empty, services
            | Static { subdirs = Some (Suffixes m) ;
                       services } ->
                m, services
            | Static { subdirs = Some (Arg (arg, _)) ; _ } ->
                conflict path (CType (arg.descr, [name]))
            | Dynamic _ -> conflict path CBuilder
            | DynamicTail _ -> conflict path CTail in
          let dir =
            try StringMap.find name dirmap with Not_found -> empty in
          let rebuild s =
            let subdirs =
              Some (Suffixes (StringMap.add name s dirmap)) in
            rebuild (Static { subdirs ; services }) in
          dir, rebuild
        end
      | Dynamic (subpath, arg) -> begin
          let subdir, rebuild = insert subpath dir in
          let dir, services =
            match subdir with
            | Empty ->
                Empty, MethMap.empty
            | Static { subdirs = None ; services } ->
                Empty, services
            | Static { subdirs = Some (Arg (arg', dir)) ;
                       services } -> begin
                try
                  let Eq = Ty.eq arg.id arg'.id in
                  (dir :> k directory), services
                with Ty.Not_equal ->
                  conflict path (CTypes (arg.descr, arg'.descr))
              end
            | Static { subdirs = Some (Suffixes m) ; _ } ->
                conflict path
                  (CType (arg.descr, List.map fst (StringMap.bindings m)))
            | Dynamic _ -> conflict path CBuilder
            | DynamicTail _ -> conflict path CTail
          in
          let rebuild s =
            let subdirs = Some (Arg (arg, s)) in
            rebuild (Static { subdirs ; services }) in
          dir, rebuild
        end
      | DynamicTail (subpath, arg) -> begin
          let subdir, rebuild = insert subpath dir in
          match subdir with
          | Empty ->
              let rebuild s = rebuild (DynamicTail (arg, s)) in
              empty, rebuild
          | Static { subdirs = None ; services } ->
              conflict path (CService (fst (MethMap.min_binding services)))
          | Static { subdirs = Some (Arg (arg, _)) ; _ } ->
              conflict path (CType (arg.descr, []))
          | Static { subdirs = Some (Suffixes m) ; _ } ->
              conflict path
                (CType (arg.descr, List.map fst (StringMap.bindings m)))
          | Dynamic _ -> conflict path CBuilder
          | DynamicTail _ -> conflict path CTail
        end

  let register
    : type p q i o e pr.
      pr directory -> (_, pr, p, q, i, o, e) Service.t ->
      (p -> q -> i -> (o, e) Answer.t Lwt.t) -> pr directory =
    fun root s handler ->
      let s = Service.Internal.to_service s in
      let register
        : type k. (pr, k) path -> (k -> q -> i -> (o, e) Answer.t Lwt.t) ->
          pr directory =
        fun path handler ->
          let dir, insert = insert path root in
          let rs =
            let description : _ Description.service = {
              meth = s.meth ;
              path = describe_rpath [] path ;
              description = s.description ;
              query = describe_query (Resto.Internal.to_query s.types.query) ;
              input = begin
                match s.types.input with
                | Service.No_input -> None
                | Service.Input input -> Some (Encoding.schema input)
              end ;
              output = Encoding.schema s.types.output ;
              error = Encoding.schema s.types.error ;
            } in
            let builder key = Lwt.return (Service {
                types = s.types ;
                handler = handler key ;
              }) in
            { meth = s.meth ; description ; builder } in
          match dir with
          | Empty ->
              insert (Static { services = MethMap.singleton s.meth rs ;
                               subdirs = None })
          | Static ({ services ; _ } as dir)
            when not (MethMap.mem s.meth services) ->
              insert (Static { dir with services = MethMap.add s.meth rs services })
          | Static _ -> conflict path (CService s.meth)
          | Dynamic _ -> conflict path CBuilder
          | DynamicTail _ -> conflict path CTail in
      register s.path handler

  let register =
    (register
     : _ -> (Resto.meth, _, _, _, _, _, _) Service.t ->
     (_ -> _ -> _ -> (_, _) Answer.t Lwt.t) -> _
     :> _ -> ([< Resto.meth ], _, _, _, _, _, _) Service.t ->
     (_ -> _ -> _ -> [< (_, _) Answer.t ] Lwt.t) -> _)

  let register_dynamic_directory
    : type pr a pr.
      ?descr:string ->
      pr directory -> (pr, a) Path.path ->
      (a -> a directory Lwt.t) -> pr directory =
    fun ?descr root path builder ->
      let path = Resto.Internal.to_path path in
      let register
        : type k. (pr, k) path -> (k -> k directory Lwt.t) -> pr directory =
        fun path builder ->
          let dir, insert = insert path root in
          match dir with
          | Empty ->
              insert (Dynamic (descr, builder))
          | Static ({ services ; subdirs = None }) ->
              conflict path (CService (fst (MethMap.choose services)))
          | Static ({ subdirs = Some _ ; _ }) -> conflict path CDir
          | Dynamic _ -> conflict path CBuilder
          | DynamicTail _ -> conflict path CTail in
      register path builder

  let register_describe_directory_service
    : type pr.
      pr directory ->
      (pr, pr, _) Service.description_service ->
      pr directory
    = fun root service ->
      let dir = ref root in
      let lookup (args, path) { Description.recurse } () =
        resolve [] root args path >>= function
        | Error `Not_found
        | Error `Cannot_parse_path _ ->
            Lwt.return (`Not_found None)
        | Ok (Dir (dir, arg)) ->
            describe_directory ~recurse ~arg (Static dir) >>= function
            | Static { services ; _ }
              when not recurse && MethMap.is_empty services ->
                Lwt.return (`Not_found None)
            | d ->
                Lwt.return (`Ok d)
      in
      dir := register root service lookup ;
      !dir

  (****************************************************************************
   * Let's currify!
   ****************************************************************************)

  open Curry

  let register0 root s f = register root s (curry Z f)
  let register1 root s f = register root s (curry (S Z) f)
  let register2 root s f = register root s (curry (S (S Z)) f)
  let register3 root s f = register root s (curry (S (S (S Z))) f)
  let register4 root s f = register root s (curry (S (S (S (S Z)))) f)
  let register5 root s f = register root s (curry (S (S (S (S (S Z))))) f)

  let register_dynamic_directory1 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S Z) f)
  let register_dynamic_directory2 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S Z)) f)
  let register_dynamic_directory3 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S (S Z))) f)


end
