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

module StringMap = Map.Make(String)

type 'key t = 'key description ref

and 'key description =
  | Empty : 'key description
  | Value : { get: 'key -> 'a option tzresult Lwt.t ;
              encoding: 'a Data_encoding.t } -> 'key description
  | NamedDir: 'key t StringMap.t -> 'key description
  | IndexedDir: { arg: 'a RPC_arg.t ;
                  arg_encoding: 'a Data_encoding.t ;
                  list: 'key -> 'a list tzresult Lwt.t ;
                  subdir: ('key * 'a) t }-> 'key description

let rec register_named_subcontext : type r. r t -> string list -> r t =
  fun dir names ->
    match !dir, names with
    | _, [] -> dir
    | Value _, _ -> invalid_arg ""
    | IndexedDir _, _ -> invalid_arg ""
    | Empty, name :: names ->
        let subdir = ref Empty in
        dir := NamedDir (StringMap.singleton name subdir) ;
        register_named_subcontext subdir names
    | NamedDir map, name :: names ->
        let subdir =
          match StringMap.find_opt name map with
          | Some subdir -> subdir
          | None ->
              let subdir = ref Empty in
              dir := NamedDir (StringMap.add name subdir map) ;
              subdir in
        register_named_subcontext subdir names

type (_, _, _) args =
  | One : { rpc_arg: 'a RPC_arg.t ;
            encoding: 'a Data_encoding.t ;
            compare: 'a -> 'a -> int } -> ('key, 'a, 'key * 'a) args
  | Pair : ('key, 'a, 'inter_key) args *
           ('inter_key, 'b, 'sub_key) args -> ('key, 'a * 'b, 'sub_key) args

let rec unpack : type a b c. (a, b, c) args -> c -> a * b = function
  | One _ -> (fun x -> x)
  | Pair (l, r) ->
      let unpack_l = unpack l in
      let unpack_r = unpack r in
      fun x ->
        let c, d = unpack_r x in
        let b, a = unpack_l c in
        (b, (a, d))

let rec pack : type a b c. (a, b, c) args -> a -> b -> c = function
  | One _ -> (fun b a -> (b, a))
  | Pair (l, r) ->
      let pack_l = pack l in
      let pack_r = pack r in
      fun b (a, d) ->
        let c = pack_l b a in
        pack_r c d

let rec compare : type a b c. (a, b, c) args -> b -> b -> int = function
  | One { compare ; _ } -> compare
  | Pair (l, r) ->
      let compare_l = compare l in
      let compare_r = compare r in
      fun (a1, b1) (a2, b2) ->
        match compare_l a1 a2 with
        | 0 -> compare_r b1 b2
        | x -> x

let destutter equal l =
  match l with
  | [] -> []
  | (i, _) :: l ->
      let rec loop acc i = function
        | [] -> acc
        | (j, _) :: l ->
            if equal i j then loop acc i l
            else loop (j :: acc) j l in
      loop [i] i l

let rec register_indexed_subcontext
  : type r a b. r t -> list:(r -> a list tzresult Lwt.t) ->
  (r, a, b) args -> b t =
  fun dir ~list path ->
    match path with
    | Pair (left, right) ->
        let compare_left = compare left in
        let equal_left x y = Compare.Int.(compare_left x y = 0) in
        let list_left r =
          list r >>=? fun l ->
          return (destutter equal_left l) in
        let list_right r =
          let a, k = unpack left r in
          list a >>=? fun l ->
          return
            (List.map snd
               (List.filter (fun (x, _) ->  equal_left x k) l)) in
        register_indexed_subcontext
          (register_indexed_subcontext dir ~list:list_left left)
          ~list:list_right right
    | One { rpc_arg = arg ; encoding = arg_encoding ; _ } ->
        match !dir with
        | Value _ -> invalid_arg ""
        | NamedDir _ -> invalid_arg ""
        | Empty ->
            let subdir = ref Empty in
            dir := IndexedDir { arg ; arg_encoding ; list ;  subdir };
            subdir
        | IndexedDir { arg = inner_arg ; subdir ; _ } ->
            match RPC_arg.eq arg inner_arg with
            | None -> invalid_arg ""
            | Some RPC_arg.Eq -> subdir

let register_value :
  type a b. a t -> get:(a -> b option tzresult Lwt.t) -> b Data_encoding.t -> unit =
  fun dir ~get encoding ->
    match !dir with
    | Empty -> dir := Value { get ; encoding }
    | _ -> invalid_arg ""

let create () = ref Empty

let rec pp : type a. Format.formatter -> a t -> unit = fun ppf dir ->
  match !dir with
  | Empty ->
      Format.fprintf ppf "EMPTY"
  | Value _e ->
      Format.fprintf ppf "Value"
  | NamedDir map ->
      Format.fprintf ppf "@[<v>%a@]"
        (Format.pp_print_list pp_item)
        (StringMap.bindings map)
  | IndexedDir { arg ; subdir ; _ } ->
      let name = Format.asprintf "<%s>" (RPC_arg.descr arg).name in
      pp_item ppf (name, subdir)

and pp_item : type a. Format.formatter -> (string * a t) -> unit =
  fun ppf (name, dir) ->
    Format.fprintf ppf "@[<v 2>%s@ %a@]"
      name
      pp dir


module type INDEX = sig
  type t
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  val rpc_arg: t RPC_arg.t
  val encoding: t Data_encoding.t
  val compare: t -> t -> int
end

type _ handler =
    Handler :
      { encoding: 'a Data_encoding.t ;
        get: 'key -> int -> 'a tzresult Lwt.t } -> 'key handler

type _ opt_handler =
    Opt_handler :
      { encoding: 'a Data_encoding.t ;
        get: 'key -> int -> 'a option tzresult Lwt.t } -> 'key opt_handler

let rec combine_object = function
  | [] -> Handler { encoding = Data_encoding.unit ;
                    get = fun _ _ -> return_unit }
  | (name, Opt_handler handler) :: fields ->
      let Handler handlers = combine_object fields in
      Handler { encoding =
                  Data_encoding.merge_objs
                    Data_encoding.(obj1 (opt name (dynamic_size handler.encoding)))
                    handlers.encoding ;
                get = fun k i ->
                  handler.get k i >>=? fun v1 ->
                  handlers.get k i >>=? fun v2 ->
                  return (v1, v2) }

type query = {
  depth: int ;
}

let depth_query =
  let open RPC_query in
  query (fun depth -> { depth })
  |+ field "depth" RPC_arg.int 0 (fun t -> t.depth)
  |> seal

let build_directory : type key. key t -> key RPC_directory.t =
  fun dir ->
    let rpc_dir = ref (RPC_directory.empty : key RPC_directory.t) in
    let register : type ikey. (key, ikey) RPC_path.t -> ikey opt_handler -> unit =
      fun path (Opt_handler { encoding ; get }) ->
        let service =
          RPC_service.get_service
            ~query: depth_query
            ~output: encoding
            path in
        rpc_dir :=
          RPC_directory.register !rpc_dir service begin
            fun k q () ->
              get k (q.depth + 1) >>=? function
              | None -> raise Not_found
              | Some x -> return x
          end in
    let rec build_handler : type ikey. ikey t -> (key, ikey) RPC_path.t -> ikey opt_handler =
      fun dir path ->
        match !dir with
        | Empty -> Opt_handler { encoding = Data_encoding.unit ;
                                 get = fun _ _ -> return_none }
        | Value { get ; encoding } ->
            let handler =
              Opt_handler {
                encoding ;
                get =
                  fun k i -> if Compare.Int.(i < 0) then return_none else get k
              } in
            register path handler ;
            handler
        | NamedDir map ->
            let fields = StringMap.bindings map in
            let fields =
              List.map
                (fun (name, dir) ->
                   (name, build_handler dir RPC_path.(path / name)))
                fields in
            let Handler handler = combine_object fields in
            let handler =
              Opt_handler
                { encoding = handler.encoding ;
                  get = fun k i ->
                    if Compare.Int.(i < 0) then
                      return_none
                    else
                      handler.get k (i-1) >>=? fun v ->
                      return_some v } in
            register path handler ;
            handler
        | IndexedDir { arg ; arg_encoding ; list ; subdir } ->
            let Opt_handler handler =
              build_handler subdir RPC_path.(path /: arg) in
            let encoding =
              let open Data_encoding in
              union [
                case (Tag 0)
                  ~title:"Leaf"
                  (dynamic_size arg_encoding)
                  (function (key, None) -> Some key | _ -> None)
                  (fun key -> (key, None)) ;
                case (Tag 1)
                  ~title:"Dir"
                  (tup2
                     (dynamic_size arg_encoding)
                     (dynamic_size handler.encoding))
                  (function (key, Some value) -> Some (key, value) | _ -> None)
                  (fun (key, value) -> (key, Some value)) ;
              ] in
            let get k i =
              if Compare.Int.(i < 0) then return_none
              else if Compare.Int.(i = 0) then return_some []
              else
                list k >>=? fun keys ->
                map_p
                  (fun key ->
                     if Compare.Int.(i = 1) then
                       return (key, None)
                     else
                       handler.get (k, key) (i-1) >>=? fun value ->
                       return (key, value))
                  keys >>=? fun values ->
                return_some values in
            let handler =
              Opt_handler {
                encoding = Data_encoding.(list (dynamic_size encoding)) ;
                get ;
              } in
            register path handler ;
            handler in
    ignore (build_handler dir RPC_path.open_root : key opt_handler) ;
    !rpc_dir

