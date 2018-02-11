(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzServices
open EzResto
open EzDirectory
open Lwt.Infix

let () =
  Lwt_main.run begin
    allowed_methods dir ["foo";"3";"repeat"] >>= function
    | Ok [`POST] -> Lwt.return_unit
    | _ -> assert false
  end

let () =
  Lwt_main.run begin
    allowed_methods dir ["bar";"3";"4";"add"] >>= function
    | Ok [`GET;`POST] -> Lwt.return_unit
    | _ -> assert false
  end

module Test(Request : sig
    val request:
      ('meth, 'params, 'query, 'input, 'output, 'error) EzResto.service ->
      'params  -> 'query -> 'input -> [> ('output, 'error) Answer.t ] Lwt.t
  end) = struct

  let () =
    Lwt_main.run begin
      Request.request describe_service ((), []) { Description.recurse = true } () >>= function
      | `Ok dir ->
          Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
          Lwt.return_unit
      | _ -> assert false
    end

  let () =
    let test service args arg expected =
      Lwt_main.run (Request.request service args () arg) = `Ok expected in
    assert (test repeat_service ((), 3) (`A []) (`A (repeat 3 (`A [])))) ;
    assert (test add_service ((), 2) 3 5) ;
    assert (test alternate_add_service (((), 1), 2.5) () 3.5) ;
    assert (test alternate_add_service' (((), 1), 2.) () 3) ;
    ()
end

let split_path path =
  let l = String.length path in
  let rec do_slashes acc i =
    if i >= l then
      List.rev acc
    else if String.get path i = '/' then
      do_slashes acc (i + 1)
    else
      do_component acc i i
  and do_component acc i j =
    if j >= l then
      if i = j then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if String.get path j = '/' then
      do_slashes (String.sub path i (j - i) :: acc) j
    else
      do_component acc i (j + 1) in
  do_slashes [] 0

module Faked = Test(struct
    (** Testing faked client/server communication. *)
    let request (type i) (service: (_,_,_,i,_,_) service) params query (arg: i) =
      let { meth ; uri ; input } = forge_request service params query in
      Format.eprintf "\nREQUEST: %a@." Uri.pp_hum uri ;
      let path = split_path (Uri.path uri) in
      let query =
        List.map
          (fun (n,vs) -> (n, String.concat "," vs))
          (Uri.query uri) in
      Format.eprintf "\nREQUEST: %a@." Uri.pp_hum uri ;
      let json =
        match input with
        | No_input -> `O []
        | Input input -> Json_encoding.construct input arg in
      lookup dir meth path >>= function
      | Ok (Service s) -> begin
          let query = Resto.Query.parse s.types.query query in
          begin
            match s.types.input with
            | No_input -> s.handler query ()
            | Input input ->
                s.handler query @@ Json_encoding.destruct input json
          end >>= function
          | `Ok res ->
              let json = Json_encoding.construct s.types.output res in
              Lwt.return (`Ok (Json_encoding.destruct (output_encoding service) json))
          | _ -> failwith "Unexpected lwt result (1)"
        end
      | _ -> failwith "Unexpected lwt result (2)"
  end)

module Transparent = Test(struct
    let request x = transparent_lookup dir x
  end)

let () =
  Printf.printf "\n### OK EzResto ###\n\n%!"
