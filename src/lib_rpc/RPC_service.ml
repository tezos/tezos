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

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

let string_of_meth = Resto.string_of_meth
let meth_of_string = Resto.meth_of_string

let meth_encoding =
  let open Data_encoding in
  conv
    string_of_meth
    (fun m ->
       match meth_of_string m with
       | None -> Pervasives.failwith "Cannot parse methods"
       | Some s -> s)
    string

module MethMap = Resto.MethMap

type (+'m,'pr,'p,'q,'i,'o, 'e) raw =
  ('m,'pr,'p,'q,'i,'o, 'e) Resto.MakeService(RPC_encoding).t
  constraint 'meth = [< meth ]

type error = Error_monad.error list

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) t =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth ]

type (+'meth, 'prefix, 'params, 'query, 'input, 'output) service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output, error) raw
  constraint 'meth = [< meth ]

include (Resto.MakeService(RPC_encoding)
         : (module type of struct include Resto.MakeService(RPC_encoding) end
             with type (+'m,'pr,'p,'q,'i,'o, 'e) t := ('m,'pr,'p,'q,'i,'o, 'e) raw
              and type (+'m,'pr,'p,'q,'i,'o, 'e) service := ('m,'pr,'p,'q,'i,'o, 'e) raw)
        )


let error_path = ref None

let error_encoding =
  let open Data_encoding in
  delayed begin fun () ->
    let { meth ; uri ; _ } =
      match !error_path with
      | None -> assert false
      | Some p -> p in
    def
      "error"
      ~description:
        (Printf.sprintf
           "The full list of error is available with \
            the global RPC `%s %s`"
           (string_of_meth meth) (Uri.path_and_query uri)) @@
    conv
      ~schema:Json_schema.any
      (fun exn -> `A (List.map Error_monad.json_of_error exn))
      (function `A exns -> List.map Error_monad.error_of_json exns | _ -> [])
      json
  end

let get_service = get_service ~error:error_encoding
let post_service = post_service ~error:error_encoding
let delete_service = delete_service ~error:error_encoding
let patch_service = patch_service ~error:error_encoding
let put_service = put_service ~error:error_encoding

let error_service =
  get_service
    ~description: "Schema for all the RPC errors from the shell"
    ~query: RPC_query.empty
    ~output: Data_encoding.json_schema
    RPC_path.(root / "errors")

let () = error_path := Some (forge_request error_service () ())

let description_service =
  description_service
    ~description: "RPCs documentation and input/output schema"
    error_encoding
    RPC_path.(root / "describe")
