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

module Request = struct
  type view =
    | New_head of Block_hash.t
    | New_branch of Block_hash.t * int

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0) ~title:"New_head"
          (obj2
             (req "request" (constant "new_head"))
             (req "block" Block_hash.encoding))
          (function New_head h -> Some ((), h) | _ -> None)
          (fun ((), h) -> New_head h) ;
        case (Tag 1) ~title:"New_branch"
          (obj3
             (req "request" (constant "new_branch"))
             (req "block" Block_hash.encoding)
             (req "locator_length" uint16))
          (function New_branch (h, l) -> Some ((), h, l) | _ -> None)
          (fun ((), h, l) -> New_branch (h, l)) ]

  let pp ppf = function
    | New_head hash ->
        Format.fprintf ppf "New head %a" Block_hash.pp hash
    | New_branch (hash, len) ->
        Format.fprintf ppf "New branch %a, locator length %d"
          Block_hash.pp hash len
end

module Event = struct
  type t =
    | Request of (Request.view * Worker_types.request_status * error list option)
    | Debug of string

  let level req =
    match req with
    | Debug _ -> Internal_event.Debug
    | Request _ -> Internal_event.Info

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Debug"
          (obj1 (req "message" string))
          (function Debug msg -> Some msg | _ -> None)
          (fun msg -> Debug msg) ;
        case (Tag 1)
          ~title:"Request"
          (obj2
             (req "request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Request (req, t, None) -> Some (req, t) | _ -> None)
          (fun (req, t) -> Request (req, t, None)) ;
        case (Tag 2)
          ~title:"Failed request"
          (obj3
             (req "error" RPC_error.encoding)
             (req "failed_request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Request (req, t, Some errs) -> Some (errs, req, t) | _ -> None)
          (fun (errs, req, t) -> Request (req, t, Some errs)) ]

  let pp ppf = function
    | Debug msg -> Format.fprintf ppf "%s" msg
    | Request (view, { pushed ; treated ; completed }, None)  ->
        Format.fprintf ppf
          "@[<v 0>%a@,\
           Pushed: %a, Treated: %a, Completed: %a@]"
          Request.pp view
          Time.System.pp_hum pushed Time.System.pp_hum treated Time.System.pp_hum completed
    | Request (view, { pushed ; treated ; completed }, Some errors)  ->
        Format.fprintf ppf
          "@[<v 0>%a@,\
           Pushed: %a, Treated: %a, Failed: %a@,\
           %a@]"
          Request.pp view
          Time.System.pp_hum pushed Time.System.pp_hum treated Time.System.pp_hum completed
          (Format.pp_print_list Error_monad.pp) errors
end

module Worker_state = struct
  type view =
    { bootstrapped : bool ;
      mutable last_validated_head: Block_hash.t ;
      mutable last_advertised_head: Block_hash.t }
  let encoding =
    let open Data_encoding in
    conv
      (function { bootstrapped ; last_validated_head ; last_advertised_head } ->
         (bootstrapped, last_validated_head, last_advertised_head))
      (function (bootstrapped, last_validated_head, last_advertised_head) ->
         { bootstrapped ; last_validated_head ; last_advertised_head })
      (obj3
         (req "bootstrapped" bool)
         (req "last_validated_head" Block_hash.encoding)
         (req "last_advertised_head" Block_hash.encoding))

  let pp ppf state =
    Format.fprintf ppf
      "@[<v 0>Bootstrapped: %s@,\
       Last validated head: %a@,\
       Last advertised head: %a@]"
      (if state.bootstrapped then "yes" else "no")
      Block_hash.pp state.last_validated_head
      Block_hash.pp state.last_advertised_head

end
