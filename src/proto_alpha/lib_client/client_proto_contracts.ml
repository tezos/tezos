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

open Proto_alpha
open Alpha_context

module ContractEntity = struct
  type t = Contract.t
  let encoding = Contract.encoding
  let of_source s =
    match Contract.of_b58check s with
    | Error _ as err ->
        Lwt.return (Alpha_environment.wrap_error err)
        |> trace (failure "bad contract notation")
    | Ok s -> return s
  let to_source s = return (Contract.to_b58check s)
  let name = "contract"
end

module RawContractAlias = Client_aliases.Alias (ContractEntity)

module ContractAlias = struct

  let find cctxt s =
    RawContractAlias.find_opt cctxt s >>=? function
    | Some v -> return (s, v)
    | None ->
        Client_keys.Public_key_hash.find_opt cctxt s >>=? function
        | Some v ->
            return (s, Contract.implicit_contract v)
        | None ->
            failwith "no contract or key named %s" s

  let find_key cctxt name =
    Client_keys.Public_key_hash.find cctxt name >>=? fun v ->
    return (name, Contract.implicit_contract v)

  let rev_find cctxt c =
    match Contract.is_implicit c with
    | Some hash -> begin
        Client_keys.Public_key_hash.rev_find cctxt hash >>=? function
        | Some name -> return_some ("key:" ^ name)
        | None -> return_none
      end
    | None -> RawContractAlias.rev_find cctxt c

  let get_contract cctxt s =
    match String.split ~limit:1 ':' s with
    | [ "key" ; key ]->
        find_key cctxt key
    | _ -> find cctxt s

  let autocomplete cctxt =
    Client_keys.Public_key_hash.autocomplete cctxt >>=? fun keys ->
    RawContractAlias.autocomplete cctxt >>=? fun contracts ->
    return (List.map ((^) "key:") keys @ contracts)

  let alias_param ?(name = "name") ?(desc = "existing contract alias") next =
    let desc =
      desc ^ "\n"
      ^ "Can be a contract alias or a key alias (autodetected in order).\n\
         Use 'key:name' to force the later." in
    Clic.(
      param ~name ~desc
        (parameter ~autocomplete:autocomplete
           (fun cctxt p -> get_contract cctxt p))
        next)

  let destination_param ?(name = "dst") ?(desc = "destination contract") next =
    let desc =
      desc ^ "\n"
      ^ "Can be an alias, a key, or a literal (autodetected in order).\n\
         Use 'text:literal', 'alias:name', 'key:name' to force." in
    Clic.(
      param ~name ~desc
        (parameter
           ~autocomplete:(fun cctxt ->
               autocomplete cctxt >>=? fun list1 ->
               Client_keys.Public_key_hash.autocomplete cctxt >>=? fun list2 ->
               return (list1 @ list2))
           (fun cctxt s ->
              begin
                match String.split ~limit:1 ':' s with
                | [ "alias" ; alias ]->
                    find cctxt alias
                | [ "key" ; text ] ->
                    Client_keys.Public_key_hash.find cctxt text >>=? fun v ->
                    return (s, Contract.implicit_contract v)
                | _ ->
                    find cctxt s >>= function
                    | Ok v -> return v
                    | Error k_errs ->
                        ContractEntity.of_source s >>= function
                        | Ok v -> return (s, v)
                        | Error c_errs ->
                            Lwt.return (Error (k_errs @ c_errs))
              end)))
      next

  let name cctxt contract =
    rev_find cctxt contract >>=? function
    | None -> return (Contract.to_b58check contract)
    | Some name -> return name

end

let list_contracts cctxt =
  RawContractAlias.load cctxt >>=? fun raw_contracts ->
  Lwt_list.map_s
    (fun (n, v) -> Lwt.return ("", n, v))
    raw_contracts >>= fun contracts ->
  Client_keys.Public_key_hash.load cctxt >>=? fun keys ->
  (* List accounts (implicit contracts of identities) *)
  map_s (fun (n, v) ->
      RawContractAlias.mem cctxt n >>=? fun mem ->
      let p = if mem then "key:" else "" in
      let v' = Contract.implicit_contract v in
      return (p, n, v'))
    keys >>=? fun accounts ->
  return (contracts @ accounts)

let get_manager cctxt ~chain ~block source =
  match Contract.is_implicit source with
  | Some hash -> return hash
  | None -> Alpha_services.Contract.manager cctxt (chain, block) source

let get_delegate cctxt ~chain ~block source =
  Alpha_services.Contract.delegate_opt cctxt (chain, block) source
