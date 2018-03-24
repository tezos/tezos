(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let name = "bigmap"
module Logger = Logging.Make(struct let name = name end)

open Logger

module Helpers = Isolate_helpers
module Assert = Helpers.Assert

let (>>??) = Helpers.Assert.(>>??)
let (>>=??) = Helpers.Assert.(>>=??)

let parse_expr s : Proto_alpha.Alpha_context.Script.expr tzresult =
  Micheline_parser.no_parsing_error (Michelson_v1_parser.parse_expression s) >>? fun parsed ->
  ok parsed.expanded

let parse_script code_str storage_str : Proto_alpha.Alpha_context.Script.t tzresult =
  parse_expr code_str >>? fun code ->
  parse_expr storage_str >>? fun storage ->
  ok { Proto_alpha.Alpha_context.Script.code ; storage }

let code = {|
{ parameter (list (pair string int)) ;
  storage (pair (big_map string int) unit) ;
  return unit ;
  code { UNPAAIAIR ;
         ITER { UNPAIR ; DUUUP ; DUUP; GET ;
                IF_NONE { PUSH int 0 } {} ;
                SWAP ; DIP { ADD ; SOME } ;
                UPDATE } ;
         PAIR ; UNIT ; PAIR } }
|}

let storage = {| Pair { Elt "A" 1 ; Elt "B" 2 } Unit |}

let expect_big_map tc contract print_key key_type print_data data_type contents =
  let open Proto_alpha.Error_monad in
  iter_p
    (fun (n, exp) ->
       Lwt.return @@ Proto_alpha.Script_ir_translator.hash_data tc key_type n >>=? fun (key, _tc) ->
       Proto_alpha.Alpha_context.Contract.Big_map.get_opt tc contract key >>=? fun data ->
       match data, exp with
       | None, None ->
           debug " - big_map[%a] is not defined (ok)" print_key n ;
           return ()
       | None, Some _ ->
           debug " - big_map[%a] is not defined (error)" print_key n ;
           Helpers_assert.fail_msg "Wrong big map contents"
       | Some data, None ->
           Proto_alpha.Script_ir_translator.parse_data tc data_type (Micheline.root data) >>=? fun (data, _tc) ->
           debug " - big_map[%a] = %a (error)" print_key n print_data data ;
           Helpers_assert.fail_msg "Wrong big map contents"
       | Some data, Some exp ->
           Proto_alpha.Script_ir_translator.parse_data tc data_type (Micheline.root data) >>=? fun (data, _tc) ->
           debug " - big_map[%a] = %a (expected %a)" print_key n print_data data print_data exp ;
           Helpers_assert.equal data exp ;
           return ())
    contents

let main () =
  Helpers.Init.main () >>=?? fun pred ->
  let tc = pred.Helpers.Block.tezos_context in
  let src = List.hd Helpers.Account.bootstrap_accounts in
  Lwt.return (parse_script code storage) >>=? fun script ->
  Helpers.Apply.script_origination_pred
    ~tc ~pred (script , src, 100_00) >>=?? fun ((contracts, errs), tc) ->
  begin match errs with
    | None | Some [] -> Proto_alpha.Error_monad.return ()
    | Some (err :: _) -> Proto_alpha.Error_monad.fail err
  end >>=?? fun () ->
  begin match contracts with
    | [ contract ] -> return contract
    | _ -> failwith "more than one contract"
  end >>=? fun contract ->
  debug "contract created" ;
  let expect_big_map tc exp =
    expect_big_map tc contract
      (fun ppf k -> Format.fprintf ppf "%s" k)
      Proto_alpha.Script_typed_ir.String_t
      (fun ppf n -> Format.fprintf ppf "%s" (Proto_alpha.Alpha_context.Script_int.to_string n))
      Proto_alpha.Script_typed_ir.Int_t
      (List.map (fun (n, v) -> (n, Option.map ~f:Proto_alpha.Alpha_context.Script_int.of_int v)) exp) in
  expect_big_map tc
    [ "A", Some 1 ; "B", Some 2 ; "C", None ; "D", None ] >>=?? fun () ->
  debug "initial big map is ok" ;
  let call tc input result =
    Lwt.return (parse_expr input) >>=? fun parameters ->
    let gas = Proto_alpha.Alpha_context.Constants.hard_gas_limit_per_operation tc in
    Helpers.Operation.transaction_full
      src contract (Helpers_cast.cents_of_int 100_00)
      gas (Helpers_cast.ctxt_of_tc tc)
      ~parameters >>=?? fun op ->
    Helpers.Apply.operation ~tc
      ~src pred.Helpers_block.hash
      (Helpers_block.get_op_header_res pred)
      op >>=?? fun ((_, errs), tc) ->
    begin match errs with
      | None | Some [] -> Proto_alpha.Error_monad.return ()
      | Some (err :: _) -> Proto_alpha.Error_monad.fail err
    end >>=?? fun () ->
    expect_big_map tc result >>=?? fun () ->
    debug "big map after call %s is ok" input ;
    return tc in
  call tc
    {| {} |}
    [ "A", Some 1 ; "B", Some 2 ; "C", None ; "D", None ] >>=? fun tc ->
  call tc
    {| { Pair "A" 2 } |}
    [ "A", Some 3 ; "B", Some 2 ; "C", None ; "D", None ] >>=? fun tc ->
  call tc
    {| { Pair "A" 2 ; Pair "A" 2 ; Pair "D" 8 } |}
    [ "A", Some 7 ; "B", Some 2 ; "C", None ; "D", Some 8 ] >>=? fun tc ->
  call tc
    {| { Pair "C" 3 } |}
    [ "A", Some 7 ; "B", Some 2 ; "C", Some 3 ; "D", Some 8 ] >>=? fun _tc ->
  Error_monad.return ()


let tests = [
  "bigmaps", (fun _ -> main ()) ;
]
