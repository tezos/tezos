open Internal_pervasives

module Key = struct
  module Of_name = struct
    type t =
      { name: string
      ; pkh: Tezos_crypto.Ed25519.Public_key_hash.t
      ; pk: Tezos_crypto.Ed25519.Public_key.t
      ; sk: Tezos_crypto.Ed25519.Secret_key.t }

    let make name =
      let seed =
        Tezos_stdlib.MBytes.of_string
          (String.concat ~sep:"" (List.init 42 ~f:(fun _ -> name)))
      in
      let pkh, pk, sk = Tezos_crypto.Ed25519.generate_key ~seed () in
      {name; pkh; pk; sk}

    let pubkey n = Tezos_crypto.Ed25519.Public_key.to_b58check (make n).pk

    let pubkey_hash n =
      Tezos_crypto.Ed25519.Public_key_hash.to_b58check (make n).pkh

    let private_key n =
      "unencrypted:" ^ Tezos_crypto.Ed25519.Secret_key.to_b58check (make n).sk
  end
end

module Script = struct
  type origin = [`Sandbox_faucet | `String of string]

  let exn_tezos msg = function
    | Ok o -> o
    | Error el ->
        Format.kasprintf failwith "Script-error: %s: %a" msg
          Tezos_error_monad.Error_monad.pp_print_error el

  let exn_shell msg res =
    Tezos_client_alpha.Proto_alpha.Alpha_environment.wrap_error res
    |> exn_tezos msg

  let parse exprs =
    Tezos_client_alpha.Michelson_v1_parser.(
      (parse_expression exprs |> fst).expanded)

  let code_of_json_exn s =
    match Tezos_data_encoding.Data_encoding.Json.from_string s with
    | Ok json ->
        let repr =
          Tezos_data_encoding.Data_encoding.Json.destruct
            Tezos_client_alpha.Proto_alpha.Script_repr.encoding json
        in
        let ( (expr_code :
                Tezos_client_alpha.Proto_alpha.Michelson_v1_primitives.prim
                Tezos_micheline.Micheline.canonical)
            , _ ) =
          Tezos_client_alpha.Proto_alpha.Script_repr.(force_decode repr.code)
          |> exn_shell "decoding script-repr"
        in
        let module Alph = Tezos_client_alpha.Proto_alpha in
        let strings_node =
          Alph.Michelson_v1_primitives.strings_of_prims expr_code
          |> Alph.Alpha_environment.Micheline.root
        in
        Format.eprintf ">> %a\n%!" Tezos_micheline.Micheline_printer.print_expr
          (Tezos_micheline.Micheline.map_node
             (fun _ -> Tezos_micheline.Micheline_printer.{comment= None})
             (fun x -> x)
             strings_node) ;
        expr_code
    | Error e -> Format.kasprintf failwith "JSON-of-string: %s" e

  let json_script_repr code storage =
    match
      Tezos_data_encoding.Data_encoding.Json.construct
        Tezos_client_alpha.Proto_alpha.Script_repr.encoding
        Tezos_client_alpha.Proto_alpha.Script_repr.
          {code= lazy_expr code; storage= lazy_expr storage}
    with
    | `O _ as o -> (o : Ezjsonm.t)
    | _other ->
        Format.kasprintf failwith "JSON-of-script-repr: not a json object"

  let original_json =
    (* looks like "./src/bin_client/test/contracts/attic/faucet.tz" *)
    {json|{ "code":
          [ { "prim": "parameter",
              "args": [ { "prim": "key_hash" } ] },
            { "prim": "storage",
              "args": [ { "prim": "timestamp" } ] },
            { "prim": "code",
              "args":
              [ [ [ [ { "prim": "DUP" }, { "prim": "CAR" },
                      { "prim": "DIP", "args": [ [ { "prim": "CDR" } ] ] } ] ],
                  { "prim": "SWAP" },
                  { "prim": "PUSH", "args": [ { "prim": "int" }, { "int": "300" } ] },
                  { "prim": "ADD", "annots": [ "@FIVE_MINUTES_LATER" ] },
                  { "prim": "NOW" },
                  [ [ { "prim": "COMPARE" }, { "prim": "GE" } ],
                    { "prim": "IF",
                      "args":
                      [ [],
                        [ [ { "prim": "UNIT" },
                            { "prim": "FAILWITH" } ] ] ] } ],
                  { "prim": "IMPLICIT_ACCOUNT" },
                  { "prim": "PUSH", "args": [ { "prim": "mutez" }, { "int": "1000000" } ] },
                  { "prim": "UNIT" },
                  { "prim": "TRANSFER_TOKENS" },
                  { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                  { "prim": "SWAP" },
                  { "prim": "CONS" },
                  { "prim": "DIP", "args": [ [ { "prim": "NOW" } ] ] },
                  { "prim": "PAIR" } ] ] } ],
          "storage": { "int": "0" } }|json}

  let faucet_tz =
    (* exactly "./src/bin_client/test/contracts/attic/faucet.tz" *)
    {tz|
{ parameter key_hash ;
  storage timestamp ;
  code { UNPAIR ; SWAP ;
         PUSH int 300 ; ADD @FIVE_MINUTES_LATER ;
         NOW ; ASSERT_CMPGE ;
         IMPLICIT_ACCOUNT ; PUSH mutez 1000000 ; UNIT ; TRANSFER_TOKENS ;
         NIL operation ; SWAP ; CONS ; DIP { NOW } ; PAIR } }
|tz}

  let print code storage =
    let json_repr = json_script_repr code storage in
    Format.eprintf "script-repr: %s\n%!" (Ezjsonm.to_string json_repr) ;
    ()

  let load : origin -> _ = function
    | `Sandbox_faucet ->
        let code = code_of_json_exn original_json in
        json_script_repr code (parse "0")
    | `String s -> json_script_repr (parse s) (parse "0")

  let test () =
    let faucet_like =
      {mich| {parameter key_hash ;
  storage timestamp ;
  code { { { DUP ; CAR ; DIP { CDR } } } ;
         SWAP ;
         PUSH int 300 ;
         ADD @FIVE_MINUTES_LATER ;
         NOW ;
         { { COMPARE ; GE } ; IF {} { { UNIT ; FAILWITH } } } ;
         IMPLICIT_ACCOUNT ;
         PUSH mutez 1000000 ;
         UNIT ;
         TRANSFER_TOKENS ;
         NIL operation ;
         SWAP ;
         CONS ;
         DIP { NOW } ;
         PAIR }} |mich}
    in
    print (parse faucet_like) (parse "0") ;
    let original = code_of_json_exn original_json in
    print original (parse "0") ;
    (* print (parse faucet_tz) (parse "0") ; *)
    ()
end

module Account = struct
  type t =
    | Of_name of string
    | Key_pair of
        { name: string
        ; pubkey: string
        ; pubkey_hash: string
        ; private_key: string }

  let of_name s = Of_name s
  let of_namef fmt = ksprintf of_name fmt
  let name = function Of_name n -> n | Key_pair k -> k.name

  let key_pair name ~pubkey ~pubkey_hash ~private_key =
    Key_pair {name; pubkey; pubkey_hash; private_key}

  let pubkey = function
    | Of_name n -> Key.Of_name.pubkey n
    | Key_pair k -> k.pubkey

  let pubkey_hash = function
    | Of_name n -> Key.Of_name.pubkey_hash n
    | Key_pair k -> k.pubkey_hash

  let private_key = function
    | Of_name n -> Key.Of_name.private_key n
    | Key_pair k -> k.private_key
end

type t =
  { id: string
  ; bootstrap_accounts: (Account.t * int) list
  ; dictator: Account.t
  ; bootstrap_contracts: (Account.t * int * Script.origin) list
  ; expected_pow: int
  ; name: string (* e.g. alpha *)
  ; hash: string
  ; time_between_blocks: int list
  ; blocks_per_roll_snapshot: int
  ; blocks_per_voting_period: int
  ; blocks_per_cycle: int
  ; preserved_cycles: int
  ; proof_of_work_threshold: int }

let compare a b = String.compare a.id b.id

let default () =
  let dictator = Account.of_name "dictator-default" in
  { id= "default-bootstrap"
  ; bootstrap_accounts=
      List.init 4 ~f:(fun n ->
          (Account.of_namef "bootacc-%d" n, 4_000_000_000_000) )
  ; dictator
  ; bootstrap_contracts= [(dictator, 10_000_000, `Sandbox_faucet)]
  ; expected_pow= 1
  ; name= "alpha"
  ; hash= "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  ; time_between_blocks= [2; 3]
  ; blocks_per_roll_snapshot= 4
  ; blocks_per_voting_period= 16
  ; blocks_per_cycle= 8
  ; preserved_cycles= 2
  ; proof_of_work_threshold= -1 }

let protocol_parameters_json t : Ezjsonm.t =
  let open Ezjsonm in
  let make_account (account, amount) =
    strings [Account.pubkey account; sprintf "%d" amount]
  in
  let make_contract (deleg, amount, script) =
    dict
      [ ("delegate", string (Account.pubkey_hash deleg))
      ; ("amount", ksprintf string "%d" amount)
      ; ("script", (Script.load script :> Ezjsonm.value)) ]
  in
  dict
    [ ( "bootstrap_accounts"
      , list make_account (t.bootstrap_accounts @ [(t.dictator, 1)]) )
    ; ("bootstrap_contracts", list make_contract t.bootstrap_contracts)
    ; ("time_between_blocks", list (ksprintf string "%d") t.time_between_blocks)
    ; ("blocks_per_roll_snapshot", int t.blocks_per_roll_snapshot)
    ; ("blocks_per_voting_period", int t.blocks_per_voting_period)
    ; ("blocks_per_cycle", int t.blocks_per_cycle)
    ; ("preserved_cycles", int t.preserved_cycles)
    ; ( "proof_of_work_threshold"
      , ksprintf string "%d" t.proof_of_work_threshold ) ]

let sandbox {dictator; _} =
  let pk = Account.pubkey dictator in
  Ezjsonm.to_string (`O [("genesis_pubkey", `String pk)])

let protocol_parameters t =
  Ezjsonm.to_string ~minify:false (protocol_parameters_json t)

let expected_pow t = t.expected_pow
let id t = t.id
let bootstrap_accounts t = List.map ~f:fst t.bootstrap_accounts
let dictator_name {dictator; _} = Account.name dictator
let dictator_secret_key {dictator; _} = Account.private_key dictator
let make_path config t = Paths.root config // sprintf "protocol-%s" (id t)
let sandbox_path ~config t = make_path config t // "sandbox.json"

let protocol_parameters_path ~config t =
  make_path config t // "protocol_parameters.json"

let ensure_script ~config t =
  let open Genspio.EDSL in
  let file string p =
    let path = p ~config t in
    ( Filename.basename path
    , write_stdout ~path:(str path)
        (feed ~string:(str (string t)) (exec ["cat"])) )
  in
  check_sequence
    ~verbosity:(`Announce (sprintf "Ensure-protocol-%s" (id t)))
    [ ("directory", exec ["mkdir"; "-p"; make_path config t])
    ; file sandbox sandbox_path
    ; file protocol_parameters protocol_parameters_path ]

let ensure t ~config =
  match
    Sys.command (Genspio.Compile.to_one_liner (ensure_script ~config t))
  with
  | 0 -> return ()
  | _other ->
      Lwt_exception.fail (Failure "sys.command non-zero")
        ~attach:[("location", "Tezos_protocol.ensure")]

let cli_term () =
  let open Cmdliner in
  let open Term in
  pure (fun remove_default_bas (`Time_between_blocks tbb) add_bootstraps ->
      let d = default () in
      let id =
        if add_bootstraps = [] && remove_default_bas = false then d.id
        else "default-and-command-line"
      in
      let time_between_blocks =
        Option.value tbb ~default:d.time_between_blocks
      in
      let bootstrap_accounts =
        add_bootstraps
        @ if remove_default_bas then [] else d.bootstrap_accounts
      in
      {d with id; bootstrap_accounts; time_between_blocks} )
  $ Arg.(
      value
        (flag
           (info ~doc:"Do not create any of the default bootstrap accounts."
              ["remove-default-bootstrap-accounts"])))
  $ Arg.(
      pure (fun x -> `Time_between_blocks x)
      $ value
          (opt
             (some (list ~sep:',' int))
             None
             (info ["time-between-blocks"] ~docv:"COMMA-SEPARATED-SECONDS"
                ~doc:
                  "Set the time between blocks bootstrap-parameter, e.g. \
                   `2,3,2`.")))
  $ Arg.(
      pure (fun l ->
          List.map l ~f:(fun ((name, pubkey, pubkey_hash, private_key), tez) ->
              (Account.key_pair name ~pubkey ~pubkey_hash ~private_key, tez) )
      )
      $ value
          (opt_all
             (pair ~sep:'@' (t4 ~sep:',' string string string string) int)
             []
             (info ["add-bootstrap-account"]
                ~docv:"NAME,PUBKEY,PUBKEY-HASH,PRIVATE-URI@MUTEZ-AMOUNT"
                ~doc:
                  "Add a custom bootstrap account, e.g. \
                   `LedgerBaker,edpku...,tz1YPS...,ledger://crouching-tiger.../ed25519/0'/0'@20_000_000_000`.")))
