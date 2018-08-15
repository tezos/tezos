
let test_example () =
  let name = Ledger_names.crouching_tiger "12345" in
  assert (name = "calculating-meerkat-straight-beetle")

let tests = [
  Alcotest.test_case "print_example" `Quick test_example;
]

let () =
  Alcotest.run "tezos-signed-backends" [
    "ledger-names", tests
  ]
