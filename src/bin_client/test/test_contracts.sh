#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

parameters_file=$test_dir/protocol_parameters.json

if ! [ -f "$parameters_file" ]; then
	cat > "$parameters_file" <<EOF
{ "bootstrap_accounts":
  [
      [ "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav", "4000000000000" ],
      [ "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9", "4000000000000" ],
      [ "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV", "4000000000000" ],
      [ "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU", "4000000000000" ],
      [ "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n", "4000000000000" ]
  ],
  "commitments": [
    [ "btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343" ],
    [ "btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032" ],
    [ "btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348" ],
    [ "btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031" ],
    [ "btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550" ],
    [ "btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555" ],
    [ "btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443" ],
    [ "btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525" ],
    [ "btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693" ],
    [ "btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478" ]
  ],
  "time_between_blocks" : [ "1", "0" ],
  "blocks_per_cycle" : 128,
  "blocks_per_roll_snapshot" : 32,
  "blocks_per_voting_period" : 256,
  "preserved_cycles" : 1,
  "proof_of_work_threshold": "-1",
  "minimum_endorsements_per_priority": []
}
EOF
fi

start_node 1
activate_alpha

$client -w none config update

bake

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

printf "\n\n"

# Assert all contracts typecheck
if [ ! $NO_TYPECHECK ] ; then
    for contract in `ls $contract_attic_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# Typing gas bounds checks
tee /tmp/first_explosion.tz <<EOF
{ parameter unit;
  storage unit;
  code{ DROP; PUSH nat 0 ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR ;
        DUP ; PAIR } }'
EOF
assert_fails $client originate contract first_explosion for bootstrap1 \
             transferring 0 from bootstrap1 \
             running /tmp/first_explosion.tz -G 8000 --burn-cap 10

# Serialization gas bounds checks
tee /tmp/second_explosion.tz <<EOF
{ parameter (list int) ;
  storage (list (list (list int))) ;
  code { CAR ; DIP { NIL (list int) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; DIP { NIL (list (list int)) } ;
         DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
         DROP ; NIL operation ; PAIR } }
EOF
assert_success $client run script /tmp/second_explosion.tz \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
#assert_fails $client run script /tmp/second_explosion.tz \
#               on storage '{}' \
#               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'

# Test sets and map literals
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 0 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ "A" ; "C" ; "B" }' against type '(set string)'
assert_fails $client typecheck data '{ "A" ; "B" ; "B" }' against type '(set string)'

# Test for issue #262 (bad serialization of 3+ arity primitives)
tee /tmp/bug_262.tz <<EOF
{ parameter unit ;
  storage unit ;
  code { DROP ;
         LAMBDA unit unit {} ; UNIT ; EXEC ;
         NIL operation ; PAIR } }
EOF
init_with_transfer /tmp/bug_262.tz $key1 'Unit' 1 bootstrap1
assert_balance bug_262 "1 ꜩ"

printf "\nEnd of test\n"

show_logs="no"
