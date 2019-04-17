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
    for contract in `ls $contract_macros_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# TODO add tests for fail.tz, macro_annotations.tz, min.tz, pair_macro.tz, take_my_money.tz, unpair_macro.tz

# FORMAT: assert_output contract_file storage input expected_result

# Build list
assert_storage $contract_macros_dir/build_list.tz '{}' 0 "{ 0 }"
assert_storage $contract_macros_dir/build_list.tz '{}' 3 "{ 0 ; 1 ; 2 ; 3 }"
assert_storage $contract_macros_dir/build_list.tz '{}' 10 \
			  "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }"

# Find maximum int in list -- returns None if not found
assert_storage $contract_macros_dir/max_in_list.tz None '{}' 'None'
assert_storage $contract_macros_dir/max_in_list.tz None '{ 1 }' '(Some 1)'
assert_storage $contract_macros_dir/max_in_list.tz None '{ -1 }' '(Some -1)'
assert_storage $contract_macros_dir/max_in_list.tz None \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_storage $contract_macros_dir/max_in_list.tz None \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_storage $contract_macros_dir/max_in_list.tz None \
			  '{ -10 ; -1 ; -20 ; -100 }' '(Some -1)'

# Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 1000000 2000000)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 2000000 1000000)' '{ False ; True ; False ; True ; False }'
assert_storage $contract_macros_dir/compare.tz '{}' '(Pair 2370000 2370000)' '{ True ; False ; False ; True ; True }'

# Test ASSERT
assert_storage $contract_macros_dir/assert.tz Unit True Unit
assert_fails $client run script $contract_macros_dir/assert.tz on storage Unit and input False

# ASSERT_{OP}
assert_storage $contract_macros_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'
assert_storage $contract_macros_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_macros_dir/assert_neq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_neq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_macros_dir/assert_lt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_macros_dir/assert_lt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails $client run script $contract_macros_dir/assert_lt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_macros_dir/assert_le.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_macros_dir/assert_le.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_macros_dir/assert_le.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_macros_dir/assert_gt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_gt.tz on storage Unit and input '(Pair -1 0)'
assert_fails $client run script $contract_macros_dir/assert_gt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_macros_dir/assert_ge.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_macros_dir/assert_ge.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_ge.tz on storage Unit and input '(Pair -1 0)'

# ASSERT_CMP{OP}
assert_storage $contract_macros_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_macros_dir/assert_cmpneq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmpneq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_macros_dir/assert_cmplt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 0)'
assert_fails $client run script $contract_macros_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_macros_dir/assert_cmple.tz Unit '(Pair -1 0)' Unit
assert_storage $contract_macros_dir/assert_cmple.tz Unit '(Pair 0 0)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmple.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_macros_dir/assert_cmpgt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmpgt.tz on storage Unit and input '(Pair 0 0)'
assert_fails $client run script $contract_macros_dir/assert_cmpgt.tz on storage Unit and input '(Pair -1 0)'


assert_storage $contract_macros_dir/assert_cmpge.tz Unit '(Pair 0 -1)' Unit
assert_storage $contract_macros_dir/assert_cmpge.tz Unit '(Pair 0 0)' Unit
assert_fails $client run script $contract_macros_dir/assert_cmpge.tz on storage Unit and input '(Pair -1 0)'

# Tests the SET_CAR and SET_CDR instructions
assert_storage  $contract_macros_dir/set_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 0) 4) 5))) 6)' \
'3000000' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 3000000) 4) 5))) 6)'

assert_storage  $contract_macros_dir/map_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 0) 4) 5))) 6)' \
'Unit' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 1000000) 4) 5))) 6)'

# Test comparisons on bytes { EQ ; GT ; LT ; GE ; LE }
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x34)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x33aa)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x33 0x33)' '{ True ; False ; False ; True ; True }'
assert_storage $contract_macros_dir/compare_bytes.tz '{}' '(Pair 0x34 0x33)' '{ False ; True ; False ; True ; False }'

# Test goldenbook

init_with_transfer $contract_macros_dir/guestbook.tz $key1\
                   '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }' \
                   100 bootstrap1
assert_fails $client transfer 0 from bootstrap2 to guestbook -arg '"Pas moi"' --burn-cap 10
bake_after $client transfer 0 from bootstrap1 to guestbook -arg '"Coucou"' --burn-cap 10
assert_storage_contains guestbook '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }'
assert_fails $client transfer 0 from bootstrap3 to guestbook -arg '"Pas moi non plus"' --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to guestbook -arg '"Recoucou ?"' --burn-cap 10

# Test for big maps
init_with_transfer $contract_macros_dir/big_map_mem.tz $key1\
                   '(Pair { Elt 1 Unit ; Elt 2 Unit ; Elt 3 Unit } Unit)' \
                   100 bootstrap1
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 False)' --burn-cap 10
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 True)' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 True)' --burn-cap 10
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 False)' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 True)' --burn-cap 10
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 False)' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 True)' --burn-cap 10
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 False)' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 False)' --burn-cap 10
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 True)' --burn-cap 10
assert_fails $client typecheck data '3' against type \
             '(int :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'
$client typecheck data '3' against type \
        '(int :aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'

init_with_transfer $contract_macros_dir/big_map_get_add.tz $key1\
                   '(Pair { Elt 0 1 ; Elt 1 2 ; Elt 2 3 } Unit)' \
                   100 bootstrap1

bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 (Some 2)) (Pair 200 (Some 2)))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 200 None))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 300 None))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 None) (Pair 200 None))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 (Some 2)) (Pair 0 (Some 1)))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 400 (Some 1232)) (Pair 400 (Some 1232)))' --burn-cap 10
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 401 (Some 0)) (Pair 400 (Some 1232)))' --burn-cap 10

printf "\nEnd of test\n"

show_logs="no"
