#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

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
    for contract in `ls $contract_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# FORMAT: assert_output contract_file storage input expected_result

assert_storage $contract_dir/ret_int.tz None Unit '(Some 300)'

# Identity on strings
assert_storage $contract_dir/str_id.tz None '"Hello"' '(Some "Hello")'
assert_storage $contract_dir/str_id.tz None '"abcd"' '(Some "abcd")'

# Identity on pairs
assert_storage $contract_dir/pair_id.tz None '(Pair True False)' '(Some (Pair True False))'
assert_storage $contract_dir/pair_id.tz None '(Pair False True)' '(Some (Pair False True))'
assert_storage $contract_dir/pair_id.tz None '(Pair True True)' '(Some (Pair True True))'
assert_storage $contract_dir/pair_id.tz None '(Pair False False)' '(Some (Pair False False))'

# Logical not
assert_storage $contract_dir/not.tz None True '(Some False)'
assert_storage $contract_dir/not.tz None False '(Some True)'

# Logical and
assert_storage $contract_dir/and.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_dir/and.tz None "(Pair False True)" '(Some False)'
assert_storage $contract_dir/and.tz None "(Pair True False)" '(Some False)'
assert_storage $contract_dir/and.tz None "(Pair True True)" '(Some True)'

# Logical or
assert_storage $contract_dir/or.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_dir/or.tz None "(Pair False True)" '(Some True)'
assert_storage $contract_dir/or.tz None "(Pair True False)" '(Some True)'
assert_storage $contract_dir/or.tz None "(Pair True True)" '(Some True)'

# XOR
assert_storage $contract_dir/xor.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_dir/xor.tz None "(Pair False True)" '(Some True)'
assert_storage $contract_dir/xor.tz None "(Pair True False)" '(Some True)'
assert_storage $contract_dir/xor.tz None "(Pair True True)" '(Some False)'


# Build list
assert_storage $contract_dir/build_list.tz '{}' 0 "{ 0 }"
assert_storage $contract_dir/build_list.tz '{}' 3 "{ 0 ; 1 ; 2 ; 3 }"
assert_storage $contract_dir/build_list.tz '{}' 10 \
			  "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }"

# Concatenate all strings of a list into one string
assert_storage $contract_dir/concat_list.tz '""' '{ "a" ; "b" ; "c" }' '"abc"'
assert_storage $contract_dir/concat_list.tz '""' '{}' '""'
assert_storage $contract_dir/concat_list.tz \
			  '""' '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'

# Find maximum int in list -- returns None if not found
assert_storage $contract_dir/max_in_list.tz None '{}' 'None'
assert_storage $contract_dir/max_in_list.tz None '{ 1 }' '(Some 1)'
assert_storage $contract_dir/max_in_list.tz None '{ -1 }' '(Some -1)'
assert_storage $contract_dir/max_in_list.tz None \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_storage $contract_dir/max_in_list.tz None \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_storage $contract_dir/max_in_list.tz None \
			  '{ -10 ; -1 ; -20 ; -100 }' '(Some -1)'

# Identity on lists
assert_storage $contract_dir/list_id.tz '{""}' '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_storage $contract_dir/list_id.tz '{""}' '{}' '{}'
assert_storage $contract_dir/list_id.tz '{""}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'

assert_storage $contract_dir/list_id_map.tz '{""}' '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_storage $contract_dir/list_id_map.tz '{""}' '{}' '{}'
assert_storage $contract_dir/list_id_map.tz '{""}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'


# Identity on maps
assert_storage $contract_dir/map_id.tz '{}' '{ Elt 0 1 }' '{ Elt 0 1 }'
assert_storage $contract_dir/map_id.tz '{}' '{ Elt 0 0 }' '{ Elt 0 0 }'
assert_storage $contract_dir/map_id.tz '{}' '{ Elt 0 0 ; Elt 3 4 }' '{ Elt 0 0 ; Elt 3 4 }'

# Map block on lists
assert_storage $contract_dir/list_map_block.tz '{0}' '{}' '{}'
assert_storage $contract_dir/list_map_block.tz '{0}' '{ 1 ; 1 ; 1 ; 1 }' '{ 1 ; 2 ; 3 ; 4 }'
assert_storage $contract_dir/list_map_block.tz '{0}' '{ 1 ; 2 ; 3 ; 0 }' '{ 1 ; 3 ; 5 ; 3 }'

# List iter
assert_storage $contract_dir/list_iter.tz 0 '{ 10 ; 2 ; 1 }' 20
assert_storage $contract_dir/list_iter.tz 0 '{ 3 ; 6 ; 9 }' 162

assert_storage $contract_dir/list_iter2.tz '"abc"' '{ "d" ; "e" ; "f" }' '"abcdef"'
assert_storage $contract_dir/list_iter2.tz '"abc"' '{}' '"abc"'

assert_storage $contract_dir/list_iter2_bytes.tz '0x00ab' '{ 0xcd ; 0xef ; 0x00 }' '0x00abcdef00'
assert_storage $contract_dir/list_iter2_bytes.tz '0x' '{ 0x00 ; 0x11 ; 0x00 }' '0x001100'
assert_storage $contract_dir/list_iter2_bytes.tz '0xabcd' '{}' '0xabcd'
assert_storage $contract_dir/list_iter2_bytes.tz '0x' '{}' '0x'

# Identity on sets
assert_storage $contract_dir/set_id.tz '{}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'
assert_storage $contract_dir/set_id.tz '{}' '{}' '{}'
assert_storage $contract_dir/set_id.tz '{}' '{ "asdf" ; "bcde" }' '{ "asdf" ; "bcde" }'

# Set member -- set is in storage
assert_storage $contract_dir/set_member.tz '(Pair {} None)' '"Hi"' '(Pair {} (Some False))'
assert_storage $contract_dir/set_member.tz '(Pair { "Hi" } None)' '"Hi"' '(Pair { "Hi" } (Some True))'
assert_storage $contract_dir/set_member.tz '(Pair { "Hello" ; "World" } None)' '""' '(Pair { "Hello" ; "World" } (Some False))'

# Set size
assert_storage $contract_dir/set_size.tz 111 '{}' 0
assert_storage $contract_dir/set_size.tz 111 '{ 1 }' 1
assert_storage $contract_dir/set_size.tz 111 '{ 1 ; 2 ; 3 }' 3
assert_storage $contract_dir/set_size.tz 111 '{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }' 6

# Set iter
assert_storage $contract_dir/set_iter.tz 111 '{}' 0
assert_storage $contract_dir/set_iter.tz 111 '{ 1 }' 1
assert_storage $contract_dir/set_iter.tz 111 '{ -100 ; 1 ; 2 ; 3 }' '-94'

# Map size
assert_storage $contract_dir/map_size.tz 111 '{}' 0
assert_storage $contract_dir/map_size.tz 111 '{ Elt "a" 1 }' 1
assert_storage $contract_dir/map_size.tz 111 \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 }' 3
assert_storage $contract_dir/map_size.tz 111 \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 ; Elt "d" 4 ; Elt "e" 5 ; Elt "f" 6 }' 6

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_storage $contract_dir/contains_all.tz \
			  None '(Pair {} {})' '(Some True)'
assert_storage $contract_dir/contains_all.tz \
			  None '(Pair { "a" } { "B" })' '(Some False)'
assert_storage $contract_dir/contains_all.tz \
			  None '(Pair { "A" } { "B" })' '(Some False)'
assert_storage $contract_dir/contains_all.tz \
			  None '(Pair { "B" } { "B" })' '(Some True)'
assert_storage $contract_dir/contains_all.tz None \
			  '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })' '(Some True)'
assert_storage $contract_dir/contains_all.tz None \
			  '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })' '(Some True)'

# Concatenate the string in storage with all strings in the given list
assert_storage $contract_dir/concat_hello.tz '{}' \
			  '{ "World!" }' '{ "Hello World!" }'
assert_storage $contract_dir/concat_hello.tz '{}' \
			  '{}' '{}'
assert_storage $contract_dir/concat_hello.tz '{}' \
			  '{ "test1" ; "test2" }' '{ "Hello test1" ; "Hello test2" }'

# Create an empty map and add a string to it
assert_storage $contract_dir/empty_map.tz '{}' Unit \
			  '{ Elt "hello" "world" }'

# Get the value stored at the given key in the map
assert_storage $contract_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '"hello"' '(Pair (Some "hi") { Elt "hello" "hi" })'
assert_storage $contract_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '""' '(Pair None { Elt "hello" "hi" })'
assert_storage $contract_dir/get_map_value.tz \
			  '(Pair None { Elt "1" "one" ; Elt "2" "two" })' \
			  '"1"' '(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })'

# Map iter
assert_storage $contract_dir/map_iter.tz '(Pair 0 0)' '{ Elt 0 100 ; Elt 2 100 }' '(Pair 2 200)'
assert_storage $contract_dir/map_iter.tz '(Pair 0 0)' '{ Elt 1 1 ; Elt 2 100 }' '(Pair 3 101)'

# Return True if True branch of if was taken and False otherwise
assert_storage $contract_dir/if.tz None True '(Some True)'
assert_storage $contract_dir/if.tz None False '(Some False)'

# Generate a pair of or types
assert_storage $contract_dir/swap_left_right.tz '(Left "X")' '(Left True)' '(Right True)'
assert_storage $contract_dir/swap_left_right.tz '(Left "X")' '(Right "a")' '(Left "a")'

# Reverse a list
assert_storage $contract_dir/reverse.tz '{""}' '{}' '{}'
assert_storage $contract_dir/reverse.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'
assert_storage $contract_dir/reverse_loop.tz '{""}' '{}' '{}'
assert_storage $contract_dir/reverse_loop.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Reverse using LOOP_LEFT
assert_storage $contract_dir/loop_left.tz '{""}' '{}' '{}'
assert_storage $contract_dir/loop_left.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Exec concat contract
assert_storage $contract_dir/exec_concat.tz '"?"' '""' '"_abc"'
assert_storage $contract_dir/exec_concat.tz '"?"' '"test"' '"test_abc"'

# Test PACK/UNPACK and binary format
assert_success $client run script $contract_dir/packunpack.tz on storage Unit and input \
               '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) 0x05070707070100000004746f746f020000000800030007000900010200000006000100020003)'

assert_fails $client run script $contract_dir/packunpack.tz on storage Unit and input \
               '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) 0x05070707070100000004746f746f0200000008000300070009000102000000060001000200030004)'

# Get current steps to quota
assert_storage $contract_dir/steps_to_quota.tz 111 Unit 399813

# Typing gas bounds checks
assert_fails $client originate contract first_explosion for bootstrap1 \
             transferring 0 from bootstrap1 \
             running '{parameter unit;storage unit;code{DROP;PUSH nat 0;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;DUP;PAIR;}}' -G 8000

# Serialization gas bounds checks
assert_success $client run script  '{parameter (list int);storage (list (list (list int)));code{CAR;DIP{NIL (list int)};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;DIP{NIL (list (list int))};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;NIL operation;PAIR}}' \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'
assert_fails $client run script  '{parameter (list int);storage (list (list (list int)));code{CAR;DIP{NIL (list int)};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;DIP{NIL (list (list int))};DUP;ITER{DROP;DUP;DIP{CONS}};DROP;NIL operation;PAIR}}' \
               on storage '{}' \
               and input '{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}'

# Get the current balance of the contract
assert_storage $contract_dir/balance.tz '111' Unit '4000000000000'

# Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
assert_storage $contract_dir/compare.tz '{}' '(Pair 1000000 2000000)' '{ False ; False ; True ; False ; True }'
assert_storage $contract_dir/compare.tz '{}' '(Pair 2000000 1000000)' '{ False ; True ; False ; True ; False }'
assert_storage $contract_dir/compare.tz '{}' '(Pair 2370000 2370000)' '{ True ; False ; False ; True ; True }'

# Test addition and subtraction on tez
assert_storage $contract_dir/tez_add_sub.tz None '(Pair 2000000 1000000)' '(Some (Pair 3000000 1000000))'
assert_storage $contract_dir/tez_add_sub.tz None '(Pair 2310000 1010000)' '(Some (Pair 3320000 1300000))'

# Test get first element of list
assert_storage $contract_dir/first.tz '111' '{ 1 ; 2 ; 3 ; 4 }' '1'
assert_storage $contract_dir/first.tz '111' '{ 4 }' '4'

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_storage $contract_dir/hash_string.tz '0x00' '"abcdefg"' '0x46fdbcb4ea4eadad5615cdaa17d67f783e01e21149ce2b27de497600b4cd8f4e'
assert_storage $contract_dir/hash_string.tz '0x00' '"12345"' '0xb4c26c20de52a4eaf0d8a340db47ad8cb1e74049570859c9a9a3952b204c772f'

# Test ASSERT
assert_storage $contract_dir/assert.tz Unit True Unit
assert_fails $client run script $contract_dir/assert.tz on storage Unit and input False

# COMPARE; ASSERT_
assert_storage $contract_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_neq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_neq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_dir/assert_lt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_dir/assert_lt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails $client run script $contract_dir/assert_lt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_dir/assert_le.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_dir/assert_le.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_dir/assert_le.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_gt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_gt.tz on storage Unit and input '(Pair -1 0)'
assert_fails $client run script $contract_dir/assert_gt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_dir/assert_ge.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_dir/assert_ge.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_ge.tz on storage Unit and input '(Pair -1 0)'

# ASSERT_CMP
assert_storage $contract_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails $client run script $contract_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_cmpneq.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_cmpneq.tz on storage Unit and input '(Pair -1 -1)'

assert_storage $contract_dir/assert_cmplt.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails $client run script $contract_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_dir/assert_cmple.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_dir/assert_cmple.tz Unit '(Pair -1 0)' Unit
assert_fails $client run script $contract_dir/assert_cmple.tz on storage Unit and input '(Pair 0 -1)'

assert_storage $contract_dir/assert_cmpgt.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_cmpgt.tz on storage Unit and input '(Pair -1 0)'
assert_fails $client run script $contract_dir/assert_cmpgt.tz on storage Unit and input '(Pair 0 0)'

assert_storage $contract_dir/assert_cmpge.tz Unit '(Pair 0 0)' Unit
assert_storage $contract_dir/assert_cmpge.tz Unit '(Pair 0 -1)' Unit
assert_fails $client run script $contract_dir/assert_cmpge.tz on storage Unit and input '(Pair -1 0)'

# IF_SOME
assert_storage $contract_dir/if_some.tz '"?"' '(Some "hello")' '"hello"'
assert_storage $contract_dir/if_some.tz '"?"' 'None' '""'

# Tests the SET_CAR and SET_CDR instructions
assert_storage  $contract_dir/set_car.tz '(Pair "hello" 0)' '"world"' '(Pair "world" 0)'
assert_storage  $contract_dir/set_car.tz '(Pair "hello" 0)' '"abc"' '(Pair "abc" 0)'
assert_storage  $contract_dir/set_car.tz '(Pair "hello" 0)' '""' '(Pair "" 0)'
assert_fails $client run script $contract_dir/set_car.tz on storage '(Pair %wrong %field "hello" 0)' Unit and input '""'

assert_storage  $contract_dir/set_cdr.tz '(Pair "hello" 0)' '1' '(Pair "hello" 1)'
assert_storage  $contract_dir/set_cdr.tz '(Pair "hello" 500)' '3' '(Pair "hello" 3)'
assert_storage  $contract_dir/set_cdr.tz '(Pair "hello" 7)' '100' '(Pair "hello" 100)'

assert_storage  $contract_dir/set_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 0) 4) 5))) 6)' \
'3000000' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 3000000) 4) 5))) 6)'

assert_storage  $contract_dir/map_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 0) 4) 5))) 6)' \
'Unit' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 1000000) 4) 5))) 6)'

# Did the given key sign the string? (key is bootstrap1)
assert_success $client run script $contract_dir/check_signature.tz \
               on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "hello")' \
               and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'

assert_fails $client run script $contract_dir/check_signature.tz \
             on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "abcd")' \
             and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'


# Convert a public key to a public key hash
assert_storage $contract_dir/hash_key.tz None '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' \
               '(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")'
assert_storage $contract_dir/hash_key.tz None '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' \
               '(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")'


bake_after $client transfer 1,000 from bootstrap1 to $key1
bake_after $client transfer 2,000 from bootstrap1 to $key2

assert_balance $key1 "1000 ꜩ"
assert_balance $key2 "2000 ꜩ"

# Create a contract and transfer 100 ꜩ to it
init_with_transfer $contract_dir/store_input.tz $key1 '""' 100 bootstrap1
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"abcdefg"'
assert_balance store_input "200 ꜩ"
assert_storage_contains store_input '"abcdefg"'
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"xyz"'
assert_storage_contains store_input '"xyz"'

init_with_transfer $contract_dir/transfer_amount.tz $key1 '0' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to transfer_amount -arg Unit
assert_storage_contains transfer_amount 500

# This tests the `NOW` instruction.
# This test may fail if timings are marginal, though I have not yet seen this happen
init_with_transfer $contract_dir/store_now.tz $key1 '"2017-07-13T09:19:01Z"' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to store_now -arg Unit
assert_storage_contains store_now "$($client get timestamp)"

# Test timestamp operations
assert_storage $contract_dir/add_timestamp_delta.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_dir/add_timestamp_delta.tz None '(Pair 100 -100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_dir/add_timestamp_delta.tz None '(Pair "1970-01-01T00:00:00Z" 0)' '(Some "1970-01-01T00:00:00Z")'

assert_storage $contract_dir/add_delta_timestamp.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_dir/add_delta_timestamp.tz None '(Pair -100 100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_dir/add_delta_timestamp.tz None '(Pair 0 "1970-01-01T00:00:00Z")' '(Some "1970-01-01T00:00:00Z")'

assert_storage $contract_dir/sub_timestamp_delta.tz 111 '(Pair 100 100)' '"1970-01-01T00:00:00Z"'
assert_storage $contract_dir/sub_timestamp_delta.tz 111 '(Pair 100 -100)' '"1970-01-01T00:03:20Z"'
assert_storage $contract_dir/sub_timestamp_delta.tz 111 '(Pair 100 2000000000000000000)' -1999999999999999900

assert_storage $contract_dir/diff_timestamps.tz 111 '(Pair 0 0)' 0
assert_storage $contract_dir/diff_timestamps.tz 111 '(Pair 0 1)' -1
assert_storage $contract_dir/diff_timestamps.tz 111 '(Pair 1 0)' 1
assert_storage $contract_dir/diff_timestamps.tz 111 '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200


# Tests TRANSFER_TO
bake_after $client originate account "test_transfer_account1" for $key1 transferring 100 from bootstrap1
bake_after $client originate account "test_transfer_account2" for $key1 transferring 20 from bootstrap1
init_with_transfer $contract_dir/transfer_to.tz $key2 Unit 1,000 bootstrap1
assert_balance test_transfer_account1 "100 ꜩ"
bake_after $client transfer 100 from bootstrap1 to transfer_to \
           -arg "\"$(get_contract_addr test_transfer_account1)\""
assert_balance test_transfer_account1 "200 ꜩ" # Why isn't this 200 ꜩ? Baking fee?
bake_after $client transfer 100 from bootstrap1 to transfer_to \
            -arg "\"$(get_contract_addr test_transfer_account2)\""
assert_balance test_transfer_account2 "120 ꜩ" # Why isn't this 120 ꜩ? Baking fee?


# Test replay prevention
init_with_transfer $contract_dir/replay.tz $key2 Unit 0 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to replay

# Tests create_account
init_with_transfer $contract_dir/create_account.tz $key2 None 1,000 bootstrap1
assert_balance create_account "1000 ꜩ"
created_account=\
`$client transfer 100 from bootstrap1 to create_account -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' \
| grep 'New contract' \
| sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
| head -1`
bake
assert_balance $created_account "100 ꜩ"
assert_balance create_account "1000 ꜩ"

# Creates a contract, transfers data to it and stores the data
init_with_transfer $contract_dir/create_contract.tz $key2 Unit 1,000 bootstrap1
assert_balance create_contract "1000 ꜩ"
created_contract=\
`$client transfer 0 from bootstrap1 to create_contract -arg '(Left "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")' \
| grep 'New contract' \
| sed -E 's/.*(KT1[a-zA-Z0-9]+).*/\1/' \
| head -1`
bake
assert_storage_contains $created_contract '"abcdefg"'
assert_balance $created_contract "100 ꜩ"
assert_balance create_contract "900 ꜩ"

# Test IMPLICIT_ACCOUNT
init_with_transfer $contract_dir/default_account.tz $key1 \
				   Unit 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\""
assert_balance $BOOTSTRAP4_IDENTITY "4000100 ꜩ"
account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$account\""
assert_balance $account "100 ꜩ"

# Test SELF
init_with_transfer $contract_dir/self.tz $key1 \
				   '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to self
assert_storage_contains self "\"$(get_contract_addr self)\""

# Test bytes, SHA256, CHECK_SIGNATURE
init_with_transfer $contract_dir/reveal_signed_preimage.tz bootstrap1 \
				   '(Pair 0x9995c2ef7bcc7ae3bd15bdd9b02dc6e877c27b26732340d641a4cbc6524813bb "p2pk66uq221795tFxT7jfNmXtBMdjMf6RAaxRTwv1dbuSHbH6yfqGwz")' 1,000 bootstrap1
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972 "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'
assert_fails $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
             '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigvgDSBnN1bUsfwyMvqpJA1cFhE5s5oi7SetJVQ6LJsbFrU2idPvnvwJhf5v9DhM9ZTX1euS9DgWozVw6BTHiK9VcQVpAU8")'
assert_success $client transfer 0 from bootstrap1 to reveal_signed_preimage -arg \
               '(Pair 0x050100000027566f756c657a2d766f757320636f75636865722061766563206d6f692c20636520736f6972203f "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ")'
bake

# Test SET_DELEGATE
b2='tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN'
b3='tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU'
b4='tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv'
b5='tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv'
init_with_transfer $contract_dir/vote_for_delegate.tz bootstrap1 \
				   "(Pair (Pair \"$b3\" None) (Pair \"$b4\" None))" 1,000 bootstrap1
$client get delegate for vote_for_delegate | assert_in_output none

assert_fails $client transfer 0 from bootstrap1 to vote_for_delegate -arg None
assert_fails $client transfer 0 from bootstrap2 to vote_for_delegate -arg None
bake_after $client transfer 0 from bootstrap3 to vote_for_delegate -arg "(Some \"$b5\")"
assert_storage_contains vote_for_delegate "\"$b5\""
$client get delegate for vote_for_delegate | assert_in_output none
bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b2\")"
assert_storage_contains vote_for_delegate "\"$b2\""
$client get delegate for vote_for_delegate | assert_in_output none
bake_after $client transfer 0 from bootstrap4 to vote_for_delegate -arg "(Some \"$b5\")"
$client get delegate for vote_for_delegate | assert_in_output "$b5"

# Test sets and map literals
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 0 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ "A" ; "C" ; "B" }' against type '(set string)'
assert_fails $client typecheck data '{ "A" ; "B" ; "B" }' against type '(set string)'

# Test hash consistency between Michelson and the CLI
hash_result=`$client hash data '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' \
                     of type '(pair mutez (pair timestamp int))' | grep Blake2b | sed 's/.*: *//'`

assert_storage $contract_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

assert_storage $contract_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"

# Test goldenbook

init_with_transfer $contract_dir/guestbook.tz $key1\
                   '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }' \
                   100 bootstrap1
assert_fails $client transfer 0 from bootstrap2 to guestbook -arg '"Pas moi"'
bake_after $client transfer 0 from bootstrap1 to guestbook -arg '"Coucou"'
assert_storage_contains guestbook '{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }'
assert_fails $client transfer 0 from bootstrap3 to guestbook -arg '"Pas moi non plus"'
assert_fails $client transfer 0 from bootstrap1 to guestbook -arg '"Recoucou ?"'

# Test for big maps
init_with_transfer $contract_dir/big_map_mem.tz $key1\
                   '(Pair { Elt 1 Unit ; Elt 2 Unit ; Elt 3 Unit } Unit)' \
                   100 bootstrap1
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 False)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 0 True)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 1 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 2 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 True)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 3 False)'
bake_after $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 False)'
assert_fails $client transfer 1 from bootstrap1 to big_map_mem -arg '(Pair 4 True)'
assert_fails $client typecheck data '3' against type \
             '(int @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'
$client typecheck data '3' against type \
        '(int @aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)'

init_with_transfer $contract_dir/big_map_get_add.tz $key1\
                   '(Pair { Elt 0 1 ; Elt 1 2 ; Elt 2 3 } Unit)' \
                   100 bootstrap1

bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 (Some 2)) (Pair 200 (Some 2)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 200 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 200 None) (Pair 300 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 None) (Pair 200 None))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 1 (Some 2)) (Pair 0 (Some 1)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 400 (Some 1232)) (Pair 400 (Some 1232)))'
bake_after $client transfer 1 from bootstrap1 to big_map_get_add -arg '(Pair (Pair 401 (Some 0)) (Pair 400 (Some 1232)))'

printf "\nEnd of test\n"

show_logs="no"
