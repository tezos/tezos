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
    for contract in `ls $contract_op_dir/*.tz`; do
        printf "[Typechecking %s]\n" "$contract";
        ${client} typecheck script "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# FORMAT: assert_output contract_file storage input expected_result

# TODO add tests for map_car.tz, subset.tz
# NB: noop.tz is tested in test_basic.sh

assert_storage $contract_op_dir/ret_int.tz None Unit '(Some 300)'

# Map block on lists
assert_storage $contract_op_dir/list_map_block.tz '{0}' '{}' '{}'
assert_storage $contract_op_dir/list_map_block.tz '{0}' '{ 1 ; 1 ; 1 ; 1 }' '{ 1 ; 2 ; 3 ; 4 }'
assert_storage $contract_op_dir/list_map_block.tz '{0}' '{ 1 ; 2 ; 3 ; 0 }' '{ 1 ; 3 ; 5 ; 3 }'

# Reverse a list
assert_storage $contract_op_dir/reverse.tz '{""}' '{}' '{}'
assert_storage $contract_op_dir/reverse.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Reverse using LOOP_LEFT
assert_storage $contract_op_dir/loop_left.tz '{""}' '{}' '{}'
assert_storage $contract_op_dir/loop_left.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Identity on strings
assert_storage $contract_op_dir/str_id.tz None '"Hello"' '(Some "Hello")'
assert_storage $contract_op_dir/str_id.tz None '"abcd"' '(Some "abcd")'

# Identity on pairs
assert_storage $contract_op_dir/pair_id.tz None '(Pair True False)' '(Some (Pair True False))'
assert_storage $contract_op_dir/pair_id.tz None '(Pair False True)' '(Some (Pair False True))'
assert_storage $contract_op_dir/pair_id.tz None '(Pair True True)' '(Some (Pair True True))'
assert_storage $contract_op_dir/pair_id.tz None '(Pair False False)' '(Some (Pair False False))'

# Logical not
assert_storage $contract_op_dir/not.tz None True '(Some False)'
assert_storage $contract_op_dir/not.tz None False '(Some True)'

# Logical and
assert_storage $contract_op_dir/and.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair False True)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair True False)" '(Some False)'
assert_storage $contract_op_dir/and.tz None "(Pair True True)" '(Some True)'

# Logical or
assert_storage $contract_op_dir/or.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_op_dir/or.tz None "(Pair False True)" '(Some True)'
assert_storage $contract_op_dir/or.tz None "(Pair True False)" '(Some True)'
assert_storage $contract_op_dir/or.tz None "(Pair True True)" '(Some True)'

# XOR
assert_storage $contract_op_dir/xor.tz None "(Pair False False)" '(Some False)'
assert_storage $contract_op_dir/xor.tz None "(Pair False True)" '(Some True)'
assert_storage $contract_op_dir/xor.tz None "(Pair True False)" '(Some True)'
assert_storage $contract_op_dir/xor.tz None "(Pair True True)" '(Some False)'


# Concatenate all strings of a list into one string
assert_storage $contract_op_dir/concat_list.tz '""' '{ "a" ; "b" ; "c" }' '"abc"'
assert_storage $contract_op_dir/concat_list.tz '""' '{}' '""'
assert_storage $contract_op_dir/concat_list.tz \
			  '""' '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'

# Identity on lists
assert_storage $contract_op_dir/list_id.tz '{""}' '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_storage $contract_op_dir/list_id.tz '{""}' '{}' '{}'
assert_storage $contract_op_dir/list_id.tz '{""}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'

assert_storage $contract_op_dir/list_id_map.tz '{""}' '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_storage $contract_op_dir/list_id_map.tz '{""}' '{}' '{}'
assert_storage $contract_op_dir/list_id_map.tz '{""}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'


# Identity on maps
assert_storage $contract_op_dir/map_id.tz '{}' '{ Elt 0 1 }' '{ Elt 0 1 }'
assert_storage $contract_op_dir/map_id.tz '{}' '{ Elt 0 0 }' '{ Elt 0 0 }'
assert_storage $contract_op_dir/map_id.tz '{}' '{ Elt 0 0 ; Elt 3 4 }' '{ Elt 0 0 ; Elt 3 4 }'

# Identity on sets
assert_storage $contract_op_dir/set_id.tz '{}' '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'
assert_storage $contract_op_dir/set_id.tz '{}' '{}' '{}'
assert_storage $contract_op_dir/set_id.tz '{}' '{ "asdf" ; "bcde" }' '{ "asdf" ; "bcde" }'

# List concat
assert_storage $contract_op_dir/list_concat.tz '"abc"' '{ "d" ; "e" ; "f" }' '"abcdef"'
assert_storage $contract_op_dir/list_concat.tz '"abc"' '{}' '"abc"'

assert_storage $contract_op_dir/list_concat_bytes.tz '0x00ab' '{ 0xcd ; 0xef ; 0x00 }' '0x00abcdef00'
assert_storage $contract_op_dir/list_concat_bytes.tz '0x' '{ 0x00 ; 0x11 ; 0x00 }' '0x001100'
assert_storage $contract_op_dir/list_concat_bytes.tz '0xabcd' '{}' '0xabcd'
assert_storage $contract_op_dir/list_concat_bytes.tz '0x' '{}' '0x'

# List iter
assert_storage $contract_op_dir/list_iter.tz 0 '{ 10 ; 2 ; 1 }' 20
assert_storage $contract_op_dir/list_iter.tz 0 '{ 3 ; 6 ; 9 }' 162

# Set member -- set is in storage
assert_storage $contract_op_dir/set_member.tz '(Pair {} None)' '"Hi"' '(Pair {} (Some False))'
assert_storage $contract_op_dir/set_member.tz '(Pair { "Hi" } None)' '"Hi"' '(Pair { "Hi" } (Some True))'
assert_storage $contract_op_dir/set_member.tz '(Pair { "Hello" ; "World" } None)' '""' '(Pair { "Hello" ; "World" } (Some False))'

# Set size
assert_storage $contract_op_dir/set_size.tz 111 '{}' 0
assert_storage $contract_op_dir/set_size.tz 111 '{ 1 }' 1
assert_storage $contract_op_dir/set_size.tz 111 '{ 1 ; 2 ; 3 }' 3
assert_storage $contract_op_dir/set_size.tz 111 '{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }' 6

# Set iter
assert_storage $contract_op_dir/set_iter.tz 111 '{}' 0
assert_storage $contract_op_dir/set_iter.tz 111 '{ 1 }' 1
assert_storage $contract_op_dir/set_iter.tz 111 '{ -100 ; 1 ; 2 ; 3 }' '-94'

# Map size
assert_storage $contract_op_dir/map_size.tz 111 '{}' 0
assert_storage $contract_op_dir/map_size.tz 111 '{ Elt "a" 1 }' 1
assert_storage $contract_op_dir/map_size.tz 111 \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 }' 3
assert_storage $contract_op_dir/map_size.tz 111 \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 ; Elt "d" 4 ; Elt "e" 5 ; Elt "f" 6 }' 6

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_storage $contract_op_dir/contains_all.tz \
			  None '(Pair {} {})' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz \
			  None '(Pair { "a" } { "B" })' '(Some False)'
assert_storage $contract_op_dir/contains_all.tz \
			  None '(Pair { "A" } { "B" })' '(Some False)'
assert_storage $contract_op_dir/contains_all.tz \
			  None '(Pair { "B" } { "B" })' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz None \
			  '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })' '(Some True)'
assert_storage $contract_op_dir/contains_all.tz None \
			  '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })' '(Some True)'

# Concatenate the string in storage with all strings in the given list
assert_storage $contract_op_dir/concat_hello.tz '{}' \
			  '{ "World!" }' '{ "Hello World!" }'
assert_storage $contract_op_dir/concat_hello.tz '{}' \
			  '{}' '{}'
assert_storage $contract_op_dir/concat_hello.tz '{}' \
			  '{ "test1" ; "test2" }' '{ "Hello test1" ; "Hello test2" }'

# Create an empty map and add a string to it
assert_storage $contract_op_dir/empty_map.tz '{}' Unit \
			  '{ Elt "hello" "world" }'

# Get the value stored at the given key in the map
assert_storage $contract_op_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '"hello"' '(Pair (Some "hi") { Elt "hello" "hi" })'
assert_storage $contract_op_dir/get_map_value.tz '(Pair None { Elt "hello" "hi" })' \
			  '""' '(Pair None { Elt "hello" "hi" })'
assert_storage $contract_op_dir/get_map_value.tz \
			  '(Pair None { Elt "1" "one" ; Elt "2" "two" })' \
			  '"1"' '(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })'

# Map iter
assert_storage $contract_op_dir/map_iter.tz '(Pair 0 0)' '{ Elt 0 100 ; Elt 2 100 }' '(Pair 2 200)'
assert_storage $contract_op_dir/map_iter.tz '(Pair 0 0)' '{ Elt 1 1 ; Elt 2 100 }' '(Pair 3 101)'

# Return True if True branch of if was taken and False otherwise
assert_storage $contract_op_dir/if.tz None True '(Some True)'
assert_storage $contract_op_dir/if.tz None False '(Some False)'

# Generate a pair of or types
assert_storage $contract_op_dir/left_right.tz '(Left "X")' '(Left True)' '(Right True)'
assert_storage $contract_op_dir/left_right.tz '(Left "X")' '(Right "a")' '(Left "a")'

# Reverse a list
assert_storage $contract_op_dir/reverse_loop.tz '{""}' '{}' '{}'
assert_storage $contract_op_dir/reverse_loop.tz '{""}' '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Exec concat contract
assert_storage $contract_op_dir/exec_concat.tz '"?"' '""' '"_abc"'
assert_storage $contract_op_dir/exec_concat.tz '"?"' '"test"' '"test_abc"'

# Test PACK/UNPACK and binary format
assert_success $client run script $contract_op_dir/packunpack.tz on storage Unit and input \
               '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) 0x05070707070100000004746f746f020000000800030007000900010200000006000100020003)'

assert_fails $client run script $contract_op_dir/packunpack.tz on storage Unit and input \
               '(Pair (Pair (Pair "toto" {3;7;9;1}) {1;2;3}) 0x05070707070100000004746f746f0200000008000300070009000102000000060001000200030004)'

# Get current steps to quota
assert_storage $contract_op_dir/steps_to_quota.tz 111 Unit 399813

# Get the current balance of the contract
assert_storage $contract_op_dir/balance.tz '111' Unit '4000000000000'

# Test addition and subtraction on tez
assert_storage $contract_op_dir/tez_add_sub.tz None '(Pair 2000000 1000000)' '(Some (Pair 3000000 1000000))'
assert_storage $contract_op_dir/tez_add_sub.tz None '(Pair 2310000 1010000)' '(Some (Pair 3320000 1300000))'

# Test get first element of list
assert_storage $contract_op_dir/first.tz '111' '{ 1 ; 2 ; 3 ; 4 }' '1'
assert_storage $contract_op_dir/first.tz '111' '{ 4 }' '4'

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_storage $contract_op_dir/hash_string.tz '0x00' '"abcdefg"' '0x46fdbcb4ea4eadad5615cdaa17d67f783e01e21149ce2b27de497600b4cd8f4e'
assert_storage $contract_op_dir/hash_string.tz '0x00' '"12345"' '0xb4c26c20de52a4eaf0d8a340db47ad8cb1e74049570859c9a9a3952b204c772f'

# IF_SOME
assert_storage $contract_op_dir/if_some.tz '"?"' '(Some "hello")' '"hello"'
assert_storage $contract_op_dir/if_some.tz '"?"' 'None' '""'

# Tests the SET_CAR and SET_CDR instructions
assert_storage  $contract_op_dir/set_car.tz '(Pair "hello" 0)' '"world"' '(Pair "world" 0)'
assert_storage  $contract_op_dir/set_car.tz '(Pair "hello" 0)' '"abc"' '(Pair "abc" 0)'
assert_storage  $contract_op_dir/set_car.tz '(Pair "hello" 0)' '""' '(Pair "" 0)'
assert_fails $client run script $contract_op_dir/set_car.tz on storage '(Pair %wrong %field "hello" 0)' Unit and input '""'

assert_storage  $contract_op_dir/set_cdr.tz '(Pair "hello" 0)' '1' '(Pair "hello" 1)'
assert_storage  $contract_op_dir/set_cdr.tz '(Pair "hello" 500)' '3' '(Pair "hello" 3)'
assert_storage  $contract_op_dir/set_cdr.tz '(Pair "hello" 7)' '100' '(Pair "hello" 100)'

# Did the given key sign the string? (key is bootstrap1)
assert_success $client run script $contract_op_dir/check_signature.tz \
               on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "hello")' \
               and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'

assert_fails $client run script $contract_op_dir/check_signature.tz \
             on storage '(Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" "abcd")' \
             and input '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"'


# Convert a public key to a public key hash
assert_storage $contract_op_dir/hash_key.tz None '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' \
               '(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")'
assert_storage $contract_op_dir/hash_key.tz None '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' \
               '(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")'

bake_after $client transfer 1,000 from bootstrap1 to $key1 --burn-cap 0.257
bake_after $client transfer 2,000 from bootstrap1 to $key2 --burn-cap 0.257

assert_balance $key1 "1000 ꜩ"
assert_balance $key2 "2000 ꜩ"

# Create a contract and transfer 100 ꜩ to it
init_with_transfer $contract_op_dir/store_input.tz $key1 '""' 100 bootstrap1
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"abcdefg"' --burn-cap 10
assert_balance store_input "200 ꜩ"
assert_storage_contains store_input '"abcdefg"'
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"xyz"' --burn-cap 10
assert_storage_contains store_input '"xyz"'

init_with_transfer $contract_op_dir/transfer_amount.tz $key1 '0' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to transfer_amount -arg Unit --burn-cap 10
assert_storage_contains transfer_amount 500


# This tests the `NOW` instruction.
# This test may fail if timings are marginal, though I have not yet seen this happen
init_with_transfer $contract_op_dir/store_now.tz $key1 '"2017-07-13T09:19:01Z"' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to store_now -arg Unit --burn-cap 10
assert_storage_contains store_now "$($client get timestamp)"

# Test timestamp operations
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair 100 -100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_op_dir/add_timestamp_delta.tz None '(Pair "1970-01-01T00:00:00Z" 0)' '(Some "1970-01-01T00:00:00Z")'

assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair 100 100)' '(Some "1970-01-01T00:03:20Z")'
assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair -100 100)' '(Some "1970-01-01T00:00:00Z")'
assert_storage $contract_op_dir/add_delta_timestamp.tz None '(Pair 0 "1970-01-01T00:00:00Z")' '(Some "1970-01-01T00:00:00Z")'

assert_storage $contract_op_dir/sub_timestamp_delta.tz 111 '(Pair 100 100)' '"1970-01-01T00:00:00Z"'
assert_storage $contract_op_dir/sub_timestamp_delta.tz 111 '(Pair 100 -100)' '"1970-01-01T00:03:20Z"'
assert_storage $contract_op_dir/sub_timestamp_delta.tz 111 '(Pair 100 2000000000000000000)' -1999999999999999900

assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 0 0)' 0
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 0 1)' -1
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair 1 0)' 1
assert_storage $contract_op_dir/diff_timestamps.tz 111 '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200

# Tests TRANSFER_TOKENS
bake_after $client originate account "test_transfer_account1" for $key1 transferring 100 from bootstrap1 --burn-cap 10
bake_after $client originate account "test_transfer_account2" for $key1 transferring 20 from bootstrap1 --burn-cap 10
init_with_transfer $contract_op_dir/transfer_tokens.tz $key2 Unit 1,000 bootstrap1
assert_balance test_transfer_account1 "100 ꜩ"
bake_after $client transfer 100 from bootstrap1 to transfer_tokens \
           -arg "\"$(get_contract_addr test_transfer_account1)\"" --burn-cap 10
assert_balance test_transfer_account1 "200 ꜩ" # Why isn't this 200 ꜩ? Baking fee?
bake_after $client transfer 100 from bootstrap1 to transfer_tokens \
            -arg "\"$(get_contract_addr test_transfer_account2)\"" --burn-cap 10
assert_balance test_transfer_account2 "120 ꜩ" # Why isn't this 120 ꜩ? Baking fee?

# Test SELF
init_with_transfer $contract_op_dir/self.tz $key1 \
				   '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to self --burn-cap 10
assert_storage_contains self "\"$(get_contract_addr self)\""

# Test SLICE and SIZE on bytes
init_with_transfer $contract_op_dir/slices.tz bootstrap1 \
				   '"sppk7dBPqMPjDjXgKbb5f7V3PuKUrA4Zuwc3c3H7XqQerqPUWbK7Hna"' 1,000 bootstrap1

assert_fails $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345 "p2sigsceCzcDw2AeYDzUonj4JT341WC9Px4wdhHBxbZcG1FhfqFVuG7f2fGCzrEHSAZgrsrQWpxduDPk9qZRgrpzwJnSHC3gZJ")' --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xeaa9ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345 "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")' --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2deaad01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345 "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")' --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150733eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345 "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")' --burn-cap 10
assert_fails $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xe009ab79e8b84ef0 "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")' --burn-cap 10
assert_success $client transfer 0 from bootstrap1 to slices -arg \
        '(Pair 0xe009ab79e8b84ef0e55c43a9a857214d8761e67b75ba63500a5694fb2ffe174acc2de22d01ccb7259342437f05e1987949f0ad82e9f32e9a0b79cb252d7f7b8236ad728893f4e7150742eefdbeda254970f9fcd92c6228c178e1a923e5600758eb83f2a05edd0be7625657901f2ba81eaf145d003dbef78e33f43a32a3788bdf0501000000085341554349535345 "spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm")' --burn-cap 10
bake

init_with_transfer $contract_op_dir/split_string.tz bootstrap1 '{}' 1,000 bootstrap1

bake_after $client transfer 0 from bootstrap1 to split_string -arg '"abc"' --burn-cap 10
assert_storage_contains split_string '{ "a" ; "b" ; "c" }'
bake_after $client transfer 0 from bootstrap1 to split_string -arg '"def"' --burn-cap 10
assert_storage_contains split_string '{ "a" ; "b" ; "c" ; "d" ; "e" ; "f" }'

init_with_transfer $contract_op_dir/split_bytes.tz bootstrap1 '{}' 1,000 bootstrap1

bake_after $client transfer 0 from bootstrap1 to split_bytes -arg '0xaabbcc' --burn-cap 10
assert_storage_contains split_bytes '{ 0xaa ; 0xbb ; 0xcc }'
bake_after $client transfer 0 from bootstrap1 to split_bytes -arg '0xddeeff' --burn-cap 10
assert_storage_contains split_bytes '{ 0xaa ; 0xbb ; 0xcc ; 0xdd ; 0xee ; 0xff }'

# Test hash consistency between Michelson and the CLI
hash_result=`$client hash data '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' \
                     of type '(pair mutez (pair timestamp int))' | grep Blake2b | sed 's/.*: *//'`

assert_storage $contract_op_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

assert_storage $contract_op_dir/hash_consistency_checker.tz '0x00' \
              '(Pair 22220000000 (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"


printf "\nEnd of test\n"

show_logs="no"
