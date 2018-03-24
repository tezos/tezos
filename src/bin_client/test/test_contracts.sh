#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

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
        ${client} typecheck program "$contract";
    done
    printf "All contracts are well typed\n\n"
fi

# FORMAT: assert_output contract_file storage input expected_result

assert_output $contract_dir/ret_int.tz Unit Unit 300

# Identity on strings
assert_output $contract_dir/str_id.tz Unit '"Hello"' '"Hello"'
assert_output $contract_dir/str_id.tz Unit '"abcd"' '"abcd"'

# Identity on pairs
assert_output $contract_dir/pair_id.tz Unit '(Pair True False)' '(Pair True False)'
assert_output $contract_dir/pair_id.tz Unit '(Pair False True)' '(Pair False True)'
assert_output $contract_dir/pair_id.tz Unit '(Pair True True)' '(Pair True True)'
assert_output $contract_dir/pair_id.tz Unit '(Pair False False)' '(Pair False False)'

# Logical not
assert_output $contract_dir/not.tz Unit True False
assert_output $contract_dir/not.tz Unit False True

# Logical and
assert_output $contract_dir/and.tz Unit "(Pair False False)" False
assert_output $contract_dir/and.tz Unit "(Pair False True)" False
assert_output $contract_dir/and.tz Unit "(Pair True False)" False
assert_output $contract_dir/and.tz Unit "(Pair True True)" True

# Logical or
assert_output $contract_dir/or.tz Unit "(Pair False False)" False
assert_output $contract_dir/or.tz Unit "(Pair False True)" True
assert_output $contract_dir/or.tz Unit "(Pair True False)" True
assert_output $contract_dir/or.tz Unit "(Pair True True)" True

# XOR
assert_output $contract_dir/xor.tz Unit "(Pair False False)" False
assert_output $contract_dir/xor.tz Unit "(Pair False True)" True
assert_output $contract_dir/xor.tz Unit "(Pair True False)" True
assert_output $contract_dir/xor.tz Unit "(Pair True True)" False


# Build list
assert_output $contract_dir/build_list.tz Unit 0 "{ 0 }"
assert_output $contract_dir/build_list.tz Unit 3 "{ 0 ; 1 ; 2 ; 3 }"
assert_output $contract_dir/build_list.tz Unit 10 \
			  "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }"

# Concatenate all strings of a list into one string
assert_output $contract_dir/concat_list.tz Unit '{ "a" ; "b" ; "c" }' '"abc"'
assert_output $contract_dir/concat_list.tz Unit '{}' '""'
assert_output $contract_dir/concat_list.tz \
			  Unit '{ "Hello" ; " " ; "World" ; "!" }' '"Hello World!"'

# Find maximum int in list -- returns None if not found
assert_output $contract_dir/max_in_list.tz Unit '{}' 'None'
assert_output $contract_dir/max_in_list.tz Unit '{ 1 }' '(Some 1)'
assert_output $contract_dir/max_in_list.tz Unit '{ -1 }' '(Some -1)'
assert_output $contract_dir/max_in_list.tz Unit \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_output $contract_dir/max_in_list.tz Unit \
			  '{ 10 ; -1 ; -20 ; 100 ; 0 }' '(Some 100)'
assert_output $contract_dir/max_in_list.tz Unit \
			  '{ -10 ; -1 ; -20 ; -100 }' '(Some -1)'

# Identity on lists
assert_output $contract_dir/list_id.tz Unit '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_output $contract_dir/list_id.tz Unit '{}' '{}'
assert_output $contract_dir/list_id.tz Unit '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'

assert_output $contract_dir/list_id_map.tz Unit '{ "1" ; "2" ; "3" }' '{ "1" ; "2" ; "3" }'
assert_output $contract_dir/list_id_map.tz Unit '{}' '{}'
assert_output $contract_dir/list_id_map.tz Unit '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'


# Identity on maps
assert_output $contract_dir/map_id.tz Unit '{ Elt 0 1 }' '{ Elt 0 1 }'
assert_output $contract_dir/map_id.tz Unit '{ Elt 0 0 }' '{ Elt 0 0 }'
assert_output $contract_dir/map_id.tz Unit '{ Elt 0 0 ; Elt 3 4 }' '{ Elt 0 0 ; Elt 3 4 }'

# Map block on lists
assert_output $contract_dir/list_map_block.tz Unit '{}' '{}'
assert_output $contract_dir/list_map_block.tz Unit '{ 1 ; 1 ; 1 ; 1 }' '{ 1 ; 2 ; 3 ; 4 }'
assert_output $contract_dir/list_map_block.tz Unit '{ 1 ; 2 ; 3 ; 0 }' '{ 1 ; 3 ; 5 ; 3 }'

# List iter
assert_output $contract_dir/list_iter.tz Unit '{ 10 ; 2 ; 1 }' 20
assert_output $contract_dir/list_iter.tz Unit '{ 3 ; 6 ; 9 }' 162

assert_output $contract_dir/list_iter2.tz Unit '{ "a" ; "b" ; "c" }' '"cba"'
assert_output $contract_dir/list_iter2.tz Unit '{}' '""'


# Identity on sets
assert_output $contract_dir/set_id.tz Unit '{ "a" ; "b" ; "c" }' '{ "a" ; "b" ; "c" }'
assert_output $contract_dir/set_id.tz Unit '{}' '{}'
assert_output $contract_dir/set_id.tz Unit '{ "asdf" ; "bcde" }' '{ "asdf" ; "bcde" }'

# Set member -- set is in storage
assert_output $contract_dir/set_member.tz '{}' '"Hi"' 'False'
assert_output $contract_dir/set_member.tz '{ "Hi" }' '"Hi"' 'True'
assert_output $contract_dir/set_member.tz '{ "Hello" ; "World" }' '""' 'False'

# Set size
assert_output $contract_dir/set_size.tz Unit '{}' 0
assert_output $contract_dir/set_size.tz Unit '{ 1 }' 1
assert_output $contract_dir/set_size.tz Unit '{ 1 ; 2 ; 3 }' 3
assert_output $contract_dir/set_size.tz Unit '{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }' 6

# Set iter
assert_output $contract_dir/set_iter.tz Unit '{}' 0
assert_output $contract_dir/set_iter.tz Unit '{ 1 }' 1
assert_output $contract_dir/set_iter.tz Unit '{ -100 ; 1 ; 2 ; 3 }' '-94'

# Map size
assert_output $contract_dir/map_size.tz Unit '{}' 0
assert_output $contract_dir/map_size.tz Unit '{ Elt "a" 1 }' 1
assert_output $contract_dir/map_size.tz Unit \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 }' 3
assert_output $contract_dir/map_size.tz Unit \
              '{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 ; Elt "d" 4 ; Elt "e" 5 ; Elt "f" 6 }' 6

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_output $contract_dir/contains_all.tz \
			  Unit '(Pair {} {})' 'True'
assert_output $contract_dir/contains_all.tz \
			  Unit '(Pair { "a" } { "B" })' 'False'
assert_output $contract_dir/contains_all.tz \
			  Unit '(Pair { "A" } { "B" })' 'False'
assert_output $contract_dir/contains_all.tz \
			  Unit '(Pair { "B" } { "B" })' 'True'
assert_output $contract_dir/contains_all.tz Unit \
			  '(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })' 'True'
assert_output $contract_dir/contains_all.tz Unit \
			  '(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })' 'True'

# Concatenate the string in storage with all strings in the given list
assert_output $contract_dir/concat_hello.tz Unit \
			  '{ "World!" }' '{ "Hello World!" }'
assert_output $contract_dir/concat_hello.tz Unit \
			  '{}' '{}'
assert_output $contract_dir/concat_hello.tz Unit \
			  '{ "test1" ; "test2" }' '{ "Hello test1" ; "Hello test2" }'

# Create an empty map and add a string to it
assert_output $contract_dir/empty_map.tz Unit Unit \
			  '{ Elt "hello" "world" }'

# Get the value stored at the given key in the map
assert_output $contract_dir/get_map_value.tz '{ Elt "hello" "hi" }' \
			  '"hello"' '(Some "hi")'
assert_output $contract_dir/get_map_value.tz '{ Elt "hello" "hi" }' \
			  '""' 'None'
assert_output $contract_dir/get_map_value.tz \
			  '{ Elt "1" "one" ; Elt "2" "two" }' \
			  '"1"' '(Some "one")'

# Map iter
assert_output $contract_dir/map_iter.tz Unit '{ Elt 0 100 ; Elt 2 100 }' '(Pair 2 200)'
assert_output $contract_dir/map_iter.tz Unit '{ Elt 1 1 ; Elt 2 100 }' '(Pair 3 101)'

# Return True if True branch of if was taken and False otherwise
assert_output $contract_dir/if.tz Unit True True
assert_output $contract_dir/if.tz Unit False False

# Generate a pair of or types
assert_output $contract_dir/swap_left_right.tz Unit '(Left True)' '(Right True)'
assert_output $contract_dir/swap_left_right.tz Unit '(Right "a")' '(Left "a")'

# Reverse a list
assert_output $contract_dir/reverse.tz Unit '{}' '{}'
assert_output $contract_dir/reverse.tz Unit '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'
assert_output $contract_dir/reverse_loop.tz Unit '{}' '{}'
assert_output $contract_dir/reverse_loop.tz Unit '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Reverse using LOOP_LEFT
assert_output $contract_dir/loop_left.tz Unit '{}' '{}'
assert_output $contract_dir/loop_left.tz Unit '{ "c" ; "b" ; "a" }' '{ "a" ; "b" ; "c" }'

# Exec concat contract
assert_output $contract_dir/exec_concat.tz Unit '""' '"_abc"'
assert_output $contract_dir/exec_concat.tz Unit '"test"' '"test_abc"'

# Get current steps to quota
assert_output $contract_dir/steps_to_quota.tz Unit Unit 39989

# Get the current balance of the contract
assert_output $contract_dir/balance.tz Unit Unit '"4,000,000"'

# Test comparisons on tez { EQ ; GT ; LT ; GE ; LE }
assert_output $contract_dir/compare.tz Unit '(Pair "1.00" "2.00")' '{ False ; False ; True ; False ; True }'
assert_output $contract_dir/compare.tz Unit '(Pair "2.00" "1.00")' '{ False ; True ; False ; True ; False }'
assert_output $contract_dir/compare.tz Unit '(Pair "2.37" "2.37")' '{ True ; False ; False ; True ; True }'

# Test addition and subtraction on tez
assert_output $contract_dir/tez_add_sub.tz Unit '(Pair "2" "1")' '(Pair "3" "1")'
assert_output $contract_dir/tez_add_sub.tz Unit '(Pair "2.31" "1.01")' '(Pair "3.32" "1.3")'

# Test get first element of list
assert_output $contract_dir/first.tz Unit '{ 1 ; 2 ; 3 ; 4 }' '1'
assert_output $contract_dir/first.tz Unit '{ 4 }' '4'

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_output $contract_dir/hash_string.tz Unit '"abcdefg"' '"exprv3MnhXvjthGzZ7jDtXRRFremZyey9rsGtL7JRkeaQX1fThN7WF"'
assert_output $contract_dir/hash_string.tz Unit '"12345"' '"expru81QVHsW2qaWLNHnMHSxDNhqtat17ajadri6mKUvXyc2EWHZC3"'

# Test ASSERT
assert_output $contract_dir/assert.tz Unit True Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert.tz on storage Unit and input False

# COMPARE; ASSERT_
assert_output $contract_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_neq.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_neq.tz on storage Unit and input '(Pair -1 -1)'

assert_output $contract_dir/assert_lt.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_lt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails ${TZCLIENT} run program $contract_dir/assert_lt.tz on storage Unit and input '(Pair 0 0)'

assert_output $contract_dir/assert_le.tz Unit '(Pair 0 0)' Unit
assert_output $contract_dir/assert_le.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_le.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_gt.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_gt.tz on storage Unit and input '(Pair -1 0)'
assert_fails ${TZCLIENT} run program $contract_dir/assert_gt.tz on storage Unit and input '(Pair 0 0)'

assert_output $contract_dir/assert_ge.tz Unit '(Pair 0 0)' Unit
assert_output $contract_dir/assert_ge.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_ge.tz on storage Unit and input '(Pair -1 0)'

# ASSERT_CMP
assert_output $contract_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_cmpneq.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpneq.tz on storage Unit and input '(Pair -1 -1)'

assert_output $contract_dir/assert_cmplt.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmplt.tz on storage Unit and input '(Pair 0 0)'

assert_output $contract_dir/assert_cmple.tz Unit '(Pair 0 0)' Unit
assert_output $contract_dir/assert_cmple.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmple.tz on storage Unit and input '(Pair 0 -1)'

assert_output $contract_dir/assert_cmpgt.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpgt.tz on storage Unit and input '(Pair -1 0)'
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpgt.tz on storage Unit and input '(Pair 0 0)'

assert_output $contract_dir/assert_cmpge.tz Unit '(Pair 0 0)' Unit
assert_output $contract_dir/assert_cmpge.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $contract_dir/assert_cmpge.tz on storage Unit and input '(Pair -1 0)'

# IF_SOME
assert_output $contract_dir/if_some.tz Unit '(Some "hello")' '"hello"'
assert_output $contract_dir/if_some.tz Unit 'None' '""'

# Tests the SET_CAR and SET_CDR instructions
assert_output  $contract_dir/set_car.tz '(Pair "hello" 0)' '"world"' '(Pair "world" 0)'
assert_output  $contract_dir/set_car.tz '(Pair "hello" 0)' '"abc"' '(Pair "abc" 0)'
assert_output  $contract_dir/set_car.tz '(Pair "hello" 0)' '""' '(Pair "" 0)'

assert_output  $contract_dir/set_cdr.tz '(Pair "hello" 0)' '1' '(Pair "hello" 1)'
assert_output  $contract_dir/set_cdr.tz '(Pair "hello" 500)' '3' '(Pair "hello" 3)'
assert_output  $contract_dir/set_cdr.tz '(Pair "hello" 7)' '100' '(Pair "hello" 100)'

assert_storage  $contract_dir/set_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "0") 4) 5))) 6)' \
'"3"' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "3") 4) 5))) 6)'

assert_storage  $contract_dir/map_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "0") 4) 5))) 6)' \
'Unit' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "1") 4) 5))) 6)'

# Did the given key sign the string? (key is bootstrap1)
assert_output $contract_dir/check_signature.tz \
'(Pair "1f19f8f37e80d96797b019f30d23ede6a26a0f698220f942103a3401f047623746e51a9c6e77e269b5df9593994ab96b001aae0f73728a2259187cb640b61e01" "hello")' \
'"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' True

assert_output $contract_dir/check_signature.tz \
'(Pair "1f19f8f37e80d96797b019f30d23ede6a26a0f698220f942103a3401f047623746e51a9c6e77e269b5df9593994ab96b001aae0f73728a2259187cb640b61e01" "abcd")' \
'"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' False

# Convert a public key to a public key hash
assert_output $contract_dir/hash_key.tz Unit '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'
assert_output $contract_dir/hash_key.tz Unit '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' '"tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k"'


bake_after $client transfer 1,000 from bootstrap1 to $key1
bake_after $client transfer 2,000 from bootstrap1 to $key2

assert_balance $key1 "1,000 ꜩ"
assert_balance $key2 "2,000 ꜩ"

# Create a contract and transfer 100 ꜩ to it
init_with_transfer $contract_dir/store_input.tz $key1 '""' 100 bootstrap1
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"abcdefg"'
assert_balance store_input "200 ꜩ"
assert_storage_contains store_input '"abcdefg"'
bake_after $client transfer 100 from bootstrap1 to store_input -arg '"xyz"'
assert_storage_contains store_input '"xyz"'

init_with_transfer $contract_dir/transfer_amount.tz $key1 '"0"' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to transfer_amount -arg Unit
assert_storage_contains transfer_amount 500

# This tests the `NOW` instruction.
# This test may fail if timings are marginal, though I have not yet seen this happen
init_with_transfer $contract_dir/store_now.tz $key1 '"2017-07-13T09:19:01Z"' "100" bootstrap1
bake_after $client transfer 500 from bootstrap1 to store_now -arg Unit
assert_storage_contains store_now "$($client get timestamp)"

# Test timestamp operations
assert_output $contract_dir/add_timestamp_delta.tz Unit '(Pair 100 100)' '"1970-01-01T00:03:20Z"'
assert_output $contract_dir/add_timestamp_delta.tz Unit '(Pair 100 -100)' '"1970-01-01T00:00:00Z"'
assert_output $contract_dir/add_timestamp_delta.tz Unit '(Pair "1970-01-01T00:00:00Z" 0)' '"1970-01-01T00:00:00Z"'

assert_output $contract_dir/add_delta_timestamp.tz Unit '(Pair 100 100)' '"1970-01-01T00:03:20Z"'
assert_output $contract_dir/add_delta_timestamp.tz Unit '(Pair -100 100)' '"1970-01-01T00:00:00Z"'
assert_output $contract_dir/add_delta_timestamp.tz Unit '(Pair 0 "1970-01-01T00:00:00Z")' '"1970-01-01T00:00:00Z"'

assert_output $contract_dir/sub_timestamp_delta.tz Unit '(Pair 100 100)' '"1970-01-01T00:00:00Z"'
assert_output $contract_dir/sub_timestamp_delta.tz Unit '(Pair 100 -100)' '"1970-01-01T00:03:20Z"'
assert_output $contract_dir/sub_timestamp_delta.tz Unit '(Pair 100 2000000000000000000)' -1999999999999999900

assert_output $contract_dir/diff_timestamps.tz Unit '(Pair 0 0)' 0
assert_output $contract_dir/diff_timestamps.tz Unit '(Pair 0 1)' -1
assert_output $contract_dir/diff_timestamps.tz Unit '(Pair 1 0)' 1
assert_output $contract_dir/diff_timestamps.tz Unit '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200


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


# Tests create_account
init_with_transfer $contract_dir/create_account.tz $key2 \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1,000 bootstrap1
$client transfer 100 from bootstrap1 to create_account \
           -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' | assert_in_output "New contract"
bake

# Creates a contract, transfers data to it and stores the data
init_with_transfer $contract_dir/create_contract.tz $key2 \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to create_contract -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'
assert_storage_contains create_contract '"abcdefg"'

# Test DEFAULT_ACCOUNT
init_with_transfer $contract_dir/default_account.tz $key1 \
				   Unit 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\""
assert_balance $BOOTSTRAP4_IDENTITY "4,000,100 ꜩ"
account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
bake_after $client transfer 0 from bootstrap1 to default_account  -arg "\"$account\""
assert_balance $account "100 ꜩ"

# Test SELF
init_with_transfer $contract_dir/self.tz $key1 \
				   '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' 1,000 bootstrap1
bake_after $client transfer 0 from bootstrap1 to self
assert_storage_contains self "\"$(get_contract_addr self)\""

# Test sets and map literals
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 0 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }' against type '(map nat nat)'
assert_fails $client typecheck data '{ "A" ; "C" ; "B" }' against type '(set string)'
assert_fails $client typecheck data '{ "A" ; "B" ; "B" }' against type '(set string)'

# Test hash consistency between Michelson and the CLI
hash_result=`$client hash data '(Pair "22220.00" (Pair "2017-12-13T04:49:00Z" 034))' \
                     of type '(pair tez (pair timestamp int))' | grep expr`

assert_output $contract_dir/hash_consistency_checker.tz Unit \
              '(Pair "22220.00" (Pair "2017-12-13T04:49:00Z" 034))' "$hash_result"

assert_output $contract_dir/hash_consistency_checker.tz Unit \
              '(Pair "22,220" (Pair "2017-12-13T04:49:00+00:00" 34))' "$hash_result"

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
