#!/bin/bash

set -e
set -o pipefail

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/lib/test_lib.inc.sh "$@"

start_node 1
activate_alpha

sleep 2
$client bake for bootstrap5 -max-priority 512

key1=foo
key2=bar

$client gen keys $key1
$client gen keys $key2

CONTRACT_PATH=contracts

printf "\n\n"

# Assert well typed
echo "Typechecking contracts in '${CONTRACT_PATH}'"
ls $CONTRACT_PATH \
    | xargs -I{} $client typecheck program $CONTRACT_PATH/{} > /dev/null

printf "All contracts are well typed\n\n"

# Assert all contracts typecheck
for contract in `ls $CONTRACT_PATH/*.tz`; do
    printf "[Typechecking %s]\n" "$contract";
    ${client} typecheck program "$contract";
done

# FORMAT: assert_output contract_file storage input expected_result

assert_output $CONTRACT_PATH/ret_int.tz Unit Unit 300

# Identity on strings
assert_output $CONTRACT_PATH/str_id.tz Unit '"Hello"' '"Hello"'
assert_output $CONTRACT_PATH/str_id.tz Unit '"abcd"' '"abcd"'

# Identity on pairs
assert_output $CONTRACT_PATH/pair_id.tz Unit '(Pair True False)' '(Pair True False)'
assert_output $CONTRACT_PATH/pair_id.tz Unit '(Pair False True)' '(Pair False True)'
assert_output $CONTRACT_PATH/pair_id.tz Unit '(Pair True True)' '(Pair True True)'
assert_output $CONTRACT_PATH/pair_id.tz Unit '(Pair False False)' '(Pair False False)'

# Logical not
assert_output $CONTRACT_PATH/not.tz Unit True False
assert_output $CONTRACT_PATH/not.tz Unit False True

# Logical and
assert_output $CONTRACT_PATH/and.tz Unit "(Pair False False)" False
assert_output $CONTRACT_PATH/and.tz Unit "(Pair False True)" False
assert_output $CONTRACT_PATH/and.tz Unit "(Pair True False)" False
assert_output $CONTRACT_PATH/and.tz Unit "(Pair True True)" True

# Logical or
assert_output $CONTRACT_PATH/or.tz Unit "(Pair False False)" False
assert_output $CONTRACT_PATH/or.tz Unit "(Pair False True)" True
assert_output $CONTRACT_PATH/or.tz Unit "(Pair True False)" True
assert_output $CONTRACT_PATH/or.tz Unit "(Pair True True)" True

# XOR
assert_output $CONTRACT_PATH/xor.tz Unit "(Pair False False)" False
assert_output $CONTRACT_PATH/xor.tz Unit "(Pair False True)" True
assert_output $CONTRACT_PATH/xor.tz Unit "(Pair True False)" True
assert_output $CONTRACT_PATH/xor.tz Unit "(Pair True True)" False


# Build list
assert_output $CONTRACT_PATH/build_list.tz Unit 0 "(List 0)"
assert_output $CONTRACT_PATH/build_list.tz Unit 3 "(List 0 1 2 3)"
assert_output $CONTRACT_PATH/build_list.tz Unit 10 \
			  "(List 0 1 2 3 4 5 6 7 8 9 10)"

# Concatenate all strings of a list into one string
assert_output $CONTRACT_PATH/concat_list.tz Unit '(List "a" "b" "c")' '"abc"'
assert_output $CONTRACT_PATH/concat_list.tz Unit '(List )' '""'
assert_output $CONTRACT_PATH/concat_list.tz \
			  Unit '(List "Hello" " " "World" "!")' '"Hello World!"'

# Find maximum int in list -- returns None if not found
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List)' 'None'
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List 1)' '(Some 1)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List -1)' '(Some -1)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List 10 -1 -20 100 0)' '(Some 100)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List 10 -1 -20 100 0)' '(Some 100)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List -10 -1 -20 -100)' '(Some -1)'

# Identity on lists
assert_output $CONTRACT_PATH/list_id.tz Unit '(List "1" "2" "3")' '(List "1" "2" "3")'
assert_output $CONTRACT_PATH/list_id.tz Unit '(List)' 'List'
assert_output $CONTRACT_PATH/list_id.tz Unit '(List "a" "b" "c")' '(List "a" "b" "c")'

assert_output $CONTRACT_PATH/list_id_map.tz Unit '(List "1" "2" "3")' '(List "1" "2" "3")'
assert_output $CONTRACT_PATH/list_id_map.tz Unit '(List)' 'List'
assert_output $CONTRACT_PATH/list_id_map.tz Unit '(List "a" "b" "c")' '(List "a" "b" "c")'


# Identity on maps
assert_output $CONTRACT_PATH/map_id.tz Unit '(Map (Item 0 1))' '(Map (Item 0 1))'
assert_output $CONTRACT_PATH/map_id.tz Unit '(Map (Item 0 0))' '(Map (Item 0 0))'
assert_output $CONTRACT_PATH/map_id.tz Unit '(Map (Item 0 0) (Item 3 4))' '(Map (Item 0 0) (Item 3 4))'

# Map block on lists
assert_output $CONTRACT_PATH/list_map_block.tz Unit '(List)' 'List'
assert_output $CONTRACT_PATH/list_map_block.tz Unit '(List 1 1 1 1)' '(List 1 2 3 4)'
assert_output $CONTRACT_PATH/list_map_block.tz Unit '(List 1 2 3 0)' '(List 1 3 5 3)'

# List iter
assert_output $CONTRACT_PATH/list_iter.tz Unit '(List 10 2 1)' 20
assert_output $CONTRACT_PATH/list_iter.tz Unit '(List 3 6 9)' 162

assert_output $CONTRACT_PATH/list_iter2.tz Unit '(List "a" "b" "c")' '"cba"'
assert_output $CONTRACT_PATH/list_iter2.tz Unit '(List)' '""'


# Identity on sets
assert_output $CONTRACT_PATH/set_id.tz Unit '(Set "a" "b" "c")' '(Set "a" "b" "c")'
assert_output $CONTRACT_PATH/set_id.tz Unit '(Set)' 'Set'
assert_output $CONTRACT_PATH/set_id.tz Unit '(Set "asdf" "bcde")' '(Set "asdf" "bcde")'

# Set member -- set is in storage
assert_output $CONTRACT_PATH/set_member.tz '(Set)' '"Hi"' 'False'
assert_output $CONTRACT_PATH/set_member.tz '(Set "Hi")' '"Hi"' 'True'
assert_output $CONTRACT_PATH/set_member.tz '(Set "Hello" "World")' '""' 'False'

# Set size
assert_output $CONTRACT_PATH/set_size.tz Unit '(Set)' 0
assert_output $CONTRACT_PATH/set_size.tz Unit '(Set 1)' 1
assert_output $CONTRACT_PATH/set_size.tz Unit '(Set 1 2 3)' 3
assert_output $CONTRACT_PATH/set_size.tz Unit '(Set 1 2 3 4 5 6)' 6

# Set iter
assert_output $CONTRACT_PATH/set_iter.tz Unit '(Set)' 0
assert_output $CONTRACT_PATH/set_iter.tz Unit '(Set 1)' 1
assert_output $CONTRACT_PATH/set_iter.tz Unit '(Set -100 1 2 3)' '-94'

# Map size
assert_output $CONTRACT_PATH/map_size.tz Unit '(Map)' 0
assert_output $CONTRACT_PATH/map_size.tz Unit '(Map (Item "a" 1))' 1
assert_output $CONTRACT_PATH/map_size.tz Unit \
              '(Map (Item "a" 1) (Item "b" 2) (Item "c" 3))' 3
assert_output $CONTRACT_PATH/map_size.tz Unit \
              '(Map (Item "a" 1) (Item "b" 2) (Item "c" 3) (Item "d" 4) (Item "e" 5) (Item "f" 6))' 6

# Contains all elements -- does the second list contain all of the same elements
# as the first one? I'm ignoring element multiplicity
assert_output $CONTRACT_PATH/contains_all.tz \
			  Unit '(Pair (List) (List))' 'True'
assert_output $CONTRACT_PATH/contains_all.tz \
			  Unit '(Pair (List "a") (List "B"))' 'False'
assert_output $CONTRACT_PATH/contains_all.tz \
			  Unit '(Pair (List "A") (List "B"))' 'False'
assert_output $CONTRACT_PATH/contains_all.tz \
			  Unit '(Pair (List "B") (List "B"))' 'True'
assert_output $CONTRACT_PATH/contains_all.tz Unit \
			  '(Pair (List "B" "C" "asdf") (List "B" "B" "asdf" "C"))' 'True'
assert_output $CONTRACT_PATH/contains_all.tz Unit \
			  '(Pair (List "B" "B" "asdf" "C") (List "B" "C" "asdf"))' 'True'

# Concatenate the string in storage with all strings in the given list
assert_output $CONTRACT_PATH/concat_hello.tz Unit \
			  '(List "World!")' '(List "Hello World!")'
assert_output $CONTRACT_PATH/concat_hello.tz Unit \
			  '(List)' 'List'
assert_output $CONTRACT_PATH/concat_hello.tz Unit \
			  '(List "test1" "test2")' '(List "Hello test1" "Hello test2")'

# Create an empty map and add a string to it
assert_output $CONTRACT_PATH/empty_map.tz Unit Unit \
			  '(Map (Item "hello" "world"))'

# Get the value stored at the given key in the map
assert_output $CONTRACT_PATH/get_map_value.tz '(Map (Item "hello" "hi"))' \
			  '"hello"' '(Some "hi")'
assert_output $CONTRACT_PATH/get_map_value.tz '(Map (Item "hello" "hi"))' \
			  '""' 'None'
assert_output $CONTRACT_PATH/get_map_value.tz \
			  '(Map (Item "1" "one") (Item "2" "two"))' \
			  '"1"' '(Some "one")'

# Map iter
assert_output $CONTRACT_PATH/map_iter.tz Unit '(Map (Item 0 100) (Item 2 100))' '(Pair 2 200)'
assert_output $CONTRACT_PATH/map_iter.tz Unit '(Map (Item 1 1) (Item 2 100))' '(Pair 3 101)'

# Return True if True branch of if was taken and False otherwise
assert_output $CONTRACT_PATH/if.tz Unit True True
assert_output $CONTRACT_PATH/if.tz Unit False False

# Generate a pair of or types
assert_output $CONTRACT_PATH/swap_left_right.tz Unit '(Left True)' '(Right True)'
assert_output $CONTRACT_PATH/swap_left_right.tz Unit '(Right "a")' '(Left "a")'

# Reverse a list
assert_output $CONTRACT_PATH/reverse.tz Unit '(List )' 'List'
assert_output $CONTRACT_PATH/reverse.tz Unit '(List "c" "b" "a")' '(List "a" "b" "c")'
assert_output $CONTRACT_PATH/reverse_loop.tz Unit '(List )' 'List'
assert_output $CONTRACT_PATH/reverse_loop.tz Unit '(List "c" "b" "a")' '(List "a" "b" "c")'

# Reverse using LOOP_LEFT
assert_output $CONTRACT_PATH/loop_left.tz Unit '(List )' 'List'
assert_output $CONTRACT_PATH/loop_left.tz Unit '(List "c" "b" "a")' '(List "a" "b" "c")'

# Exec concat contract
assert_output $CONTRACT_PATH/exec_concat.tz Unit '""' '"_abc"'
assert_output $CONTRACT_PATH/exec_concat.tz Unit '"test"' '"test_abc"'

# Get current steps to quota
assert_output $CONTRACT_PATH/steps_to_quota.tz Unit Unit 16382

# Get the current balance of the contract
assert_output $CONTRACT_PATH/balance.tz Unit Unit '"4,000,000.00"'

# Test comparisons on tez (List EQ GT LT GE LE)
assert_output $CONTRACT_PATH/compare.tz Unit '(Pair "1.00" "2.00")' '(List False False True False True)'
assert_output $CONTRACT_PATH/compare.tz Unit '(Pair "2.00" "1.00")' '(List False True False True False)'
assert_output $CONTRACT_PATH/compare.tz Unit '(Pair "2.37" "2.37")' '(List True False False True True)'

# Test addition and subtraction on tez
assert_output $CONTRACT_PATH/tez_add_sub.tz Unit '(Pair "2.00" "1.00")' '(Pair "3.00" "1.00")'
assert_output $CONTRACT_PATH/tez_add_sub.tz Unit '(Pair "2.31" "1.01")' '(Pair "3.32" "1.30")'

# Test get first element of list
assert_output $CONTRACT_PATH/first.tz Unit '(List 1 2 3 4)' '1'
assert_output $CONTRACT_PATH/first.tz Unit '(List 4)' '4'

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_output $CONTRACT_PATH/hash_string.tz Unit '"abcdefg"' '"exprv3MnhXvjthGzZ7jDtXRRFremZyey9rsGtL7JRkeaQX1fThN7WF"'
assert_output $CONTRACT_PATH/hash_string.tz Unit '"12345"' '"expru81QVHsW2qaWLNHnMHSxDNhqtat17ajadri6mKUvXyc2EWHZC3"'

# Test ASSERT
assert_output $CONTRACT_PATH/assert.tz Unit True Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert.tz on storage Unit and input False

# COMPARE; ASSERT_
assert_output $CONTRACT_PATH/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_eq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_eq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_neq.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_neq.tz on storage Unit and input '(Pair -1 -1)'

assert_output $CONTRACT_PATH/assert_lt.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_lt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_lt.tz on storage Unit and input '(Pair 0 0)'

assert_output $CONTRACT_PATH/assert_le.tz Unit '(Pair 0 0)' Unit
assert_output $CONTRACT_PATH/assert_le.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_le.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_gt.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_gt.tz on storage Unit and input '(Pair -1 0)'
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_gt.tz on storage Unit and input '(Pair 0 0)'

assert_output $CONTRACT_PATH/assert_ge.tz Unit '(Pair 0 0)' Unit
assert_output $CONTRACT_PATH/assert_ge.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_ge.tz on storage Unit and input '(Pair -1 0)'

# ASSERT_CMP
assert_output $CONTRACT_PATH/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_cmpeq.tz Unit '(Pair -1 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpeq.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_cmpneq.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpneq.tz on storage Unit and input '(Pair -1 -1)'

assert_output $CONTRACT_PATH/assert_cmplt.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmplt.tz on storage Unit and input '(Pair 0 -1)'
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmplt.tz on storage Unit and input '(Pair 0 0)'

assert_output $CONTRACT_PATH/assert_cmple.tz Unit '(Pair 0 0)' Unit
assert_output $CONTRACT_PATH/assert_cmple.tz Unit '(Pair -1 0)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmple.tz on storage Unit and input '(Pair 0 -1)'

assert_output $CONTRACT_PATH/assert_cmpgt.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpgt.tz on storage Unit and input '(Pair -1 0)'
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpgt.tz on storage Unit and input '(Pair 0 0)'

assert_output $CONTRACT_PATH/assert_cmpge.tz Unit '(Pair 0 0)' Unit
assert_output $CONTRACT_PATH/assert_cmpge.tz Unit '(Pair 0 -1)' Unit
assert_fails ${TZCLIENT} run program $CONTRACT_PATH/assert_cmpge.tz on storage Unit and input '(Pair -1 0)'

# IF_SOME
assert_output $CONTRACT_PATH/if_some.tz Unit '(Some "hello")' '"hello"'
assert_output $CONTRACT_PATH/if_some.tz Unit 'None' '""'

# Tests the SET_CAR and SET_CDR instructions
assert_output  $CONTRACT_PATH/set_car.tz '(Pair "hello" 0)' '"world"' '(Pair "world" 0)'
assert_output  $CONTRACT_PATH/set_car.tz '(Pair "hello" 0)' '"abc"' '(Pair "abc" 0)'
assert_output  $CONTRACT_PATH/set_car.tz '(Pair "hello" 0)' '""' '(Pair "" 0)'

assert_output  $CONTRACT_PATH/set_cdr.tz '(Pair "hello" 0)' '1' '(Pair "hello" 1)'
assert_output  $CONTRACT_PATH/set_cdr.tz '(Pair "hello" 500)' '3' '(Pair "hello" 3)'
assert_output  $CONTRACT_PATH/set_cdr.tz '(Pair "hello" 7)' '100' '(Pair "hello" 100)'

assert_storage  $CONTRACT_PATH/set_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "0.00") 4) 5))) 6)' \
'"3.00"' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "3.00") 4) 5))) 6)'

assert_storage  $CONTRACT_PATH/map_caddaadr.tz \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "0.00") 4) 5))) 6)' \
'Unit' \
'(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 "1.00") 4) 5))) 6)'

# Did the given key sign the string? (key is bootstrap1)
assert_output $CONTRACT_PATH/check_signature.tz \
'(Pair "26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309" "hello")' \
'"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' True

assert_output $CONTRACT_PATH/check_signature.tz \
'(Pair "26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309" "abcd")' \
'"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' False

# Convert a public key to a public key hash
assert_output $CONTRACT_PATH/hash_key.tz Unit '"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"' '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'
assert_output $CONTRACT_PATH/hash_key.tz Unit '"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"' '"tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k"'

$client transfer 1000 from bootstrap1 to $key1
$client transfer 2000 from bootstrap1 to $key2

assert_balance $key1 "1,000.00 ꜩ"
assert_balance $key2 "2,000.00 ꜩ"

# Create a contract and transfer 100 ꜩ to it
init_with_transfer $CONTRACT_PATH/store_input.tz $key1 '""' 100 bootstrap1
$client transfer 100 from bootstrap1 to store_input -arg '"abcdefg"'
assert_balance store_input "200.00 ꜩ"
assert_storage_contains store_input '"abcdefg"'
$client transfer 100 from bootstrap1 to store_input -arg '"xyz"'
assert_storage_contains store_input '"xyz"'

init_with_transfer $CONTRACT_PATH/transfer_amount.tz $key1 '"0"' "100" bootstrap1
$client transfer 500 from bootstrap1 to transfer_amount -arg Unit
assert_storage_contains transfer_amount 500

# This tests the `NOW` instruction.
# This test may fail if timings are marginal, though I have not yet seen this happen
init_with_transfer $CONTRACT_PATH/store_now.tz $key1 '"2017-07-13T09:19:01Z"' "100" bootstrap1
$client transfer 500 from bootstrap1 to store_now -arg Unit
assert_storage_contains store_now "$($client get timestamp)"

# Test timestamp operations
assert_output $CONTRACT_PATH/add_timestamp_delta.tz Unit '(Pair 100 100)' '"1970-01-01T00:03:20Z"'
assert_output $CONTRACT_PATH/add_timestamp_delta.tz Unit '(Pair 100 -100)' '"1970-01-01T00:00:00Z"'
assert_output $CONTRACT_PATH/add_timestamp_delta.tz Unit '(Pair "1970-01-01T00:00:00Z" 0)' '"1970-01-01T00:00:00Z"'

assert_output $CONTRACT_PATH/add_delta_timestamp.tz Unit '(Pair 100 100)' '"1970-01-01T00:03:20Z"'
assert_output $CONTRACT_PATH/add_delta_timestamp.tz Unit '(Pair -100 100)' '"1970-01-01T00:00:00Z"'
assert_output $CONTRACT_PATH/add_delta_timestamp.tz Unit '(Pair 0 "1970-01-01T00:00:00Z")' '"1970-01-01T00:00:00Z"'

assert_output $CONTRACT_PATH/sub_timestamp_delta.tz Unit '(Pair 100 100)' '"1970-01-01T00:00:00Z"'
assert_output $CONTRACT_PATH/sub_timestamp_delta.tz Unit '(Pair 100 -100)' '"1970-01-01T00:03:20Z"'
assert_output $CONTRACT_PATH/sub_timestamp_delta.tz Unit '(Pair 100 2000000000000000000)' -1999999999999999900

assert_output $CONTRACT_PATH/diff_timestamps.tz Unit '(Pair 0 0)' 0
assert_output $CONTRACT_PATH/diff_timestamps.tz Unit '(Pair 0 1)' -1
assert_output $CONTRACT_PATH/diff_timestamps.tz Unit '(Pair 1 0)' 1
assert_output $CONTRACT_PATH/diff_timestamps.tz Unit '(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")' 200

# Tests TRANSFER_TO
$client originate account "test_transfer_account1" for $key1 transferring 100 from bootstrap1
$client originate account "test_transfer_account2" for $key1 transferring 20 from bootstrap1
init_with_transfer $CONTRACT_PATH/transfer_to.tz $key2 Unit 1000 bootstrap1
assert_balance test_transfer_account1 "100.00 ꜩ"
$client transfer 100 from bootstrap1 to transfer_to \
            -arg "\"$(get_contract_addr test_transfer_account1)\""
assert_balance test_transfer_account1 "200.00 ꜩ" # Why isn't this 200 ꜩ? Baking fee?
$client transfer 100 from bootstrap1 to transfer_to \
            -arg "\"$(get_contract_addr test_transfer_account2)\""
assert_balance test_transfer_account2 "120.00 ꜩ" # Why isn't this 120 ꜩ? Baking fee?

# Tests create_account
init_with_transfer $CONTRACT_PATH/create_account.tz $key2 \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1000 bootstrap1
$client transfer 100 from bootstrap1 to create_account \
            -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' | assert_in_output "New contract"

# Creates a contract, transfers data to it and stores the data
init_with_transfer $CONTRACT_PATH/create_contract.tz $key2 \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1000 bootstrap1
$client transfer 0.00 from bootstrap1 to create_contract -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'
assert_storage_contains create_contract '"abcdefg"'

# Test DEFAULT_ACCOUNT
init_with_transfer $CONTRACT_PATH/default_account.tz $key1 \
				   Unit 1000 bootstrap1
$client transfer 0.00 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\""
assert_balance $BOOTSTRAP4_IDENTITY "4,000,100.00 ꜩ"
account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
$client transfer 0.00 from bootstrap1 to default_account  -arg "\"$account\""
assert_balance $account "100.00 ꜩ"

assert_fails $client typecheck data '(Map (Item 0 1) (Item 0 1))' against type '(map nat nat)'
assert_fails $client typecheck data '(Map (Item 0 1) (Item 10 1) (Item 5 1))' against type '(map nat nat)'
assert_fails $client typecheck data '(Set "A" "C" "B")' against type '(set string)'
assert_fails $client typecheck data '(Set "A" "B" "B")' against type '(set string)'

printf "\nEnd of test\n"

show_logs="no"
