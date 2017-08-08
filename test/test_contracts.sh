#!/bin/bash

set -e

source test_utils.sh

CONTRACT_PATH=contracts

# FORMAT: assert_output contract_file storage input expected_result

assert_output $CONTRACT_PATH/ret_int.tz Unit Unit 300
assert_output $CONTRACT_PATH/str_id.tz Unit '"Hello"' '"Hello"'
assert_output $CONTRACT_PATH/str_id.tz Unit '"abcd"' '"abcd"'

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

# Find maximum int32 in list -- returns None if not found
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List)' 'None'
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List 1)' '(Some 1)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit '(List -1)' '(Some -1)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List 10 -1 -20 100 0)' '(Some 100)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List 10 -1 -20 100 0)' '(Some 100)'
assert_output $CONTRACT_PATH/max_in_list.tz Unit \
			  '(List -10 -1 -20 -100)' '(Some -1)'

# Set member -- set is in storage
assert_output $CONTRACT_PATH/set_member.tz '(Set)' '"Hi"' 'False'
assert_output $CONTRACT_PATH/set_member.tz '(Set "Hi")' '"Hi"' 'True'
assert_output $CONTRACT_PATH/set_member.tz '(Set "Hello" "World")' '""' 'False'

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

# Return True if True branch of if was taken and False otherwise
assert_output $CONTRACT_PATH/if.tz Unit True True
assert_output $CONTRACT_PATH/if.tz Unit False False

# Generate a pair of or types
assert_output $CONTRACT_PATH/swap_left_right.tz Unit '(Left True)' '(Right True)'
assert_output $CONTRACT_PATH/swap_left_right.tz Unit '(Right "a")' '(Left "a")'

# Reverse a list
assert_output $CONTRACT_PATH/reverse.tz Unit '(List )' 'List'
assert_output $CONTRACT_PATH/reverse.tz Unit '(List "c" "b" "a")' '(List "a" "b" "c")'

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

# Hash input string
# Test assumed to be correct -- hash is based on encoding of AST
assert_output $CONTRACT_PATH/hash_string.tz Unit '"abcdefg"' '"exprv3MnhXvjthGzZ7jDtXRRFremZyey9rsGtL7JRkeaQX1fThN7WF"'
assert_output $CONTRACT_PATH/hash_string.tz Unit '"12345"' '"expru81QVHsW2qaWLNHnMHSxDNhqtat17ajadri6mKUvXyc2EWHZC3"'

# Did the given key sign the string?
assert_output $CONTRACT_PATH/check_signature.tz \
'(Pair "26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309" "hello")' \
'"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' True

assert_output $CONTRACT_PATH/check_signature.tz \
'(Pair "26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309" "abcd")' \
'"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' False


${TZCLIENT} transfer 1000 from bootstrap1 to ${KEY1}
${TZCLIENT} transfer 2000 from bootstrap1 to ${KEY2}

assert_balance ${KEY1} "1,000.00 ꜩ"
assert_balance ${KEY2} "2,000.00 ꜩ"

# Create a contract and transfer 100 ꜩ to it
init_with_transfer $CONTRACT_PATH/store_input.tz ${KEY1} '""' 100 bootstrap1
${TZCLIENT} transfer 100 from bootstrap1 to store_input -arg '"abcdefg"'
assert_balance store_input "200.00 ꜩ"
assert_storage_contains store_input '"abcdefg"'
${TZCLIENT} transfer 100 from bootstrap1 to store_input -arg '"xyz"'
assert_storage_contains store_input '"xyz"'

init_with_transfer $CONTRACT_PATH/transfer_amount.tz ${KEY1} '"0"' "100" bootstrap1
${TZCLIENT} transfer 500 from bootstrap1 to transfer_amount -arg Unit
assert_storage_contains transfer_amount 500

# This tests the `NOW` instruction.
# This test may fail if timings are marginal, though I have not yet seen this happen
init_with_transfer $CONTRACT_PATH/store_now.tz ${KEY1} '"2017-07-13T09:19:01Z"' "100" bootstrap1
${TZCLIENT} transfer 500 from bootstrap1 to store_now -arg Unit
assert_storage_contains store_now "$(${TZCLIENT} get timestamp)"

# Tests TRANSFER_TO
${TZCLIENT} originate account "test_transfer_account1" for ${KEY1} transferring 100 from bootstrap1
${TZCLIENT} originate account "test_transfer_account2" for ${KEY1} transferring 20 from bootstrap1
init_with_transfer $CONTRACT_PATH/transfer_to.tz ${KEY2} Unit 1000 bootstrap1
assert_balance test_transfer_account1 "100.00 ꜩ"
${TZCLIENT} transfer 100 from bootstrap1 to transfer_to \
            -arg "\"$(get_contract_addr test_transfer_account1)\""
assert_balance test_transfer_account1 "200.00 ꜩ" # Why isn't this 200 ꜩ? Mining fee?
${TZCLIENT} transfer 100 from bootstrap1 to transfer_to \
            -arg "\"$(get_contract_addr test_transfer_account2)\""
assert_balance test_transfer_account2 "120.00 ꜩ" # Why isn't this 120 ꜩ? Mining fee?

# Tests create_account
init_with_transfer $CONTRACT_PATH/create_account.tz ${KEY2} \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1000 bootstrap1
${TZCLIENT} transfer 100 from bootstrap1 to create_account \
            -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"' | assert_in_output "New contract"

# Creates a contract, transfers data to it and stores the data
init_with_transfer $CONTRACT_PATH/create_contract.tz ${KEY2} \
                   "\"$(get_contract_addr test_transfer_account1)\"" 1000 bootstrap1
${TZCLIENT} transfer 0.00 from bootstrap1 to create_contract -arg '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'
assert_storage_contains create_contract '"abcdefg"'

# Test DEFAULT_ACCOUNT
init_with_transfer $CONTRACT_PATH/default_account.tz ${KEY1} \
				   Unit 1000 bootstrap1
${TZCLIENT} transfer 0.00 from bootstrap1 to default_account  -arg "\"$BOOTSTRAP4_IDENTITY\""
assert_balance $BOOTSTRAP4_IDENTITY "4,000,100.00 ꜩ"
account=tz1SuakBpFdG9b4twyfrSMqZzruxhpMeSrE5
${TZCLIENT} transfer 0.00 from bootstrap1 to default_account  -arg "\"$account\""
assert_balance $account "100.00 ꜩ"
